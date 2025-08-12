%%% @doc eorm_schema_inspector 最终覆盖率测试
%%% 针对未覆盖的行：346, 350, 355
%%% 这些行在 SQLite 索引信息解析函数中
%%% @end
-module(eorm_schema_inspector_final_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试用例
%%====================================================================

%% 测试行346：元组行格式转换为列名（SQLite index_info 解析）
sqlite_tuple_row_format_test() ->
    %% 模拟 SQLite PRAGMA index_info 返回元组格式的行
    %% 元组格式需要至少3个元素，第三个元素是列名
    MockRows = [
        {0, 0, <<"username">>},    %% 3元素元组格式 - 触发行346
        {1, 1, <<"email">>, extra}, %% 4元素元组格式 - 触发行346
        {2, 2, <<"created_at">>, extra1, extra2} %% 5元素元组格式 - 触发行346
    ],
    
    %% 模拟 eorm_adapter:query 的返回
    meck:new(eorm_adapter, [passthrough]),
    meck:expect(eorm_adapter, query, fun(sqlite, SQL, []) ->
        case string:str(SQL, "index_info") > 0 of
            true -> {ok, MockRows};
            false -> {ok, []}
        end
    end),
    
    try
        %% 调用私有函数 get_sqlite_index_info 来触发行346
        %% 参数格式：[Seq, Name, Unique, Origin, Partial]
        IndexRow = [0, <<"idx_user">>, 0, <<"c">>, 0],
        Result = apply(eorm_schema_inspector, get_sqlite_index_info, [IndexRow]),
        
        %% 验证结果：应该解析出索引列名  
        ?assertMatch([{idx_user, [username, email, created_at]}], Result)
        
    after
        meck:unload(eorm_adapter)
    end.

%% 测试行350：处理意外的行格式（SQLite index_info 中的 false 分支）
sqlite_unexpected_row_format_test() ->
    %% 模拟 SQLite PRAGMA index_info 返回意外格式的行
    MockRows = [
        unexpected_format,        %% 意外格式
        {incomplete_tuple},       %% 不完整的元组（少于3个元素）
        [1, 2],                  %% 列表格式但缺少第三个元素
        invalid_data,            %% 无效数据
        <<"string_instead_of_tuple">> %% 字符串而不是元组或列表
    ],
    
    meck:new(eorm_adapter, [passthrough]),
    meck:expect(eorm_adapter, query, fun(sqlite, SQL, []) ->
        case string:str(SQL, "index_info") > 0 of
            true -> {ok, MockRows};
            false -> {ok, []}
        end
    end),
    
    try
        %% 调用 get_sqlite_index_info 函数处理意外格式，应该触发行350的false分支
        IndexRow = [0, <<"idx_test">>, 0, <<"c">>, 0],
        Result = apply(eorm_schema_inspector, get_sqlite_index_info, [IndexRow]),
        
        %% 由于所有行都是意外格式，应该返回空的索引列信息
        %% 但索引名称仍应存在
        ?assertMatch([{idx_test, []}], Result)
        
    after
        meck:unload(eorm_adapter)
    end.

%% 测试行355：空结果集处理（SQLite index_info 查询失败或返回错误）
sqlite_query_failure_test() ->
    %% 模拟 SQLite PRAGMA index_info 查询失败
    meck:new(eorm_adapter, [passthrough]),
    meck:expect(eorm_adapter, query, fun(sqlite, SQL, []) ->
        case string:str(SQL, "index_info") > 0 of
            true -> {error, database_error}; %% 查询失败，触发行355
            false -> {ok, []}
        end
    end),
    
    try
        %% 调用 get_sqlite_index_info 函数处理查询失败，应该触发行355返回空列表
        IndexRow = [0, <<"idx_error">>, 0, <<"c">>, 0],
        Result = apply(eorm_schema_inspector, get_sqlite_index_info, [IndexRow]),
        
        %% 查询失败时应该返回空列表（触发行355）
        ?assertEqual([], Result)
        
    after
        meck:unload(eorm_adapter)
    end.

%% 测试纯元组格式：仅元组行来确保覆盖行346
pure_tuple_format_test() ->
    %% 只有元组格式的行，确保覆盖行346
    MockRows = [
        {0, 0, <<"col1">>},         %% 元组格式，触发行346  
        {1, 1, <<"col2">>},         %% 元组格式，触发行346
        {2, 2, <<"col3">>}          %% 元组格式，触发行346
    ],
    
    meck:new(eorm_adapter, [passthrough]),
    meck:expect(eorm_adapter, query, fun(sqlite, SQL, []) ->
        case string:str(SQL, "index_info") > 0 of
            true -> {ok, MockRows};
            false -> {ok, []}
        end
    end),
    
    try
        IndexRow = [0, <<"pure_tuple_idx">>, 0, <<"c">>, 0],
        Result = apply(eorm_schema_inspector, get_sqlite_index_info, [IndexRow]),
        
        %% 应该解析出3个列名，全部来自元组格式
        ?assertMatch([{pure_tuple_idx, [col1, col2, col3]}], Result)
        
    after
        meck:unload(eorm_adapter)
    end.

%% 测试混合场景：同时测试多种行格式
mixed_row_formats_test() ->
    %% 模拟混合的行格式，包含有效和无效的格式
    MockRows = [
        {0, 0, <<"valid_col1">>},    %% 有效的元组格式（触发行346）
        [1, 1, <<"valid_col2">>],    %% 有效的列表格式
        {incomplete},                %% 无效的元组（触发行350）
        [2, 2],                     %% 无效的列表（触发行350）
        invalid_row                  %% 完全无效的格式（触发行350）
    ],
    
    meck:new(eorm_adapter, [passthrough]),
    meck:expect(eorm_adapter, query, fun(sqlite, SQL, []) ->
        case string:str(SQL, "index_info") > 0 of
            true -> {ok, MockRows};
            false -> {ok, []}
        end
    end),
    
    try
        IndexRow = [0, <<"mixed_idx">>, 0, <<"c">>, 0],
        Result = apply(eorm_schema_inspector, get_sqlite_index_info, [IndexRow]),
        
        %% 应该只解析出有效的列名，忽略无效格式
        ?assertMatch([{mixed_idx, [valid_col1, valid_col2]}], Result)
        
    after
        meck:unload(eorm_adapter)
    end.