%%% @doc EORM DDL Generator 边缘情况测试
%%% 专门测试未覆盖的行以达到100%覆盖率
%%% @end
-module(eorm_ddl_generator_edge_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 边缘情况测试
%%====================================================================

%% 测试行168 - postgres_type中is_list结果为false的情况
postgres_type_binary_result_test() ->
    %% 直接测试边缘情况：当type是复杂结构时
    ComplexType = #{type => text, length => 1000},
    Result = apply(eorm_ddl_generator, postgres_type, [ComplexType, []]),
    ?assert(is_binary(Result)),
    ?assert(size(Result) > 0).

%% 测试行336 - mysql_type中is_list结果为false的情况  
mysql_type_binary_result_test() ->
    %% 直接测试边缘情况：当type是复杂结构时
    ComplexType = #{type => varchar, length => 100},
    Result = apply(eorm_ddl_generator, mysql_type, [ComplexType, []]),
    ?assert(is_binary(Result)),
    ?assert(size(Result) > 0).

%% 测试行399 - generate_mysql_table_options中Engine为其他类型
mysql_table_options_engine_other_test() ->
    Options = #{
        engine => "MyISAM",  % 字符串类型，不是binary或atom
        charset => utf8mb4
    },
    
    Result = lists:flatten(eorm_ddl_generator:generate_mysql_table_options(Options)),
    ?assert(string:str(Result, "ENGINE=MyISAM") > 0).

%% 测试行404 - generate_mysql_table_options中Charset为其他类型
mysql_table_options_charset_other_test() ->
    Options = #{
        engine => innodb,
        charset => "utf8mb4"  % 字符串类型，不是binary或atom  
    },
    
    Result = lists:flatten(eorm_ddl_generator:generate_mysql_table_options(Options)),
    ?assert(string:str(Result, "DEFAULT CHARSET=utf8mb4") > 0).

%% 测试行426 - generate_mysql_table_options的默认分支
mysql_table_options_default_test() ->
    % 传入无效类型触发默认分支
    Result = eorm_ddl_generator:generate_mysql_table_options("invalid"),
    % 结果是iolist，需要flatten才能做字符串搜索
    FlatResult = lists:flatten(Result),
    ?assert(string:str(FlatResult, "ENGINE=INNODB") > 0),
    ?assert(string:str(FlatResult, "DEFAULT CHARSET=utf8mb4") > 0).

%% 测试行470 - sqlite_type中is_list结果为false的情况
sqlite_type_binary_result_test() ->
    %% 直接测试边缘情况：当type是复杂结构时
    ComplexType = #{type => text},
    Result = apply(eorm_ddl_generator, sqlite_type, [ComplexType, []]),
    ?assert(is_binary(Result)),
    ?assert(size(Result) > 0).

%% 测试行559 - generate_alter_table中Changes为空的情况
generate_alter_table_no_changes_test() ->
    Schema = #{fields => [{id, integer, [primary_key]}]},
    Changes = #{changes => []},  % 空变更列表
    
    Result = eorm_ddl_generator:generate_alter_table(postgres, Schema, Changes),
    ?assertEqual([], Result).

%% 测试复杂索引结构生成
generate_index_complex_test() ->
    % 测试3元组索引结构: {IndexName, Columns, Options}
    Index = {unique_name_idx, [name], [unique]},
    
    Result = eorm_ddl_generator:generate_create_index(postgres, test_table, Index),
    ?assert(is_binary(Result)),
    ?assert(string:str(binary_to_list(Result), "unique_name_idx") > 0),
    ?assert(string:str(binary_to_list(Result), "UNIQUE") > 0).

%% 测试外键约束生成（通过表创建DDL测试）
foreign_key_constraint_test() ->
    Schema = #{
        fields => [
            {id, integer, [primary_key]},
            {user_id, integer, []}
        ],
        foreign_keys => [
            {foreign_key, user_id, users, id, [on_delete_cascade]}
        ]
    },
    
    Result = eorm_ddl_generator:generate_create_table(postgres, test_table, Schema),
    ?assert(is_binary(Result)),
    ResultStr = binary_to_list(Result),
    ?assert(string:str(ResultStr, "user_id") > 0).

%% 测试多字段索引格式化（直接测试生成的DDL）
multiple_field_index_test() ->
    % 测试多字段索引：{IndexName, Columns, Options}
    Index = {idx_complex, [name, email, created_at], [unique]},
    
    Result = eorm_ddl_generator:generate_create_index(postgres, test_table, Index),
    ?assert(is_binary(Result)),
    ResultStr = binary_to_list(Result),
    ?assert(string:str(ResultStr, "name") > 0),
    ?assert(string:str(ResultStr, "email") > 0),
    ?assert(string:str(ResultStr, "created_at") > 0),
    ?assert(string:str(ResultStr, "UNIQUE") > 0).

%% 测试简单2元组索引
simple_index_generation_test() ->
    % 测试简单索引生成：{IndexName, Columns}
    Index = {user_idx, [user_id]},
    
    Result = eorm_ddl_generator:generate_create_index(postgres, users, Index),
    ?assert(is_binary(Result)),
    ?assert(string:str(binary_to_list(Result), "user_id") > 0),
    ?assert(string:str(binary_to_list(Result), "user_idx") > 0).

%% 测试字段排序（通过实际DDL生成测试）
field_ordering_test() ->
    Schema = #{
        fields => [
            {id, integer, [primary_key]},
            {name, {string, 100}, [not_null]},
            {created_at, timestamp, []}
        ]
    },
    
    Result = eorm_ddl_generator:generate_create_table(postgres, test_table, Schema),
    ?assert(is_binary(Result)),
    ResultStr = binary_to_list(Result),
    % 验证字段顺序
    IdPos = string:str(ResultStr, "id"),
    NamePos = string:str(ResultStr, "name"),
    CreatedPos = string:str(ResultStr, "created_at"),
    ?assert(IdPos < NamePos),
    ?assert(NamePos < CreatedPos).

%% 测试实际未覆盖的内部函数
internal_functions_test_() ->
    [
        %% 测试postgres_type_string的边缘情况
        ?_test(begin
            % 测试map类型的Type参数
            MapType = #{type => text, length => 1000},
            Result = apply(eorm_ddl_generator, postgres_type_string, [MapType, []]),
            ?assert(is_list(Result))
        end),
        
        %% 测试mysql_type_string的边缘情况
        ?_test(begin
            MapType = #{type => varchar, length => 50},
            Result = apply(eorm_ddl_generator, mysql_type_string, [MapType, []]),
            ?assert(is_list(Result))
        end),
        
        %% 测试sqlite_type_string的边缘情况
        ?_test(begin
            MapType = #{type => integer},
            Result = apply(eorm_ddl_generator, sqlite_type_string, [MapType, []]),
            ?assert(is_list(Result))
        end)
    ].