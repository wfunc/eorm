%%% @doc EORM Schema Inspector 测试
%%% @end
-module(eorm_schema_inspector_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Schema 检查测试
%%====================================================================

schema_inspection_test_() ->
    [
        {"获取表信息测试", fun test_get_table_info/0},
        {"获取列信息测试", fun test_get_columns/0},
        {"获取索引信息测试", fun test_get_indexes/0},
        {"获取约束信息测试", fun test_get_constraints/0},
        {"表存在性检查测试", fun test_table_exists/0}
    ].

test_get_table_info() ->
    %% 获取表信息
    Result = eorm_schema_inspector:get_table_info(<<"users">>),
    ?assertMatch({ok, _}, Result),
    
    {ok, Info} = Result,
    ?assert(is_map(Info)),
    ?assert(maps:is_key(table_name, Info)),
    ?assert(maps:is_key(columns, Info)),
    ?assert(maps:is_key(indexes, Info)).

test_get_columns() ->
    %% 获取列信息
    Result = eorm_schema_inspector:get_columns(<<"users">>),
    ?assertMatch({ok, _}, Result),
    
    {ok, Columns} = Result,
    ?assert(is_list(Columns)),
    
    %% 每列应该有名称、类型、约束等信息
    lists:foreach(fun(Col) ->
        ?assert(is_map(Col)),
        ?assert(maps:is_key(name, Col)),
        ?assert(maps:is_key(type, Col)),
        ?assert(maps:is_key(nullable, Col))
    end, Columns).

test_get_indexes() ->
    %% 获取索引信息
    Result = eorm_schema_inspector:get_indexes(<<"users">>),
    ?assertMatch({ok, _}, Result),
    
    {ok, Indexes} = Result,
    ?assert(is_list(Indexes)),
    
    %% 每个索引应该有名称、列等信息
    lists:foreach(fun(Idx) ->
        ?assert(is_map(Idx)),
        ?assert(maps:is_key(name, Idx)),
        ?assert(maps:is_key(columns, Idx)),
        ?assert(maps:is_key(unique, Idx))
    end, Indexes).

test_get_constraints() ->
    %% 获取约束信息
    Result = eorm_schema_inspector:get_constraints(<<"users">>),
    ?assertMatch({ok, _}, Result),
    
    {ok, Constraints} = Result,
    ?assert(is_list(Constraints)),
    
    %% 每个约束应该有名称、类型等信息
    lists:foreach(fun(C) ->
        ?assert(is_map(C)),
        ?assert(maps:is_key(name, C)),
        ?assert(maps:is_key(type, C))
    end, Constraints).

test_table_exists() ->
    %% 检查表是否存在
    Result1 = eorm_schema_inspector:table_exists(<<"users">>),
    ?assertMatch({ok, _}, Result1),
    
    Result2 = eorm_schema_inspector:table_exists(<<"non_existent_table">>),
    ?assertMatch({ok, false}, Result2).

%%====================================================================
%% Schema 比较测试
%%====================================================================

schema_comparison_test_() ->
    [
        {"比较 Schema 差异", fun test_compare_schemas/0},
        {"检测新增字段", fun test_detect_new_fields/0},
        {"检测删除字段", fun test_detect_removed_fields/0},
        {"检测修改字段", fun test_detect_modified_fields/0},
        {"检测索引变化", fun test_detect_index_changes/0}
    ].

test_compare_schemas() ->
    Schema1 = #{
        table => <<"users">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]}
        ]
    },
    
    Schema2 = #{
        table => <<"users">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]},
            #{name => email, type => string, opts => [unique]}
        ]
    },
    
    Diff = eorm_schema_inspector:compare_schemas(Schema1, Schema2),
    ?assert(is_map(Diff)),
    ?assert(maps:is_key(added_fields, Diff)),
    ?assert(maps:is_key(removed_fields, Diff)),
    ?assert(maps:is_key(modified_fields, Diff)).

test_detect_new_fields() ->
    Current = [
        #{name => id, type => integer, opts => [primary_key]}
    ],
    
    Target = [
        #{name => id, type => integer, opts => [primary_key]},
        #{name => name, type => string, opts => []},
        #{name => email, type => string, opts => []}
    ],
    
    Added = eorm_schema_inspector:detect_new_fields(Current, Target),
    ?assertEqual(2, length(Added)),
    
    Names = [maps:get(name, F) || F <- Added],
    ?assert(lists:member(name, Names)),
    ?assert(lists:member(email, Names)).

test_detect_removed_fields() ->
    Current = [
        #{name => id, type => integer, opts => [primary_key]},
        #{name => name, type => string, opts => []},
        #{name => old_field, type => string, opts => []}
    ],
    
    Target = [
        #{name => id, type => integer, opts => [primary_key]},
        #{name => name, type => string, opts => []}
    ],
    
    Removed = eorm_schema_inspector:detect_removed_fields(Current, Target),
    ?assertEqual(1, length(Removed)),
    ?assertEqual(old_field, maps:get(name, hd(Removed))).

test_detect_modified_fields() ->
    Current = [
        #{name => id, type => integer, opts => [primary_key]},
        #{name => name, type => string, opts => []},
        #{name => age, type => string, opts => []}
    ],
    
    Target = [
        #{name => id, type => integer, opts => [primary_key]},
        #{name => name, type => string, opts => [required]},
        #{name => age, type => integer, opts => []}
    ],
    
    Modified = eorm_schema_inspector:detect_modified_fields(Current, Target),
    ?assertEqual(2, length(Modified)),
    
    Names = [maps:get(name, F) || F <- Modified],
    ?assert(lists:member(name, Names)),
    ?assert(lists:member(age, Names)).

test_detect_index_changes() ->
    Current = [
        #{name => idx_name, columns => [name]}
    ],
    
    Target = [
        #{name => idx_name, columns => [name]},
        #{name => idx_email, columns => [email], unique => true}
    ],
    
    {Added, Removed, Modified} = eorm_schema_inspector:detect_index_changes(Current, Target),
    ?assertEqual(1, length(Added)),
    ?assertEqual(0, length(Removed)),
    ?assertEqual(0, length(Modified)),
    ?assertEqual(idx_email, maps:get(name, hd(Added))).

%%====================================================================
%% 数据库信息测试
%%====================================================================

database_info_test_() ->
    [
        {"获取数据库版本", fun test_get_database_version/0},
        {"获取数据库字符集", fun test_get_database_charset/0},
        {"获取所有表", fun test_list_all_tables/0},
        {"获取表大小", fun test_get_table_size/0}
    ].

test_get_database_version() ->
    Result = eorm_schema_inspector:get_database_version(),
    ?assertMatch({ok, _}, Result),
    
    {ok, Version} = Result,
    ?assert(is_binary(Version)).

test_get_database_charset() ->
    Result = eorm_schema_inspector:get_database_charset(),
    ?assertMatch({ok, _}, Result),
    
    {ok, Charset} = Result,
    ?assert(is_binary(Charset)).

test_list_all_tables() ->
    Result = eorm_schema_inspector:list_all_tables(),
    ?assertMatch({ok, _}, Result),
    
    {ok, Tables} = Result,
    ?assert(is_list(Tables)),
    lists:foreach(fun(T) ->
        ?assert(is_binary(T))
    end, Tables).

test_get_table_size() ->
    Result = eorm_schema_inspector:get_table_size(<<"users">>),
    ?assertMatch({ok, _}, Result),
    
    {ok, Size} = Result,
    ?assert(is_map(Size)),
    ?assert(maps:is_key(rows, Size)),
    ?assert(maps:is_key(data_size, Size)),
    ?assert(maps:is_key(index_size, Size)).

%%====================================================================
%% 适配器特定测试
%%====================================================================

adapter_specific_test_() ->
    [
        {"PostgreSQL 适配器测试", fun test_postgres_adapter/0},
        {"MySQL 适配器测试", fun test_mysql_adapter/0},
        {"SQLite 适配器测试", fun test_sqlite_adapter/0}
    ].

test_postgres_adapter() ->
    %% 测试 PostgreSQL 特定功能
    ?assertEqual(false, eorm_schema_inspector:table_exists(postgres, users)),
    ?assertEqual([], eorm_schema_inspector:get_columns(postgres, users)),
    ?assertEqual([], eorm_schema_inspector:get_indexes(postgres, users)),
    ?assertEqual([], eorm_schema_inspector:get_constraints(postgres, users)),
    
    %% 测试表 schema 获取
    Schema = eorm_schema_inspector:get_table_schema(postgres, users),
    ?assertMatch(#{
        columns := [],
        indexes := [],
        constraints := []
    }, Schema).

test_mysql_adapter() ->
    %% 测试 MySQL 特定功能
    ?assertEqual(false, eorm_schema_inspector:table_exists(mysql, products)),
    ?assertEqual([], eorm_schema_inspector:get_columns(mysql, products)),
    ?assertEqual([], eorm_schema_inspector:get_indexes(mysql, products)),
    ?assertEqual([], eorm_schema_inspector:get_constraints(mysql, products)),
    
    %% 测试表 schema 获取
    Schema = eorm_schema_inspector:get_table_schema(mysql, products),
    ?assertMatch(#{
        columns := [],
        indexes := [],
        constraints := []
    }, Schema).

test_sqlite_adapter() ->
    %% 测试 SQLite 特定功能
    ?assertEqual(false, eorm_schema_inspector:table_exists(sqlite, orders)),
    ?assertEqual([], eorm_schema_inspector:get_columns(sqlite, orders)),
    ?assertEqual([], eorm_schema_inspector:get_indexes(sqlite, orders)),
    ?assertEqual([], eorm_schema_inspector:get_constraints(sqlite, orders)),
    
    %% 测试表 schema 获取
    Schema = eorm_schema_inspector:get_table_schema(sqlite, orders),
    ?assertMatch(#{
        columns := [],
        indexes := [],
        constraints := []
    }, Schema).

%%====================================================================
%% 边界情况和错误处理测试
%%====================================================================

edge_cases_test_() ->
    [
        {"空 Schema 比较", fun test_empty_schema_comparison/0},
        {"相同 Schema 比较", fun test_identical_schemas/0},
        {"大量字段检测", fun test_large_field_sets/0},
        {"复杂索引变化", fun test_complex_index_changes/0}
    ].

test_empty_schema_comparison() ->
    EmptySchema1 = #{fields => []},
    EmptySchema2 = #{fields => []},
    
    Diff = eorm_schema_inspector:compare_schemas(EmptySchema1, EmptySchema2),
    ?assertMatch(#{
        added_fields := [],
        removed_fields := [],
        modified_fields := []
    }, Diff).

test_identical_schemas() ->
    Schema = #{
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]}
        ]
    },
    
    Diff = eorm_schema_inspector:compare_schemas(Schema, Schema),
    ?assertMatch(#{
        added_fields := [],
        removed_fields := [],
        modified_fields := []
    }, Diff).

test_large_field_sets() ->
    %% 创建大量字段进行测试
    LargeFieldSet1 = [#{name => list_to_atom("field_" ++ integer_to_list(I)), 
                       type => string, opts => []} || I <- lists:seq(1, 100)],
    LargeFieldSet2 = LargeFieldSet1 ++ [#{name => extra_field, type => integer, opts => []}],
    
    Added = eorm_schema_inspector:detect_new_fields(LargeFieldSet1, LargeFieldSet2),
    ?assertEqual(1, length(Added)),
    ?assertEqual(extra_field, maps:get(name, hd(Added))).

test_complex_index_changes() ->
    %% 复杂的索引变化测试
    CurrentIndexes = [
        #{name => idx1, columns => [col1]},
        #{name => idx2, columns => [col2, col3]},
        #{name => idx3, columns => [col4]}
    ],
    
    TargetIndexes = [
        #{name => idx1, columns => [col1]},        % 保持不变
        #{name => idx4, columns => [col5]},        % 新增
        #{name => idx5, columns => [col6, col7]}   % 新增
    ],
    
    {Added, Removed, Modified} = eorm_schema_inspector:detect_index_changes(CurrentIndexes, TargetIndexes),
    ?assertEqual(2, length(Added)),      % idx4, idx5
    ?assertEqual(2, length(Removed)),    % idx2, idx3
    ?assertEqual(0, length(Modified)).

%%====================================================================
%% 性能和压力测试
%%====================================================================

performance_test_() ->
    [
        {"大量字段修改检测", fun test_massive_field_modification/0},
        {"多次 Schema 比较", fun test_multiple_comparisons/0}
    ].

test_massive_field_modification() ->
    %% 大量字段的修改检测测试 - 简化测试，重点测试功能
    BaseFields = [
        #{name => field1, type => string, opts => []},
        #{name => field2, type => string, opts => []},
        #{name => field3, type => string, opts => []}
    ],
    
    %% 修改字段类型
    ModifiedFields = [
        #{name => field1, type => integer, opts => []},  % 类型改变
        #{name => field2, type => string, opts => []},   % 无变化
        #{name => field3, type => text, opts => []}      % 类型改变
    ],
    
    Modified = eorm_schema_inspector:detect_modified_fields(BaseFields, ModifiedFields),
    ?assertEqual(2, length(Modified)).  % field1 和 field3 被修改

test_multiple_comparisons() ->
    %% 多次比较测试
    Schema1 = #{fields => [#{name => id, type => integer}]},
    Schema2 = #{fields => [#{name => id, type => string}]},
    
    %% 多次执行比较操作
    Results = [eorm_schema_inspector:compare_schemas(Schema1, Schema2) || _ <- lists:seq(1, 10)],
    
    %% 确保所有结果一致
    ?assertEqual(10, length(Results)),
    lists:foreach(fun(Result) ->
        ?assertMatch(#{
            added_fields := [],
            removed_fields := [],
            modified_fields := [_]
        }, Result)
    end, Results).

%%====================================================================
%% 实际使用场景测试
%%====================================================================

real_world_scenarios_test_() ->
    [
        {"用户表迁移场景", fun test_user_table_migration/0},
        {"添加新字段场景", fun test_add_new_field_scenario/0},
        {"删除废弃字段场景", fun test_remove_deprecated_field/0},
        {"修改字段类型场景", fun test_modify_field_type/0}
    ].

test_user_table_migration() ->
    %% 模拟真实的用户表迁移场景
    OldUserSchema = #{
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => username, type => {varchar, 50}, opts => [required, unique]},
            #{name => password, type => {varchar, 255}, opts => [required]},
            #{name => email, type => {varchar, 100}, opts => [unique]},
            #{name => created_at, type => timestamp, opts => []}
        ]
    },
    
    NewUserSchema = #{
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => username, type => {varchar, 50}, opts => [required, unique]},
            #{name => password_hash, type => {varchar, 255}, opts => [required]},  % 重命名
            #{name => email, type => {varchar, 150}, opts => [unique]},            % 扩大长度
            #{name => phone, type => {varchar, 20}, opts => []},                   % 新增
            #{name => status, type => {enum, [active, inactive]}, opts => []},     % 新增
            #{name => created_at, type => timestamp, opts => []},
            #{name => updated_at, type => timestamp, opts => []}                   % 新增
        ]
    },
    
    Diff = eorm_schema_inspector:compare_schemas(OldUserSchema, NewUserSchema),
    ?assert(length(maps:get(added_fields, Diff)) >= 3),  % phone, status, updated_at
    ?assert(length(maps:get(removed_fields, Diff)) >= 1), % password
    ?assert(length(maps:get(modified_fields, Diff)) >= 1). % email

test_add_new_field_scenario() ->
    %% 添加新字段的场景
    Current = [
        #{name => id, type => integer},
        #{name => title, type => string}
    ],
    
    Target = Current ++ [
        #{name => description, type => text},
        #{name => published, type => boolean},
        #{name => published_at, type => timestamp}
    ],
    
    Added = eorm_schema_inspector:detect_new_fields(Current, Target),
    ?assertEqual(3, length(Added)),
    
    AddedNames = [maps:get(name, F) || F <- Added],
    ?assert(lists:member(description, AddedNames)),
    ?assert(lists:member(published, AddedNames)),
    ?assert(lists:member(published_at, AddedNames)).

test_remove_deprecated_field() ->
    %% 删除废弃字段的场景
    Current = [
        #{name => id, type => integer},
        #{name => title, type => string},
        #{name => old_field1, type => string},
        #{name => deprecated_field, type => integer},
        #{name => temp_field, type => text}
    ],
    
    Target = [
        #{name => id, type => integer},
        #{name => title, type => string}
    ],
    
    Removed = eorm_schema_inspector:detect_removed_fields(Current, Target),
    ?assertEqual(3, length(Removed)),
    
    RemovedNames = [maps:get(name, F) || F <- Removed],
    ?assert(lists:member(old_field1, RemovedNames)),
    ?assert(lists:member(deprecated_field, RemovedNames)),
    ?assert(lists:member(temp_field, RemovedNames)).

test_modify_field_type() ->
    %% 修改字段类型的场景
    Current = [
        #{name => id, type => integer, nullable => false},
        #{name => price, type => float, nullable => true},
        #{name => status, type => string, nullable => true}
    ],
    
    Target = [
        #{name => id, type => bigint, nullable => false},           % 扩展类型
        #{name => price, type => decimal, nullable => false},       % 修改类型和约束
        #{name => status, type => {enum, [active, inactive]}, nullable => false}  % 修改类型和约束
    ],
    
    Modified = eorm_schema_inspector:detect_modified_fields(Current, Target),
    ?assertEqual(3, length(Modified)),
    
    ModifiedNames = [maps:get(name, F) || F <- Modified],
    ?assert(lists:member(id, ModifiedNames)),
    ?assert(lists:member(price, ModifiedNames)),
    ?assert(lists:member(status, ModifiedNames)).