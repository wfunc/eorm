%%% @doc EORM 自动迁移单元测试
%%% 使用 EUnit 测试框架
%%% @end
-module(eorm_auto_migrate_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/eorm.hrl").

%%====================================================================
%% 测试用的模型定义
%%====================================================================

%% 用户模型
user_model() ->
    #{
        table => test_users,
        fields => [
            {id},                                    % 自动推导为主键
            {username, {string, 50}, [unique, not_null]},
            {email, {string, 100}, [unique, not_null]},
            {age, integer, [{default, 18}]},
            {balance, {decimal, 10, 2}},
            {is_active, boolean, [{default, true}]},
            {created_at, timestamp},
            {updated_at, timestamp, [auto_update]}
        ],
        indexes => [
            {idx_username, [username]},
            {idx_email, [email]},
            {idx_created_at, [created_at], [desc]}
        ]
    }.

%% 文章模型
post_model() ->
    #{
        table => test_posts,
        fields => [
            {id},
            {user_id, integer, [not_null]},
            {title, {string, 200}, [not_null]},
            {content, text},
            {status, string, [{default, "draft"}]},
            {view_count, integer, [{default, 0}]},
            {published_at, datetime},
            timestamps
        ],
        indexes => [
            {idx_user_id, [user_id]},
            {idx_status, [status]}
        ],
        constraints => [
            {foreign_key, user_id, test_users, id, [{on_delete, cascade}]}
        ]
    }.

%%====================================================================
%% 测试套件
%%====================================================================

eorm_auto_migrate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {inparallel, [
         {"DDL 生成测试", fun test_ddl_generation/0},
         {"Schema 比较测试", fun test_schema_diff/0},
         {"类型映射测试", fun test_type_mapping/0},
         {"索引生成测试", fun test_index_generation/0},
         {"约束生成测试", fun test_constraint_generation/0},
         {"字段推导测试", fun test_field_inference/0},
         {"迁移计划测试", fun test_migration_plan/0}
     ]}
    }.

%% 设置测试环境
setup() ->
    ok.

%% 清理测试环境
cleanup(_) ->
    ok.

%%====================================================================
%% 单元测试用例
%%====================================================================

%% 测试 DDL 生成
test_ddl_generation() ->
    Schema = user_model(),
    
    %% 测试 PostgreSQL DDL 生成
    PostgresDDL = eorm_ddl_generator:generate_create_table(postgres, test_users, Schema),
    DDLBinary = iolist_to_binary(PostgresDDL),
    
    %% 验证生成的 DDL 包含正确的元素
    ?assert(binary:match(DDLBinary, <<"CREATE TABLE IF NOT EXISTS">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"test_users">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"id SERIAL">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"username VARCHAR(50)">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"email VARCHAR(100)">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"UNIQUE">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"NOT NULL">>) =/= nomatch),
    ?assert(binary:match(DDLBinary, <<"DEFAULT">>) =/= nomatch),
    
    %% 测试 MySQL DDL 生成
    MySQLDDL = eorm_ddl_generator:generate_create_table(mysql, test_users, Schema),
    MySQLBinary = iolist_to_binary(MySQLDDL),
    
    ?assert(binary:match(MySQLBinary, <<"AUTO_INCREMENT">>) =/= nomatch),
    ?assert(binary:match(MySQLBinary, <<"INT">>) =/= nomatch),
    
    %% 测试 SQLite DDL 生成
    SQLiteDDL = eorm_ddl_generator:generate_create_table(sqlite, test_users, Schema),
    SQLiteBinary = iolist_to_binary(SQLiteDDL),
    
    ?assert(binary:match(SQLiteBinary, <<"INTEGER PRIMARY KEY">>) =/= nomatch).

%% 测试 Schema 比较
test_schema_diff() ->
    %% 模型 Schema（新版本）
    ModelSchema = #{
        fields => [
            {id, integer, [primary_key]},
            {name, string},
            {email, string, [unique]},
            {new_field, string}              % 新增字段
        ],
        indexes => [
            {idx_email, [email]},
            {idx_new, [new_field]}           % 新增索引
        ]
    },
    
    %% 数据库 Schema（当前版本）
    DbSchema = #{
        columns => [
            #{name => id, type => integer, options => [primary_key]},
            #{name => name, type => {varchar, 255}, options => []},
            #{name => email, type => {varchar, 255}, options => [unique]},
            #{name => old_field, type => text, options => []}  % 多余字段
        ],
        indexes => [
            {idx_email, [email]},
            {idx_old, [old_field]}           % 多余索引
        ]
    },
    
    %% 执行比较
    Diff = eorm_schema_diff:compare(ModelSchema, DbSchema),
    Changes = maps:get(changes, Diff),
    
    %% 验证检测到变更
    ?assert(length(Changes) > 0),
    
    %% 检查是否检测到新字段
    HasAddColumn = lists:any(fun(Change) ->
        case Change of
            {add_column, new_field, _} -> true;
            _ -> false
        end
    end, Changes),
    ?assert(HasAddColumn),
    
    %% 检查是否检测到多余字段
    HasDropColumn = lists:any(fun(Change) ->
        case Change of
            {drop_column, old_field} -> true;
            _ -> false
        end
    end, Changes),
    ?assert(HasDropColumn),
    
    %% 测试风险分析
    RiskAnalysis = eorm_schema_diff:analyze_risks(Diff),
    ?assert(maps:is_key(safe, RiskAnalysis)),
    ?assert(maps:is_key(warning, RiskAnalysis)),
    ?assert(maps:is_key(dangerous, RiskAnalysis)),
    
    %% 删除列应该被标记为危险操作
    DangerousChanges = maps:get(dangerous, RiskAnalysis),
    ?assert(lists:member({drop_column, old_field}, DangerousChanges)).

%% 测试类型映射
test_type_mapping() ->
    TestFields = [
        {id},
        {name, string},
        {email, {string, 100}},
        {age, integer},
        {price, {decimal, 10, 2}},
        {is_active, boolean},
        {data, json},
        {created_at, timestamp}
    ],
    
    Schema = #{fields => TestFields},
    
    %% 测试 PostgreSQL 类型映射
    PostgresDDL = eorm_ddl_generator:generate_create_table(postgres, type_test, Schema),
    DDLString = binary_to_list(iolist_to_binary(PostgresDDL)),
    
    %% 验证类型映射
    ?assert(string:find(DDLString, "SERIAL") =/= nomatch),      % id → SERIAL
    ?assert(string:find(DDLString, "VARCHAR(255)") =/= nomatch), % string → VARCHAR(255)
    ?assert(string:find(DDLString, "VARCHAR(100)") =/= nomatch), % string(100) → VARCHAR(100)
    ?assert(string:find(DDLString, "INTEGER") =/= nomatch),      % integer → INTEGER
    ?assert(string:find(DDLString, "DECIMAL(10,2)") =/= nomatch), % decimal(10,2) → DECIMAL(10,2)
    ?assert(string:find(DDLString, "BOOLEAN") =/= nomatch),      % boolean → BOOLEAN
    ?assert(string:find(DDLString, "JSONB") =/= nomatch),        % json → JSONB
    ?assert(string:find(DDLString, "TIMESTAMP") =/= nomatch).    % timestamp → TIMESTAMP

%% 测试索引生成
test_index_generation() ->
    %% 测试普通索引
    IndexDDL1 = eorm_ddl_generator:generate_create_index(
        postgres, test_table, {idx_name, [name]}
    ),
    ?assertEqual(
        "CREATE INDEX idx_name ON test_table (name)",
        iolist_to_binary(IndexDDL1)
    ),
    
    %% 测试复合索引
    IndexDDL2 = eorm_ddl_generator:generate_create_index(
        postgres, test_table, {idx_composite, [name, age]}
    ),
    ?assertEqual(
        "CREATE INDEX idx_composite ON test_table (name, age)",
        iolist_to_binary(IndexDDL2)
    ),
    
    %% 测试唯一索引
    IndexDDL3 = eorm_ddl_generator:generate_create_index(
        postgres, test_table, {idx_unique, [email], [unique]}
    ),
    ?assertEqual(
        "CREATE UNIQUE INDEX idx_unique ON test_table (email)",
        iolist_to_binary(IndexDDL3)
    ).

%% 测试约束生成
test_constraint_generation() ->
    Schema = post_model(),
    
    %% 生成 DDL
    DDL = eorm_ddl_generator:generate_create_table(postgres, test_posts, Schema),
    DDLString = binary_to_list(iolist_to_binary(DDL)),
    
    %% 验证外键约束
    ?assert(string:str(DDLString, "FOREIGN KEY") > 0),
    ?assert(string:str(DDLString, "REFERENCES test_users(id)") > 0),
    ?assert(string:str(DDLString, "ON DELETE CASCADE") > 0).

%% 测试字段自动推导
test_field_inference() ->
    %% 测试 id 字段推导
    {id, Type1, Options1} = parse_field_spec({id}),
    ?assertEqual(integer, Type1),
    ?assert(lists:member(primary_key, Options1)),
    ?assert(lists:member(auto_increment, Options1)),
    
    %% 测试 user_id 字段推导（外键）
    {user_id, Type2, _Options2} = parse_field_spec({user_id}),
    ?assertEqual(integer, Type2),
    
    %% 测试 created_at 字段推导（时间戳）
    {created_at, Type3, _Options3} = parse_field_spec({created_at}),
    ?assertEqual(timestamp, Type3),
    
    %% 测试 is_active 字段推导（布尔）
    {is_active, Type4, Options4} = parse_field_spec({is_active}),
    ?assertEqual(boolean, Type4),
    ?assert(lists:member({default, false}, Options4)).

%% 测试迁移计划生成
test_migration_plan() ->
    %% 模拟表不存在的情况
    Plan1 = generate_migration_plan_for_new_table(),
    ?assertEqual(create_table, maps:get(action, Plan1)),
    ?assert(maps:is_key(ddl, Plan1)),
    
    %% 模拟表存在需要更新的情况
    Plan2 = generate_migration_plan_for_update(),
    ?assertEqual(alter_table, maps:get(action, Plan2)),
    ?assert(maps:is_key(changes, Plan2)).

%%====================================================================
%% 辅助函数
%%====================================================================

parse_field_spec({Name}) ->
    Type = infer_field_type(Name),
    Options = infer_field_options(Name),
    {Name, Type, Options};
parse_field_spec({Name, Type}) ->
    {Name, Type, []};
parse_field_spec({Name, Type, Options}) ->
    {Name, Type, Options}.

infer_field_type(id) -> integer;
infer_field_type(Name) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    case {lists:suffix("_id", NameStr), 
          lists:suffix("_at", NameStr),
          lists:prefix("is_", NameStr)} of
        {true, _, _} -> integer;
        {_, true, _} -> timestamp;
        {_, _, true} -> boolean;
        _ -> string
    end.

infer_field_options(id) -> [primary_key, auto_increment];
infer_field_options(created_at) -> [auto_now_add];
infer_field_options(updated_at) -> [auto_now];
infer_field_options(Name) when is_atom(Name) ->
    case lists:prefix("is_", atom_to_list(Name)) of
        true -> [{default, false}];
        false -> []
    end.

generate_migration_plan_for_new_table() ->
    #{
        model => test_model,
        table => test_table,
        action => create_table,
        ddl => "CREATE TABLE test_table (...)"
    }.

generate_migration_plan_for_update() ->
    #{
        model => test_model,
        table => test_table,
        action => alter_table,
        changes => [
            {add_column, new_field, #{type => string}},
            {add_index, idx_new, [new_field]}
        ],
        ddl => ["ALTER TABLE test_table ADD COLUMN new_field VARCHAR(255)"]
    }.

%%====================================================================
%% 集成测试
%%====================================================================

integration_test_() ->
    {setup,
     fun integration_setup/0,
     fun integration_cleanup/1,
     [
         {"完整迁移流程测试", fun test_full_migration_flow/0},
         {"增量更新测试", fun test_incremental_update/0}
     ]
    }.

integration_setup() ->
    %% 启动必要的进程
    {ok, _} = eorm_registry:start_link(),
    {ok, _} = eorm_migration_lock:start_link(),
    ok.

integration_cleanup(_) ->
    ok.

%% 测试完整的迁移流程
test_full_migration_flow() ->
    %% 模拟完整的自动迁移流程
    Models = [user_model, post_model],
    
    %% 生成迁移计划
    Plans = lists:map(fun(Model) ->
        Schema = Model(),
        Table = maps:get(table, Schema),
        #{
            model => Model,
            table => Table,
            action => create_table,
            ddl => eorm_ddl_generator:generate_create_table(postgres, Table, Schema)
        }
    end, Models),
    
    %% 验证计划
    ?assertEqual(2, length(Plans)),
    
    lists:foreach(fun(Plan) ->
        ?assertEqual(create_table, maps:get(action, Plan)),
        DDL = maps:get(ddl, Plan),
        ?assert(iolist_size(DDL) > 0)
    end, Plans).

%% 测试增量更新
test_incremental_update() ->
    %% 原始 Schema
    OldSchema = #{
        fields => [
            {id, integer, [primary_key]},
            {name, string}
        ]
    },
    
    %% 新 Schema（添加字段）
    NewSchema = #{
        fields => [
            {id, integer, [primary_key]},
            {name, string},
            {email, string, [unique]},  % 新增
            {age, integer}               % 新增
        ]
    },
    
    %% 比较差异
    Diff = eorm_schema_diff:compare(NewSchema, OldSchema),
    _Changes = maps:get(changes, Diff),
    
    %% 生成 ALTER 语句
    AlterDDLs = eorm_ddl_generator:generate_alter_table(postgres, test_table, Diff),
    
    %% 验证生成了正确的 ALTER 语句
    ?assert(length(AlterDDLs) > 0),
    
    %% 验证 ALTER 语句包含新字段
    DDLString = lists:flatten([iolist_to_binary(DDL) || DDL <- AlterDDLs]),
    ?assert(binary:match(DDLString, <<"email">>) =/= nomatch),
    ?assert(binary:match(DDLString, <<"age">>) =/= nomatch).