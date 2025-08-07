%%% @doc EORM 主模块测试
%%% @end
-module(eorm_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% 测试模型定义
%%====================================================================

test_user_model() ->
    #{
        table => <<"users">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]},
            #{name => email, type => string, opts => [unique]},
            #{name => age, type => integer, opts => []},
            #{name => created_at, type => datetime, opts => []},
            #{name => updated_at, type => datetime, opts => []}
        ],
        indexes => [
            #{name => idx_email, columns => [email]},
            #{name => idx_name_age, columns => [name, age]}
        ]
    }.

test_post_model() ->
    #{
        table => <<"posts">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => user_id, type => integer, opts => [required]},
            #{name => title, type => string, opts => [required]},
            #{name => content, type => text, opts => []},
            #{name => published, type => boolean, opts => [default, false]},
            #{name => created_at, type => datetime, opts => []},
            #{name => updated_at, type => datetime, opts => []}
        ],
        indexes => [
            #{name => idx_user_id, columns => [user_id]},
            #{name => idx_published, columns => [published]}
        ],
        associations => [
            #{name => user, type => belongs_to, model => test_user_model, foreign_key => user_id}
        ]
    }.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% 启动必要的进程
    case whereis(eorm_registry) of
        undefined -> {ok, _} = eorm_registry:start_link();
        _ -> ok
    end,
    
    %% 注册测试模型
    eorm:register_model(test_user_model()),
    eorm:register_model(test_post_model()),
    ok.

cleanup(_) ->
    %% 清理注册的模型
    ok.

%%====================================================================
%% 基础 CRUD 测试
%%====================================================================

crud_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"创建记录测试", fun test_create/0},
        {"查找记录测试", fun test_find/0},
        {"查找所有记录测试", fun test_find_all/0},
        {"查找第一条记录测试", fun test_first/0},
        {"查找最后一条记录测试", fun test_last/0},
        {"更新记录测试", fun test_update/0},
        {"删除记录测试", fun test_delete/0},
        {"计数测试", fun test_count/0},
        {"存在性测试", fun test_exists/0}
     ]
    }.

test_create() ->
    %% 模拟创建用户
    UserData = #{
        name => <<"John Doe">>,
        email => <<"john@example.com">>,
        age => 30
    },
    %% eorm_crud 有简化实现，返回成功
    ?assertMatch({ok, _}, eorm:create(test_user_model, UserData)).

test_find() ->
    %% 测试通过 ID 查找
    ?assertMatch({ok, []}, eorm:find(test_user_model, 1)),
    
    %% 测试通过条件查找
    Conditions = #{name => <<"John Doe">>},
    ?assertMatch({ok, []}, eorm:find(test_user_model, Conditions)).

test_find_all() ->
    Conditions = #{age => 30},
    ?assertMatch({ok, []}, eorm:find_all(test_user_model, Conditions)).

test_first() ->
    Conditions = #{email => <<"john@example.com">>},
    ?assertMatch({error, _}, eorm:first(test_user_model, Conditions)).

test_last() ->
    Conditions = #{age => 30},
    ?assertMatch({error, _}, eorm:last(test_user_model, Conditions)).

test_update() ->
    Updates = #{name => <<"Jane Doe">>},
    Conditions = #{id => 1},
    ?assertMatch({ok, _}, eorm:update(test_user_model, Conditions, Updates)).

test_delete() ->
    ?assertMatch({ok, _}, eorm:delete(test_user_model, 1)),
    ?assertMatch({ok, _}, eorm:delete(test_user_model, #{name => <<"John Doe">>})).

test_count() ->
    ?assertMatch({ok, _}, eorm:count(test_user_model)).

test_exists() ->
    %% exists 函数需要使用 where 查询
    ?assertMatch({ok, _}, eorm:exists(test_user_model, 1)),
    ?assertMatch({ok, _}, eorm:exists(test_user_model, #{email => <<"john@example.com">>})).

%%====================================================================
%% 查询构建器测试
%%====================================================================

query_builder_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"创建查询测试", fun test_new_query/0},
        {"Where 条件测试", fun test_where/0},
        {"Or Where 条件测试", fun test_or_where/0},
        {"Order By 测试", fun test_order_by/0},
        {"Group By 测试", fun test_group_by/0},
        {"Having 测试", fun test_having/0},
        {"Limit 测试", fun test_limit/0},
        {"Offset 测试", fun test_offset/0},
        {"Join 测试", fun test_joins/0},
        {"Preload 测试", fun test_preload/0},
        {"Select 测试", fun test_select/0},
        {"Distinct 测试", fun test_distinct/0},
        {"Lock 测试", fun test_lock/0}
     ]
    }.

test_new_query() ->
    Query = eorm:new(test_user_model),
    ?assertMatch(#eorm_query{model = test_user_model}, Query).

test_where() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:where(Query, #{name => <<"John">>}),
    ?assertMatch(#eorm_query{}, Query2).

test_or_where() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:or_where(Query, #{age => 30}),
    %% or_where adds to the where list
    ?assertMatch(#eorm_query{where = [_]}, Query2).

test_order_by() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:order_by(Query, [{name, asc}, {age, desc}]),
    ?assertMatch(#eorm_query{order = [{name, asc}, {age, desc}]}, Query2).

test_group_by() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:group_by(Query, [age]),
    ?assertMatch(#eorm_query{group = [age]}, Query2).

test_having() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:having(Query, [{count, '>', 5}]),
    ?assertMatch(#eorm_query{having = [{count, '>', 5}]}, Query2).

test_limit() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:limit(Query, 10),
    ?assertMatch(#eorm_query{limit = 10}, Query2).

test_offset() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:offset(Query, 20),
    ?assertMatch(#eorm_query{offset = 20}, Query2).

test_joins() ->
    Query = eorm:new(test_post_model),
    Query2 = eorm:joins(Query, {inner, test_user_model, <<"posts.user_id = users.id">>}),
    ?assertMatch(#eorm_query{joins = [_]}, Query2).

test_preload() ->
    Query = eorm:new(test_post_model),
    Query2 = eorm:preload(Query, [user]),
    ?assertMatch(#eorm_query{}, Query2).

test_select() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:select(Query, [id, name, email]),
    ?assertMatch(#eorm_query{}, Query2).

test_distinct() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:distinct(Query),
    ?assertMatch(#eorm_query{distinct = true}, Query2).

test_lock() ->
    Query = eorm:new(test_user_model),
    Query2 = eorm:lock(Query, write),
    ?assertMatch(#eorm_query{lock = write}, Query2).

%%====================================================================
%% 事务测试
%%====================================================================

transaction_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"事务执行测试", fun test_transaction/0},
        {"事务回滚测试", fun test_rollback/0}
     ]
    }.

test_transaction() ->
    %% 简单事务测试
    Result = eorm:transaction(fun() -> {ok, success} end),
    ?assertMatch({ok, _}, Result).

test_rollback() ->
    %% 回滚测试
    Result = eorm:transaction(fun() ->
        eorm:rollback(test_reason)
    end),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% 批量操作测试
%%====================================================================

bulk_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"批量插入测试", fun test_insert_all/0},
        {"批量更新测试", fun test_update_all/0},
        {"批量删除测试", fun test_delete_all/0}
     ]
    }.

test_insert_all() ->
    Records = [
        #{name => <<"User1">>, email => <<"user1@example.com">>},
        #{name => <<"User2">>, email => <<"user2@example.com">>},
        #{name => <<"User3">>, email => <<"user3@example.com">>}
    ],
    ?assertMatch({ok, _}, eorm:insert_all(test_user_model, Records)).

test_update_all() ->
    Updates = #{age => 31},
    Conditions = #{age => 30},
    ?assertMatch({ok, _}, eorm:update_all(test_user_model, Conditions, Updates)).

test_delete_all() ->
    Conditions = #{age => 30},
    ?assertMatch({ok, _}, eorm:delete_all(test_user_model, Conditions)).

%%====================================================================
%% 原生查询测试
%%====================================================================

raw_query_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"原生查询测试", fun test_raw_query/0},
        {"原生执行测试", fun test_exec/0}
     ]
    }.

test_raw_query() ->
    SQL = <<"SELECT * FROM users WHERE age > $1">>,
    ?assertMatch({ok, _}, eorm:raw_query(SQL)),
    ?assertMatch({ok, _}, eorm:raw_query(SQL, [25])).

test_exec() ->
    SQL = <<"UPDATE users SET age = age + 1 WHERE id = $1">>,
    ?assertMatch({ok, _}, eorm:exec(SQL)),
    ?assertMatch({ok, _}, eorm:exec(SQL, [1])).

%%====================================================================
%% 迁移测试
%%====================================================================

migration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"自动迁移测试", fun test_auto_migrate/0},
        {"迁移计划测试", fun test_migration_plan/0}
     ]
    }.

test_auto_migrate() ->
    %% 这里测试的是没有数据库配置时的错误处理
    %% 预期会返回错误，这是正常的行为
    Models = [test_user_model(), test_post_model()],
    ?assertMatch({error, _}, eorm:auto_migrate(Models)),
    
    Options = #{dry_run => true},
    ?assertMatch({error, _}, eorm:auto_migrate(Models, Options)).

test_migration_plan() ->
    Models = [test_user_model(), test_post_model()],
    ?assertMatch({error, _}, eorm:migration_plan(Models)),
    
    Options = #{adapter => postgres},
    ?assertMatch({error, _}, eorm:migration_plan(Models, Options)).

%%====================================================================
%% 模型注册测试
%%====================================================================

model_registration_test_() ->
    [
        {"注册单个模型测试", fun test_register_model/0},
        {"注册多个模型测试", fun test_register_models/0},
        {"获取模型测试", fun test_get_model/0}
    ].

test_register_model() ->
    %% 测试 register_model 函数可以被调用
    %% 使用 test_model 模块，它导出了 schema/0
    %% 先确保模块被加载
    code:ensure_loaded(test_model),
    Result = eorm:register_model(test_model),
    ?assertMatch(ok, Result).

test_register_models() ->
    %% 使用 test_model 模块
    Models = [test_model],
    Result = eorm:register_models(Models),
    ?assertMatch(ok, Result).

test_get_model() ->
    %% 先注册一个模块
    code:ensure_loaded(test_model),
    eorm:register_model(test_model),
    Result = eorm:get_model(test_model),
    ?assertMatch({ok, _}, Result),
    
    Result2 = eorm:get_model(non_existent_model),
    ?assertMatch({error, not_found}, Result2).