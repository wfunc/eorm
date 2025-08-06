%%% @doc EORM 主接口模块
%%% 提供所有对外 API
%%% @end
-module(eorm).

%% 基础 CRUD API
-export([
    create/2,
    find/2,
    find_all/2,
    first/2,
    last/2,
    update/3,
    delete/2,
    count/1,
    exists/2
]).

%% 查询构建器 API
-export([
    new/1,
    where/2,
    or_where/2,
    order_by/2,
    group_by/2,
    having/2,
    limit/2,
    offset/2,
    joins/2,
    preload/2,
    select/2,
    distinct/1,
    lock/2
]).

%% 事务 API
-export([
    transaction/1,
    transaction/2,
    rollback/1
]).

%% 批量操作 API
-export([
    insert_all/2,
    update_all/3,
    delete_all/2
]).

%% 原生查询 API
-export([
    raw_query/1,
    raw_query/2,
    exec/1,
    exec/2
]).

%% 迁移 API
-export([
    auto_migrate/1,
    auto_migrate/2,
    migration_plan/1,
    migration_plan/2
]).

%% 模型注册 API
-export([
    register_model/1,
    register_models/1,
    get_model/1
]).

-include("eorm.hrl").

%%====================================================================
%% 基础 CRUD API
%%====================================================================

%% @doc 创建记录
-spec create(module(), map()) -> {ok, map()} | {error, term()}.
create(Model, Data) ->
    eorm_crud:create(Model, Data).

%% @doc 查找记录
-spec find(module(), map() | integer()) -> {ok, [map()]} | {error, term()}.
find(Model, Conditions) when is_map(Conditions) ->
    eorm_crud:find(Model, Conditions);
find(Model, Id) when is_integer(Id) ->
    eorm_crud:find(Model, #{id => Id}).

%% @doc 查找所有记录
-spec find_all(module(), map()) -> {ok, [map()]} | {error, term()}.
find_all(Model, Conditions) ->
    eorm_crud:find_all(Model, Conditions).

%% @doc 查找第一条记录
-spec first(module(), map()) -> {ok, map()} | {error, not_found}.
first(Model, Conditions) ->
    eorm_crud:first(Model, Conditions).

%% @doc 查找最后一条记录
-spec last(module(), map()) -> {ok, map()} | {error, not_found}.
last(Model, Conditions) ->
    eorm_crud:last(Model, Conditions).

%% @doc 更新记录
-spec update(module(), map() | integer(), map()) -> {ok, integer()} | {error, term()}.
update(Model, Conditions, Updates) when is_map(Conditions) ->
    eorm_crud:update(Model, Conditions, Updates);
update(Model, Id, Updates) when is_integer(Id) ->
    eorm_crud:update(Model, #{id => Id}, Updates).

%% @doc 删除记录
-spec delete(module(), map() | integer()) -> {ok, integer()} | {error, term()}.
delete(Model, Conditions) when is_map(Conditions) ->
    eorm_crud:delete(Model, Conditions);
delete(Model, Id) when is_integer(Id) ->
    eorm_crud:delete(Model, #{id => Id}).

%% @doc 计数
-spec count(module() | #eorm_query{}) -> {ok, integer()} | {error, term()}.
count(Model) when is_atom(Model) ->
    eorm_crud:count(Model);
count(Query = #eorm_query{}) ->
    eorm_query:count(Query).

%% @doc 检查记录是否存在
-spec exists(module(), map()) -> boolean().
exists(Model, Conditions) ->
    Query = where(new(Model), Conditions),
    case count(Query) of
        {ok, Count} -> Count > 0;
        _ -> false
    end.

%%====================================================================
%% 查询构建器 API
%%====================================================================

%% @doc 创建新查询
-spec new(module()) -> #eorm_query{}.
new(Model) ->
    eorm_query:new(Model).

%% @doc 添加 WHERE 条件
-spec where(#eorm_query{} | map(), map() | {atom(), term()} | list()) -> #eorm_query{}.
where(Query = #eorm_query{}, Conditions) ->
    eorm_query:where(Query, Conditions);
where(Conditions, Query = #eorm_query{}) ->
    eorm_query:where(Query, Conditions).

%% @doc 添加 OR WHERE 条件
-spec or_where(#eorm_query{}, map() | list()) -> #eorm_query{}.
or_where(Query, Conditions) ->
    eorm_query:or_where(Query, Conditions).

%% @doc 设置排序
-spec order_by(#eorm_query{}, list() | {atom(), asc | desc}) -> #eorm_query{}.
order_by(Query, Orders) ->
    eorm_query:order_by(Query, Orders).

%% @doc 设置分组
-spec group_by(#eorm_query{}, list() | atom()) -> #eorm_query{}.
group_by(Query, Fields) ->
    eorm_query:group_by(Query, Fields).

%% @doc 设置 HAVING 条件
-spec having(#eorm_query{}, map() | list()) -> #eorm_query{}.
having(Query, Conditions) ->
    eorm_query:having(Query, Conditions).

%% @doc 设置限制数量
-spec limit(#eorm_query{}, integer()) -> #eorm_query{}.
limit(Query, Limit) ->
    eorm_query:limit(Query, Limit).

%% @doc 设置偏移量
-spec offset(#eorm_query{}, integer()) -> #eorm_query{}.
offset(Query, Offset) ->
    eorm_query:offset(Query, Offset).

%% @doc 添加 JOIN
-spec joins(#eorm_query{}, atom() | {atom(), map()}) -> #eorm_query{}.
joins(Query, Association) ->
    eorm_query:joins(Query, Association).

%% @doc 预加载关联
-spec preload(#eorm_query{}, list() | atom()) -> #eorm_query{}.
preload(Query, Associations) ->
    eorm_query:preload(Query, Associations).

%% @doc 选择字段
-spec select(#eorm_query{}, list()) -> #eorm_query{}.
select(Query, Fields) ->
    eorm_query:select(Query, Fields).

%% @doc 设置去重
-spec distinct(#eorm_query{}) -> #eorm_query{}.
distinct(Query) ->
    eorm_query:distinct(Query).

%% @doc 设置锁
-spec lock(#eorm_query{}, boolean() | write | read) -> #eorm_query{}.
lock(Query, Lock) ->
    eorm_query:lock(Query, Lock).

%%====================================================================
%% 事务 API
%%====================================================================

%% @doc 执行事务
-spec transaction(fun()) -> {ok, term()} | {error, term()}.
transaction(Fun) ->
    eorm_transaction:transaction(Fun).

%% @doc 带选项的事务
-spec transaction(fun(), map()) -> {ok, term()} | {error, term()}.
transaction(Fun, Options) ->
    eorm_transaction:transaction(Fun, Options).

%% @doc 回滚事务
-spec rollback(term()) -> no_return().
rollback(Reason) ->
    eorm_transaction:rollback(Reason).

%%====================================================================
%% 批量操作 API
%%====================================================================

%% @doc 批量插入
-spec insert_all(module(), [map()]) -> {ok, integer()} | {error, term()}.
insert_all(Model, Records) ->
    eorm_bulk:insert_all(Model, Records).

%% @doc 批量更新
-spec update_all(module(), map(), map()) -> {ok, integer()} | {error, term()}.
update_all(Model, Conditions, Updates) ->
    eorm_bulk:update_all(Model, Conditions, Updates).

%% @doc 批量删除
-spec delete_all(module(), map()) -> {ok, integer()} | {error, term()}.
delete_all(Model, Conditions) ->
    eorm_bulk:delete_all(Model, Conditions).

%%====================================================================
%% 原生查询 API
%%====================================================================

%% @doc 执行原生查询
-spec raw_query(string()) -> {ok, list()} | {error, term()}.
raw_query(SQL) ->
    eorm_raw:query(SQL).

%% @doc 带参数的原生查询
-spec raw_query(string(), list()) -> {ok, list()} | {error, term()}.
raw_query(SQL, Params) ->
    eorm_raw:query(SQL, Params).

%% @doc 执行原生语句（非查询）
-spec exec(string()) -> {ok, integer()} | {error, term()}.
exec(SQL) ->
    eorm_raw:execute(SQL).

%% @doc 带参数的原生语句执行
-spec exec(string(), list()) -> {ok, integer()} | {error, term()}.
exec(SQL, Params) ->
    eorm_raw:execute(SQL, Params).

%%====================================================================
%% 迁移 API
%%====================================================================

%% @doc 自动迁移
-spec auto_migrate([module()]) -> ok | {error, term()}.
auto_migrate(Models) ->
    eorm_auto_migrate:auto_migrate(Models).

%% @doc 带选项的自动迁移
-spec auto_migrate([module()], map()) -> ok | {error, term()}.
auto_migrate(Models, Options) ->
    eorm_auto_migrate:auto_migrate(Models, Options).

%% @doc 生成迁移计划
-spec migration_plan([module()]) -> {ok, list()} | {error, term()}.
migration_plan(Models) ->
    eorm_auto_migrate:migration_plan(Models).

%% @doc 带选项的迁移计划
-spec migration_plan([module()], map()) -> {ok, list()} | {error, term()}.
migration_plan(Models, Options) ->
    eorm_auto_migrate:migration_plan(Models, Options).

%%====================================================================
%% 模型注册 API
%%====================================================================

%% @doc 注册单个模型
-spec register_model(module()) -> ok.
register_model(Model) ->
    eorm_registry:register_model(Model).

%% @doc 注册多个模型
-spec register_models([module()]) -> ok.
register_models(Models) ->
    lists:foreach(fun register_model/1, Models).

%% @doc 获取模型信息
-spec get_model(module()) -> {ok, #eorm_model{}} | {error, not_found}.
get_model(Model) ->
    eorm_registry:get_model(Model).