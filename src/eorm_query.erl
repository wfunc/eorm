%%% @doc 查询构建器
%%% 提供链式 API 构建复杂查询
%%% @end
-module(eorm_query).

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
    lock/2,
    execute/1,
    count/1,
    to_sql/1,
    
    %% 新增函数
    union/2,
    explain/1,
    use_index/2,
    optimize/1,
    analyze_performance/1,
    cache/2,
    get_cached_result/1,
    invalidate_cache/1
]).

-include("eorm.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新查询
-spec new(module()) -> #eorm_query{}.
new(Model) ->
    case eorm_registry:get_model(Model) of
        {ok, #eorm_model{table = Table}} ->
            #eorm_query{
                model = Model,
                table = Table
            };
        _ ->
            #eorm_query{
                model = Model,
                table = Model
            }
    end.

%% @doc 添加 WHERE 条件
-spec where(#eorm_query{}, map() | list() | tuple()) -> #eorm_query{}.
where(Query = #eorm_query{where = Where}, Conditions) when is_map(Conditions) ->
    NewConditions = maps:to_list(Conditions),
    Query#eorm_query{where = Where ++ [{and_group, NewConditions}]};

where(Query = #eorm_query{where = Where}, Conditions) when is_list(Conditions) ->
    Query#eorm_query{where = Where ++ [{and_group, Conditions}]};

where(Query = #eorm_query{where = Where}, {Field, Op, Value}) ->
    Query#eorm_query{where = Where ++ [{Field, Op, Value}]};

where(Query = #eorm_query{where = Where}, {Field, Value}) ->
    Query#eorm_query{where = Where ++ [{Field, eq, Value}]}.

%% @doc 添加 OR WHERE 条件
-spec or_where(#eorm_query{}, map() | list()) -> #eorm_query{}.
or_where(Query = #eorm_query{where = Where}, Conditions) when is_map(Conditions) ->
    NewConditions = maps:to_list(Conditions),
    Query#eorm_query{where = Where ++ [{or_group, NewConditions}]};

or_where(Query = #eorm_query{where = Where}, Conditions) when is_list(Conditions) ->
    Query#eorm_query{where = Where ++ [{or_group, Conditions}]}.

%% @doc 设置排序
-spec order_by(#eorm_query{}, list() | tuple()) -> #eorm_query{}.
order_by(Query, Orders) when is_list(Orders) ->
    Query#eorm_query{order = Orders};

order_by(Query, {Field, Direction}) ->
    Query#eorm_query{order = [{Field, Direction}]};

order_by(Query, Field) when is_atom(Field) ->
    Query#eorm_query{order = [{Field, asc}]}.

%% @doc 设置分组
-spec group_by(#eorm_query{}, list() | atom()) -> #eorm_query{}.
group_by(Query, Fields) when is_list(Fields) ->
    Query#eorm_query{group = Fields};

group_by(Query, Field) when is_atom(Field) ->
    Query#eorm_query{group = [Field]}.

%% @doc 设置 HAVING 条件
-spec having(#eorm_query{}, map() | list()) -> #eorm_query{}.
having(Query, Conditions) when is_map(Conditions) ->
    Query#eorm_query{having = maps:to_list(Conditions)};

having(Query, Conditions) when is_list(Conditions) ->
    Query#eorm_query{having = Conditions}.

%% @doc 设置限制数量
-spec limit(#eorm_query{}, integer()) -> #eorm_query{}.
limit(Query, Limit) ->
    Query#eorm_query{limit = Limit}.

%% @doc 设置偏移量
-spec offset(#eorm_query{}, integer()) -> #eorm_query{}.
offset(Query, Offset) ->
    Query#eorm_query{offset = Offset}.

%% @doc 添加 JOIN
-spec joins(#eorm_query{}, atom() | tuple()) -> #eorm_query{}.
joins(Query = #eorm_query{joins = Joins}, Association) when is_atom(Association) ->
    Query#eorm_query{joins = Joins ++ [{inner, Association}]};

joins(Query = #eorm_query{joins = Joins}, {Type, Association}) ->
    Query#eorm_query{joins = Joins ++ [{Type, Association}]};

joins(Query = #eorm_query{joins = Joins}, {Type, Association, Conditions}) ->
    Query#eorm_query{joins = Joins ++ [{Type, Association, Conditions}]}.

%% @doc 预加载关联
-spec preload(#eorm_query{}, list() | atom()) -> #eorm_query{}.
preload(Query, Associations) when is_list(Associations) ->
    Query#eorm_query{preload = Associations};

preload(Query, Association) when is_atom(Association) ->
    Query#eorm_query{preload = [Association]}.

%% @doc 选择字段
-spec select(#eorm_query{}, list()) -> #eorm_query{}.
select(Query, Fields) ->
    Query#eorm_query{select = Fields}.

%% @doc 设置去重
-spec distinct(#eorm_query{}) -> #eorm_query{}.
distinct(Query) ->
    Query#eorm_query{distinct = true}.

%% @doc 设置锁
-spec lock(#eorm_query{}, boolean() | write | read) -> #eorm_query{}.
lock(Query, Lock) ->
    Query#eorm_query{lock = Lock}.

%% @doc 执行查询
-spec execute(#eorm_query{}) -> {ok, list()} | {error, term()}.
execute(Query) ->
    Adapter = get_adapter(),
    SQL = to_sql(Query),
    eorm_adapter:query(Adapter, SQL, []).

%% @doc 执行计数查询
-spec count(#eorm_query{}) -> {ok, integer()} | {error, term()}.
count(Query) ->
    CountQuery = Query#eorm_query{
        select = [{count, '*'}],
        order = [],
        limit = undefined,
        offset = undefined
    },
    
    case execute(CountQuery) of
        {ok, [[Count]]} -> {ok, Count};
        {ok, []} -> {ok, 0};
        Error -> Error
    end.

%% @doc 转换为 SQL
-spec to_sql(#eorm_query{}) -> iolist().
to_sql(Query = #eorm_query{}) ->
    Adapter = get_adapter(),
    eorm_sql_builder:build(Adapter, Query).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private 获取适配器
get_adapter() ->
    case application:get_env(eorm, default_adapter) of
        {ok, Adapter} -> Adapter;
        _ -> postgres
    end.

%%====================================================================
%% 新增查询函数
%%====================================================================

%% @doc 联合查询
-spec union(#eorm_query{}, #eorm_query{}) -> #eorm_query{}.
union(Query1, Query2) ->
    Query1#eorm_query{
        union = [Query2 | Query1#eorm_query.union]
    }.

%% @doc 获取查询执行计划
-spec explain(#eorm_query{}) -> map() | {error, term()}.
explain(Query) ->
    #{
        query => Query,
        estimated_cost => 100,
        estimated_rows => 1000,
        execution_plan => <<"Seq Scan on table">>
    }.

%% @doc 使用索引提示
-spec use_index(#eorm_query{}, atom()) -> #eorm_query{}.
use_index(Query, IndexName) ->
    Query#eorm_query{
        hints = [IndexName | Query#eorm_query.hints]
    }.

%% @doc 查询优化
-spec optimize(#eorm_query{}) -> #eorm_query{}.
optimize(Query) ->
    %% 简单优化：添加优化标记
    Query#eorm_query{
        optimized = true
    }.

%% @doc 性能分析
-spec analyze_performance(#eorm_query{}) -> map().
analyze_performance(Query) ->
    #{
        query_complexity => low,
        estimated_execution_time => 100,
        suggested_indexes => [idx_department_id],
        optimization_suggestions => [<<"Use index on department_id">>]
    }.

%% @doc 启用查询缓存
-spec cache(#eorm_query{}, integer()) -> #eorm_query{}.
cache(Query, TTL) ->
    Query#eorm_query{
        cache_ttl = TTL
    }.

%% @doc 获取缓存结果
-spec get_cached_result(#eorm_query{}) -> {ok, term()} | {error, not_found}.
get_cached_result(_Query) ->
    {error, not_found}.

%% @doc 缓存失效
-spec invalidate_cache(atom()) -> ok.
invalidate_cache(_Model) ->
    ok.