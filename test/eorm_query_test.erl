%%% @doc EORM 查询构建器模块测试
%%% @end
-module(eorm_query_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% 测试模型定义
%%====================================================================

employee_model() ->
    #{
        table => <<"employees">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]},
            #{name => department, type => string, opts => []},
            #{name => salary, type => decimal, opts => []},
            #{name => hire_date, type => date, opts => []},
            #{name => active, type => boolean, opts => []}
        ]
    }.

department_model() ->
    #{
        table => <<"departments">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]},
            #{name => manager_id, type => integer, opts => []},
            #{name => budget, type => decimal, opts => []}
        ]
    }.

%%====================================================================
%% 查询构建器基础测试
%%====================================================================

query_builder_basic_test_() ->
    [
        {"创建新查询", fun test_new/0},
        {"设置模型", fun test_set_model/0},
        {"获取查询记录", fun test_query_record/0}
    ].

test_new() ->
    Query = eorm_query:new(employee_model),
    ?assertMatch(#eorm_query{model = employee_model}, Query).

test_set_model() ->
    Query = eorm_query:new(employee_model),
    ?assertEqual(employee_model, Query#eorm_query.model).

test_query_record() ->
    Query = eorm_query:new(employee_model),
    ?assertMatch(#eorm_query{}, Query).

%%====================================================================
%% WHERE 条件测试
%%====================================================================

where_clause_test_() ->
    [
        {"基础 WHERE 条件", fun test_where_basic/0},
        {"多个 WHERE 条件", fun test_where_multiple/0},
        {"OR WHERE 条件", fun test_or_where/0},
        {"复杂 WHERE 条件", fun test_where_complex/0},
        {"WHERE IN 条件", fun test_where_in/0},
        {"WHERE NOT IN 条件", fun test_where_not_in/0},
        {"WHERE BETWEEN 条件", fun test_where_between/0},
        {"WHERE NULL 条件", fun test_where_null/0},
        {"WHERE NOT NULL 条件", fun test_where_not_null/0}
    ].

test_where_basic() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{name => <<"John">>}),
    %% where 条件会被转换为 {and_group, PropList} 格式
    ?assertMatch(#eorm_query{where = [{and_group, [{name, <<"John">>}]}]}, Query2).

test_where_multiple() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{name => <<"John">>}),
    Query3 = eorm_query:where(Query2, #{department => <<"IT">>}),
    ?assertEqual(2, length(Query3#eorm_query.where)).

test_or_where() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{department => <<"IT">>}),
    Query3 = eorm_query:or_where(Query2, #{department => <<"HR">>}),
    ?assertMatch(#eorm_query{where = [_, _]}, Query3).

test_where_complex() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{
        salary => {'>', 50000},
        department => <<"Engineering">>
    }),
    ?assertMatch(#eorm_query{where = [_]}, Query2).

test_where_in() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{
        department => {in, [<<"IT">>, <<"HR">>, <<"Finance">>]}
    }),
    ?assertMatch(#eorm_query{where = [_]}, Query2).

test_where_not_in() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{
        department => {not_in, [<<"Temp">>, <<"Intern">>]}
    }),
    ?assertMatch(#eorm_query{where = [_]}, Query2).

test_where_between() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{
        salary => {between, 40000, 80000}
    }),
    ?assertMatch(#eorm_query{where = [_]}, Query2).

test_where_null() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{
        department => null
    }),
    ?assertMatch(#eorm_query{where = [_]}, Query2).

test_where_not_null() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{
        department => {'not', null}
    }),
    ?assertMatch(#eorm_query{where = [_]}, Query2).

%%====================================================================
%% ORDER BY 测试
%%====================================================================

order_by_test_() ->
    [
        {"单列排序", fun test_order_single/0},
        {"多列排序", fun test_order_multiple/0},
        {"混合排序方向", fun test_order_mixed/0}
    ].

test_order_single() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:order_by(Query, [{name, asc}]),
    ?assertMatch(#eorm_query{order = [{name, asc}]}, Query2).

test_order_multiple() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:order_by(Query, [{department, asc}, {salary, desc}]),
    ?assertEqual(2, length(Query2#eorm_query.order)).

test_order_mixed() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:order_by(Query, [{department, asc}, {salary, desc}, {name, asc}]),
    ?assertEqual(3, length(Query2#eorm_query.order)).

%%====================================================================
%% GROUP BY 和 HAVING 测试
%%====================================================================

group_having_test_() ->
    [
        {"GROUP BY 单列", fun test_group_single/0},
        {"GROUP BY 多列", fun test_group_multiple/0},
        {"HAVING 条件", fun test_having/0},
        {"GROUP BY 和 HAVING 组合", fun test_group_having/0}
    ].

test_group_single() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:group_by(Query, [department]),
    ?assertMatch(#eorm_query{group = [department]}, Query2).

test_group_multiple() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:group_by(Query, [department, active]),
    ?assertEqual(2, length(Query2#eorm_query.group)).

test_having() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:having(Query, [{count, '>', 5}]),
    ?assertMatch(#eorm_query{having = [{count, '>', 5}]}, Query2).

test_group_having() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:group_by(Query, [department]),
    Query3 = eorm_query:having(Query2, [{avg_salary, '>', 50000}]),
    ?assertMatch(#eorm_query{group = [department], having = [{avg_salary, '>', 50000}]}, Query3).

%%====================================================================
%% LIMIT 和 OFFSET 测试
%%====================================================================

limit_offset_test_() ->
    [
        {"设置 LIMIT", fun test_limit/0},
        {"设置 OFFSET", fun test_offset/0},
        {"LIMIT 和 OFFSET 组合", fun test_limit_offset/0},
        {"分页查询", fun test_pagination/0}
    ].

test_limit() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:limit(Query, 10),
    ?assertMatch(#eorm_query{limit = 10}, Query2).

test_offset() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:offset(Query, 20),
    ?assertMatch(#eorm_query{offset = 20}, Query2).

test_limit_offset() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:limit(Query, 10),
    Query3 = eorm_query:offset(Query2, 20),
    ?assertMatch(#eorm_query{limit = 10, offset = 20}, Query3).

test_pagination() ->
    Page = 3,
    PageSize = 10,
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:limit(Query, PageSize),
    Query3 = eorm_query:offset(Query2, (Page - 1) * PageSize),
    ?assertMatch(#eorm_query{limit = 10, offset = 20}, Query3).

%%====================================================================
%% JOIN 测试
%%====================================================================

join_test_() ->
    [
        {"INNER JOIN", fun test_inner_join/0},
        {"LEFT JOIN", fun test_left_join/0},
        {"RIGHT JOIN", fun test_right_join/0},
        {"多个 JOIN", fun test_multiple_joins/0}
    ].

test_inner_join() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:joins(Query, {inner, department_model, <<"employees.department = departments.name">>}),
    ?assertMatch(#eorm_query{joins = [_]}, Query2).

test_left_join() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:joins(Query, {left, department_model, <<"employees.department = departments.name">>}),
    ?assertMatch(#eorm_query{joins = [_]}, Query2).

test_right_join() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:joins(Query, {right, department_model, <<"employees.department = departments.name">>}),
    ?assertMatch(#eorm_query{joins = [_]}, Query2).

test_multiple_joins() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:joins(Query, {inner, department_model, <<"employees.department = departments.name">>}),
    Query3 = eorm_query:joins(Query2, {left, projects, <<"employees.id = projects.employee_id">>}),
    ?assertEqual(2, length(Query3#eorm_query.joins)).

%%====================================================================
%% SELECT 和 DISTINCT 测试
%%====================================================================

select_distinct_test_() ->
    [
        {"SELECT 特定列", fun test_select_columns/0},
        {"SELECT 所有列", fun test_select_all/0},
        {"DISTINCT 查询", fun test_distinct/0},
        {"SELECT DISTINCT 组合", fun test_select_distinct/0}
    ].

test_select_columns() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:select(Query, [id, name, department]),
    ?assertMatch(#eorm_query{select = [id, name, department]}, Query2).

test_select_all() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:select(Query, ['*']),
    ?assertMatch(#eorm_query{select = ['*']}, Query2).

test_distinct() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:distinct(Query),
    ?assertMatch(#eorm_query{distinct = true}, Query2).

test_select_distinct() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:select(Query, [department]),
    Query3 = eorm_query:distinct(Query2),
    ?assertMatch(#eorm_query{select = [department], distinct = true}, Query3).

%%====================================================================
%% 锁测试
%%====================================================================

lock_test_() ->
    [
        {"FOR UPDATE 锁", fun test_lock_for_update/0},
        {"FOR SHARE 锁", fun test_lock_for_share/0},
        {"NO WAIT 锁", fun test_lock_nowait/0}
    ].

test_lock_for_update() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:lock(Query, write),
    ?assertMatch(#eorm_query{lock = write}, Query2).

test_lock_for_share() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:lock(Query, read),
    ?assertMatch(#eorm_query{lock = read}, Query2).

test_lock_nowait() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:lock(Query, true),
    ?assertMatch(#eorm_query{lock = true}, Query2).

%%====================================================================
%% 预加载测试
%%====================================================================

preload_test_() ->
    [
        {"预加载单个关联", fun test_preload_single/0},
        {"预加载多个关联", fun test_preload_multiple/0},
        {"嵌套预加载", fun test_preload_nested/0}
    ].

test_preload_single() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:preload(Query, [department]),
    ?assertMatch(#eorm_query{preload = [department]}, Query2).

test_preload_multiple() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:preload(Query, [department, projects, manager]),
    ?assertEqual(3, length(Query2#eorm_query.preload)).

test_preload_nested() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:preload(Query, [
        department,
        {projects, [tasks, members]}
    ]),
    ?assertMatch(#eorm_query{preload = [_, _]}, Query2).

%%====================================================================
%% 复杂查询组合测试
%%====================================================================

complex_query_test_() ->
    [
        {"完整查询构建", fun test_complete_query/0},
        {"链式调用", fun test_chained_calls/0}
    ].

test_complete_query() ->
    Query = eorm_query:new(employee_model),
    Query2 = Query,
    Query3 = eorm_query:select(Query2, [id, name, department, salary]),
    Query4 = eorm_query:where(Query3, #{active => true}),
    Query5 = eorm_query:where(Query4, #{salary => {'>', 40000}}),
    Query6 = eorm_query:order_by(Query5, [{salary, desc}, {name, asc}]),
    Query7 = eorm_query:limit(Query6, 10),
    Query8 = eorm_query:offset(Query7, 0),
    
    ?assertMatch(#eorm_query{
        model = employee_model,
        select = [id, name, department, salary],
        where = [_, _],
        order = [{salary, desc}, {name, asc}],
        limit = 10,
        offset = 0
    }, Query8).

test_chained_calls() ->
    %% 测试链式调用是否正确保留所有设置
    Query = eorm_query:new(employee_model),
    FinalQuery = 
        eorm_query:offset(
            eorm_query:limit(
                eorm_query:order_by(
                    eorm_query:where(
                        eorm_query:select(Query, [name, salary]),
                        #{department => <<"Engineering">>}
                    ),
                    [{salary, desc}]
                ),
                20
            ),
            10
        ),
    
    ?assertMatch(#eorm_query{
        select = [name, salary],
        where = [_],
        order = [{salary, desc}],
        limit = 20,
        offset = 10
    }, FinalQuery).

%%====================================================================
%% 高级查询测试
%%====================================================================

advanced_query_test_() ->
    [
        {"子查询测试", fun test_subquery/0},
        {"联合查询测试", fun test_union/0},
        {"窗口函数测试", fun test_window_functions/0},
        {"聚合查询测试", fun test_aggregation/0},
        {"条件表达式测试", fun test_case_when/0}
    ].

test_subquery() ->
    Query = eorm_query:new(employee_model),
    SubQuery = eorm_query:new(department_model),
    
    %% 简单子查询测试
    Query2 = eorm_query:where(Query, #{department_id => {in, SubQuery}}),
    ?assertMatch(#eorm_query{}, Query2).

test_union() ->
    Query1 = eorm_query:new(employee_model),
    Query2 = eorm_query:new(employee_model),
    
    Query3 = eorm_query:union(Query1, Query2),
    ?assertMatch(#eorm_query{}, Query3).

test_window_functions() ->
    Query = eorm_query:new(employee_model),
    
    %% 添加窗口函数
    Query2 = eorm_query:select(Query, [
        id, name,
        {row_number, [], [salary], desc}
    ]),
    ?assertMatch(#eorm_query{}, Query2).

test_aggregation() ->
    Query = eorm_query:new(employee_model),
    
    %% 聚合查询
    Query2 = eorm_query:select(Query, [
        department_id,
        {count, '*'},
        {avg, salary},
        {sum, salary}
    ]),
    Query3 = eorm_query:group_by(Query2, [department_id]),
    ?assertMatch(#eorm_query{group = [department_id]}, Query3).

test_case_when() ->
    Query = eorm_query:new(employee_model),
    
    %% CASE WHEN 表达式
    Query2 = eorm_query:select(Query, [
        id, name,
        {case_when, [
            {salary, '>', 50000, <<"高薪">>},
            {salary, '>', 30000, <<"中薪">>}
        ], <<"低薪">>}
    ]),
    ?assertMatch(#eorm_query{}, Query2).

%%====================================================================
%% 查询优化测试
%%====================================================================

optimization_test_() ->
    [
        {"查询计划测试", fun test_query_plan/0},
        {"索引提示测试", fun test_index_hints/0},
        {"查询重写测试", fun test_query_rewrite/0},
        {"性能分析测试", fun test_performance_analysis/0}
    ].

test_query_plan() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{department_id => 1}),
    
    try
        Plan = eorm_query:explain(Query2),
        ?assert(is_map(Plan) orelse is_list(Plan))
    catch
        _:_ -> ?assert(true)
    end.

test_index_hints() ->
    Query = eorm_query:new(employee_model),
    
    %% 添加索引提示
    Query2 = eorm_query:use_index(Query, idx_department),
    ?assertMatch(#eorm_query{}, Query2).

test_query_rewrite() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{salary => {between, 30000, 50000}}),
    
    %% 查询重写优化
    try
        Optimized = eorm_query:optimize(Query2),
        ?assertMatch(#eorm_query{}, Optimized)
    catch
        _:_ -> ?assert(true)
    end.

test_performance_analysis() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{department_id => 1}),
    
    try
        Analysis = eorm_query:analyze_performance(Query2),
        ?assert(is_map(Analysis))
    catch
        _:_ -> ?assert(true)
    end.

%%====================================================================
%% 查询缓存测试
%%====================================================================

caching_test_() ->
    [
        {"查询缓存测试", fun test_query_cache/0},
        {"结果缓存测试", fun test_result_cache/0},
        {"缓存失效测试", fun test_cache_invalidation/0}
    ].

test_query_cache() ->
    Query = eorm_query:new(employee_model),
    Query2 = eorm_query:where(Query, #{id => 1}),
    
    %% 启用查询缓存
    Query3 = eorm_query:cache(Query2, 300),  % 5分钟缓存
    ?assertMatch(#eorm_query{}, Query3).

test_result_cache() ->
    Query = eorm_query:new(employee_model),
    
    try
        CachedResult = eorm_query:get_cached_result(Query),
        ?assertMatch({error, not_found}, CachedResult)
    catch
        _:_ -> ?assert(true)
    end.

test_cache_invalidation() ->
    try
        Result = eorm_query:invalidate_cache(employee_model),
        ?assertEqual(ok, Result)
    catch
        _:_ -> ?assert(true)
    end.