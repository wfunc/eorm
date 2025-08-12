%%% @doc Comprehensive tests for eorm_query to achieve 100% coverage
%%% @end
-module(eorm_query_comprehensive_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

setup() ->
    %% Mock eorm_adapter
    meck:new(eorm_adapter, [non_strict]),
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    
    %% Mock eorm_sql_builder
    meck:new(eorm_sql_builder, [non_strict]),
    meck:expect(eorm_sql_builder, build, fun(_, _) -> <<"SELECT * FROM users">> end),
    
    %% Mock eorm_registry
    meck:new(eorm_registry, [non_strict]),
    meck:expect(eorm_registry, get_model, fun(_) -> {error, not_found} end),
    
    %% Mock application for adapter config
    meck:new(application, [unstick, passthrough]),
    meck:expect(application, get_env, fun
        (eorm, default_adapter) -> {ok, postgres};
        (App, Key) -> meck:passthrough([App, Key])
    end),
    
    ok.

teardown(_) ->
    meck:unload(),
    ok.

%%====================================================================
%% Test Suites
%%====================================================================

query_builder_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
         fun test_new/0,
         fun test_new_with_model/0,
         fun test_select/0,
         fun test_from/0,
         fun test_where/0,
         fun test_where_tuple/0,
         fun test_where_pair/0,
         fun test_join/0,
         fun test_left_join/0,
         fun test_right_join/0,
         fun test_inner_join/0,
         fun test_order_by/0,
         fun test_order_by_tuple/0,
         fun test_group_by/0,
         fun test_group_by_atom/0,
         fun test_having/0,
         fun test_having_map/0,
         fun test_limit/0,
         fun test_offset/0,
         fun test_joins_atom/0,
         fun test_joins_type/0,
         fun test_preload_atom/0,
         fun test_build/0,
         fun test_execute/0,
         fun test_execute_with_adapter/0,
         fun test_execute_with_adapter_direct/0,
         fun test_count/0,
         fun test_count_empty/0,
         fun test_count_error/0,
         fun test_advanced_functions/0,
         fun test_missed_coverage/0,
         fun test_exists/0,
         fun test_chain_operations/0,
         fun test_complex_query/0,
         fun test_or_where/0,
         fun test_or_where_list/0,
         fun test_where_in/0,
         fun test_where_not_in/0,
         fun test_where_null/0,
         fun test_where_not_null/0,
         fun test_where_between/0,
         fun test_where_like/0,
         fun test_raw_where/0,
         fun test_distinct/0,
         fun test_union/0
     ]}.

%%====================================================================
%% Test Cases
%%====================================================================

test_new() ->
    Query = eorm_query:new(),
    ?assertMatch(#eorm_query{}, Query),
    ?assertEqual(undefined, Query#eorm_query.model),
    ?assertEqual(undefined, Query#eorm_query.table).

test_select() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:select(Query, [id, name]),
    ?assertEqual([id, name], Query1#eorm_query.select).

test_from() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:from(Query, users),
    ?assertEqual(users, Query1#eorm_query.from).

test_where() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:where(Query, {id, '=', 1}),
    ?assertMatch([{id, '=', 1}], Query1#eorm_query.where).

test_join() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:join(Query, posts, {users, id, posts, user_id}),
    ?assertMatch([{join, posts, _}], Query1#eorm_query.joins).

test_left_join() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:left_join(Query, posts, {users, id, posts, user_id}),
    ?assertMatch([{left_join, posts, _}], Query1#eorm_query.joins).

test_right_join() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:right_join(Query, posts, {users, id, posts, user_id}),
    ?assertMatch([{right_join, posts, _}], Query1#eorm_query.joins).

test_inner_join() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:inner_join(Query, posts, {users, id, posts, user_id}),
    ?assertMatch([{inner_join, posts, _}], Query1#eorm_query.joins).

test_order_by() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:order_by(Query, {created_at, desc}),
    ?assertMatch([{created_at, desc}], Query1#eorm_query.order).

test_group_by() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:group_by(Query, [category]),
    ?assertEqual([category], Query1#eorm_query.group).

test_having() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:having(Query, [{'COUNT(*)', '>', 5}]),
    ?assertMatch([{'COUNT(*)', '>', 5}], Query1#eorm_query.having).

test_limit() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:limit(Query, 10),
    ?assertEqual(10, Query1#eorm_query.limit).

test_offset() ->
    Query = eorm_query:new(),
    Query1 = eorm_query:offset(Query, 20),
    ?assertEqual(20, Query1#eorm_query.offset).

test_build() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:select(Query0, [id, name]),
    Query2 = eorm_query:from(Query1, users),
    Query3 = eorm_query:where(Query2, {active, '=', true}),
    Query = eorm_query:limit(Query3, 10),
    SQL = eorm_query:build(Query),
    ?assert(is_binary(SQL)).

test_execute() ->
    %% Mock actual query execution
    meck:expect(eorm_adapter, query, fun(postgres, SQL, _) ->
        ?assert(is_binary(SQL)),
        {ok, [#{id => 1, name => <<"John">>}]}
    end),
    Query0 = eorm_query:new(),
    Query = eorm_query:from(Query0, users),
    Result = eorm_query:execute(Query),
    ?assertMatch({ok, [#{id := 1}]}, Result).

test_execute_with_adapter() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [#{id => 1}]} end),
    Query0 = eorm_query:new(),
    Query = eorm_query:from(Query0, users),
    Result = eorm_query:execute(Query, postgres),
    ?assertMatch({ok, [#{id := 1}]}, Result).

test_count() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [[42]]} end),
    Query0 = eorm_query:new(),
    Query = eorm_query:from(Query0, users),
    Result = eorm_query:count(Query),
    ?assertEqual({ok, 42}, Result).

test_exists() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [#{exists => true}]} end),
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where(Query1, {id, '=', 1}),
    Result = eorm_query:exists(Query),
    ?assertEqual({ok, true}, Result).

test_chain_operations() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:select(Query0, [id, name, email]),
    Query2 = eorm_query:from(Query1, users),
    Query3 = eorm_query:where(Query2, {active, '=', true}),
    Query4 = eorm_query:where(Query3, {age, '>', 18}),
    Query5 = eorm_query:order_by(Query4, {created_at, desc}),
    Query6 = eorm_query:limit(Query5, 10),
    Query = eorm_query:offset(Query6, 0),
    ?assertEqual([id, name, email], Query#eorm_query.select),
    ?assertEqual(users, Query#eorm_query.from),
    ?assertEqual([{active, '=', true}, {age, '>', 18}], Query#eorm_query.where),
    ?assertEqual([{created_at, desc}], Query#eorm_query.order),
    ?assertEqual(10, Query#eorm_query.limit),
    ?assertEqual(0, Query#eorm_query.offset).

test_complex_query() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:select(Query0, [users, '*', posts, title]),
    Query2 = eorm_query:from(Query1, users),
    Query3 = eorm_query:left_join(Query2, posts, {users, id, posts, user_id}),
    Query4 = eorm_query:where(Query3, {active, '=', true}),
    Query5 = eorm_query:group_by(Query4, [users, id]),
    Query6 = eorm_query:having(Query5, [{'COUNT(posts.id)', '>', 5}]),
    Query = eorm_query:order_by(Query6, {created_at, desc}),
    SQL = eorm_query:build(Query),
    ?assert(is_binary(SQL)).

test_or_where() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query2 = eorm_query:where(Query1, {status, '=', active}),
    Query = eorm_query:or_where(Query2, [{status, '=', pending}]),
    ?assert(length(Query#eorm_query.where) > 0).

test_where_in() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where_in(Query1, id, [1, 2, 3]),
    ?assertMatch([{id, 'IN', [1, 2, 3]}], Query#eorm_query.where).

test_where_not_in() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where_not_in(Query1, id, [4, 5, 6]),
    ?assertMatch([{id, 'NOT IN', [4, 5, 6]}], Query#eorm_query.where).

test_where_null() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where_null(Query1, deleted_at),
    ?assertMatch([{deleted_at, 'IS', null}], Query#eorm_query.where).

test_where_not_null() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where_not_null(Query1, email),
    ?assertMatch([{email, 'IS NOT', null}], Query#eorm_query.where).

test_where_between() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where_between(Query1, age, 18, 65),
    ?assertMatch([{age, 'BETWEEN', {18, 65}}], Query#eorm_query.where).

test_where_like() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:where_like(Query1, email, <<"%@example.com">>),
    ?assertMatch([{email, 'LIKE', <<"%@example.com">>}], Query#eorm_query.where).

test_raw_where() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:from(Query0, users),
    Query = eorm_query:raw_where(Query1, <<"age > 18 AND status = 'active'">>),
    ?assertMatch([{raw, <<"age > 18 AND status = 'active'">>}], Query#eorm_query.where).

test_distinct() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:distinct(Query0),
    Query2 = eorm_query:select(Query1, [category]),
    Query = eorm_query:from(Query2, products),
    ?assertEqual(true, Query#eorm_query.distinct).

test_union() ->
    Q1_0 = eorm_query:new(),
    Q1_1 = eorm_query:select(Q1_0, [id, name]),
    Q1_2 = eorm_query:from(Q1_1, users),
    Query1 = eorm_query:where(Q1_2, {active, '=', true}),
    
    Q2_0 = eorm_query:new(),
    Q2_1 = eorm_query:select(Q2_0, [id, name]),
    Q2_2 = eorm_query:from(Q2_1, archived_users),
    Query2 = eorm_query:where(Q2_2, {archived_at, '>', <<"2024-01-01">>}),
    
    UnionQuery = eorm_query:union(Query1, Query2),
    ?assertMatch([_|_], UnionQuery#eorm_query.union).

%% New test functions for uncovered cases
test_new_with_model() ->
    %% Test with model registered in registry
    meck:expect(eorm_registry, get_model, fun(registered_model) -> 
        {ok, #eorm_model{module = registered_model, table = custom_table}}
    end),
    Query = eorm_query:new(registered_model),
    ?assertEqual(registered_model, Query#eorm_query.model),
    ?assertEqual(custom_table, Query#eorm_query.table).

test_where_tuple() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:where(Query0, {status, '=', active}),
    ?assertMatch([{status, '=', active}], Query#eorm_query.where).

test_where_pair() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:where(Query0, {status, active}),
    ?assertMatch([{status, eq, active}], Query#eorm_query.where).

test_order_by_tuple() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:order_by(Query0, {name, desc}),
    ?assertMatch([{name, desc}], Query#eorm_query.order).

test_group_by_atom() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:group_by(Query0, department),
    ?assertEqual([department], Query#eorm_query.group).

test_having_map() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:having(Query0, #{'COUNT(*)' => 10}),
    ?assertMatch([{'COUNT(*)', 10}], Query#eorm_query.having).

test_joins_atom() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:joins(Query0, posts),
    ?assertMatch([{inner, posts}], Query#eorm_query.joins).

test_joins_type() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:joins(Query0, {left, comments}),
    ?assertMatch([{left, comments}], Query#eorm_query.joins).

test_preload_atom() ->
    Query0 = eorm_query:new(),
    Query = eorm_query:preload(Query0, author),
    ?assertEqual([author], Query#eorm_query.preload).

test_or_where_list() ->
    Query0 = eorm_query:new(),
    Query1 = eorm_query:where(Query0, {active, '=', true}),
    Query = eorm_query:or_where(Query1, [{pending, '=', true}]),
    ?assert(length(Query#eorm_query.where) > 0).

test_execute_with_adapter_direct() ->
    %% Test direct execute/2 function
    meck:expect(eorm_adapter, query, fun(postgres, _, _) -> {ok, [#{id => 100}]} end),
    Query0 = eorm_query:new(),
    Query = eorm_query:from(Query0, users),
    Result = eorm_query:execute(Query, postgres),
    ?assertMatch({ok, [#{id := 100}]}, Result).

test_count_empty() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    Query0 = eorm_query:new(),
    Query = eorm_query:from(Query0, users),
    Result = eorm_query:count(Query),
    ?assertEqual({ok, 0}, Result).

test_count_error() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {error, connection_failed} end),
    Query0 = eorm_query:new(),
    Query = eorm_query:from(Query0, users),
    Result = eorm_query:count(Query),
    ?assertEqual({error, connection_failed}, Result).

%% Test advanced functions for coverage 
test_advanced_functions() ->
    Query = eorm_query:new(),
    
    %% Test explain
    ExplainResult = eorm_query:explain(Query),
    ?assertMatch(#{query := _}, ExplainResult),
    
    %% Test use_index
    Query1 = eorm_query:use_index(Query, idx_name),
    ?assertMatch([idx_name], Query1#eorm_query.hints),
    
    %% Test optimize
    Query2 = eorm_query:optimize(Query),
    ?assertEqual(true, Query2#eorm_query.optimized),
    
    %% Test analyze_performance
    PerfResult = eorm_query:analyze_performance(Query),
    ?assertMatch(#{query_complexity := low}, PerfResult),
    
    %% Test cache
    Query3 = eorm_query:cache(Query, 3600),
    ?assertEqual(3600, Query3#eorm_query.cache_ttl),
    
    %% Test get_cached_result
    CacheResult = eorm_query:get_cached_result(Query),
    ?assertEqual({error, not_found}, CacheResult),
    
    %% Test invalidate_cache
    InvalidateResult = eorm_query:invalidate_cache(test_model),
    ?assertEqual(ok, InvalidateResult).

%% Test specifically missed coverage lines
test_missed_coverage() ->
    Query = eorm_query:new(),
    
    %% Test WHERE with 3-tuple {Field, Op, Value} - line 67
    Query1 = eorm_query:where(Query, {status, '!=', inactive}),
    ?assertMatch([{status, '!=', inactive}], Query1#eorm_query.where),
    
    %% Test WHERE with 2-tuple {Field, Value} - line 70  
    Query2 = eorm_query:where(Query, {active, true}),
    ?assertMatch([{active, eq, true}], Query2#eorm_query.where),
    
    %% Test OR WHERE with list - line 79
    Query3 = eorm_query:or_where(Query, [{status, active}, {type, premium}]),
    ?assertMatch([{or_group, [{status, active}, {type, premium}]}], Query3#eorm_query.where),
    
    %% Test ORDER BY with tuple - line 87
    Query4 = eorm_query:order_by(Query, {name, asc}),
    ?assertMatch([{name, asc}], Query4#eorm_query.order),
    
    %% Test GROUP BY with atom - line 98
    Query5 = eorm_query:group_by(Query, category),
    ?assertEqual([category], Query5#eorm_query.group),
    
    %% Test HAVING with map - line 103
    Query6 = eorm_query:having(Query, #{count => 5}),
    ?assertMatch([{count, 5}], Query6#eorm_query.having),
    
    %% Test JOINS with atom - line 121
    Query7 = eorm_query:joins(Query, posts),
    ?assertMatch([{inner, posts}], Query7#eorm_query.joins),
    
    %% Test JOINS with type tuple - line 124
    Query8 = eorm_query:joins(Query, {left, comments}),
    ?assertMatch([{left, comments}], Query8#eorm_query.joins),
    
    %% Test PRELOAD with atom - line 135
    Query9 = eorm_query:preload(Query, author),
    ?assertEqual([author], Query9#eorm_query.preload),
    
    %% Test actual execute call - line 157
    meck:expect(eorm_adapter, query, fun(postgres, _, _) -> {ok, [#{id => 123}]} end),
    Query10 = eorm_query:from(Query, test_table),
    ExecuteResult = eorm_query:execute(Query10),
    ?assertMatch({ok, [#{id := 123}]}, ExecuteResult),
    
    %% Test count with different result formats - lines 170-172
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [[99]]} end),
    CountResult1 = eorm_query:count(Query10),
    ?assertEqual({ok, 99}, CountResult1),
    
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    CountResult2 = eorm_query:count(Query10),
    ?assertEqual({ok, 0}, CountResult2),
    
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {error, timeout} end),
    CountResult3 = eorm_query:count(Query10),
    ?assertEqual({error, timeout}, CountResult3).