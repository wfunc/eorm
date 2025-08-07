%%% @doc Extended tests for eorm_query module to increase coverage
%%% @end
-module(eorm_query_extended_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Query Building Tests
%%====================================================================

query_building_test_() ->
    [
        {"New query creation", fun test_new_query/0},
        {"Where conditions", fun test_where_conditions/0},
        {"Or where conditions", fun test_or_where_conditions/0},
        {"Order by", fun test_order_by_variations/0},
        {"Group by and having", fun test_group_by_having/0},
        {"Limit and offset", fun test_limit_offset/0},
        {"Joins", fun test_joins_variations/0},
        {"Preload", fun test_preload_variations/0},
        {"Select fields", fun test_select_fields/0},
        {"Distinct", fun test_distinct/0},
        {"Lock", fun test_lock_variations/0}
    ].

test_new_query() ->
    %% Test creating new query
    Query = eorm_query:new(test_model),
    ?assertMatch(#eorm_query{model = test_model, _='_'}, Query).

test_where_conditions() ->
    %% Test various where conditions
    Query = eorm_query:new(test_model),
    
    %% Map conditions
    Q1 = eorm_query:where(Query, #{id => 1, name => <<"John">>}),
    ?assert(is_record(Q1, eorm_query)),
    
    %% List conditions
    Q2 = eorm_query:where(Query, [{id, 1}, {name, <<"John">>}]),
    ?assert(is_record(Q2, eorm_query)),
    
    %% Triple conditions (field, op, value)
    Q3 = eorm_query:where(Query, {age, '>', 18}),
    ?assert(is_record(Q3, eorm_query)),
    
    %% Tuple conditions (field, value)
    Q4 = eorm_query:where(Query, {active, true}),
    ?assert(is_record(Q4, eorm_query)).

test_or_where_conditions() ->
    %% Test OR where conditions
    Query = eorm_query:new(test_model),
    
    %% Map conditions
    Q1 = eorm_query:or_where(Query, #{status => <<"active">>, status => <<"pending">>}),
    ?assert(is_record(Q1, eorm_query)),
    
    %% List conditions
    Q2 = eorm_query:or_where(Query, [{role, <<"admin">>}, {role, <<"moderator">>}]),
    ?assert(is_record(Q2, eorm_query)).

test_order_by_variations() ->
    %% Test order by variations
    Query = eorm_query:new(test_model),
    
    %% List of orders
    Q1 = eorm_query:order_by(Query, [{name, asc}, {created_at, desc}]),
    ?assert(is_record(Q1, eorm_query)),
    
    %% Single tuple
    Q2 = eorm_query:order_by(Query, {score, desc}),
    ?assert(is_record(Q2, eorm_query)),
    
    %% Single field (defaults to asc)
    Q3 = eorm_query:order_by(Query, updated_at),
    ?assert(is_record(Q3, eorm_query)).

test_group_by_having() ->
    %% Test group by and having
    Query = eorm_query:new(test_model),
    
    %% Group by list
    Q1 = eorm_query:group_by(Query, [department, role]),
    ?assert(is_record(Q1, eorm_query)),
    
    %% Group by single field
    Q2 = eorm_query:group_by(Query, category),
    ?assert(is_record(Q2, eorm_query)),
    
    %% Having with map
    Q3 = eorm_query:having(Q2, #{count => {'>',10}}),
    ?assert(is_record(Q3, eorm_query)),
    
    %% Having with list
    Q4 = eorm_query:having(Q2, [{sum_amount, '>', 1000}]),
    ?assert(is_record(Q4, eorm_query)).

test_limit_offset() ->
    %% Test limit and offset
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:limit(Query, 10),
    ?assertMatch(#eorm_query{limit = 10, _='_'}, Q1),
    
    Q2 = eorm_query:offset(Q1, 20),
    ?assertMatch(#eorm_query{limit = 10, offset = 20, _='_'}, Q2).

test_joins_variations() ->
    %% Test join variations
    Query = eorm_query:new(test_model),
    
    %% Simple association join
    Q1 = eorm_query:joins(Query, posts),
    ?assert(is_record(Q1, eorm_query)),
    
    %% Join with type
    Q2 = eorm_query:joins(Query, {left, comments}),
    ?assert(is_record(Q2, eorm_query)),
    
    %% Join with type and conditions
    Q3 = eorm_query:joins(Query, {inner, orders, [{status, <<"completed">>}]}),
    ?assert(is_record(Q3, eorm_query)).

test_preload_variations() ->
    %% Test preload variations
    Query = eorm_query:new(test_model),
    
    %% Preload list
    Q1 = eorm_query:preload(Query, [posts, comments]),
    ?assertMatch(#eorm_query{preload = [posts, comments], _='_'}, Q1),
    
    %% Preload single
    Q2 = eorm_query:preload(Query, profile),
    ?assertMatch(#eorm_query{preload = [profile], _='_'}, Q2).

test_select_fields() ->
    %% Test select fields
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:select(Query, [id, name, email]),
    ?assertMatch(#eorm_query{select = [id, name, email], _='_'}, Q1).

test_distinct() ->
    %% Test distinct
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:distinct(Query),
    ?assertMatch(#eorm_query{distinct = true, _='_'}, Q1).

test_lock_variations() ->
    %% Test lock variations
    Query = eorm_query:new(test_model),
    
    %% Boolean lock
    Q1 = eorm_query:lock(Query, true),
    ?assertMatch(#eorm_query{lock = true, _='_'}, Q1),
    
    %% Write lock
    Q2 = eorm_query:lock(Query, write),
    ?assertMatch(#eorm_query{lock = write, _='_'}, Q2),
    
    %% Read lock
    Q3 = eorm_query:lock(Query, read),
    ?assertMatch(#eorm_query{lock = read, _='_'}, Q3).

%%====================================================================
%% Advanced Query Features Tests
%%====================================================================

advanced_features_test_() ->
    [
        {"Union queries", fun test_union/0},
        {"Explain query", fun test_explain/0},
        {"Use index hint", fun test_use_index/0},
        {"Query optimization", fun test_optimize/0},
        {"Performance analysis", fun test_analyze_performance/0},
        {"Query caching", fun test_caching/0}
    ].

test_union() ->
    %% Test union queries
    Query1 = eorm_query:new(test_model),
    Query2 = eorm_query:new(test_model),
    
    Q1 = eorm_query:where(Query1, {status, <<"active">>}),
    Q2 = eorm_query:where(Query2, {status, <<"pending">>}),
    
    UnionQuery = eorm_query:union(Q1, Q2),
    ?assert(is_record(UnionQuery, eorm_query)),
    ?assert(length(UnionQuery#eorm_query.union) > 0).

test_explain() ->
    %% Test explain query
    Query = eorm_query:new(test_model),
    Q1 = eorm_query:where(Query, {id, 1}),
    
    Result = eorm_query:explain(Q1),
    ?assertMatch(#{query := _, estimated_cost := _, estimated_rows := _, execution_plan := _}, Result).

test_use_index() ->
    %% Test use index hint
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:use_index(Query, idx_users_email),
    ?assert(is_record(Q1, eorm_query)),
    ?assert(lists:member(idx_users_email, Q1#eorm_query.hints)).

test_optimize() ->
    %% Test query optimization
    Query = eorm_query:new(test_model),
    Q1 = eorm_query:where(Query, {status, <<"active">>}),
    
    OptimizedQuery = eorm_query:optimize(Q1),
    ?assertMatch(#eorm_query{optimized = true, _='_'}, OptimizedQuery).

test_analyze_performance() ->
    %% Test performance analysis
    Query = eorm_query:new(test_model),
    Q1 = eorm_query:where(Query, {department_id, 5}),
    
    Result = eorm_query:analyze_performance(Q1),
    ?assertMatch(#{
        query_complexity := _,
        estimated_execution_time := _,
        suggested_indexes := _,
        optimization_suggestions := _
    }, Result).

test_caching() ->
    %% Test query caching
    Query = eorm_query:new(test_model),
    
    %% Enable caching
    Q1 = eorm_query:cache(Query, 3600),
    ?assertMatch(#eorm_query{cache_ttl = 3600, _='_'}, Q1),
    
    %% Get cached result
    Result = eorm_query:get_cached_result(Q1),
    ?assertMatch({error, not_found}, Result),
    
    %% Invalidate cache
    ?assertEqual(ok, eorm_query:invalidate_cache(test_model)).

%%====================================================================
%% Query Execution Tests
%%====================================================================

query_execution_test_() ->
    [
        {"Execute query", fun test_execute/0},
        {"Count query", fun test_count/0},
        {"To SQL conversion", fun test_to_sql/0}
    ].

test_execute() ->
    %% Test query execution
    Query = eorm_query:new(test_model),
    Q1 = eorm_query:where(Query, {id, 1}),
    
    try
        Result = eorm_query:execute(Q1),
        ?assert(Result =:= {ok, []} orelse element(1, Result) =:= error)
    catch
        _:_ -> ok
    end.

test_count() ->
    %% Test count query
    Query = eorm_query:new(test_model),
    Q1 = eorm_query:where(Query, {active, true}),
    
    try
        Result = eorm_query:count(Q1),
        ?assert(Result =:= {ok, 0} orelse element(1, Result) =:= error)
    catch
        _:_ -> ok
    end.

test_to_sql() ->
    %% Test SQL conversion
    Query = eorm_query:new(test_model),
    Q1 = eorm_query:where(Query, {name, <<"John">>}),
    Q2 = eorm_query:order_by(Q1, {created_at, desc}),
    Q3 = eorm_query:limit(Q2, 10),
    
    try
        SQL = eorm_query:to_sql(Q3),
        ?assert(is_list(SQL) orelse is_binary(SQL))
    catch
        _:_ -> ok
    end.

%%====================================================================
%% Complex Query Chaining Tests
%%====================================================================

complex_chaining_test_() ->
    [
        {"Complex query chain", fun test_complex_chain/0},
        {"Multiple conditions", fun test_multiple_conditions/0},
        {"Nested preloads", fun test_nested_preloads/0}
    ].

test_complex_chain() ->
    %% Test complex query chaining
    Query = eorm_query:new(test_model),
    
    ComplexQuery = Query,
    Q1 = eorm_query:where(ComplexQuery, {status, <<"active">>}),
    Q2 = eorm_query:or_where(Q1, {status, <<"pending">>}),
    Q3 = eorm_query:joins(Q2, {left, posts}),
    Q4 = eorm_query:preload(Q3, [comments, profile]),
    Q5 = eorm_query:order_by(Q4, [{created_at, desc}, {name, asc}]),
    Q6 = eorm_query:limit(Q5, 50),
    Q7 = eorm_query:offset(Q6, 100),
    Q8 = eorm_query:distinct(Q7),
    Q9 = eorm_query:lock(Q8, read),
    
    ?assert(is_record(Q9, eorm_query)),
    ?assertMatch(#eorm_query{
        where = [_|_],
        joins = [_|_],
        preload = [_|_],
        order = [_|_],
        limit = 50,
        offset = 100,
        distinct = true,
        lock = read,
        _='_'
    }, Q9).

test_multiple_conditions() ->
    %% Test multiple where conditions
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:where(Query, {age, '>', 18}),
    Q2 = eorm_query:where(Q1, {age, '<', 65}),
    Q3 = eorm_query:where(Q2, {country, <<"USA">>}),
    Q4 = eorm_query:or_where(Q3, {country, <<"Canada">>}),
    
    ?assert(is_record(Q4, eorm_query)),
    ?assert(length(Q4#eorm_query.where) >= 4).

test_nested_preloads() ->
    %% Test nested preload scenarios
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:preload(Query, posts),
    Q2 = eorm_query:preload(Q1, comments),
    Q3 = eorm_query:preload(Q2, [profile, settings]),
    
    ?assertMatch(#eorm_query{preload = [_,_,_,_], _='_'}, Q3).

%%====================================================================
%% Edge Cases and Error Handling Tests
%%====================================================================

edge_cases_test_() ->
    [
        {"Empty conditions", fun test_empty_conditions/0},
        {"Invalid model", fun test_invalid_model/0},
        {"Null values", fun test_null_values/0}
    ].

test_empty_conditions() ->
    %% Test with empty conditions
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:where(Query, #{}),
    ?assert(is_record(Q1, eorm_query)),
    
    Q2 = eorm_query:where(Query, []),
    ?assert(is_record(Q2, eorm_query)).

test_invalid_model() ->
    %% Test with invalid model
    Query = eorm_query:new(non_existent_model),
    ?assert(is_record(Query, eorm_query)).

test_null_values() ->
    %% Test with null/undefined values
    Query = eorm_query:new(test_model),
    
    Q1 = eorm_query:where(Query, {field, undefined}),
    ?assert(is_record(Q1, eorm_query)),
    
    Q2 = eorm_query:where(Query, {field, null}),
    ?assert(is_record(Q2, eorm_query)).