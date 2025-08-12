%%% @doc Comprehensive tests for eorm_query_extended module
%%% Aim for 100% code coverage
%%% @end
-module(eorm_query_extended_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Test Functions
%%====================================================================

query_extended_test_() ->
    [
        {"From function tests", fun test_from/0},
        {"Join function tests", fun test_joins/0},
        {"Exists function tests", fun test_exists/0},
        {"Where IN tests", fun test_where_in/0},
        {"Where NOT IN tests", fun test_where_not_in/0},
        {"Where NULL tests", fun test_where_null/0},
        {"Where NOT NULL tests", fun test_where_not_null/0},
        {"Where BETWEEN tests", fun test_where_between/0},
        {"Where LIKE tests", fun test_where_like/0},
        {"Raw WHERE tests", fun test_raw_where/0},
        {"Build function tests", fun test_build/0},
        {"Execute with adapter tests", fun test_execute_with_adapter/0}
    ].

%%====================================================================
%% Individual Test Functions
%%====================================================================

test_from() ->
    Query = #eorm_query{},
    Result = eorm_query_extended:from(Query, users),
    ?assertEqual(users, Result#eorm_query.table),
    ?assertEqual(users, Result#eorm_query.from).

test_joins() ->
    Query = #eorm_query{joins = []},
    
    %% Test join
    Q1 = eorm_query_extended:join(Query, posts, {users, id, posts, user_id}),
    ?assertEqual([{join, posts, {users, id, posts, user_id}}], Q1#eorm_query.joins),
    
    %% Test left_join
    Q2 = eorm_query_extended:left_join(Query, comments, {posts, id, comments, post_id}),
    ?assertEqual([{left_join, comments, {posts, id, comments, post_id}}], Q2#eorm_query.joins),
    
    %% Test right_join
    Q3 = eorm_query_extended:right_join(Query, tags, {posts, id, tags, post_id}),
    ?assertEqual([{right_join, tags, {posts, id, tags, post_id}}], Q3#eorm_query.joins),
    
    %% Test inner_join
    Q4 = eorm_query_extended:inner_join(Query, categories, {posts, category_id, categories, id}),
    ?assertEqual([{inner_join, categories, {posts, category_id, categories, id}}], Q4#eorm_query.joins),
    
    %% Test multiple joins
    Q5 = eorm_query_extended:join(Query, posts, {users, id, posts, user_id}),
    Q6 = eorm_query_extended:left_join(Q5, comments, {posts, id, comments, post_id}),
    ?assertEqual(2, length(Q6#eorm_query.joins)).

test_exists() ->
    %% Mock eorm_query:execute to test different scenarios
    meck:new(eorm_query, [passthrough]),
    
    try
        Query = #eorm_query{table = users, where = [{id, '=', 1}]},
        
        %% Test case 1: No records found (line 54)
        meck:expect(eorm_query, execute, fun(_Q) -> {ok, []} end),
        Result1 = eorm_query_extended:exists(Query),
        ?assertEqual({ok, false}, Result1),
        
        %% Test case 2: Records found (line 55)
        meck:expect(eorm_query, execute, fun(_Q) -> {ok, [#{id => 1, name => <<"John">>}]} end),
        Result2 = eorm_query_extended:exists(Query),
        ?assertEqual({ok, true}, Result2),
        
        %% Test case 3: Multiple records found
        meck:expect(eorm_query, execute, fun(_Q) -> 
            {ok, [#{id => 1}, #{id => 2}, #{id => 3}]} 
        end),
        Result3 = eorm_query_extended:exists(Query),
        ?assertEqual({ok, true}, Result3),
        
        %% Test case 4: Error case (line 56)
        meck:expect(eorm_query, execute, fun(_Q) -> {error, database_unavailable} end),
        Result4 = eorm_query_extended:exists(Query),
        ?assertEqual({error, database_unavailable}, Result4),
        
        %% Test case 5: Another error scenario
        meck:expect(eorm_query, execute, fun(_Q) -> {error, {badmatch, something}} end),
        Result5 = eorm_query_extended:exists(Query),
        ?assertEqual({error, {badmatch, something}}, Result5)
        
    after
        meck:unload(eorm_query)
    end.

test_where_in() ->
    Query = #eorm_query{where = []},
    Values = [1, 2, 3, 4, 5],
    
    Result = eorm_query_extended:where_in(Query, id, Values),
    ?assertEqual([{id, 'IN', Values}], Result#eorm_query.where),
    
    %% Test with existing where conditions
    Query2 = #eorm_query{where = [{status, '=', active}]},
    Result2 = eorm_query_extended:where_in(Query2, id, Values),
    ?assertEqual([{status, '=', active}, {id, 'IN', Values}], Result2#eorm_query.where).

test_where_not_in() ->
    Query = #eorm_query{where = []},
    Values = [10, 20, 30],
    
    Result = eorm_query_extended:where_not_in(Query, status_id, Values),
    ?assertEqual([{status_id, 'NOT IN', Values}], Result#eorm_query.where),
    
    %% Test with existing conditions
    Query2 = #eorm_query{where = [{active, '=', true}]},
    Result2 = eorm_query_extended:where_not_in(Query2, banned_id, [99, 100]),
    ?assertEqual([{active, '=', true}, {banned_id, 'NOT IN', [99, 100]}], Result2#eorm_query.where).

test_where_null() ->
    Query = #eorm_query{where = []},
    
    Result = eorm_query_extended:where_null(Query, deleted_at),
    ?assertEqual([{deleted_at, 'IS', null}], Result#eorm_query.where),
    
    %% Test with multiple null checks
    Result2 = eorm_query_extended:where_null(Result, archived_at),
    ?assertEqual([{deleted_at, 'IS', null}, {archived_at, 'IS', null}], Result2#eorm_query.where).

test_where_not_null() ->
    Query = #eorm_query{where = []},
    
    Result = eorm_query_extended:where_not_null(Query, email_verified_at),
    ?assertEqual([{email_verified_at, 'IS NOT', null}], Result#eorm_query.where),
    
    %% Test chaining
    Result2 = eorm_query_extended:where_not_null(Result, phone),
    ?assertEqual([{email_verified_at, 'IS NOT', null}, {phone, 'IS NOT', null}], Result2#eorm_query.where).

test_where_between() ->
    Query = #eorm_query{where = []},
    
    %% Test numeric range
    Result1 = eorm_query_extended:where_between(Query, age, 18, 65),
    ?assertEqual([{age, 'BETWEEN', {18, 65}}], Result1#eorm_query.where),
    
    %% Test date range
    Result2 = eorm_query_extended:where_between(Query, created_at, 
                                                {2024, 1, 1}, {2024, 12, 31}),
    ?assertEqual([{created_at, 'BETWEEN', {{2024, 1, 1}, {2024, 12, 31}}}], 
                 Result2#eorm_query.where),
    
    %% Test with existing conditions
    Query3 = #eorm_query{where = [{status, '=', active}]},
    Result3 = eorm_query_extended:where_between(Query3, price, 10.0, 100.0),
    ?assertEqual([{status, '=', active}, {price, 'BETWEEN', {10.0, 100.0}}], 
                 Result3#eorm_query.where).

test_where_like() ->
    Query = #eorm_query{where = []},
    
    %% Test prefix pattern
    Result1 = eorm_query_extended:where_like(Query, name, <<"%John">>),
    ?assertEqual([{name, 'LIKE', <<"%John">>}], Result1#eorm_query.where),
    
    %% Test suffix pattern
    Result2 = eorm_query_extended:where_like(Query, email, <<"%@gmail.com">>),
    ?assertEqual([{email, 'LIKE', <<"%@gmail.com">>}], Result2#eorm_query.where),
    
    %% Test contains pattern
    Result3 = eorm_query_extended:where_like(Query, description, <<"%product%">>),
    ?assertEqual([{description, 'LIKE', <<"%product%">>}], Result3#eorm_query.where),
    
    %% Test multiple LIKE conditions
    Q4 = eorm_query_extended:where_like(Query, title, <<"%Erlang%">>),
    Q5 = eorm_query_extended:where_like(Q4, content, <<"%OTP%">>),
    ?assertEqual([{title, 'LIKE', <<"%Erlang%">>}, {content, 'LIKE', <<"%OTP%">>}], 
                 Q5#eorm_query.where).

test_raw_where() ->
    Query = #eorm_query{where = []},
    
    %% Test simple raw SQL
    Result1 = eorm_query_extended:raw_where(Query, <<"age > 18 AND age < 65">>),
    ?assertEqual([{raw, <<"age > 18 AND age < 65">>}], Result1#eorm_query.where),
    
    %% Test complex raw SQL
    ComplexSQL = <<"(status = 'active' OR (status = 'pending' AND created_at > NOW() - INTERVAL '7 days'))">>,
    Result2 = eorm_query_extended:raw_where(Query, ComplexSQL),
    ?assertEqual([{raw, ComplexSQL}], Result2#eorm_query.where),
    
    %% Test combining with regular conditions
    Query3 = #eorm_query{where = [{deleted, '=', false}]},
    Result3 = eorm_query_extended:raw_where(Query3, <<"score > 100">>),
    ?assertEqual([{deleted, '=', false}, {raw, <<"score > 100">>}], Result3#eorm_query.where).

test_build() ->
    %% Mock eorm_query:to_sql
    meck:new(eorm_query, [passthrough]),
    
    try
        Query = #eorm_query{
            table = users,
            select = [id, name, email],
            where = [{active, '=', true}]
        },
        
        ExpectedSQL = <<"SELECT id, name, email FROM users WHERE active = true">>,
        meck:expect(eorm_query, to_sql, fun(_Q) -> ExpectedSQL end),
        
        Result = eorm_query_extended:build(Query),
        ?assertEqual(ExpectedSQL, Result),
        
        %% Verify the function was called with the correct query
        ?assert(meck:called(eorm_query, to_sql, [Query]))
        
    after
        meck:unload(eorm_query)
    end.

test_execute_with_adapter() ->
    %% Mock both eorm_query:to_sql and eorm_adapter:query
    meck:new(eorm_query, [passthrough]),
    meck:new(eorm_adapter, [passthrough]),
    
    try
        Query = #eorm_query{
            table = products,
            select = ['*'],
            where = [{price, '>', 100}]
        },
        
        SQL = <<"SELECT * FROM products WHERE price > 100">>,
        meck:expect(eorm_query, to_sql, fun(_Q) -> SQL end),
        
        %% Test successful execution
        meck:expect(eorm_adapter, query, fun(postgres, _SQL, _Params) -> 
            {ok, [#{id => 1, name => <<"Product1">>, price => 150}]}
        end),
        
        Result1 = eorm_query_extended:execute_with_adapter(Query, postgres),
        ?assertMatch({ok, [#{id := 1}]}, Result1),
        
        %% Test error case
        meck:expect(eorm_adapter, query, fun(mysql, _SQL, _Params) -> 
            {error, connection_lost}
        end),
        
        Result2 = eorm_query_extended:execute_with_adapter(Query, mysql),
        ?assertEqual({error, connection_lost}, Result2),
        
        %% Verify calls
        ?assert(meck:called(eorm_query, to_sql, [Query])),
        ?assert(meck:called(eorm_adapter, query, [postgres, SQL, []])),
        ?assert(meck:called(eorm_adapter, query, [mysql, SQL, []]))
        
    after
        meck:unload(eorm_query),
        meck:unload(eorm_adapter)
    end.

%%====================================================================
%% Additional Coverage Tests
%%====================================================================

additional_coverage_test_() ->
    [
        {"Complex query building", fun test_complex_query_building/0},
        {"Edge cases", fun test_edge_cases/0}
    ].

test_complex_query_building() ->
    %% Build a complex query using multiple functions
    Q1 = #eorm_query{},
    Q2 = eorm_query_extended:from(Q1, users),
    Q3 = eorm_query_extended:left_join(Q2, posts, {users, id, posts, user_id}),
    Q4 = eorm_query_extended:where_not_null(Q3, email_verified_at),
    Q5 = eorm_query_extended:where_in(Q4, status, [active, premium]),
    Q6 = eorm_query_extended:where_between(Q5, age, 25, 45),
    Q7 = eorm_query_extended:where_like(Q6, name, <<"%Smith%">>),
    
    %% Verify the query structure
    ?assertEqual(users, Q7#eorm_query.table),
    ?assertEqual(1, length(Q7#eorm_query.joins)),
    ?assertEqual(4, length(Q7#eorm_query.where)).

test_edge_cases() ->
    Query = #eorm_query{where = []},
    
    %% Test empty IN list
    Q1 = eorm_query_extended:where_in(Query, id, []),
    ?assertEqual([{id, 'IN', []}], Q1#eorm_query.where),
    
    %% Test single value IN
    Q2 = eorm_query_extended:where_in(Query, status, [active]),
    ?assertEqual([{status, 'IN', [active]}], Q2#eorm_query.where),
    
    %% Test empty pattern for LIKE
    Q3 = eorm_query_extended:where_like(Query, description, <<"">>),
    ?assertEqual([{description, 'LIKE', <<"">>}], Q3#eorm_query.where),
    
    %% Test raw where with empty string
    Q4 = eorm_query_extended:raw_where(Query, <<"">>),
    ?assertEqual([{raw, <<"">>}], Q4#eorm_query.where).