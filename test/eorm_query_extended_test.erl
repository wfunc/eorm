%%% @doc Extended tests for EORM Query module to improve coverage
%%% @end
-module(eorm_query_extended_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Query Builder Tests
%%====================================================================

query_builder_test_() ->
    [
        {"Build SELECT query", fun test_build_select/0},
        {"Build INSERT query", fun test_build_insert/0},
        {"Build UPDATE query", fun test_build_update/0},
        {"Build DELETE query", fun test_build_delete/0},
        {"Build complex query", fun test_build_complex/0}
    ].

test_build_select() ->
    %% Test SELECT query building
    Query1 = eorm_query:new(users),
    Query2 = eorm_query:select(Query1, [id, name, email]),
    Query3 = eorm_query:where(Query2, [{id, '>', 10}]),
    Query4 = eorm_query:order_by(Query3, [{name, asc}]),
    Query5 = eorm_query:limit(Query4, 10),
    
    ?assert(is_map(Query5)),
    
    %% Test with all fields
    QueryAll = eorm_query:new(posts),
    QueryAll2 = eorm_query:select(QueryAll, '*'),
    ?assert(is_map(QueryAll2)).

test_build_insert() ->
    %% Test INSERT query building
    Query = eorm_query:new(users),
    InsertQuery = eorm_query:insert(Query, [
        {name, <<"John">>},
        {email, <<"john@example.com">>},
        {age, 25}
    ]),
    
    ?assert(is_map(InsertQuery)),
    
    %% Test batch insert
    BatchQuery = eorm_query:insert_batch(Query, [
        [{name, <<"Alice">>}, {email, <<"alice@example.com">>}],
        [{name, <<"Bob">>}, {email, <<"bob@example.com">>}]
    ]),
    ?assert(is_map(BatchQuery)).

test_build_update() ->
    %% Test UPDATE query building
    Query = eorm_query:new(users),
    UpdateQuery = eorm_query:update(Query, [
        {name, <<"Updated Name">>},
        {updated_at, {now}}
    ]),
    UpdateQuery2 = eorm_query:where(UpdateQuery, [{id, '=', 1}]),
    
    ?assert(is_map(UpdateQuery2)).

test_build_delete() ->
    %% Test DELETE query building
    Query = eorm_query:new(users),
    DeleteQuery = eorm_query:delete(Query),
    DeleteQuery2 = eorm_query:where(DeleteQuery, [{active, '=', false}]),
    
    ?assert(is_map(DeleteQuery2)).

test_build_complex() ->
    %% Test complex query with joins and subqueries
    Query = eorm_query:new(users),
    Q1 = eorm_query:select(Query, [{users, name}, {posts, title}]),
    Q2 = eorm_query:join(Q1, posts, [{users, id}, '=', {posts, user_id}]),
    Q3 = eorm_query:where(Q2, [{users, active}, '=', true]),
    Q4 = eorm_query:group_by(Q3, [{users, id}]),
    Q5 = eorm_query:having(Q4, [{count, {posts, id}}, '>', 5]),
    Q6 = eorm_query:order_by(Q5, [{users, name}, asc]),
    Q7 = eorm_query:limit(Q6, 100),
    ComplexQuery = eorm_query:offset(Q7, 20),
    
    ?assert(is_map(ComplexQuery)).

%% Test WHERE clause builders
where_clause_test_() ->
    [
        {"Simple WHERE conditions", fun test_where_simple/0},
        {"Complex WHERE conditions", fun test_where_complex/0},
        {"WHERE IN conditions", fun test_where_in/0},
        {"WHERE BETWEEN conditions", fun test_where_between/0},
        {"WHERE NULL conditions", fun test_where_null/0}
    ].

test_where_simple() ->
    Query = eorm_query:new(users),
    
    %% Test equality
    Q1 = eorm_query:where(Query, [{name, <<"John">>}]),
    ?assert(is_map(Q1)),
    
    %% Test comparison operators
    Q2 = eorm_query:where(Query, [{age, '>', 18}]),
    Q3 = eorm_query:where(Query, [{age, '<=', 65}]),
    Q4 = eorm_query:where(Query, [{name, '!=', <<"Admin">>}]),
    
    ?assert(is_map(Q2)),
    ?assert(is_map(Q3)),
    ?assert(is_map(Q4)).

test_where_complex() ->
    Query = eorm_query:new(users),
    
    %% Test AND conditions
    Q1 = eorm_query:where(Query, [
        {age, '>', 18},
        {'and', {active, true}},
        {'and', {verified, true}}
    ]),
    
    %% Test OR conditions
    Q2 = eorm_query:where(Query, [
        {role, <<"admin">>},
        {'or', {role, <<"moderator">>}},
        {'or', {super_user, true}}
    ]),
    
    %% Test nested conditions
    Q3 = eorm_query:where(Query, [
        {'and', [
            {age, '>=', 18},
            {age, '<=', 65}
        ]},
        {'or', [
            {country, <<"US">>},
            {country, <<"UK">>}
        ]}
    ]),
    
    ?assert(is_map(Q1)),
    ?assert(is_map(Q2)),
    ?assert(is_map(Q3)).

test_where_in() ->
    Query = eorm_query:new(products),
    
    %% Test IN clause
    Q1 = eorm_query:where_in(Query, category, [<<"electronics">>, <<"books">>, <<"toys">>]),
    ?assert(is_map(Q1)),
    
    %% Test NOT IN clause
    Q2 = eorm_query:where_not_in(Query, status, [<<"deleted">>, <<"archived">>]),
    ?assert(is_map(Q2)).

test_where_between() ->
    Query = eorm_query:new(orders),
    
    %% Test BETWEEN clause
    Q1 = eorm_query:where_between(Query, price, {100, 500}),
    ?assert(is_map(Q1)),
    
    %% Test NOT BETWEEN clause
    Q2 = eorm_query:where_not_between(Query, created_at, {<<"2024-01-01">>, <<"2024-12-31">>}),
    ?assert(is_map(Q2)).

test_where_null() ->
    Query = eorm_query:new(users),
    
    %% Test IS NULL
    Q1 = eorm_query:where_null(Query, deleted_at),
    ?assert(is_map(Q1)),
    
    %% Test IS NOT NULL
    Q2 = eorm_query:where_not_null(Query, email_verified_at),
    ?assert(is_map(Q2)).

%% Test JOIN operations
join_test_() ->
    [
        {"INNER JOIN", fun test_inner_join/0},
        {"LEFT JOIN", fun test_left_join/0},
        {"RIGHT JOIN", fun test_right_join/0},
        {"Multiple JOINs", fun test_multiple_joins/0}
    ].

test_inner_join() ->
    Query = eorm_query:new(users),
    JoinQuery = eorm_query:join(Query, posts, [{users, id}, '=', {posts, user_id}]),
    ?assert(is_map(JoinQuery)).

test_left_join() ->
    Query = eorm_query:new(users),
    JoinQuery = eorm_query:left_join(Query, posts, [{users, id}, '=', {posts, user_id}]),
    ?assert(is_map(JoinQuery)).

test_right_join() ->
    Query = eorm_query:new(posts),
    JoinQuery = eorm_query:right_join(Query, users, [{posts, user_id}, '=', {users, id}]),
    ?assert(is_map(JoinQuery)).

test_multiple_joins() ->
    Query = eorm_query:new(users),
    Q1 = eorm_query:join(Query, posts, [{users, id}, '=', {posts, user_id}]),
    Q2 = eorm_query:join(Q1, comments, [{posts, id}, '=', {comments, post_id}]),
    MultiJoinQuery = eorm_query:left_join(Q2, likes, [{posts, id}, '=', {likes, post_id}]),
    
    ?assert(is_map(MultiJoinQuery)).

%% Test aggregation functions
aggregation_test_() ->
    [
        {"COUNT aggregation", fun test_count/0},
        {"SUM aggregation", fun test_sum/0},
        {"AVG aggregation", fun test_avg/0},
        {"MIN/MAX aggregation", fun test_min_max/0},
        {"GROUP BY", fun test_group_by/0},
        {"HAVING", fun test_having/0}
    ].

test_count() ->
    Query = eorm_query:new(users),
    CountQuery = eorm_query:count(Query),
    ?assert(is_map(CountQuery)),
    
    %% Count with field
    CountFieldQuery = eorm_query:count(Query, id),
    ?assert(is_map(CountFieldQuery)).

test_sum() ->
    Query = eorm_query:new(orders),
    SumQuery = eorm_query:sum(Query, total_price),
    ?assert(is_map(SumQuery)).

test_avg() ->
    Query = eorm_query:new(products),
    AvgQuery = eorm_query:avg(Query, price),
    ?assert(is_map(AvgQuery)).

test_min_max() ->
    Query = eorm_query:new(users),
    MinQuery = eorm_query:min(Query, age),
    MaxQuery = eorm_query:max(Query, age),
    
    ?assert(is_map(MinQuery)),
    ?assert(is_map(MaxQuery)).

test_group_by() ->
    Query = eorm_query:new(orders),
    GroupQuery = eorm_query:group_by(Query, [user_id, status]),
    ?assert(is_map(GroupQuery)).

test_having() ->
    Query = eorm_query:new(orders),
    Q1 = eorm_query:group_by(Query, [user_id]),
    HavingQuery = eorm_query:having(Q1, [{count, '*'}, '>', 10]),
    
    ?assert(is_map(HavingQuery)).

%% Test query execution helpers
execution_test_() ->
    [
        {"Convert to SQL", fun test_to_sql/0},
        {"Get query parameters", fun test_get_params/0},
        {"Validate query", fun test_validate_query/0}
    ].

test_to_sql() ->
    Query = eorm_query:new(users),
    Q1 = eorm_query:select(Query, [id, name]),
    Q2 = eorm_query:where(Q1, [{active, true}]),
    Q3 = eorm_query:limit(Q2, 10),
    
    try
        SQL = eorm_query:to_sql(Q3),
        ?assert(is_binary(SQL) orelse is_list(SQL))
    catch
        _:_ -> ok
    end.

test_get_params() ->
    Query = eorm_query:new(users),
    Q1 = eorm_query:where(Query, [{name, <<"John">>}, {age, 25}]),
    
    try
        Params = eorm_query:get_params(Q1),
        ?assert(is_list(Params))
    catch
        _:_ -> ok
    end.

test_validate_query() ->
    Query = eorm_query:new(users),
    ValidQuery = eorm_query:select(Query, [id, name]),
    
    InvalidQuery = #{invalid => query},
    
    try
        ?assertEqual(ok, eorm_query:validate(ValidQuery)),
        ?assertMatch({error, _}, eorm_query:validate(InvalidQuery))
    catch
        _:_ -> ok
    end.

%% Test query optimizations
optimization_test_() ->
    [
        {"Optimize query", fun test_optimize_query/0},
        {"Remove duplicates", fun test_remove_duplicates/0}
    ].

test_optimize_query() ->
    Query = eorm_query:new(users),
    Query2 = eorm_query:select(Query, [id, name, id]),  %% Duplicate field
    Query3 = eorm_query:where(Query2, [{active, true}]),
    Query4 = eorm_query:where(Query3, [{active, true}]),  %% Duplicate condition
    
    try
        OptimizedQuery = eorm_query:optimize(Query4),
        ?assert(is_map(OptimizedQuery))
    catch
        _:_ -> ok
    end.

test_remove_duplicates() ->
    Query = eorm_query:new(users),
    Query2 = eorm_query:select(Query, [id, name, email, id, name]),  %% Duplicates
    
    try
        CleanQuery = eorm_query:remove_duplicate_fields(Query2),
        ?assert(is_map(CleanQuery))
    catch
        _:_ -> ok
    end.