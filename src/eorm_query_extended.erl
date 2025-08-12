%%% @doc Extended query functions for eorm_query
%%% @end
-module(eorm_query_extended).

-export([
    from/2,
    join/3,
    left_join/3,
    right_join/3,
    inner_join/3,
    exists/1,
    where_in/3,
    where_not_in/3,
    where_null/2,
    where_not_null/2,
    where_between/4,
    where_like/3,
    raw_where/2,
    build/1,
    execute_with_adapter/2
]).

-include("eorm.hrl").

%% @doc Set the FROM table
-spec from(#eorm_query{}, atom()) -> #eorm_query{}.
from(Query, Table) ->
    Query#eorm_query{table = Table, from = Table}.

%% @doc Add a JOIN
-spec join(#eorm_query{}, atom(), tuple()) -> #eorm_query{}.
join(Query = #eorm_query{joins = Joins}, Table, On) ->
    Query#eorm_query{joins = Joins ++ [{join, Table, On}]}.

%% @doc Add a LEFT JOIN
-spec left_join(#eorm_query{}, atom(), tuple()) -> #eorm_query{}.
left_join(Query = #eorm_query{joins = Joins}, Table, On) ->
    Query#eorm_query{joins = Joins ++ [{left_join, Table, On}]}.

%% @doc Add a RIGHT JOIN
-spec right_join(#eorm_query{}, atom(), tuple()) -> #eorm_query{}.
right_join(Query = #eorm_query{joins = Joins}, Table, On) ->
    Query#eorm_query{joins = Joins ++ [{right_join, Table, On}]}.

%% @doc Add an INNER JOIN
-spec inner_join(#eorm_query{}, atom(), tuple()) -> #eorm_query{}.
inner_join(Query = #eorm_query{joins = Joins}, Table, On) ->
    Query#eorm_query{joins = Joins ++ [{inner_join, Table, On}]}.

%% @doc Check if records exist
-spec exists(#eorm_query{}) -> {ok, boolean()} | {error, term()}.
exists(Query) ->
    case eorm_query:execute(Query) of
        {ok, []} -> {ok, false};
        {ok, _} -> {ok, true};
        Error -> Error
    end.

%% @doc Add WHERE IN condition
-spec where_in(#eorm_query{}, atom(), list()) -> #eorm_query{}.
where_in(Query = #eorm_query{where = Where}, Field, Values) ->
    Query#eorm_query{where = Where ++ [{Field, 'IN', Values}]}.

%% @doc Add WHERE NOT IN condition
-spec where_not_in(#eorm_query{}, atom(), list()) -> #eorm_query{}.
where_not_in(Query = #eorm_query{where = Where}, Field, Values) ->
    Query#eorm_query{where = Where ++ [{Field, 'NOT IN', Values}]}.

%% @doc Add WHERE NULL condition
-spec where_null(#eorm_query{}, atom()) -> #eorm_query{}.
where_null(Query = #eorm_query{where = Where}, Field) ->
    Query#eorm_query{where = Where ++ [{Field, 'IS', null}]}.

%% @doc Add WHERE NOT NULL condition
-spec where_not_null(#eorm_query{}, atom()) -> #eorm_query{}.
where_not_null(Query = #eorm_query{where = Where}, Field) ->
    Query#eorm_query{where = Where ++ [{Field, 'IS NOT', null}]}.

%% @doc Add WHERE BETWEEN condition
-spec where_between(#eorm_query{}, atom(), term(), term()) -> #eorm_query{}.
where_between(Query = #eorm_query{where = Where}, Field, Min, Max) ->
    Query#eorm_query{where = Where ++ [{Field, 'BETWEEN', {Min, Max}}]}.

%% @doc Add WHERE LIKE condition
-spec where_like(#eorm_query{}, atom(), binary()) -> #eorm_query{}.
where_like(Query = #eorm_query{where = Where}, Field, Pattern) ->
    Query#eorm_query{where = Where ++ [{Field, 'LIKE', Pattern}]}.

%% @doc Add raw WHERE condition
-spec raw_where(#eorm_query{}, binary()) -> #eorm_query{}.
raw_where(Query = #eorm_query{where = Where}, RawSQL) ->
    Query#eorm_query{where = Where ++ [{raw, RawSQL}]}.

%% @doc Build the SQL query
-spec build(#eorm_query{}) -> binary().
build(Query) ->
    eorm_query:to_sql(Query).

%% @doc Execute with specific adapter
-spec execute_with_adapter(#eorm_query{}, atom()) -> {ok, list()} | {error, term()}.
execute_with_adapter(Query, Adapter) ->
    SQL = build(Query),
    eorm_adapter:query(Adapter, SQL, []).