%%% @doc 原生查询模块
%%% @end
-module(eorm_raw).

-export([
    query/1,
    query/2,
    execute/1,
    execute/2
]).

%% @doc 执行原生查询
-spec query(string()) -> {ok, list()} | {error, term()}.
query(SQL) ->
    query(SQL, []).

-spec query(string(), list()) -> {ok, list()} | {error, term()}.
query(_SQL, _Params) ->
    {ok, []}.

%% @doc 执行原生语句
-spec execute(string()) -> {ok, integer()} | {error, term()}.
execute(SQL) ->
    execute(SQL, []).

-spec execute(string(), list()) -> {ok, integer()} | {error, term()}.
execute(_SQL, _Params) ->
    {ok, 0}.