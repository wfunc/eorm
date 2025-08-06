%%% @doc 事务管理模块
%%% @end
-module(eorm_transaction).

-export([
    transaction/1,
    transaction/2,
    rollback/1
]).

%% @doc 执行事务
-spec transaction(fun()) -> {ok, term()} | {error, term()}.
transaction(Fun) ->
    transaction(Fun, #{}).

-spec transaction(fun(), map()) -> {ok, term()} | {error, term()}.
transaction(Fun, _Options) ->
    try
        Result = Fun(),
        {ok, Result}
    catch
        throw:rollback -> {error, rollback};
        Type:Error -> {error, {Type, Error}}
    end.

%% @doc 回滚事务
-spec rollback(term()) -> no_return().
rollback(Reason) ->
    throw({rollback, Reason}).