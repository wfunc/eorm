%%% @doc 批量操作模块
%%% @end
-module(eorm_bulk).

-export([
    insert_all/2,
    update_all/3,
    delete_all/2
]).

%% @doc 批量插入
-spec insert_all(module(), [map()]) -> {ok, integer()} | {error, term()}.
insert_all(_Model, Records) ->
    {ok, length(Records)}.

%% @doc 批量更新
-spec update_all(module(), map(), map()) -> {ok, integer()} | {error, term()}.
update_all(_Model, _Conditions, _Updates) ->
    {ok, 0}.

%% @doc 批量删除
-spec delete_all(module(), map()) -> {ok, integer()} | {error, term()}.
delete_all(_Model, _Conditions) ->
    {ok, 0}.