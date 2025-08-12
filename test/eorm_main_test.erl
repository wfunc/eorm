%%% @doc 主模块 eorm 的测试，覆盖未测试的行
%%% @end
-module(eorm_main_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% 测试设置和清理
%%====================================================================

setup() ->
    %% 启动必需的应用
    application:ensure_all_started(meck),
    
    %% Mock eorm_crud
    meck:new(eorm_crud, [non_strict]),
    meck:expect(eorm_crud, update, fun(_, #{id := 1}, _) -> {ok, 1} end),
    meck:expect(eorm_crud, update, fun(_, _, _) -> {ok, 1} end),
    
    %% Mock eorm_query  
    meck:new(eorm_query, [non_strict]),
    meck:expect(eorm_query, count, fun(_) -> {ok, 5} end),
    meck:expect(eorm_query, where, fun(_, _) -> #eorm_query{} end),
    
    %% Mock eorm_transaction
    meck:new(eorm_transaction, [non_strict]),
    meck:expect(eorm_transaction, transaction, fun(_, _) -> {ok, result} end),
    
    ok.

teardown(_) ->
    %% 卸载所有 mock
    meck:unload(),
    ok.

%%====================================================================
%% 测试套件
%%====================================================================

eorm_main_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         fun test_update_with_id/0,
         fun test_count_with_query/0,
         fun test_where_with_conditions_first/0,
         fun test_transaction_with_options/0
     ]}.

%%====================================================================
%% 测试用例
%%====================================================================

test_update_with_id() ->
    %% 测试行 111: update(Model, Id, Updates) when is_integer(Id)
    Result = eorm:update(test_model, 1, #{name => <<"Alice">>}),
    ?assertEqual({ok, 1}, Result).

test_count_with_query() ->
    %% 测试行 125: count(Query = #eorm_query{})
    Query = #eorm_query{table = users},
    Result = eorm:count(Query),
    ?assertEqual({ok, 5}, Result).

test_where_with_conditions_first() ->
    %% 测试行 148: where(Conditions, Query = #eorm_query{})
    Query = #eorm_query{table = users},
    Conditions = #{name => <<"Bob">>},
    Result = eorm:where(Conditions, Query),
    ?assertMatch(#eorm_query{}, Result).

test_transaction_with_options() ->
    %% 测试行 217: transaction(Fun, Options)
    Fun = fun() -> ok end,
    Options = #{timeout => 5000},
    Result = eorm:transaction(Fun, Options),
    ?assertEqual({ok, result}, Result).