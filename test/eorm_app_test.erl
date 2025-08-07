%%% @doc EORM 应用测试
%%% @end
-module(eorm_app_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 应用启动测试
%%====================================================================

app_test_() ->
    [
        {"应用启动测试", fun test_start/0},
        {"应用停止测试", fun test_stop/0}
    ].

test_start() ->
    Result = eorm_app:start(normal, []),
    ?assertMatch({ok, _}, Result).

test_stop() ->
    State = undefined,
    Result = eorm_app:stop(State),
    ?assertEqual(ok, Result).