%%% @doc EORM Supervisor 测试
%%% @end
-module(eorm_sup_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Supervisor 测试
%%====================================================================

supervisor_test_() ->
    [
        {"启动 Supervisor 测试", fun test_start_link/0},
        {"初始化测试", fun test_init/0}
    ].

test_start_link() ->
    Result = eorm_sup:start_link(),
    %% 可能已经启动，接受 ok 或者 already_started
    case Result of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ?assert(false)
    end.

test_init() ->
    Args = [],
    Result = eorm_sup:init(Args),
    ?assertMatch({ok, {_, _}}, Result).