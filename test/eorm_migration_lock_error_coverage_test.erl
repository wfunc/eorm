%%% @doc EORM Migration Lock Error Coverage Tests
%%% 专门测试错误处理路径以提高代码覆盖率
%%% @end
-module(eorm_migration_lock_error_coverage_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% 直接测试异常路径的简单方法
%%====================================================================

%% 直接调用 try-catch 路径而不依赖服务器状态

%%====================================================================
%% 异常处理覆盖测试 
%%====================================================================

%% 使用简单的单一测试函数来触发异常处理路径

error_coverage_test_() ->
    [
        {"测试异常处理路径覆盖", fun test_exception_coverage/0}
    ].

test_exception_coverage() ->
    %% 通过临时停用 supervisor 来强制触发异常路径
    %% 注意：这些函数在正常运行时会返回 {error, unknown_request}
    %% 但是我们需要触发 catch 子句
    
    %% 暂时停用supervisor以阻止重启
    supervisor:terminate_child(eorm_sup, eorm_migration_lock),
    supervisor:delete_child(eorm_sup, eorm_migration_lock),
    timer:sleep(50),
    
    %% 现在这些调用应该触发异常处理
    %% 测试 is_locked 函数在服务器不可用时的异常处理 (line 114)
    Result1 = eorm_migration_lock:is_locked(<<"test_lock">>),
    ?assertEqual({error, server_unavailable}, Result1),
    
    %% 测试 acquire_with_timeout 函数异常处理 (line 123)
    Result2 = eorm_migration_lock:acquire_with_timeout(<<"test_lock">>, 100),
    ?assertEqual({error, timeout}, Result2),
    
    %% 测试 force_release 函数异常处理 (line 132)
    Result3 = eorm_migration_lock:force_release(<<"test_lock">>),
    ?assertEqual({error, server_unavailable}, Result3),
    
    %% 测试 list_all_locks 函数异常处理 (line 141)
    Result4 = eorm_migration_lock:list_all_locks(),
    ?assertEqual({error, server_unavailable}, Result4),
    
    %% 测试 cleanup_expired_locks 函数异常处理 (line 150)
    Result5 = eorm_migration_lock:cleanup_expired_locks(),
    ?assertEqual({ok, 0}, Result5),
    
    %% 测试 get_lock_info 函数异常处理 (line 159)
    Result6 = eorm_migration_lock:get_lock_info(<<"test_lock">>),
    ?assertEqual({error, not_found}, Result6),
    
    %% 测试 renew 函数异常处理 (line 168)
    Result7 = eorm_migration_lock:renew(<<"test_lock">>, 30000),
    ?assertEqual({error, server_unavailable}, Result7),
    
    %% 测试 get_waiting_queue 函数异常处理 (line 183)
    Result8 = eorm_migration_lock:get_waiting_queue(<<"test_lock">>),
    ?assertEqual([], Result8),
    
    %% 恢复服务器
    ChildSpec = #{
        id => eorm_migration_lock,
        start => {eorm_migration_lock, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [eorm_migration_lock]
    },
    supervisor:start_child(eorm_sup, ChildSpec),
    timer:sleep(100),
    
    ok.