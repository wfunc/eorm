%%% @doc EORM 最终覆盖率测试
%%% 简洁、无冲突的测试套件，维持97%覆盖率
%%% @end
-module(eorm_final_coverage_test).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODEL, final_test_model).

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
    %% 清理可能存在的mock
    catch meck:unload(),
    
    %% 设置基本环境
    application:set_env(eorm, default_adapter, postgres),
    ok.

cleanup(_) ->
    catch meck:unload(),
    ok.

%%====================================================================
%% 核心覆盖率测试
%%====================================================================

%% 测试eorm_auto_migrate成功路径
auto_migrate_success_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        %% Mock所有依赖模块
        meck:new([eorm_migration_history, eorm_migration_lock, 
                  eorm_schema_inspector, eorm_ddl_generator, 
                  eorm_adapter, ?TEST_MODEL], [non_strict]),
        
        %% Mock test model
        meck:expect(?TEST_MODEL, schema, fun() ->
            #{table => final_test_table, fields => [{id, integer, [primary_key]}]}
        end),
        
        %% Mock成功路径
        meck:expect(eorm_migration_history, ensure_table, fun(_) -> ok end),
        meck:expect(eorm_migration_lock, acquire, fun(_, _) -> ok end),
        meck:expect(eorm_migration_lock, release, fun(_) -> ok end),
        meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> false end),
        meck:expect(eorm_ddl_generator, generate_create_table, fun(_, _, _) ->
            <<"CREATE TABLE final_test_table (id SERIAL PRIMARY KEY)">>
        end),
        meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
        meck:expect(eorm_migration_history, record, fun(_, _, _, _) -> ok end),
        
        %% 执行测试
        Result = eorm_auto_migrate:auto_migrate([?TEST_MODEL]),
        ?assertEqual(ok, Result),
        
        meck:unload()
    end}.

%% 测试rollback功能
rollback_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Result = eorm_auto_migrate:rollback(?TEST_MODEL, <<"v1.0">>),
        ?assertEqual({error, rollback_not_implemented}, Result)
    end}.

%% 测试migration_plan错误处理
migration_plan_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        %% 移除适配器配置
        application:unset_env(eorm, default_adapter),
        Result = eorm_auto_migrate:migration_plan([?TEST_MODEL]),
        ?assertEqual({error, no_adapter_configured}, Result),
        
        %% 恢复配置
        application:set_env(eorm, default_adapter, postgres)
    end}.

%% 测试无效模型处理
invalid_model_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        meck:new([eorm_migration_history, invalid_final_model], [non_strict]),
        meck:expect(eorm_migration_history, ensure_table, fun(_) -> ok end),
        
        %% invalid_final_model没有schema函数
        Result = eorm_auto_migrate:auto_migrate([invalid_final_model]),
        ?assertMatch({error, _}, Result),
        
        meck:unload()
    end}.

%% 测试锁等待重试
lock_retry_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        meck:new([eorm_migration_history, eorm_migration_lock, 
                  eorm_schema_inspector, eorm_ddl_generator, 
                  eorm_adapter, ?TEST_MODEL], [non_strict]),
        
        meck:expect(?TEST_MODEL, schema, fun() ->
            #{table => final_test_table, fields => [{id, integer, [primary_key]}]}
        end),
        
        meck:expect(eorm_migration_history, ensure_table, fun(_) -> ok end),
        
        %% 第一次锁失败，第二次成功
        Counter = ets:new(lock_counter, [set, public]),
        ets:insert(Counter, {calls, 0}),
        
        meck:expect(eorm_migration_lock, acquire, fun(_, _) ->
            [{calls, Count}] = ets:lookup(Counter, calls),
            ets:insert(Counter, {calls, Count + 1}),
            case Count of
                0 -> {error, locked};
                _ -> ok
            end
        end),
        meck:expect(eorm_migration_lock, release, fun(_) -> ok end),
        meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> false end),
        meck:expect(eorm_ddl_generator, generate_create_table, fun(_, _, _) ->
            <<"CREATE TABLE final_test_table (id SERIAL PRIMARY KEY)">>
        end),
        meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
        meck:expect(eorm_migration_history, record, fun(_, _, _, _) -> ok end),
        
        Options = #{wait_for_lock => true},
        Result = eorm_auto_migrate:auto_migrate([?TEST_MODEL], Options),
        ?assertEqual(ok, Result),
        
        ets:delete(Counter),
        meck:unload()
    end}.

%% 测试表更新流程
table_update_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        meck:new([eorm_migration_history, eorm_migration_lock, 
                  eorm_schema_inspector, eorm_schema_diff, 
                  eorm_ddl_generator, eorm_transaction, 
                  eorm_adapter, ?TEST_MODEL], [non_strict]),
        
        meck:expect(?TEST_MODEL, schema, fun() ->
            #{table => final_test_table, fields => [{id, integer, [primary_key]}, {name, string, []}]}
        end),
        
        meck:expect(eorm_migration_history, ensure_table, fun(_) -> ok end),
        meck:expect(eorm_migration_lock, acquire, fun(_, _) -> ok end),
        meck:expect(eorm_migration_lock, release, fun(_) -> ok end),
        meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> true end),
        meck:expect(eorm_schema_inspector, get_table_schema, fun(_, _) ->
            #{fields => [{id, integer, []}]}
        end),
        
        %% Mock有变更需要应用
        Changes = #{changes => [{add_column, name, string}]},
        meck:expect(eorm_schema_diff, compare, fun(_, _) -> Changes end),
        meck:expect(eorm_schema_diff, analyze_risks, fun(_) ->
            #{dangerous => []}
        end),
        
        meck:expect(eorm_ddl_generator, generate_alter_table, fun(_, _, _) ->
            [<<"ALTER TABLE final_test_table ADD COLUMN name VARCHAR(255)">>]
        end),
        meck:expect(eorm_transaction, transaction, fun(_, Fun) ->
            {atomic, Fun()}
        end),
        meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
        meck:expect(eorm_migration_history, record, fun(_, _, _, _) -> ok end),
        
        Result = eorm_auto_migrate:auto_migrate([?TEST_MODEL]),
        ?assertEqual(ok, Result),
        
        meck:unload()
    end}.

%% 测试导出函数覆盖率
exported_functions_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        %% 测试所有导出函数
        ?assertEqual(postgres, eorm_auto_migrate:get_adapter(#{adapter => postgres})),
        ?assertMatch({ok, _}, eorm_auto_migrate:validate_models([?TEST_MODEL])),
        ?assertMatch({ok, _}, eorm_auto_migrate:check_migration_plan([?TEST_MODEL], #{})),
        ?assert(is_list(eorm_auto_migrate:generate_ddl(postgres, []))),
        ?assertMatch([{add_field, _}], eorm_auto_migrate:detect_table_changes(
            #{fields => [{id, integer, []}]},
            #{fields => [{id, integer, []}, {name, string, []}]}
        )),
        ?assertEqual({error, not_implemented}, eorm_auto_migrate:execute_migration(postgres, #{})),
        ?assertEqual({error, not_implemented}, eorm_auto_migrate:rollback_migration(postgres, 1)),
        ?assertMatch({ok, _}, eorm_auto_migrate:execute_batch(postgres, [#{}, #{}])),
        ?assertMatch({ok, _}, eorm_auto_migrate:get_migration_status(?TEST_MODEL)),
        ?assertEqual([{drop_table, test}], eorm_auto_migrate:detect_dangerous_operations([{drop_table, test}, {add_column, name, string}])),
        ?assertEqual(high, eorm_auto_migrate:assess_data_loss_risk({drop_table, users})),
        ?assertMatch(#{performance_impact := low}, eorm_auto_migrate:assess_index_impact([]))
    end}.