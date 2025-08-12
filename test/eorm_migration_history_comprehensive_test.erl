%%% @doc Comprehensive tests for eorm_migration_history to achieve 100% coverage
%%% @end
-module(eorm_migration_history_comprehensive_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

setup() ->
    %% Mock eorm_adapter
    meck:new(eorm_adapter, [non_strict]),
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    ok.

teardown(_) ->
    meck:unload(),
    ok.

%%====================================================================
%% Test Suites  
%%====================================================================

migration_history_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
         fun test_ensure_table_postgres/0,
         fun test_ensure_table_mysql/0,
         fun test_ensure_table_sqlite/0,
         fun test_ensure_table_unsupported/0,
         fun test_ensure_table_already_exists_postgres/0,
         fun test_ensure_table_already_exists_mysql/0,
         fun test_record_success/0,
         fun test_record_error/0,
         fun test_get_last_migration/0,
         fun test_get_last_migration_not_found/0,
         fun test_get_last_migration_error/0,
         fun test_get_all_migrations/0,
         fun test_get_all_migrations_error/0,
         fun test_update_status/0,
         fun test_update_status_error/0,
         fun test_row_to_migration/0,
         fun test_get_history/0,
         fun test_clear_all/0,
         fun test_get_all_history/0,
         fun test_get_statistics/0,
         fun test_get_history_by_date_range/0,
         fun test_get_history_by_type/0,
         fun test_generate_version/0,
         fun test_calculate_checksum/0
     ]}.

%%====================================================================
%% Test Cases
%%====================================================================

test_ensure_table_postgres() ->
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
    ?assertEqual(ok, eorm_migration_history:ensure_table(postgres)).

test_ensure_table_mysql() ->
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
    ?assertEqual(ok, eorm_migration_history:ensure_table(mysql)).

test_ensure_table_sqlite() ->
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
    ?assertEqual(ok, eorm_migration_history:ensure_table(sqlite)).

test_ensure_table_unsupported() ->
    ?assertError({unsupported_adapter, oracle}, eorm_migration_history:ensure_table(oracle)).

test_ensure_table_already_exists_postgres() ->
    %% Test postgres "already exists" error
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> 
        {error, {error, duplicate_table, <<"42P07">>, <<"relation \"eorm_migrations\" already exists">>, []}}
    end),
    ?assertEqual(ok, eorm_migration_history:ensure_table(postgres)).

test_ensure_table_already_exists_mysql() ->
    %% Test MySQL "already exists" error
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> 
        {error, {1050, <<"Table 'eorm_migrations' already exists">>}}
    end),
    ?assertEqual(ok, eorm_migration_history:ensure_table(mysql)).

test_record_success() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, 1} end),
    Result = eorm_migration_history:record(postgres, <<"test_table">>, create_table, <<"CREATE TABLE test">>),
    ?assertEqual(ok, Result).

test_record_error() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {error, db_error} end),
    Result = eorm_migration_history:record(postgres, test_atom_table, create_table, <<"CREATE TABLE test">>),
    ?assertEqual({error, db_error}, Result).

test_get_last_migration() ->
    %% Mock a successful query with a row
    meck:expect(eorm_adapter, query, fun(_, _, _) -> 
        {ok, [[1, <<"test_model">>, <<"v1">>, <<"checksum">>, <<"2024-01-01 00:00:00">>, 
               100, <<"success">>, <<"changes">>, <<"rollback">>]]}
    end),
    Result = eorm_migration_history:get_last_migration(postgres, test_model),
    ?assertMatch({ok, #eorm_migration{}}, Result).

test_get_last_migration_not_found() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    Result = eorm_migration_history:get_last_migration(postgres, test_model),
    ?assertEqual({error, not_found}, Result).

test_get_last_migration_error() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {error, db_error} end),
    Result = eorm_migration_history:get_last_migration(postgres, test_model),
    ?assertEqual({error, db_error}, Result).

test_get_all_migrations() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> 
        {ok, [[1, <<"test_model">>, <<"v1">>, <<"checksum">>, <<"2024-01-01 00:00:00">>, 
               100, <<"success">>, <<"changes">>, <<"rollback">>]]}
    end),
    Result = eorm_migration_history:get_all_migrations(postgres),
    ?assertMatch({ok, [#eorm_migration{}]}, Result).

test_get_all_migrations_error() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {error, db_error} end),
    Result = eorm_migration_history:get_all_migrations(postgres),
    ?assertEqual({error, db_error}, Result).

test_update_status() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, 1} end),
    Result = eorm_migration_history:update_status(postgres, <<"v1">>, success),
    ?assertEqual(ok, Result).

test_update_status_error() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {error, db_error} end),
    Result = eorm_migration_history:update_status(postgres, <<"v1">>, failed),
    ?assertEqual({error, db_error}, Result).

test_row_to_migration() ->
    %% This is an internal function but we can test it through get_last_migration
    meck:expect(eorm_adapter, query, fun(_, _, _) -> 
        {ok, [[1, <<"test_model">>, <<"v1">>, <<"checksum">>, <<"2024-01-01 00:00:00">>, 
               100, <<"success">>, <<"changes">>, <<"rollback">>]]}
    end),
    {ok, Migration} = eorm_migration_history:get_last_migration(postgres, test_model),
    ?assertEqual(1, Migration#eorm_migration.id),
    ?assertEqual(test_model, Migration#eorm_migration.model),
    ?assertEqual(<<"v1">>, Migration#eorm_migration.version).

test_get_history() ->
    Result = eorm_migration_history:get_history(<<"test_table">>),
    ?assertEqual({ok, []}, Result).

test_clear_all() ->
    Result = eorm_migration_history:clear_all(),
    ?assertEqual(ok, Result).

test_get_all_history() ->
    Result = eorm_migration_history:get_all_history(),
    ?assertEqual({ok, []}, Result).

test_get_statistics() ->
    Result = eorm_migration_history:get_statistics(),
    ?assertMatch({ok, #{total_migrations := 0}}, Result).

test_get_history_by_date_range() ->
    StartDate = {{2024, 1, 1}, {0, 0, 0}},
    EndDate = {{2024, 12, 31}, {23, 59, 59}},
    Result = eorm_migration_history:get_history_by_date_range(StartDate, EndDate),
    ?assertEqual({ok, []}, Result).

test_get_history_by_type() ->
    Result = eorm_migration_history:get_history_by_type(create_table),
    ?assertEqual({ok, []}, Result).

test_generate_version() ->
    {ok, Version} = eorm_migration_history:generate_version(),
    ?assert(is_binary(Version)),
    ?assert(byte_size(Version) > 0).

test_calculate_checksum() ->
    {ok, Checksum} = eorm_migration_history:calculate_checksum(<<"test data">>),
    ?assert(is_binary(Checksum)),
    ?assert(byte_size(Checksum) > 0).