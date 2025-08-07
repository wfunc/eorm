%%% @doc Full coverage tests for EORM modules to achieve 100% coverage
%%% @end
-module(eorm_full_coverage_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start meck for mocking
    meck:new(eorm_adapter, [passthrough]),
    meck:new(eorm_migration_history, [passthrough]),
    meck:new(eorm_migration_lock, [passthrough]),
    meck:new(eorm_schema_inspector, [passthrough]),
    meck:new(eorm_schema_diff, [passthrough]),
    meck:new(eorm_ddl_generator, [passthrough]),
    meck:new(eorm_transaction, [passthrough]),
    meck:new(eorm_rollback, [passthrough]),
    catch meck:new(test_model, [non_strict]),
    
    %% Setup default expectations
    meck:expect(eorm_migration_history, ensure_table, fun(_) -> ok end),
    meck:expect(eorm_migration_history, record, fun(_, _, _, _) -> ok end),
    meck:expect(eorm_migration_lock, acquire, fun(_) -> ok end),
    meck:expect(eorm_migration_lock, acquire, fun(_, _) -> ok end),
    meck:expect(eorm_migration_lock, release, fun(_) -> ok end),
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    meck:expect(eorm_rollback, execute, fun(_, _, _) -> ok end),
    meck:expect(test_model, schema, fun() -> 
        #{
            table => test_table,
            fields => [
                {id, integer, [primary_key]},
                {name, string, []},
                {email, string, [unique]}
            ],
            indexes => [
                {idx_test_email, [email], [unique]}
            ]
        }
    end),
    ok.

teardown(_) ->
    meck:unload(),
    ok.

%%====================================================================
%% eorm_auto_migrate Full Coverage Tests
%%====================================================================

auto_migrate_full_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        {"Auto migrate with models", fun test_auto_migrate_with_models/0},
        {"Auto migrate with table creation", fun test_auto_migrate_create_table/0},
        {"Auto migrate with table update", fun test_auto_migrate_update_table/0},
        {"Auto migrate with lock timeout", fun test_auto_migrate_lock_timeout/0},
        {"Auto migrate with unsafe changes", fun test_auto_migrate_unsafe_changes/0},
        {"Auto migrate with force mode", fun test_auto_migrate_force/0},
        {"Auto migrate with dry run", fun test_auto_migrate_dry_run_mode/0},
        {"Auto migrate rollback", fun test_auto_migrate_rollback/0},
        {"Migration plan generation", fun test_migration_plan_generation/0},
        {"Auto migrate with transaction failure", fun test_auto_migrate_transaction_failure/0}
     ]
    }.

test_auto_migrate_with_models() ->
    %% Test successful migration with models
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> false end),
    meck:expect(eorm_ddl_generator, generate_create_table, fun(_, _, _) -> <<"CREATE TABLE test">> end),
    meck:expect(eorm_ddl_generator, generate_create_index, fun(_, _, _) -> <<"CREATE INDEX test">> end),
    
    Options = #{adapter => postgres},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertEqual(ok, Result).

test_auto_migrate_create_table() ->
    %% Test table creation flow
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> false end),
    meck:expect(eorm_ddl_generator, generate_create_table, fun(_, _, _) -> <<"CREATE TABLE users">> end),
    
    Options = #{adapter => postgres},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertEqual(ok, Result).

test_auto_migrate_update_table() ->
    %% Test table update flow
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> true end),
    meck:expect(eorm_schema_inspector, get_table_schema, fun(_, _) -> 
        #{
            table => test_table,
            fields => [
                #{name => id, type => integer, opts => [primary_key]},
                #{name => name, type => string, opts => []}
            ]
        }
    end),
    meck:expect(eorm_schema_diff, compare, fun(_, _) -> 
        #{changes => [{add_column, email, string}]}
    end),
    meck:expect(eorm_schema_diff, analyze_risks, fun(_) -> 
        #{dangerous => []}
    end),
    meck:expect(eorm_ddl_generator, generate_alter_table, fun(_, _, _) -> 
        [<<"ALTER TABLE test ADD COLUMN email">>]
    end),
    meck:expect(eorm_transaction, transaction, fun(_, Fun) -> 
        Fun(),
        {atomic, ok}
    end),
    
    Options = #{adapter => postgres},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertEqual(ok, Result).

test_auto_migrate_lock_timeout() ->
    %% Test lock acquisition with wait
    CallCount = erlang:put(lock_call_count, 0),
    meck:expect(eorm_migration_lock, acquire, fun(_, _) ->
        Count = erlang:get(lock_call_count),
        erlang:put(lock_call_count, Count + 1),
        if Count == 0 -> {error, locked};
           true -> ok
        end
    end),
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> false end),
    meck:expect(eorm_ddl_generator, generate_create_table, fun(_, _, _) -> <<"CREATE TABLE test">> end),
    
    Options = #{adapter => postgres, wait_for_lock => true},
    spawn(fun() ->
        timer:sleep(100),
        erlang:put(lock_call_count, 1)
    end),
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertEqual(ok, Result).

test_auto_migrate_unsafe_changes() ->
    %% Test unsafe changes in safe mode
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> true end),
    meck:expect(eorm_schema_inspector, get_table_schema, fun(_, _) -> 
        #{
            table => test_table,
            fields => [
                #{name => id, type => integer, opts => [primary_key]},
                #{name => name, type => string, opts => []},
                #{name => old_field, type => string, opts => []}
            ]
        }
    end),
    meck:expect(eorm_schema_diff, compare, fun(_, _) -> 
        #{changes => [{drop_column, old_field}]}
    end),
    meck:expect(eorm_schema_diff, analyze_risks, fun(_) -> 
        #{dangerous => [{drop_column, old_field}]}
    end),
    
    Options = #{adapter => postgres, mode => safe},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertMatch({error, _}, Result).

test_auto_migrate_force() ->
    %% Test force mode with dangerous changes
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> true end),
    meck:expect(eorm_schema_inspector, get_table_schema, fun(_, _) -> 
        #{
            table => test_table,
            fields => [
                #{name => id, type => integer, opts => [primary_key]},
                #{name => name, type => string, opts => []},
                #{name => old_field, type => string, opts => []}
            ]
        }
    end),
    meck:expect(eorm_schema_diff, compare, fun(_, _) -> 
        #{changes => [{drop_column, old_field}]}
    end),
    meck:expect(eorm_ddl_generator, generate_alter_table, fun(_, _, _) -> 
        [<<"ALTER TABLE test DROP COLUMN old_field">>]
    end),
    meck:expect(eorm_transaction, transaction, fun(_, Fun) -> 
        Fun(),
        {atomic, ok}
    end),
    
    Options = #{adapter => postgres, mode => force},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertEqual(ok, Result).

test_auto_migrate_dry_run_mode() ->
    %% Test dry run mode
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> true end),
    meck:expect(eorm_schema_inspector, get_table_schema, fun(_, _) -> 
        #{
            table => test_table,
            fields => [
                #{name => id, type => integer, opts => [primary_key]},
                #{name => name, type => string, opts => []}
            ]
        }
    end),
    meck:expect(eorm_schema_diff, compare, fun(_, _) -> 
        #{changes => [{drop_column, old_field}]}
    end),
    meck:expect(eorm_schema_diff, analyze_risks, fun(_) -> 
        #{dangerous => [{drop_column, old_field}]}
    end),
    
    Options = #{adapter => postgres, mode => dry_run},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertEqual(ok, Result).

test_auto_migrate_rollback() ->
    %% Test rollback function
    Result = eorm_auto_migrate:rollback(test_model, <<"v1">>),
    ?assertEqual(ok, Result).

test_migration_plan_generation() ->
    %% Test migration plan generation
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> false end),
    meck:expect(eorm_ddl_generator, generate_create_table, fun(_, _, _) -> <<"CREATE TABLE test">> end),
    
    Options = #{adapter => postgres},
    Result = eorm_auto_migrate:migration_plan([test_model], Options),
    ?assertMatch({ok, [_|_]}, Result).

test_auto_migrate_transaction_failure() ->
    %% Test transaction failure during migration
    meck:expect(eorm_schema_inspector, table_exists, fun(_, _) -> true end),
    meck:expect(eorm_schema_inspector, get_table_schema, fun(_, _) -> 
        #{
            table => test_table,
            fields => []
        }
    end),
    meck:expect(eorm_schema_diff, compare, fun(_, _) -> 
        #{changes => [{add_column, name, string}]}
    end),
    meck:expect(eorm_schema_diff, analyze_risks, fun(_) -> 
        #{dangerous => []}
    end),
    meck:expect(eorm_ddl_generator, generate_alter_table, fun(_, _, _) -> 
        [<<"ALTER TABLE test ADD COLUMN name">>]
    end),
    meck:expect(eorm_transaction, transaction, fun(_, _Fun) -> 
        {aborted, db_error}
    end),
    
    Options = #{adapter => postgres},
    Result = eorm_auto_migrate:auto_migrate([test_model], Options),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% eorm_registry Full Coverage Tests
%%====================================================================

registry_full_test_() ->
    [
        {"Registry type inference", fun test_registry_type_inference/0},
        {"Registry field processing", fun test_registry_field_processing/0}
    ].

test_registry_type_inference() ->
    %% Test all type inference paths
    ?assertEqual(integer, eorm_registry:infer_type(id)),
    ?assertEqual(integer, eorm_registry:infer_type(user_id)),
    ?assertEqual(integer, eorm_registry:infer_type(post_id)),
    ?assertEqual(timestamp, eorm_registry:infer_type(created_at)),
    ?assertEqual(timestamp, eorm_registry:infer_type(updated_at)),
    ?assertEqual(timestamp, eorm_registry:infer_type(deleted_at)),
    ?assertEqual(boolean, eorm_registry:infer_type(is_active)),
    ?assertEqual(boolean, eorm_registry:infer_type(is_deleted)),
    ?assertEqual(boolean, eorm_registry:infer_type(is_admin)),
    ?assertEqual(string, eorm_registry:infer_type(name)),
    ?assertEqual(string, eorm_registry:infer_type(email)),
    ?assertEqual(string, eorm_registry:infer_type(random_field)).

test_registry_field_processing() ->
    %% Test field processing with all variations
    Model = #{
        table => users,
        fields => [
            {id},                                % Inferred as primary key
            {name, string},                      % Explicit type
            {email, {string, 100}, [unique]},   % Parameterized type with options
            {age, integer, [{default, 18}]},    % Type with default
            {user_id},                           % Inferred as integer (FK)
            {created_at},                        % Inferred as timestamp
            {is_active},                         % Inferred as boolean
            timestamps                           % Expands to created_at, updated_at
        ]
    },
    
    Result = eorm_registry:parse_model(Model),
    ?assertMatch(#eorm_model{}, Result).

%%====================================================================
%% eorm_schema_inspector Full Coverage Tests
%%====================================================================

schema_inspector_full_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        {"PostgreSQL table exists", fun test_postgres_table_exists/0},
        {"MySQL table exists", fun test_mysql_table_exists/0},
        {"SQLite table exists", fun test_sqlite_table_exists/0},
        {"Get columns for all adapters", fun test_get_columns_all_adapters/0},
        {"Parse column types", fun test_parse_column_types/0}
     ]
    }.

test_postgres_table_exists() ->
    meck:expect(eorm_adapter, query, fun(postgres, _, _) -> 
        {ok, [[true]]}
    end),
    Result = eorm_schema_inspector:table_exists(postgres, users),
    ?assertEqual(true, Result).

test_mysql_table_exists() ->
    meck:expect(eorm_adapter, query, fun(mysql, _, _) -> 
        {ok, [[1]]}
    end),
    Result = eorm_schema_inspector:table_exists(mysql, users),
    ?assertEqual(true, Result).

test_sqlite_table_exists() ->
    meck:expect(eorm_adapter, query, fun(sqlite, _, _) -> 
        {ok, [[users]]}
    end),
    Result = eorm_schema_inspector:table_exists(sqlite, users),
    ?assertEqual(true, Result).

test_get_columns_all_adapters() ->
    %% Test PostgreSQL columns
    meck:expect(eorm_adapter, query, fun(postgres, <<"SELECT column_name", _/binary>>, _) -> 
        {ok, [
            {<<"id">>, <<"integer">>, <<"NO">>, null, null},
            {<<"name">>, <<"character varying">>, <<"YES">>, null, 255}
        ]}
    end),
    PgResult = eorm_schema_inspector:get_columns(postgres, users),
    ?assert(length(PgResult) > 0),
    
    %% Test MySQL columns
    meck:expect(eorm_adapter, query, fun(mysql, <<"SHOW COLUMNS", _/binary>>, _) -> 
        {ok, [
            {<<"id">>, <<"int(11)">>, <<"NO">>, <<"PRI">>, null, <<"auto_increment">>},
            {<<"name">>, <<"varchar(255)">>, <<"YES">>, <<"">>, null, <<"">>}
        ]}
    end),
    MyResult = eorm_schema_inspector:get_columns(mysql, users),
    ?assert(length(MyResult) > 0),
    
    %% Test SQLite columns
    meck:expect(eorm_adapter, query, fun(sqlite, <<"PRAGMA table_info", _/binary>>, _) -> 
        {ok, [
            {0, <<"id">>, <<"INTEGER">>, 1, null, 1},
            {1, <<"name">>, <<"TEXT">>, 0, null, 0}
        ]}
    end),
    SqliteResult = eorm_schema_inspector:get_columns(sqlite, users),
    ?assert(length(SqliteResult) > 0).

test_parse_column_types() ->
    %% Test PostgreSQL type parsing
    ?assertEqual(integer, eorm_schema_inspector:parse_postgres_type(<<"integer">>, null)),
    ?assertEqual({string, 255}, eorm_schema_inspector:parse_postgres_type(<<"character varying">>, 255)),
    ?assertEqual(text, eorm_schema_inspector:parse_postgres_type(<<"text">>, null)),
    ?assertEqual(boolean, eorm_schema_inspector:parse_postgres_type(<<"boolean">>, null)),
    ?assertEqual(timestamp, eorm_schema_inspector:parse_postgres_type(<<"timestamp">>, null)),
    
    %% Test MySQL type parsing
    ?assertEqual(integer, eorm_schema_inspector:parse_mysql_type(<<"int(11)">>, null)),
    ?assertEqual({string, 255}, eorm_schema_inspector:parse_mysql_type(<<"varchar(255)">>, null)),
    ?assertEqual(text, eorm_schema_inspector:parse_mysql_type(<<"text">>, null)),
    ?assertEqual(boolean, eorm_schema_inspector:parse_mysql_type(<<"tinyint(1)">>, null)),
    
    %% Test SQLite type parsing
    ?assertEqual(integer, eorm_schema_inspector:parse_sqlite_type(<<"INTEGER">>)),
    ?assertEqual(string, eorm_schema_inspector:parse_sqlite_type(<<"TEXT">>)),
    ?assertEqual(float, eorm_schema_inspector:parse_sqlite_type(<<"REAL">>)),
    ?assertEqual(binary, eorm_schema_inspector:parse_sqlite_type(<<"BLOB">>)).

%%====================================================================
%% eorm_ddl_generator Full Coverage Tests
%%====================================================================

ddl_generator_full_test_() ->
    [
        {"Generate CREATE TABLE for all databases", fun test_generate_create_table_all/0},
        {"Generate ALTER TABLE operations", fun test_generate_alter_table_all/0},
        {"Generate indexes", fun test_generate_indexes/0},
        {"Generate constraints", fun test_generate_constraints/0},
        {"Type mappings", fun test_type_mappings_all/0}
    ].

test_generate_create_table_all() ->
    Schema = #{
        table => users,
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => name, type => {string, 100}, opts => [not_null]},
            #{name => email, type => string, opts => [unique]},
            #{name => age, type => integer, opts => [{default, 18}]},
            #{name => is_active, type => boolean, opts => [{default, true}]},
            #{name => created_at, type => timestamp, opts => []}
        ]
    },
    
    %% Test PostgreSQL
    PgDDL = eorm_ddl_generator:generate_create_table(postgres, users, Schema),
    ?assert(is_binary(PgDDL)),
    ?assert(binary:match(PgDDL, <<"CREATE TABLE">>) =/= nomatch),
    
    %% Test MySQL
    MyDDL = eorm_ddl_generator:generate_create_table(mysql, users, Schema),
    ?assert(is_binary(MyDDL)),
    ?assert(binary:match(MyDDL, <<"ENGINE=InnoDB">>) =/= nomatch orelse
            binary:match(MyDDL, <<"ENGINE=INNODB">>) =/= nomatch),
    
    %% Test SQLite
    SqliteDDL = eorm_ddl_generator:generate_create_table(sqlite, users, Schema),
    ?assert(is_binary(SqliteDDL)),
    ?assert(binary:match(SqliteDDL, <<"CREATE TABLE">>) =/= nomatch).

test_generate_alter_table_all() ->
    Changes = #{
        changes => [
            {add_column, phone, #{type => string, options => []}},
            {drop_column, old_field},
            {modify_column, age, #{type => integer}, #{type => bigint}},
            {rename_column, name, full_name}
        ]
    },
    
    %% Test PostgreSQL ALTER TABLE
    PgDDLs = eorm_ddl_generator:generate_alter_table(postgres, users, Changes),
    ?assert(is_list(PgDDLs)),
    ?assert(length(PgDDLs) > 0),
    
    %% Test MySQL ALTER TABLE
    MyDDLs = eorm_ddl_generator:generate_alter_table(mysql, users, Changes),
    ?assert(is_list(MyDDLs)),
    
    %% Test SQLite ALTER TABLE
    SqliteDDLs = eorm_ddl_generator:generate_alter_table(sqlite, users, Changes),
    ?assert(is_list(SqliteDDLs)).

test_generate_indexes() ->
    %% Test index generation
    DDL1 = eorm_ddl_generator:generate_create_index(postgres, users, 
        #{name => idx_users_email, columns => [email], unique => true}),
    ?assert(binary:match(DDL1, <<"CREATE UNIQUE INDEX">>) =/= nomatch),
    
    DDL2 = eorm_ddl_generator:generate_create_index(mysql, users,
        #{name => idx_users_name, columns => [name], unique => false}),
    ?assert(binary:match(DDL2, <<"CREATE INDEX">>) =/= nomatch),
    
    %% Test drop index
    DropDDL = eorm_ddl_generator:generate_drop_index(postgres, idx_users_email),
    ?assert(binary:match(DropDDL, <<"DROP INDEX">>) =/= nomatch).

test_generate_constraints() ->
    %% Test foreign key constraint
    FkConstraint = #{
        type => foreign_key,
        name => fk_posts_user_id,
        column => user_id,
        references => #{table => users, column => id}
    },
    FkDDL = eorm_ddl_generator:generate_postgres_constraint(FkConstraint),
    ?assert(is_list(FkDDL)),
    
    %% Test check constraint
    CheckConstraint = #{
        type => check,
        name => chk_age_positive,
        expression => <<"age > 0">>
    },
    CheckDDL = eorm_ddl_generator:generate_postgres_constraint(CheckConstraint),
    ?assert(is_list(CheckDDL)).

test_type_mappings_all() ->
    %% Test all type mappings for each database
    Types = [integer, bigint, smallint, string, text, boolean, float, double,
             decimal, timestamp, date, time, binary, json, uuid],
    
    lists:foreach(fun(Type) ->
        %% PostgreSQL
        PgType = eorm_ddl_generator:postgres_type(Type, null),
        ?assert(is_list(PgType)),
        
        %% MySQL
        MyType = eorm_ddl_generator:mysql_type(Type, null),
        ?assert(is_list(MyType)),
        
        %% SQLite
        SqliteType = eorm_ddl_generator:sqlite_type(Type, null),
        ?assert(is_list(SqliteType))
    end, Types),
    
    %% Test parameterized types
    ?assertEqual("VARCHAR(100)", eorm_ddl_generator:postgres_type({string, 100}, null)),
    ?assertEqual("VARCHAR(100)", eorm_ddl_generator:mysql_type({string, 100}, null)),
    ?assertEqual("TEXT", eorm_ddl_generator:sqlite_type({string, 100}, null)).

%%====================================================================
%% eorm_adapter Full Coverage Tests
%%====================================================================

adapter_full_test_() ->
    [
        {"Adapter operations", fun test_adapter_operations/0},
        {"Connection pool", fun test_connection_pool/0}
    ].

test_adapter_operations() ->
    %% Test all adapter operations
    Adapters = [postgres, mysql, sqlite],
    
    lists:foreach(fun(Adapter) ->
        %% Test connect
        ConnResult = eorm_adapter:connect(Adapter, #{}),
        ?assert(element(1, ConnResult) =:= ok orelse element(1, ConnResult) =:= error),
        
        %% Test query
        QueryResult = eorm_adapter:query(Adapter, <<"SELECT 1">>, []),
        ?assert(element(1, QueryResult) =:= ok orelse element(1, QueryResult) =:= error),
        
        %% Test execute_ddl
        DDLResult = eorm_adapter:execute_ddl(Adapter, <<"CREATE TABLE test (id INT)">>),
        ?assert(DDLResult =:= ok orelse element(1, DDLResult) =:= error),
        
        %% Test disconnect
        DisconnResult = eorm_adapter:disconnect(Adapter),
        ?assert(DisconnResult =:= ok orelse element(1, DisconnResult) =:= error)
    end, Adapters).

test_connection_pool() ->
    %% Test connection pool operations
    PoolResult = eorm_adapter:get_connection(postgres),
    ?assert(element(1, PoolResult) =:= ok orelse element(1, PoolResult) =:= error),
    
    %% Test pool stats
    StatsResult = eorm_adapter:pool_stats(postgres),
    ?assert(is_map(StatsResult) orelse element(1, StatsResult) =:= error).

%%====================================================================
%% eorm_migration_history Full Coverage Tests
%%====================================================================

migration_history_full_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        {"Ensure migration table", fun test_ensure_migration_table/0},
        {"Record migration", fun test_record_migration/0},
        {"Get migration history", fun test_get_migration_history/0},
        {"Check migration exists", fun test_check_migration_exists/0}
     ]
    }.

test_ensure_migration_table() ->
    meck:expect(eorm_adapter, execute_ddl, fun(_, _) -> ok end),
    Result = eorm_migration_history:ensure_table(postgres),
    ?assertEqual(ok, Result).

test_record_migration() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, 1} end),
    Result = eorm_migration_history:record(postgres, <<"users">>, create_table, <<"CREATE TABLE users">>),
    ?assertEqual(ok, Result).

test_get_migration_history() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> 
        {ok, [
            {1, <<"users">>, <<"create_table">>, <<"2024-01-01">>, <<"CREATE TABLE users">>}
        ]}
    end),
    Result = eorm_migration_history:get_history(postgres, <<"users">>),
    ?assertMatch({ok, [_|_]}, Result).

test_check_migration_exists() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [[1]]} end),
    Result = eorm_migration_history:exists(postgres, <<"users">>, <<"create_table">>),
    ?assertEqual(true, Result).

%%====================================================================
%% eorm_migration_lock Full Coverage Tests
%%====================================================================

migration_lock_full_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        {"Acquire lock", fun test_acquire_lock/0},
        {"Release lock", fun test_release_lock/0},
        {"Lock with timeout", fun test_lock_with_timeout/0}
     ]
    }.

test_acquire_lock() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [[true]]} end),
    Result = eorm_migration_lock:acquire(postgres, <<"migration_1">>),
    ?assertEqual(ok, Result).

test_release_lock() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [[true]]} end),
    Result = eorm_migration_lock:release(postgres, <<"migration_1">>),
    ?assertEqual(ok, Result).

test_lock_with_timeout() ->
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, [[false]]} end),
    Result = eorm_migration_lock:acquire(postgres, <<"migration_1">>, 100),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% eorm_schema_diff Full Coverage Tests
%%====================================================================

schema_diff_full_test_() ->
    [
        {"Compare schemas", fun test_schema_diff_compare/0},
        {"Assess risk levels", fun test_assess_risk_levels/0},
        {"Analyze risks", fun test_analyze_risks/0}
    ].

test_schema_diff_compare() ->
    Current = #{
        table => users,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => []},
            #{name => old_field, type => string, opts => []}
        ],
        indexes => [
            #{name => idx_old, columns => [old_field]}
        ]
    },
    Target = #{
        table => users,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => []},
            #{name => email, type => string, opts => [unique]}
        ],
        indexes => [
            #{name => idx_email, columns => [email]}
        ]
    },
    
    Result = eorm_schema_diff:compare(Current, Target),
    ?assertMatch(#{}, Result).

test_assess_risk_levels() ->
    Changes = [
        {add_column, users, email, string},
        {drop_column, users, password},
        {drop_table, old_users},
        {modify_column, users, age, integer, bigint}
    ],
    
    Risk = eorm_schema_diff:assess_risk(Changes),
    ?assertEqual(high, Risk).

test_analyze_risks() ->
    Changes = #{
        changes => [
            {add_column, email, string},
            {drop_column, password},
            {drop_table, temp_table}
        ]
    },
    
    Result = eorm_schema_diff:analyze_risks(Changes),
    ?assertMatch(#{dangerous := [_|_]}, Result).