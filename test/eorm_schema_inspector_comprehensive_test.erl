%%% @doc Comprehensive tests for eorm_schema_inspector to achieve 100% coverage
%%% @end
-module(eorm_schema_inspector_comprehensive_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

setup() ->
    %% Start required applications
    application:ensure_all_started(meck),
    
    %% Mock eorm_adapter for database queries
    meck:new(eorm_adapter, [non_strict]),
    
    %% Default mock responses
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    
    ok.

teardown(_) ->
    %% Unload all mocks
    meck:unload(),
    ok.

%%====================================================================
%% Test Suites
%%====================================================================

schema_inspector_suite_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         %% Table existence tests
         fun test_table_exists_postgres/0,
         fun test_table_exists_mysql/0,
         fun test_table_exists_sqlite/0,
         fun test_table_exists_single_arg/0,
         
         %% Column tests
         fun test_get_columns_postgres/0,
         fun test_get_columns_mysql/0,
         fun test_get_columns_sqlite/0,
         fun test_get_columns_single_arg/0,
         
         %% Index tests
         fun test_get_indexes_postgres/0,
         fun test_get_indexes_mysql/0,
         fun test_get_indexes_sqlite/0,
         fun test_get_indexes_single_arg/0,
         
         %% Constraint tests
         fun test_get_constraints_postgres/0,
         fun test_get_constraints_mysql/0,
         fun test_get_constraints_sqlite/0,
         fun test_get_constraints_single_arg/0,
         
         %% Schema operations
         fun test_get_table_schema/0,
         fun test_get_table_info/0,
         fun test_compare_schemas/0,
         fun test_detect_new_fields/0,
         fun test_detect_removed_fields/0,
         fun test_detect_modified_fields/0,
         fun test_detect_index_changes/0,
         
         %% Database info
         fun test_get_database_version/0,
         fun test_get_database_charset/0,
         fun test_list_all_tables/0,
         fun test_get_table_size/0,
         
         %% Parser functions
         fun test_parse_postgres_column/0,
         fun test_parse_mysql_column/0,
         fun test_parse_sqlite_column/0,
         fun test_parse_postgres_type/0,
         fun test_parse_mysql_type/0,
         fun test_parse_sqlite_type/0,
         fun test_parse_constraint_type/0
     ]}.

%%====================================================================
%% Table Existence Tests
%%====================================================================

test_table_exists_postgres() ->
    %% Test table exists
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> {ok, [[true]]} end),
    ?assertEqual(true, eorm_schema_inspector:table_exists(postgres, test_table)),
    
    %% Test table doesn't exist
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> {ok, [[false]]} end),
    ?assertEqual(false, eorm_schema_inspector:table_exists(postgres, test_table)),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> {error, db_error} end),
    ?assertEqual(false, eorm_schema_inspector:table_exists(postgres, test_table)).

test_table_exists_mysql() ->
    %% Test table exists
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> {ok, [[1]]} end),
    ?assertEqual(true, eorm_schema_inspector:table_exists(mysql, test_table)),
    
    %% Test table doesn't exist
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> {ok, [[0]]} end),
    ?assertEqual(false, eorm_schema_inspector:table_exists(mysql, test_table)),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> {error, db_error} end),
    ?assertEqual(false, eorm_schema_inspector:table_exists(mysql, test_table)).

test_table_exists_sqlite() ->
    %% Test table exists
    meck:expect(eorm_adapter, query, 
        fun(sqlite, _, _) -> {ok, [[1]]} end),
    ?assertEqual(true, eorm_schema_inspector:table_exists(sqlite, test_table)),
    
    %% Test table doesn't exist
    meck:expect(eorm_adapter, query, 
        fun(sqlite, _, _) -> {ok, [[0]]} end),
    ?assertEqual(false, eorm_schema_inspector:table_exists(sqlite, test_table)),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(sqlite, _, _) -> {error, db_error} end),
    ?assertEqual(false, eorm_schema_inspector:table_exists(sqlite, test_table)).

test_table_exists_single_arg() ->
    Result = eorm_schema_inspector:table_exists(<<"test_table">>),
    ?assertMatch({ok, false}, Result).

%%====================================================================
%% Column Tests
%%====================================================================

test_get_columns_postgres() ->
    %% Test successful column retrieval
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> 
            {ok, [
                [<<"id">>, <<"integer">>, null, <<"NO">>, null],
                [<<"name">>, <<"character varying">>, 255, <<"YES">>, null],
                [<<"created_at">>, <<"timestamp without time zone">>, null, <<"NO">>, <<"now()">>],
                [<<"is_active">>, <<"boolean">>, null, <<"YES">>, <<"true">>],
                [<<"description">>, <<"text">>, null, <<"YES">>, null]
            ]}
        end),
    
    Columns = eorm_schema_inspector:get_columns(postgres, test_table),
    ?assertEqual(5, length(Columns)),
    
    %% Test first column
    [Col1 | _] = Columns,
    ?assertMatch(#{name := id, type := integer, nullable := false}, Col1),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_columns(postgres, test_table)).

test_get_columns_mysql() ->
    %% Test successful column retrieval
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> 
            {ok, [
                [<<"id">>, <<"int">>, null, <<"NO">>, null, <<"auto_increment">>],
                [<<"name">>, <<"varchar">>, 100, <<"YES">>, null, <<"">>],
                [<<"created_at">>, <<"datetime">>, null, <<"NO">>, <<"CURRENT_TIMESTAMP">>, <<"">>],
                [<<"updated_at">>, <<"timestamp">>, null, <<"YES">>, null, <<"">>],
                [<<"content">>, <<"text">>, null, <<"YES">>, null, <<"">>]
            ]}
        end),
    
    Columns = eorm_schema_inspector:get_columns(mysql, test_table),
    ?assertEqual(5, length(Columns)),
    
    %% Test first column with auto_increment
    [Col1 | _] = Columns,
    ?assertMatch(#{name := id, type := integer}, Col1),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_columns(mysql, test_table)).

test_get_columns_sqlite() ->
    %% Test successful column retrieval
    meck:expect(eorm_adapter, query, 
        fun(sqlite, _, _) -> 
            {ok, [
                [0, <<"id">>, <<"INTEGER">>, 1, null, 1],
                [1, <<"name">>, <<"TEXT">>, 0, null, 0],
                [2, <<"value">>, <<"REAL">>, 0, <<"0.0">>, 0],
                [3, <<"data">>, <<"BLOB">>, 0, null, 0],
                [4, <<"custom">>, <<"VARCHAR(50)">>, 0, null, 0]
            ]}
        end),
    
    Columns = eorm_schema_inspector:get_columns(sqlite, test_table),
    ?assertEqual(5, length(Columns)),
    
    %% Test first column with primary key
    [Col1 | Rest] = Columns,
    ?assertMatch(#{name := id, type := integer, primary_key := true}, Col1),
    
    %% Test other column types
    [Col2, Col3, Col4, Col5] = Rest,
    ?assertMatch(#{name := name, type := text}, Col2),
    ?assertMatch(#{name := value, type := float}, Col3),
    ?assertMatch(#{name := data, type := binary}, Col4),
    ?assertMatch(#{name := custom}, Col5),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(sqlite, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_columns(sqlite, test_table)).

test_get_columns_single_arg() ->
    Result = eorm_schema_inspector:get_columns(<<"test_table">>),
    ?assertMatch({ok, [_|_]}, Result).

%%====================================================================
%% Index Tests
%%====================================================================

test_get_indexes_postgres() ->
    %% Test successful index retrieval
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> 
            {ok, [
                [<<"idx_name">>, [<<"name">>]],
                [<<"idx_composite">>, [<<"name">>, <<"created_at">>]]
            ]}
        end),
    
    Indexes = eorm_schema_inspector:get_indexes(postgres, test_table),
    ?assertEqual(2, length(Indexes)),
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_indexes(postgres, test_table)).

test_get_indexes_mysql() ->
    %% Test successful index retrieval
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> 
            {ok, [
                [<<"idx_name">>, <<"name">>],
                [<<"idx_composite">>, <<"name,created_at">>]
            ]}
        end),
    
    Indexes = eorm_schema_inspector:get_indexes(mysql, test_table),
    ?assertEqual(2, length(Indexes)),
    
    %% Verify composite index is properly parsed
    [{_, [name]}, {_, [name, created_at]}] = Indexes,
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_indexes(mysql, test_table)).

test_get_indexes_sqlite() ->
    %% Test successful index retrieval
    erlang:put(sqlite_call_count, 0),
    meck:expect(eorm_adapter, query, 
        fun(sqlite, SQL, _) ->
            case string:find(SQL, "index_list") of
                nomatch ->
                    %% PRAGMA index_info call - return list format expected by element/2
                    {ok, [
                        [0, 0, <<"name">>],
                        [1, 1, <<"created_at">>]
                    ]};
                _ ->
                    %% PRAGMA index_list call - return list format
                    {ok, [
                        [0, <<"idx_test">>, 0, <<"c">>, 0]
                    ]}
            end
        end),
    
    Indexes = eorm_schema_inspector:get_indexes(sqlite, test_table),
    ?assert(is_list(Indexes)),
    
    %% Test query error for index_list
    meck:expect(eorm_adapter, query, 
        fun(sqlite, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_indexes(sqlite, test_table)).

test_get_indexes_single_arg() ->
    Result = eorm_schema_inspector:get_indexes(<<"test_table">>),
    ?assertMatch({ok, [_|_]}, Result).

%%====================================================================
%% Constraint Tests
%%====================================================================

test_get_constraints_postgres() ->
    %% Test successful constraint retrieval
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> 
            {ok, [
                [<<"pk_test">>, <<"p">>, <<"PRIMARY KEY (id)">>],
                [<<"fk_test">>, <<"f">>, <<"FOREIGN KEY (user_id) REFERENCES users(id)">>],
                [<<"uk_test">>, <<"u">>, <<"UNIQUE (email)">>],
                [<<"ck_test">>, <<"c">>, <<"CHECK (age > 0)">>],
                [<<"unknown_test">>, <<"x">>, <<"UNKNOWN">>]
            ]}
        end),
    
    Constraints = eorm_schema_inspector:get_constraints(postgres, test_table),
    ?assertEqual(5, length(Constraints)),
    
    %% Verify constraint types
    [{constraint, pk_test, primary_key, _},
     {constraint, fk_test, foreign_key, _},
     {constraint, uk_test, unique, _},
     {constraint, ck_test, check, _},
     {constraint, unknown_test, unknown, _}] = Constraints,
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(postgres, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_constraints(postgres, test_table)).

test_get_constraints_mysql() ->
    %% Test successful constraint retrieval
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> 
            {ok, [
                [<<"PRIMARY">>, <<"PRIMARY KEY">>],
                [<<"fk_user">>, <<"FOREIGN KEY">>],
                [<<"uk_email">>, <<"UNIQUE">>],
                [<<"other">>, <<"OTHER">>]
            ]}
        end),
    
    Constraints = eorm_schema_inspector:get_constraints(mysql, test_table),
    ?assertEqual(4, length(Constraints)),
    
    %% Verify constraint types
    [{constraint, 'PRIMARY', primary_key},
     {constraint, fk_user, foreign_key},
     {constraint, uk_email, unique},
     {constraint, other, unknown}] = Constraints,
    
    %% Test query error
    meck:expect(eorm_adapter, query, 
        fun(mysql, _, _) -> {error, db_error} end),
    ?assertEqual([], eorm_schema_inspector:get_constraints(mysql, test_table)).

test_get_constraints_sqlite() ->
    %% SQLite always returns empty list for constraints
    Result = eorm_schema_inspector:get_constraints(sqlite, test_table),
    ?assertEqual([], Result).

test_get_constraints_single_arg() ->
    Result = eorm_schema_inspector:get_constraints(<<"test_table">>),
    ?assertMatch({ok, [_|_]}, Result).

%%====================================================================
%% Schema Operations Tests
%%====================================================================

test_get_table_schema() ->
    %% Mock responses for complete schema
    meck:expect(eorm_adapter, query, fun(_, _, _) -> {ok, []} end),
    
    Schema = eorm_schema_inspector:get_table_schema(postgres, test_table),
    ?assertMatch(#{columns := [], indexes := [], constraints := []}, Schema).

test_get_table_info() ->
    Result = eorm_schema_inspector:get_table_info(<<"test_table">>),
    ?assertMatch({ok, #{table_name := <<"test_table">>, columns := [], indexes := []}}, Result).

test_compare_schemas() ->
    Schema1 = #{fields => [
        #{name => id, type => integer},
        #{name => name, type => string},
        #{name => old_field, type => text}
    ]},
    
    Schema2 = #{fields => [
        #{name => id, type => integer},
        #{name => name, type => varchar},  % Modified
        #{name => new_field, type => boolean}  % New
    ]},
    
    Result = eorm_schema_inspector:compare_schemas(Schema1, Schema2),
    
    ?assertMatch(#{added_fields := [_], 
                   removed_fields := [_], 
                   modified_fields := [_]}, Result),
    
    #{added_fields := Added,
      removed_fields := Removed,
      modified_fields := Modified} = Result,
    
    ?assertEqual(1, length(Added)),
    ?assertEqual(1, length(Removed)),
    ?assertEqual(1, length(Modified)).

test_detect_new_fields() ->
    Current = [#{name => id}, #{name => name}],
    Target = [#{name => id}, #{name => name}, #{name => email}],
    
    NewFields = eorm_schema_inspector:detect_new_fields(Current, Target),
    ?assertEqual(1, length(NewFields)),
    [#{name := email}] = NewFields.

test_detect_removed_fields() ->
    Current = [#{name => id}, #{name => name}, #{name => old_field}],
    Target = [#{name => id}, #{name => name}],
    
    RemovedFields = eorm_schema_inspector:detect_removed_fields(Current, Target),
    ?assertEqual(1, length(RemovedFields)),
    [#{name := old_field}] = RemovedFields.

test_detect_modified_fields() ->
    Current = [
        #{name => id, type => integer},
        #{name => name, type => string, length => 100},
        #{name => age, type => integer}
    ],
    Target = [
        #{name => id, type => integer},
        #{name => name, type => string, length => 255},  % Modified
        #{name => age, type => integer}
    ],
    
    ModifiedFields = eorm_schema_inspector:detect_modified_fields(Current, Target),
    ?assertEqual(1, length(ModifiedFields)),
    [#{name := name, type := string, length := 255}] = ModifiedFields.

test_detect_index_changes() ->
    Current = [
        #{name => idx_old, columns => [name]},
        #{name => idx_common, columns => [id]}
    ],
    Target = [
        #{name => idx_new, columns => [email]},
        #{name => idx_common, columns => [id]}
    ],
    
    {Added, Removed, Modified} = eorm_schema_inspector:detect_index_changes(Current, Target),
    ?assertEqual(1, length(Added)),
    ?assertEqual(1, length(Removed)),
    ?assertEqual(0, length(Modified)).

%%====================================================================
%% Database Info Tests
%%====================================================================

test_get_database_version() ->
    Result = eorm_schema_inspector:get_database_version(),
    ?assertMatch({ok, <<"PostgreSQL 14.5">>}, Result).

test_get_database_charset() ->
    Result = eorm_schema_inspector:get_database_charset(),
    ?assertMatch({ok, <<"UTF8">>}, Result).

test_list_all_tables() ->
    Result = eorm_schema_inspector:list_all_tables(),
    ?assertMatch({ok, [_|_]}, Result).

test_get_table_size() ->
    Result = eorm_schema_inspector:get_table_size(<<"test_table">>),
    ?assertMatch({ok, #{rows := 1000, data_size := _, index_size := _}}, Result).

%%====================================================================
%% Parser Function Tests
%%====================================================================

test_parse_postgres_column() ->
    %% Test various column configurations
    Col1 = eorm_schema_inspector:parse_postgres_column(
        [<<"id">>, <<"integer">>, null, <<"NO">>, null]),
    ?assertMatch(#{name := id, type := integer, nullable := false}, Col1),
    
    Col2 = eorm_schema_inspector:parse_postgres_column(
        [<<"name">>, <<"character varying">>, 100, <<"YES">>, <<"'default'">>]),
    ?assertMatch(#{name := name, type := {varchar, 100}, nullable := true}, Col2),
    
    Col3 = eorm_schema_inspector:parse_postgres_column(
        [<<"description">>, <<"text">>, null, <<"YES">>, null]),
    ?assertMatch(#{name := description, type := text, nullable := true}, Col3),
    
    Col4 = eorm_schema_inspector:parse_postgres_column(
        [<<"created_at">>, <<"timestamp with time zone">>, null, <<"NO">>, <<"now()">>]),
    ?assertMatch(#{name := created_at, type := timestamp, nullable := false}, Col4),
    
    Col5 = eorm_schema_inspector:parse_postgres_column(
        [<<"is_active">>, <<"boolean">>, null, <<"YES">>, <<"true">>]),
    ?assertMatch(#{name := is_active, type := boolean, nullable := true}, Col5),
    
    %% Test unknown type
    Col6 = eorm_schema_inspector:parse_postgres_column(
        [<<"custom">>, <<"custom_type">>, null, <<"YES">>, null]),
    ?assertMatch(#{name := custom, type := custom_type}, Col6).

test_parse_mysql_column() ->
    %% Test various column configurations
    Col1 = eorm_schema_inspector:parse_mysql_column(
        [<<"id">>, <<"int">>, null, <<"NO">>, null, <<"auto_increment">>]),
    ?assertMatch(#{name := id, type := integer, nullable := false}, Col1),
    
    Col2 = eorm_schema_inspector:parse_mysql_column(
        [<<"name">>, <<"varchar">>, 255, <<"YES">>, null, <<"">>]),
    ?assertMatch(#{name := name, type := {varchar, 255}, nullable := true}, Col2),
    
    Col3 = eorm_schema_inspector:parse_mysql_column(
        [<<"content">>, <<"text">>, null, <<"YES">>, null, <<"">>]),
    ?assertMatch(#{name := content, type := text, nullable := true}, Col3),
    
    Col4 = eorm_schema_inspector:parse_mysql_column(
        [<<"created_at">>, <<"datetime">>, null, <<"NO">>, <<"CURRENT_TIMESTAMP">>, <<"">>]),
    ?assertMatch(#{name := created_at, type := datetime, nullable := false}, Col4),
    
    Col5 = eorm_schema_inspector:parse_mysql_column(
        [<<"updated_at">>, <<"timestamp">>, null, <<"YES">>, null, <<"">>]),
    ?assertMatch(#{name := updated_at, type := timestamp, nullable := true}, Col5),
    
    %% Test unknown type
    Col6 = eorm_schema_inspector:parse_mysql_column(
        [<<"custom">>, <<"custom_type">>, null, <<"YES">>, null, <<"">>]),
    ?assertMatch(#{name := custom, type := custom_type}, Col6).

test_parse_sqlite_column() ->
    %% Test various column configurations  
    Col1 = eorm_schema_inspector:parse_sqlite_column(
        [0, <<"id">>, <<"INTEGER">>, 1, null, 1]),
    ?assertMatch(#{name := id, type := integer, nullable := false, primary_key := true}, Col1),
    
    Col2 = eorm_schema_inspector:parse_sqlite_column(
        [1, <<"name">>, <<"TEXT">>, 0, null, 0]),
    ?assertMatch(#{name := name, type := text, nullable := true, primary_key := false}, Col2),
    
    Col3 = eorm_schema_inspector:parse_sqlite_column(
        [2, <<"value">>, <<"REAL">>, 0, <<"0.0">>, 0]),
    ?assertMatch(#{name := value, type := float, nullable := true}, Col3),
    
    Col4 = eorm_schema_inspector:parse_sqlite_column(
        [3, <<"data">>, <<"BLOB">>, 1, null, 0]),
    ?assertMatch(#{name := data, type := binary, nullable := false}, Col4),
    
    %% Test unknown type
    Col5 = eorm_schema_inspector:parse_sqlite_column(
        [4, <<"custom">>, <<"CUSTOM">>, 0, null, 0]),
    ?assertMatch(#{name := custom, type := 'CUSTOM'}, Col5).

test_parse_postgres_type() ->
    %% Test various PostgreSQL types
    ?assertEqual({varchar, 100}, 
        eorm_schema_inspector:parse_postgres_type(<<"character varying">>, 100)),
    ?assertEqual({varchar, 255}, 
        eorm_schema_inspector:parse_postgres_type(<<"character varying">>, null)),
    ?assertEqual(integer, 
        eorm_schema_inspector:parse_postgres_type(<<"integer">>, null)),
    ?assertEqual(text, 
        eorm_schema_inspector:parse_postgres_type(<<"text">>, null)),
    ?assertEqual(timestamp, 
        eorm_schema_inspector:parse_postgres_type(<<"timestamp without time zone">>, null)),
    ?assertEqual(timestamp, 
        eorm_schema_inspector:parse_postgres_type(<<"timestamp with time zone">>, null)),
    ?assertEqual(boolean, 
        eorm_schema_inspector:parse_postgres_type(<<"boolean">>, null)),
    ?assertEqual(custom_type, 
        eorm_schema_inspector:parse_postgres_type(<<"custom_type">>, null)).

test_parse_mysql_type() ->
    %% Test various MySQL types
    ?assertEqual({varchar, 100}, 
        eorm_schema_inspector:parse_mysql_type(<<"varchar">>, 100)),
    ?assertEqual(integer, 
        eorm_schema_inspector:parse_mysql_type(<<"int">>, null)),
    ?assertEqual(text, 
        eorm_schema_inspector:parse_mysql_type(<<"text">>, null)),
    ?assertEqual(datetime, 
        eorm_schema_inspector:parse_mysql_type(<<"datetime">>, null)),
    ?assertEqual(timestamp, 
        eorm_schema_inspector:parse_mysql_type(<<"timestamp">>, null)),
    ?assertEqual(custom_type, 
        eorm_schema_inspector:parse_mysql_type(<<"custom_type">>, null)).

test_parse_sqlite_type() ->
    %% Test various SQLite types
    ?assertEqual(integer, eorm_schema_inspector:parse_sqlite_type(<<"INTEGER">>)),
    ?assertEqual(text, eorm_schema_inspector:parse_sqlite_type(<<"TEXT">>)),
    ?assertEqual(float, eorm_schema_inspector:parse_sqlite_type(<<"REAL">>)),
    ?assertEqual(binary, eorm_schema_inspector:parse_sqlite_type(<<"BLOB">>)),
    ?assertEqual('CUSTOM', eorm_schema_inspector:parse_sqlite_type(<<"CUSTOM">>)).

test_parse_constraint_type() ->
    %% Test PostgreSQL constraint types
    ?assertEqual(foreign_key, eorm_schema_inspector:parse_constraint_type(<<"f">>)),
    ?assertEqual(primary_key, eorm_schema_inspector:parse_constraint_type(<<"p">>)),
    ?assertEqual(unique, eorm_schema_inspector:parse_constraint_type(<<"u">>)),
    ?assertEqual(check, eorm_schema_inspector:parse_constraint_type(<<"c">>)),
    
    %% Test MySQL constraint types
    ?assertEqual(foreign_key, eorm_schema_inspector:parse_constraint_type(<<"FOREIGN KEY">>)),
    ?assertEqual(primary_key, eorm_schema_inspector:parse_constraint_type(<<"PRIMARY KEY">>)),
    ?assertEqual(unique, eorm_schema_inspector:parse_constraint_type(<<"UNIQUE">>)),
    
    %% Test unknown type
    ?assertEqual(unknown, eorm_schema_inspector:parse_constraint_type(<<"UNKNOWN">>)).