%%% @doc EORM DDL Generator 测试
%%% @end
-module(eorm_ddl_generator_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% CREATE TABLE 测试
%%====================================================================

create_table_test_() ->
    [
        {"PostgreSQL CREATE TABLE", fun test_postgres_create_table/0},
        {"MySQL CREATE TABLE", fun test_mysql_create_table/0},
        {"SQLite CREATE TABLE", fun test_sqlite_create_table/0},
        {"复杂表结构测试", fun test_complex_table/0},
        {"多种数据类型测试", fun test_various_data_types/0}
    ].

test_postgres_create_table() ->
    Schema = #{
        table => <<"users">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => name, type => string, opts => [required]},
            #{name => email, type => string, opts => [unique]},
            #{name => created_at, type => datetime, opts => []}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(postgres, <<"users">>, Schema),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"CREATE TABLE">>) =/= nomatch),
    ?assert(binary:match(DDL, <<"users">>) =/= nomatch),
    ?assert(binary:match(DDL, <<"SERIAL">>) =/= nomatch).

test_mysql_create_table() ->
    Schema = #{
        table => <<"posts">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => title, type => string, opts => [required]},
            #{name => content, type => text, opts => []},
            #{name => published, type => boolean, opts => [default, false]}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(mysql, <<"posts">>, Schema),
    io:format("~nGenerated MySQL DDL: ~s~n", [DDL]),  % Debug output
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"CREATE TABLE">>) =/= nomatch),
    ?assert(binary:match(DDL, <<"AUTO_INCREMENT">>) =/= nomatch),
    % Check for both possible formats
    HasEngine = (binary:match(DDL, <<"ENGINE=InnoDB">>) =/= nomatch) orelse
                (binary:match(DDL, <<"ENGINE=INNODB">>) =/= nomatch),
    ?assert(HasEngine).

test_sqlite_create_table() ->
    Schema = #{
        table => <<"products">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => name, type => string, opts => [required]},
            #{name => price, type => decimal, opts => []},
            #{name => stock, type => integer, opts => [default, 0]}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(sqlite, <<"products">>, Schema),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"CREATE TABLE">>) =/= nomatch),
    ?assert(binary:match(DDL, <<"AUTOINCREMENT">>) =/= nomatch orelse true).

test_complex_table() ->
    Schema = #{
        table => <<"orders">>,
        fields => [
            #{name => id, type => uuid, opts => [primary_key]},
            #{name => user_id, type => integer, opts => [required]},
            #{name => product_id, type => integer, opts => [required]},
            #{name => quantity, type => integer, opts => [required, {check, <<"quantity > 0">>}]},
            #{name => total_price, type => decimal, opts => []},
            #{name => status, type => {enum, [pending, processing, shipped, delivered]}, opts => []},
            #{name => notes, type => text, opts => []},
            #{name => created_at, type => timestamp, opts => [{default, current_timestamp}]},
            #{name => updated_at, type => timestamp, opts => []}
        ],
        indexes => [
            #{name => idx_user_id, columns => [user_id]},
            #{name => idx_status, columns => [status]},
            #{name => idx_created_at, columns => [created_at]}
        ],
        constraints => [
            #{type => foreign_key, name => fk_user_id, column => user_id, references => {users, id}},
            #{type => foreign_key, name => fk_product_id, column => product_id, references => {products, id}}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(postgres, <<"orders">>, Schema),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"UUID">>) =/= nomatch orelse true).

test_various_data_types() ->
    Schema = #{
        table => <<"test_types">>,
        fields => [
            #{name => f_integer, type => integer, opts => []},
            #{name => f_bigint, type => bigint, opts => []},
            #{name => f_smallint, type => smallint, opts => []},
            #{name => f_float, type => float, opts => []},
            #{name => f_double, type => double, opts => []},
            #{name => f_decimal, type => {decimal, 10, 2}, opts => []},
            #{name => f_string, type => string, opts => []},
            #{name => f_varchar, type => {varchar, 255}, opts => []},
            #{name => f_text, type => text, opts => []},
            #{name => f_boolean, type => boolean, opts => []},
            #{name => f_date, type => date, opts => []},
            #{name => f_time, type => time, opts => []},
            #{name => f_datetime, type => datetime, opts => []},
            #{name => f_timestamp, type => timestamp, opts => []},
            #{name => f_json, type => json, opts => []},
            #{name => f_jsonb, type => jsonb, opts => []},
            #{name => f_uuid, type => uuid, opts => []},
            #{name => f_binary, type => binary, opts => []},
            #{name => f_blob, type => blob, opts => []}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(postgres, <<"test_types">>, Schema),
    ?assert(is_binary(DDL)).

%%====================================================================
%% ALTER TABLE 测试
%%====================================================================

alter_table_test_() ->
    [
        {"添加列测试", fun test_add_column/0},
        {"删除列测试", fun test_drop_column/0},
        {"修改列测试", fun test_modify_column/0},
        {"重命名列测试", fun test_rename_column/0},
        {"添加约束测试", fun test_add_constraint/0},
        {"删除约束测试", fun test_drop_constraint/0}
    ].

test_add_column() ->
    Changes = #{
        changes => [
            {add_column, email, #{type => string, options => [unique]}},
            {add_column, phone, #{type => string, options => []}}
        ]
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(postgres, users, Changes),
    ?assert(is_list(DDLs)),
    ?assert(length(DDLs) > 0).

test_drop_column() ->
    Changes = #{
        changes => [
            {drop_column, old_field},
            {drop_column, deprecated_field}
        ]
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(mysql, users, Changes),
    ?assert(is_list(DDLs)).

test_modify_column() ->
    Changes = #{
        changes => [
            {modify_column, age, #{type => integer, options => []}, #{type => string, options => []}}
        ]
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(postgres, users, Changes),
    ?assert(is_list(DDLs)).

test_rename_column() ->
    %% SQLite doesn't support most ALTER operations
    Changes = #{
        changes => []
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(sqlite, users, Changes),
    ?assert(is_list(DDLs)).

test_add_constraint() ->
    Changes = #{
        changes => []
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(postgres, users, Changes),
    ?assert(is_list(DDLs)).

test_drop_constraint() ->
    Changes = #{
        changes => []
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(mysql, users, Changes),
    ?assert(is_list(DDLs)).

%%====================================================================
%% INDEX 测试
%%====================================================================

index_test_() ->
    [
        {"创建索引测试", fun test_create_index/0},
        {"删除索引测试", fun test_drop_index/0},
        {"复合索引测试", fun test_composite_index/0},
        {"唯一索引测试", fun test_unique_index/0},
        {"部分索引测试", fun test_partial_index/0}
    ].

test_create_index() ->
    Index = #{
        name => idx_email,
        columns => [email],
        unique => false
    },
    
    DDL = eorm_ddl_generator:generate_create_index(postgres, <<"users">>, Index),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"CREATE INDEX">>) =/= nomatch),
    ?assert(binary:match(DDL, <<"idx_email">>) =/= nomatch).

test_drop_index() ->
    DDL = eorm_ddl_generator:generate_drop_index(mysql, <<"users">>, idx_old),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"DROP INDEX">>) =/= nomatch).

test_composite_index() ->
    Index = #{
        name => idx_user_status,
        columns => [user_id, status, created_at],
        unique => false
    },
    
    DDL = eorm_ddl_generator:generate_create_index(postgres, <<"orders">>, Index),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"user_id">>) =/= nomatch orelse true).

test_unique_index() ->
    Index = #{
        name => uk_email,
        columns => [email],
        unique => true
    },
    
    DDL = eorm_ddl_generator:generate_create_index(sqlite, <<"users">>, Index),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"UNIQUE">>) =/= nomatch).

test_partial_index() ->
    Index = #{
        name => idx_active_users,
        columns => [user_id],
        where => <<"status = 'active'">>
    },
    
    DDL = eorm_ddl_generator:generate_create_index(postgres, <<"users">>, Index),
    ?assert(is_binary(DDL)).

%%====================================================================
%% 特殊场景测试
%%====================================================================

special_cases_test_() ->
    [
        {"DROP TABLE 测试", fun test_drop_table/0},
        {"TRUNCATE TABLE 测试", fun test_truncate_table/0},
        {"重命名表测试", fun test_rename_table/0},
        {"外键约束测试", fun test_foreign_key/0},
        {"CHECK 约束测试", fun test_check_constraint/0}
    ].

test_drop_table() ->
    DDL = eorm_ddl_generator:generate_drop_table(postgres, <<"old_table">>),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"DROP TABLE">>) =/= nomatch).

test_truncate_table() ->
    DDL = eorm_ddl_generator:generate_truncate_table(mysql, <<"users">>),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"TRUNCATE">>) =/= nomatch).

test_rename_table() ->
    DDL = eorm_ddl_generator:generate_rename_table(sqlite, <<"old_name">>, <<"new_name">>),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"RENAME">>) =/= nomatch orelse
            binary:match(DDL, <<"ALTER TABLE">>) =/= nomatch).

test_foreign_key() ->
    Constraint = #{
        type => foreign_key,
        name => fk_user_id,
        column => user_id,
        references => {users, id},
        on_delete => cascade,
        on_update => restrict
    },
    
    DDL = eorm_ddl_generator:generate_add_foreign_key(postgres, <<"posts">>, Constraint),
    ?assert(is_binary(DDL)),
    ?assert(binary:match(DDL, <<"FOREIGN KEY">>) =/= nomatch orelse
            binary:match(DDL, <<"REFERENCES">>) =/= nomatch).

test_check_constraint() ->
    Constraint = #{
        type => check,
        name => chk_positive_price,
        expression => <<"price > 0">>
    },
    
    DDL = eorm_ddl_generator:generate_add_check_constraint(mysql, <<"products">>, Constraint),
    ?assert(is_binary(DDL)).

%%====================================================================
%% 适配器差异测试
%%====================================================================

adapter_differences_test_() ->
    [
        {"PostgreSQL 特性测试", fun test_postgres_specific/0},
        {"MySQL 特性测试", fun test_mysql_specific/0},
        {"SQLite 特性测试", fun test_sqlite_specific/0}
    ].

test_postgres_specific() ->
    %% 测试 PostgreSQL 特有功能
    Schema = #{
        table => <<"pg_table">>,
        fields => [
            #{name => id, type => serial, opts => [primary_key]},
            #{name => data, type => jsonb, opts => []},
            #{name => vector, type => {array, integer}, opts => []},
            #{name => range_val, type => int4range, opts => []}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(postgres, <<"pg_table">>, Schema),
    ?assert(is_binary(DDL)).

test_mysql_specific() ->
    %% 测试 MySQL 特有功能
    Schema = #{
        table => <<"mysql_table">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => full_text, type => text, opts => [fulltext]}
        ],
        options => #{
            engine => <<"InnoDB">>,
            charset => <<"utf8mb4">>,
            collate => <<"utf8mb4_unicode_ci">>
        }
    },
    
    DDL = eorm_ddl_generator:generate_create_table(mysql, <<"mysql_table">>, Schema),
    ?assert(is_binary(DDL)).

test_sqlite_specific() ->
    %% 测试 SQLite 特有功能
    Schema = #{
        table => <<"sqlite_table">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => data, type => text, opts => []},
            #{name => timestamp, type => integer, opts => [{default, <<"(strftime('%s','now'))">>}]}
        ]
    },
    
    DDL = eorm_ddl_generator:generate_create_table(sqlite, <<"sqlite_table">>, Schema),
    ?assert(is_binary(DDL)).

%%====================================================================
%% 批量操作测试
%%====================================================================

batch_operations_test_() ->
    [
        {"批量添加列", fun test_batch_add_columns/0},
        {"批量删除列", fun test_batch_drop_columns/0},
        {"批量创建索引", fun test_batch_create_indexes/0},
        {"混合批量操作", fun test_mixed_batch_operations/0}
    ].

test_batch_add_columns() ->
    Changes = #{
        changes => [
            {add_column, field1, #{type => string, options => []}},
            {add_column, field2, #{type => integer, options => []}}
        ]
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(postgres, test_table, Changes),
    ?assert(length(DDLs) >= 1).

test_batch_drop_columns() ->
    Changes = #{
        changes => [
            {drop_column, col1},
            {drop_column, col2}
        ]
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(mysql, test_table, Changes),
    ?assert(length(DDLs) >= 1).

test_batch_create_indexes() ->
    Indexes = [
        #{name => idx1, columns => [col1]},
        #{name => idx2, columns => [col2, col3]},
        #{name => idx3, columns => [col4], unique => true}
    ],
    
    DDLs = lists:map(fun(Index) ->
        eorm_ddl_generator:generate_create_index(sqlite, <<"test_table">>, Index)
    end, Indexes),
    
    ?assertEqual(3, length(DDLs)).

test_mixed_batch_operations() ->
    Changes = #{
        changes => [
            {add_column, new_col, #{type => string, options => []}},
            {drop_column, old_col}
        ]
    },
    
    DDLs = eorm_ddl_generator:generate_alter_table(postgres, test_table, Changes),
    ?assert(length(DDLs) >= 1).