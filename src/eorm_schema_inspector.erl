%%% @doc Schema 检查器
%%% 获取和分析数据库表结构
%%% @end
-module(eorm_schema_inspector).

-export([
    table_exists/2,
    get_table_schema/2,
    get_columns/2,
    get_indexes/2,
    get_constraints/2
]).

-include("eorm.hrl").

%% @doc 检查表是否存在
-spec table_exists(atom(), atom()) -> boolean().
table_exists(postgres, TableName) ->
    SQL = "SELECT EXISTS (
        SELECT FROM information_schema.tables 
        WHERE table_schema = 'public' 
        AND table_name = $1
    )",
    
    case eorm_adapter:query(postgres, SQL, [atom_to_list(TableName)]) of
        {ok, [[true]]} -> true;
        _ -> false
    end;

table_exists(mysql, TableName) ->
    SQL = "SELECT COUNT(*) FROM information_schema.tables 
           WHERE table_schema = DATABASE() 
           AND table_name = ?",
    
    case eorm_adapter:query(mysql, SQL, [atom_to_list(TableName)]) of
        {ok, [[Count]]} when Count > 0 -> true;
        _ -> false
    end;

table_exists(sqlite, TableName) ->
    SQL = "SELECT COUNT(*) FROM sqlite_master 
           WHERE type='table' AND name=?",
    
    case eorm_adapter:query(sqlite, SQL, [atom_to_list(TableName)]) of
        {ok, [[Count]]} when Count > 0 -> true;
        _ -> false
    end.

%% @doc 获取完整的表 schema
-spec get_table_schema(atom(), atom()) -> map().
get_table_schema(Adapter, TableName) ->
    #{
        columns => get_columns(Adapter, TableName),
        indexes => get_indexes(Adapter, TableName),
        constraints => get_constraints(Adapter, TableName)
    }.

%% @doc 获取表的列信息
-spec get_columns(atom(), atom()) -> list().
get_columns(postgres, TableName) ->
    SQL = "SELECT 
        column_name,
        data_type,
        character_maximum_length,
        is_nullable,
        column_default
    FROM information_schema.columns
    WHERE table_schema = 'public' 
    AND table_name = $1
    ORDER BY ordinal_position",
    
    case eorm_adapter:query(postgres, SQL, [atom_to_list(TableName)]) of
        {ok, Rows} ->
            [parse_postgres_column(Row) || Row <- Rows];
        _ ->
            []
    end;

get_columns(mysql, TableName) ->
    SQL = "SELECT 
        COLUMN_NAME,
        DATA_TYPE,
        CHARACTER_MAXIMUM_LENGTH,
        IS_NULLABLE,
        COLUMN_DEFAULT,
        EXTRA
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() 
    AND TABLE_NAME = ?
    ORDER BY ORDINAL_POSITION",
    
    case eorm_adapter:query(mysql, SQL, [atom_to_list(TableName)]) of
        {ok, Rows} ->
            [parse_mysql_column(Row) || Row <- Rows];
        _ ->
            []
    end;

get_columns(sqlite, TableName) ->
    SQL = "PRAGMA table_info(" ++ atom_to_list(TableName) ++ ")",
    
    case eorm_adapter:query(sqlite, SQL, []) of
        {ok, Rows} ->
            [parse_sqlite_column(Row) || Row <- Rows];
        _ ->
            []
    end.

%% @doc 获取表的索引信息
-spec get_indexes(atom(), atom()) -> list().
get_indexes(postgres, TableName) ->
    SQL = "SELECT 
        i.relname as index_name,
        array_agg(a.attname ORDER BY array_position(ix.indkey, a.attnum)) as columns
    FROM pg_index ix
    JOIN pg_class t ON t.oid = ix.indrelid
    JOIN pg_class i ON i.oid = ix.indexrelid
    JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = ANY(ix.indkey)
    WHERE t.relname = $1
    AND NOT ix.indisprimary
    GROUP BY i.relname",
    
    case eorm_adapter:query(postgres, SQL, [atom_to_list(TableName)]) of
        {ok, Rows} ->
            [parse_postgres_index(Row) || Row <- Rows];
        _ ->
            []
    end;

get_indexes(mysql, TableName) ->
    SQL = "SELECT 
        INDEX_NAME,
        GROUP_CONCAT(COLUMN_NAME ORDER BY SEQ_IN_INDEX) as columns
    FROM information_schema.STATISTICS
    WHERE TABLE_SCHEMA = DATABASE() 
    AND TABLE_NAME = ?
    AND INDEX_NAME != 'PRIMARY'
    GROUP BY INDEX_NAME",
    
    case eorm_adapter:query(mysql, SQL, [atom_to_list(TableName)]) of
        {ok, Rows} ->
            [parse_mysql_index(Row) || Row <- Rows];
        _ ->
            []
    end;

get_indexes(sqlite, TableName) ->
    SQL = "PRAGMA index_list(" ++ atom_to_list(TableName) ++ ")",
    
    case eorm_adapter:query(sqlite, SQL, []) of
        {ok, Rows} ->
            lists:flatmap(fun(Row) ->
                get_sqlite_index_info(Row)
            end, Rows);
        _ ->
            []
    end.

%% @doc 获取表的约束信息
-spec get_constraints(atom(), atom()) -> list().
get_constraints(postgres, TableName) ->
    SQL = "SELECT 
        conname,
        contype,
        pg_get_constraintdef(c.oid) as definition
    FROM pg_constraint c
    JOIN pg_class t ON t.oid = c.conrelid
    WHERE t.relname = $1",
    
    case eorm_adapter:query(postgres, SQL, [atom_to_list(TableName)]) of
        {ok, Rows} ->
            [parse_postgres_constraint(Row) || Row <- Rows];
        _ ->
            []
    end;

get_constraints(mysql, TableName) ->
    SQL = "SELECT 
        CONSTRAINT_NAME,
        CONSTRAINT_TYPE
    FROM information_schema.TABLE_CONSTRAINTS
    WHERE TABLE_SCHEMA = DATABASE() 
    AND TABLE_NAME = ?",
    
    case eorm_adapter:query(mysql, SQL, [atom_to_list(TableName)]) of
        {ok, Rows} ->
            [parse_mysql_constraint(Row) || Row <- Rows];
        _ ->
            []
    end;

get_constraints(_sqlite, _TableName) ->
    %% SQLite 约束信息通过其他方式获取
    [].

%%====================================================================
%% Internal functions
%%====================================================================

%% @private 解析 PostgreSQL 列信息
parse_postgres_column([Name, Type, MaxLen, Nullable, Default]) ->
    #{
        name => binary_to_atom(Name, utf8),
        type => parse_postgres_type(Type, MaxLen),
        nullable => Nullable =:= <<"YES">>,
        default => Default,
        options => build_column_options(Nullable, Default)
    }.

%% @private 解析 PostgreSQL 类型
parse_postgres_type(<<"character varying">>, MaxLen) when is_integer(MaxLen) ->
    {varchar, MaxLen};
parse_postgres_type(<<"character varying">>, _) ->
    {varchar, 255};
parse_postgres_type(<<"integer">>, _) ->
    integer;
parse_postgres_type(<<"text">>, _) ->
    text;
parse_postgres_type(<<"timestamp", _/binary>>, _) ->
    timestamp;
parse_postgres_type(<<"boolean">>, _) ->
    boolean;
parse_postgres_type(Type, _) ->
    binary_to_atom(Type, utf8).

%% @private 解析 MySQL 列信息
parse_mysql_column([Name, Type, MaxLen, Nullable, Default, Extra]) ->
    #{
        name => binary_to_atom(Name, utf8),
        type => parse_mysql_type(Type, MaxLen),
        nullable => Nullable =:= <<"YES">>,
        default => Default,
        options => build_mysql_column_options(Nullable, Default, Extra)
    }.

%% @private 解析 MySQL 类型
parse_mysql_type(<<"varchar">>, MaxLen) when is_integer(MaxLen) ->
    {varchar, MaxLen};
parse_mysql_type(<<"int">>, _) ->
    integer;
parse_mysql_type(<<"text">>, _) ->
    text;
parse_mysql_type(<<"datetime">>, _) ->
    datetime;
parse_mysql_type(<<"timestamp">>, _) ->
    timestamp;
parse_mysql_type(Type, _) ->
    binary_to_atom(Type, utf8).

%% @private 解析 SQLite 列信息
parse_sqlite_column([_Cid, Name, Type, NotNull, Default, Pk]) ->
    #{
        name => binary_to_atom(Name, utf8),
        type => parse_sqlite_type(Type),
        nullable => NotNull =:= 0,
        default => Default,
        primary_key => Pk =:= 1,
        options => build_sqlite_column_options(NotNull, Default, Pk)
    }.

%% @private 解析 SQLite 类型
parse_sqlite_type(<<"INTEGER">>) -> integer;
parse_sqlite_type(<<"TEXT">>) -> text;
parse_sqlite_type(<<"REAL">>) -> float;
parse_sqlite_type(<<"BLOB">>) -> binary;
parse_sqlite_type(Type) -> binary_to_atom(Type, utf8).

%% @private 构建列选项
build_column_options(Nullable, Default) ->
    Options = [],
    Options1 = case Nullable of
        <<"NO">> -> [not_null | Options];
        _ -> Options
    end,
    case Default of
        null -> Options1;
        _ -> [{default, Default} | Options1]
    end.

%% @private 构建 MySQL 列选项
build_mysql_column_options(Nullable, Default, Extra) ->
    Options = build_column_options(Nullable, Default),
    case Extra of
        <<"auto_increment">> -> [auto_increment | Options];
        _ -> Options
    end.

%% @private 构建 SQLite 列选项
build_sqlite_column_options(NotNull, Default, Pk) ->
    Options = [],
    Options1 = case NotNull of
        1 -> [not_null | Options];
        _ -> Options
    end,
    Options2 = case Pk of
        1 -> [primary_key | Options1];
        _ -> Options1
    end,
    case Default of
        null -> Options2;
        _ -> [{default, Default} | Options2]
    end.

%% @private 解析 PostgreSQL 索引
parse_postgres_index([Name, Columns]) ->
    {binary_to_atom(Name, utf8), 
     [binary_to_atom(C, utf8) || C <- Columns]}.

%% @private 解析 MySQL 索引
parse_mysql_index([Name, Columns]) ->
    ColumnList = binary:split(Columns, <<",">>, [global]),
    {binary_to_atom(Name, utf8),
     [binary_to_atom(C, utf8) || C <- ColumnList]}.

%% @private 获取 SQLite 索引信息
get_sqlite_index_info([_Seq, Name, _Unique, _Origin, _Partial]) ->
    SQL = "PRAGMA index_info(" ++ binary_to_list(Name) ++ ")",
    case eorm_adapter:query(sqlite, SQL, []) of
        {ok, Rows} ->
            Columns = [binary_to_atom(element(3, Row), utf8) || Row <- Rows],
            [{binary_to_atom(Name, utf8), Columns}];
        _ ->
            []
    end.

%% @private 解析 PostgreSQL 约束
parse_postgres_constraint([Name, Type, Definition]) ->
    {constraint, binary_to_atom(Name, utf8), 
     parse_constraint_type(Type), Definition}.

%% @private 解析 MySQL 约束
parse_mysql_constraint([Name, Type]) ->
    {constraint, binary_to_atom(Name, utf8),
     parse_constraint_type(Type)}.

%% @private 解析约束类型
parse_constraint_type(<<"f">>) -> foreign_key;
parse_constraint_type(<<"p">>) -> primary_key;
parse_constraint_type(<<"u">>) -> unique;
parse_constraint_type(<<"c">>) -> check;
parse_constraint_type(<<"FOREIGN KEY">>) -> foreign_key;
parse_constraint_type(<<"PRIMARY KEY">>) -> primary_key;
parse_constraint_type(<<"UNIQUE">>) -> unique;
parse_constraint_type(_) -> unknown.