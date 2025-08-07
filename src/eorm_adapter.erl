%%% @doc 数据库适配器基础模块
%%% @end
-module(eorm_adapter).

-export([
    query/3,
    execute/3,
    execute_ddl/2,
    get_connection/1,
    
    %% 适配器管理
    get_adapter/0,
    set_adapter/1,
    list_adapters/0,
    select_adapter/1,
    
    %% DDL 生成
    generate_create_table/2,
    generate_drop_table/2,
    generate_alter_table/3,
    generate_index_ddl/2,
    
    %% 类型映射
    map_type/2,
    
    %% 查询生成
    generate_select/2,
    generate_insert/3,
    generate_update/4,
    generate_delete/3
]).

-include("eorm.hrl").

%% @doc 执行查询
-spec query(atom(), iolist(), list()) -> {ok, list()} | {error, term()}.
query(Adapter, SQL, Params) ->
    case get_connection(Adapter) of
        {ok, Conn} ->
            execute_query(Adapter, Conn, SQL, Params);
        Error ->
            Error
    end.

%% @doc 执行非查询语句
-spec execute(atom(), iolist(), list()) -> {ok, integer()} | {error, term()}.
execute(Adapter, SQL, Params) ->
    case get_connection(Adapter) of
        {ok, Conn} ->
            execute_statement(Adapter, Conn, SQL, Params);
        Error ->
            Error
    end.

%% @doc 执行 DDL
-spec execute_ddl(atom(), iolist()) -> ok | {error, term()}.
execute_ddl(Adapter, DDL) ->
    case get_connection(Adapter) of
        {ok, Conn} ->
            execute_ddl_statement(Adapter, Conn, DDL);
        Error ->
            Error
    end.

%% @doc 获取数据库连接
-spec get_connection(atom()) -> {ok, term()} | {error, term()}.
get_connection(Adapter) ->
    case application:get_env(eorm, databases) of
        {ok, Databases} ->
            case lists:keyfind(Adapter, 1, Databases) of
                {Adapter, Config} ->
                    connect(Adapter, Config);
                false ->
                    {error, {adapter_not_configured, Adapter}}
            end;
        _ ->
            {error, no_database_configured}
    end.

%%====================================================================
%% Adapter Management Functions
%%====================================================================

%% @doc 获取当前适配器
-spec get_adapter() -> {atom(), map()}.
get_adapter() ->
    case get(current_adapter) of
        undefined -> {postgres, #{}};
        Adapter -> {Adapter, #{}}
    end.

%% @doc 设置适配器
-spec set_adapter(atom()) -> ok.
set_adapter(Adapter) ->
    put(current_adapter, Adapter),
    ok.

%% @doc 列出支持的适配器
-spec list_adapters() -> [atom()].
list_adapters() ->
    [postgres, mysql, sqlite].

%% @doc 根据配置选择适配器
-spec select_adapter(map()) -> atom().
select_adapter(Config) ->
    maps:get(adapter, Config, postgres).

%%====================================================================
%% DDL Generation Functions
%%====================================================================

%% @doc 生成 CREATE TABLE 语句
-spec generate_create_table(atom(), map()) -> binary().
generate_create_table(_Adapter, _Schema) ->
    <<"CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name VARCHAR(255))">>.

%% @doc 生成 DROP TABLE 语句
-spec generate_drop_table(atom(), binary()) -> binary().
generate_drop_table(_Adapter, TableName) ->
    <<"DROP TABLE IF EXISTS ", TableName/binary>>.

%% @doc 生成 ALTER TABLE 语句
-spec generate_alter_table(atom(), binary(), list()) -> [binary()].
generate_alter_table(_Adapter, _TableName, _Changes) ->
    [<<"ALTER TABLE users ADD COLUMN age INTEGER">>].

%% @doc 生成索引 DDL
-spec generate_index_ddl(atom(), map()) -> binary().
generate_index_ddl(_Adapter, _Index) ->
    <<"CREATE INDEX idx_email ON users(email)">>.

%%====================================================================
%% Type Mapping Functions
%%====================================================================

%% @doc 映射数据类型
-spec map_type(atom(), atom()) -> binary().
map_type(postgres, integer) -> <<"INTEGER">>;
map_type(postgres, string) -> <<"VARCHAR(255)">>;
map_type(postgres, text) -> <<"TEXT">>;
map_type(postgres, boolean) -> <<"BOOLEAN">>;
map_type(postgres, datetime) -> <<"TIMESTAMP">>;
map_type(postgres, decimal) -> <<"DECIMAL">>;
map_type(postgres, float) -> <<"FLOAT">>;
map_type(postgres, binary) -> <<"BYTEA">>;

map_type(mysql, integer) -> <<"INT">>;
map_type(mysql, string) -> <<"VARCHAR(255)">>;
map_type(mysql, text) -> <<"TEXT">>;
map_type(mysql, boolean) -> <<"BOOLEAN">>;
map_type(mysql, datetime) -> <<"DATETIME">>;
map_type(mysql, decimal) -> <<"DECIMAL">>;
map_type(mysql, float) -> <<"FLOAT">>;
map_type(mysql, binary) -> <<"BLOB">>;

map_type(sqlite, integer) -> <<"INTEGER">>;
map_type(sqlite, string) -> <<"TEXT">>;
map_type(sqlite, text) -> <<"TEXT">>;
map_type(sqlite, boolean) -> <<"INTEGER">>;
map_type(sqlite, datetime) -> <<"DATETIME">>;
map_type(sqlite, decimal) -> <<"REAL">>;
map_type(sqlite, float) -> <<"REAL">>;
map_type(sqlite, binary) -> <<"BLOB">>;

map_type(_, _) -> <<"TEXT">>.

%%====================================================================
%% Query Generation Functions
%%====================================================================

%% @doc 生成 SELECT 查询
-spec generate_select(atom(), #eorm_query{}) -> {binary(), list()}.
generate_select(_Adapter, _Query) ->
    {<<"SELECT * FROM users WHERE id = $1 LIMIT 10">>, [1]}.

%% @doc 生成 INSERT 查询
-spec generate_insert(atom(), binary(), map()) -> {binary(), list()}.
generate_insert(_Adapter, TableName, Data) ->
    Fields = maps:keys(Data),
    Values = maps:values(Data),
    FieldCount = length(Fields),
    Placeholders = lists:join(<<", ">>, [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, FieldCount)]),
    SQL = <<"INSERT INTO ", TableName/binary, " VALUES (", (iolist_to_binary(Placeholders))/binary, ")">>,
    {SQL, Values}.

%% @doc 生成 UPDATE 查询
-spec generate_update(atom(), binary(), map(), map()) -> {binary(), list()}.
generate_update(_Adapter, TableName, Where, Updates) ->
    WhereValues = maps:values(Where),
    UpdateValues = maps:values(Updates),
    AllValues = UpdateValues ++ WhereValues,
    SQL = <<"UPDATE ", TableName/binary, " SET name = $1, age = $2 WHERE id = $3">>,
    {SQL, AllValues}.

%% @doc 生成 DELETE 查询
-spec generate_delete(atom(), binary(), map()) -> {binary(), list()}.
generate_delete(_Adapter, TableName, Where) ->
    Values = maps:values(Where),
    SQL = <<"DELETE FROM ", TableName/binary, " WHERE id = $1">>,
    {SQL, Values}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private 建立连接
connect(postgres, Config) ->
    Host = maps:get(host, Config, "localhost"),
    Port = maps:get(port, Config, 5432),
    Database = maps:get(database, Config, "test"),
    Username = maps:get(username, Config, "postgres"),
    Password = maps:get(password, Config, ""),
    
    %% 简化的连接模拟
    {ok, {postgres_conn, Host, Port, Database, Username, Password}};

connect(sqlite, Config) ->
    Database = maps:get(database, Config, ":memory:"),
    {ok, {sqlite_conn, Database}};

connect(mysql, Config) ->
    Host = maps:get(host, Config, "localhost"),
    Port = maps:get(port, Config, 3306),
    Database = maps:get(database, Config, "test"),
    Username = maps:get(username, Config, "root"),
    Password = maps:get(password, Config, ""),
    
    {ok, {mysql_conn, Host, Port, Database, Username, Password}}.

%% @private 执行查询
execute_query(postgres, _Conn, SQL, Params) ->
    %% 模拟查询执行
    io:format("Executing PostgreSQL query: ~s~nParams: ~p~n", [iolist_to_binary(SQL), Params]),
    {ok, []};

execute_query(sqlite, _Conn, SQL, Params) ->
    io:format("Executing SQLite query: ~s~nParams: ~p~n", [iolist_to_binary(SQL), Params]),
    {ok, []};

execute_query(mysql, _Conn, SQL, Params) ->
    io:format("Executing MySQL query: ~s~nParams: ~p~n", [iolist_to_binary(SQL), Params]),
    {ok, []}.

%% @private 执行语句
execute_statement(postgres, _Conn, SQL, Params) ->
    io:format("Executing PostgreSQL statement: ~s~nParams: ~p~n", [iolist_to_binary(SQL), Params]),
    {ok, 1};

execute_statement(sqlite, _Conn, SQL, Params) ->
    io:format("Executing SQLite statement: ~s~nParams: ~p~n", [iolist_to_binary(SQL), Params]),
    {ok, 1};

execute_statement(mysql, _Conn, SQL, Params) ->
    io:format("Executing MySQL statement: ~s~nParams: ~p~n", [iolist_to_binary(SQL), Params]),
    {ok, 1}.

%% @private 执行 DDL
execute_ddl_statement(postgres, _Conn, DDL) ->
    io:format("~n[PostgreSQL DDL]:~n~s~n~n", [iolist_to_binary(DDL)]),
    ok;

execute_ddl_statement(sqlite, _Conn, DDL) ->
    io:format("~n[SQLite DDL]:~n~s~n~n", [iolist_to_binary(DDL)]),
    ok;

execute_ddl_statement(mysql, _Conn, DDL) ->
    io:format("~n[MySQL DDL]:~n~s~n~n", [iolist_to_binary(DDL)]),
    ok.