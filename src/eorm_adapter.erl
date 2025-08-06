%%% @doc 数据库适配器基础模块
%%% @end
-module(eorm_adapter).

-export([
    query/3,
    execute/3,
    execute_ddl/2,
    get_connection/1
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