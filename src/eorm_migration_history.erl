%%% @doc 迁移历史管理
%%% 记录和查询迁移历史
%%% @end
-module(eorm_migration_history).

-export([
    ensure_table/1,
    record/4,
    get_last_migration/2,
    get_all_migrations/1,
    update_status/3
]).

-include("eorm.hrl").

%% @doc 确保迁移历史表存在
-spec ensure_table(atom()) -> ok | {error, term()}.
ensure_table(Adapter) ->
    SQL = case Adapter of
        postgres -> postgres_create_table();
        mysql -> mysql_create_table();
        sqlite -> sqlite_create_table();
        _ -> error({unsupported_adapter, Adapter})
    end,
    
    case eorm_adapter:execute_ddl(Adapter, SQL) of
        ok -> ok;
        {error, {error, _, _, <<"relation \"eorm_migrations\" already exists">>, _}} -> ok;
        {error, {1050, _}} -> ok; %% MySQL table already exists
        Error -> Error
    end.

%% @doc 记录迁移
-spec record(atom(), atom(), atom(), iolist()) -> ok | {error, term()}.
record(Adapter, TableName, Action, DDL) ->
    SQL = "INSERT INTO eorm_migrations (model, version, checksum, status, changes, applied_at) VALUES ($1, $2, $3, $4, $5, $6)",
    
    Version = generate_version(),
    Checksum = calculate_checksum(DDL),
    Changes = iolist_to_binary(DDL),
    AppliedAt = format_timestamp(erlang:system_time(second)),
    
    Params = [
        atom_to_binary(TableName, utf8),
        Version,
        Checksum,
        <<"success">>,
        Changes,
        AppliedAt
    ],
    
    case eorm_adapter:query(Adapter, SQL, Params) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @doc 获取最后一次迁移记录
-spec get_last_migration(atom(), atom()) -> {ok, #eorm_migration{}} | {error, not_found}.
get_last_migration(Adapter, Model) ->
    SQL = "SELECT * FROM eorm_migrations WHERE model = $1 ORDER BY id DESC LIMIT 1",
    
    case eorm_adapter:query(Adapter, SQL, [atom_to_binary(Model, utf8)]) of
        {ok, [Row]} ->
            {ok, row_to_migration(Row)};
        {ok, []} ->
            {error, not_found};
        Error ->
            Error
    end.

%% @doc 获取所有迁移记录
-spec get_all_migrations(atom()) -> {ok, [#eorm_migration{}]} | {error, term()}.
get_all_migrations(Adapter) ->
    SQL = "SELECT * FROM eorm_migrations ORDER BY id ASC",
    
    case eorm_adapter:query(Adapter, SQL, []) of
        {ok, Rows} ->
            {ok, [row_to_migration(Row) || Row <- Rows]};
        Error ->
            Error
    end.

%% @doc 更新迁移状态
-spec update_status(atom(), binary(), atom()) -> ok | {error, term()}.
update_status(Adapter, Version, Status) ->
    SQL = "UPDATE eorm_migrations SET status = $1 WHERE version = $2",
    
    case eorm_adapter:query(Adapter, SQL, [atom_to_binary(Status, utf8), Version]) of
        {ok, _} -> ok;
        Error -> Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private PostgreSQL 建表语句
postgres_create_table() ->
    "CREATE TABLE IF NOT EXISTS eorm_migrations (
        id SERIAL PRIMARY KEY,
        model VARCHAR(255) NOT NULL,
        version VARCHAR(255) NOT NULL UNIQUE,
        checksum VARCHAR(64) NOT NULL,
        applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        execution_time INTEGER,
        status VARCHAR(20),
        changes TEXT,
        rollback_sql TEXT
    )".

%% @private MySQL 建表语句
mysql_create_table() ->
    "CREATE TABLE IF NOT EXISTS eorm_migrations (
        id INT AUTO_INCREMENT PRIMARY KEY,
        model VARCHAR(255) NOT NULL,
        version VARCHAR(255) NOT NULL UNIQUE,
        checksum VARCHAR(64) NOT NULL,
        applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        execution_time INT,
        status VARCHAR(20),
        changes TEXT,
        rollback_sql TEXT
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4".

%% @private SQLite 建表语句
sqlite_create_table() ->
    "CREATE TABLE IF NOT EXISTS eorm_migrations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        model TEXT NOT NULL,
        version TEXT NOT NULL UNIQUE,
        checksum TEXT NOT NULL,
        applied_at TEXT DEFAULT CURRENT_TIMESTAMP,
        execution_time INTEGER,
        status TEXT,
        changes TEXT,
        rollback_sql TEXT
    )".

%% @private 生成版本号
generate_version() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", 
                                  [Y, M, D, H, Min, S])).

%% @private 计算校验和
calculate_checksum(Data) ->
    Hash = crypto:hash(sha256, iolist_to_binary(Data)),
    base64:encode(Hash).

%% @private 格式化时间戳
format_timestamp(Seconds) ->
    {{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(
        Seconds + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    ),
    list_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                                  [Y, M, D, H, Min, S])).

%% @private 行转换为迁移记录
row_to_migration([Id, Model, Version, Checksum, AppliedAt, ExecTime, Status, Changes, Rollback]) ->
    #eorm_migration{
        id = Id,
        model = binary_to_atom(Model, utf8),
        version = Version,
        checksum = Checksum,
        applied_at = AppliedAt,
        execution_time = ExecTime,
        status = binary_to_atom(Status, utf8),
        changes = Changes,
        rollback_sql = Rollback
    }.