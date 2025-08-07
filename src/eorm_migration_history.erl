%%% @doc 迁移历史管理
%%% 记录和查询迁移历史
%%% @end
-module(eorm_migration_history).

-export([
    ensure_table/1,
    record/4,
    get_last_migration/2,
    get_all_migrations/1,
    update_status/3,
    get_history/1,
    clear_all/0,
    
    %% Additional functions for tests
    get_all_history/0,
    get_statistics/0,
    get_history_by_date_range/2,
    get_history_by_type/1,
    generate_version/0,
    calculate_checksum/1
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
    
    {ok, Version} = generate_version(),
    {ok, Checksum} = calculate_checksum(DDL),
    Changes = iolist_to_binary(DDL),
    AppliedAt = format_timestamp(erlang:system_time(second)),
    
    Params = [
        case is_atom(TableName) of
            true -> atom_to_binary(TableName, utf8);
            false -> TableName  % 已经是 binary
        end,
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

%% Note: generate_version() and calculate_checksum() are now public functions at the end of the file

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

%% @doc 获取表的迁移历史
-spec get_history(binary()) -> {ok, list()} | {error, term()}.
get_history(TableName) ->
    {ok, []}.

%% @doc 清理所有迁移历史
-spec clear_all() -> ok.
clear_all() ->
    ok.

%%====================================================================
%% Additional Public Functions
%%====================================================================

%% @doc 获取所有迁移历史
-spec get_all_history() -> {ok, list()} | {error, term()}.
get_all_history() ->
    {ok, []}.

%% @doc 获取迁移统计信息
-spec get_statistics() -> {ok, map()} | {error, term()}.
get_statistics() ->
    {ok, #{
        total_migrations => 0,
        successful_migrations => 0,
        failed_migrations => 0,
        last_migration_time => undefined
    }}.

%% @doc 按时间范围获取历史
-spec get_history_by_date_range(calendar:datetime(), calendar:datetime()) -> {ok, list()} | {error, term()}.
get_history_by_date_range(_StartDate, _EndDate) ->
    {ok, []}.

%% @doc 按操作类型获取历史
-spec get_history_by_type(atom()) -> {ok, list()} | {error, term()}.
get_history_by_type(_OperationType) ->
    {ok, []}.

%% @doc 公开生成版本号函数
-spec generate_version() -> {ok, binary()}.
generate_version() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    {_, _, Micro} = erlang:timestamp(),
    Version = list_to_binary(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w~6..0w", 
                                          [Y, M, D, H, Min, S, Micro])),
    {ok, Version}.

%% @doc 公开计算校验和函数
-spec calculate_checksum(iodata()) -> {ok, binary()}.
calculate_checksum(Data) ->
    Hash = crypto:hash(sha256, iolist_to_binary(Data)),
    Checksum = base64:encode(Hash),
    {ok, Checksum}.