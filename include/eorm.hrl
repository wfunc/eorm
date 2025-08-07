%%% @doc EORM 公共头文件
%%% @end

-ifndef(EORM_HRL).
-define(EORM_HRL, true).

%% 查询记录
-record(eorm_query, {
    model :: module(),
    table :: atom(),
    select = [] :: list(),
    where = [] :: list(),
    order = [] :: list(),
    group = [] :: list(),
    having = [] :: list(),
    joins = [] :: list(),
    preload = [] :: list(),
    limit :: integer() | undefined,
    offset :: integer() | undefined,
    distinct = false :: boolean(),
    lock = false :: boolean() | write | read,
    
    %% 新增字段
    union = [] :: list(),
    hints = [] :: list(),
    optimized = false :: boolean(),
    cache_ttl :: integer() | undefined
}).

%% 模型元数据
-record(eorm_model, {
    module :: module(),
    table :: atom(),
    fields = [] :: list(),
    indexes = [] :: list(),
    associations = [] :: list(),
    constraints = [] :: list(),
    hooks = [] :: list(),
    options = [] :: list()
}).

%% 字段定义
-record(eorm_field, {
    name :: atom(),
    type :: atom() | tuple(),
    options = [] :: list(),
    default :: any(),
    nullable = true :: boolean(),
    primary_key = false :: boolean()
}).

%% 关联定义
-record(eorm_association, {
    name :: atom(),
    type :: has_one | has_many | belongs_to | many_to_many,
    target :: module(),
    foreign_key :: atom(),
    association_foreign_key :: atom(),
    join_table :: atom(),
    options = [] :: list()
}).

%% 迁移记录
-record(eorm_migration, {
    id :: integer(),
    model :: atom(),
    version :: binary(),
    checksum :: binary(),
    applied_at :: calendar:datetime(),
    execution_time :: integer(),
    status :: pending | success | failed | rolled_back,
    changes :: binary(),
    rollback_sql :: binary()
}).

%% 连接配置
-record(eorm_connection, {
    adapter :: atom(),
    host :: string(),
    port :: integer(),
    database :: string(),
    username :: string(),
    password :: string(),
    pool_size = 10 :: integer(),
    options = [] :: list()
}).

%% 常用宏
-define(LOG_DEBUG(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(LOG_ERROR(Format, Args), error_logger:error_msg(Format, Args)).

%% 默认值
-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_BATCH_SIZE, 1000).

-endif.