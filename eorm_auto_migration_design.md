# EORM 自动迁移系统设计

## 核心理念
像 GORM 一样，通过模型定义自动管理数据库表结构，实现"代码即架构"。

## 1. 自动迁移架构

```
┌──────────────────────────────────────────────────────┐
│                    用户模型定义                        │
│                 (Erlang Records/Maps)                 │
└────────────────────────┬─────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────┐
│               模型元数据解析器                         │
│          (Model Metadata Parser)                      │
│   • 解析字段定义                                      │
│   • 提取约束信息                                      │
│   • 识别关联关系                                      │
└────────────────────────┬─────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────┐
│                Schema 比较引擎                        │
│            (Schema Diff Engine)                       │
│   • 获取当前数据库 Schema                             │
│   • 比较模型定义与数据库结构                          │
│   • 生成变更计划                                      │
└────────────────────────┬─────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────┐
│                 DDL 生成器                            │
│              (DDL Generator)                          │
│   • 生成 CREATE TABLE                                 │
│   • 生成 ALTER TABLE                                  │
│   • 生成索引和约束                                    │
└────────────────────────┬─────────────────────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────┐
│                迁移执行器                              │
│           (Migration Executor)                        │
│   • 事务管理                                          │
│   • 执行 DDL                                          │
│   • 记录迁移历史                                      │
│   • 错误回滚                                          │
└──────────────────────────────────────────────────────┘
```

## 2. 模型定义增强

### 2.1 简化的模型定义方式

```erlang
-module(user).
-behaviour(eorm_model).

-export([schema/0]).

%% 简洁的 DSL 定义
schema() ->
    #{
        %% 表名自动从模块名推导（可选覆盖）
        table => users,
        
        %% 字段定义 - 简洁语法
        fields => [
            {id},                                    % 自动推导为 integer + primary_key
            {name, string},                          % 简单类型
            {email, string, [unique]},               % 带约束
            {age, integer, [default(18)]},           % 带默认值
            {balance, decimal(10, 2)},               % 精确类型
            {status, enum([active, inactive, banned])}, % 枚举类型
            {metadata, json},                        % JSON 字段
            {avatar, binary},                        % 二进制数据
            {created_at, timestamp},                 % 自动时间戳
            {updated_at, timestamp, [auto_update]}   % 自动更新
        ],
        
        %% 复合索引
        indexes => [
            {idx_email_status, [email, status]},
            {idx_created, [created_at], [desc]}
        ],
        
        %% 表级选项
        options => [
            {engine, innodb},                       % MySQL 引擎
            {charset, utf8mb4},                     % 字符集
            {comment, "用户基础信息表"}
        ]
    }.
```

### 2.2 关联模型定义

```erlang
-module(post).
-behaviour(eorm_model).

schema() ->
    #{
        fields => [
            {id},
            {title, string(200)},
            {content, text},
            {user_id, integer},                     % 外键字段
            {category_id, integer},
            {published_at, datetime},
            {view_count, integer, [default(0)]},
            timestamps()                             % 自动添加 created_at, updated_at
        ],
        
        %% 关联关系定义
        associations => [
            {belongs_to, user, [{foreign_key, user_id}]},
            {belongs_to, category},                 % 自动推导 foreign_key
            {has_many, comments},
            {has_many, tags, [{through, post_tags}]}
        ],
        
        %% 外键约束
        constraints => [
            {foreign_key, user_id, users, id, [on_delete(cascade)]},
            {foreign_key, category_id, categories, id, [on_delete(set_null)]},
            {check, "view_count >= 0"}
        ]
    }.
```

## 3. 自动迁移 API

### 3.1 基础自动迁移

```erlang
%% 应用启动时自动迁移
-module(myapp).
-export([start/0]).

start() ->
    %% 自动迁移所有已注册的模型
    eorm:auto_migrate([
        user,
        post,
        comment,
        category
    ]).
    
%% 或者使用配置
%% config/sys.config
{eorm, [
    {auto_migrate, true},
    {models, [user, post, comment, category]},
    {migrate_on_start, true}
]}
```

### 3.2 迁移选项控制

```erlang
%% 详细的迁移控制
eorm:auto_migrate(Models, #{
    %% 迁移模式
    mode => safe,        % safe | force | dry_run
    
    %% 字段处理策略
    drop_columns => false,      % 是否删除多余字段
    drop_indexes => false,       % 是否删除多余索引
    
    %% 数据保护
    backup_before => true,       % 迁移前备份
    allow_data_loss => false,    % 是否允许数据丢失操作
    
    %% 执行控制
    batch_size => 1000,          % 批量操作大小
    timeout => 60000,            % 超时设置
    
    %% 回调
    before_migrate => fun(Model, Changes) -> 
        io:format("Migrating ~p: ~p~n", [Model, Changes])
    end,
    
    after_migrate => fun(Model, Result) ->
        log_migration(Model, Result)
    end
}).
```

## 4. Schema 比较引擎

### 4.1 Schema 获取器

```erlang
-module(eorm_schema_inspector).
-export([get_table_schema/2, compare_schemas/2]).

%% 获取数据库中的表结构
get_table_schema(Adapter, TableName) ->
    #{
        columns => get_columns(Adapter, TableName),
        indexes => get_indexes(Adapter, TableName),
        constraints => get_constraints(Adapter, TableName),
        options => get_table_options(Adapter, TableName)
    }.

%% 比较模型定义和数据库 Schema
compare_schemas(ModelSchema, DbSchema) ->
    #{
        to_add => #{
            columns => find_missing_columns(ModelSchema, DbSchema),
            indexes => find_missing_indexes(ModelSchema, DbSchema),
            constraints => find_missing_constraints(ModelSchema, DbSchema)
        },
        to_modify => #{
            columns => find_modified_columns(ModelSchema, DbSchema)
        },
        to_drop => #{
            columns => find_extra_columns(ModelSchema, DbSchema),
            indexes => find_extra_indexes(ModelSchema, DbSchema)
        }
    }.
```

### 4.2 智能变更检测

```erlang
-module(eorm_schema_diff).
-export([analyze_changes/3]).

%% 智能分析变更影响
analyze_changes(Model, CurrentSchema, NewSchema) ->
    Changes = compare_schemas(NewSchema, CurrentSchema),
    
    %% 分析每个变更的风险级别
    RiskAnalysis = #{
        safe => [],           % 无风险变更
        warning => [],        % 需要注意的变更
        dangerous => []       % 危险变更（可能丢失数据）
    },
    
    %% 分类变更
    lists:foreach(fun({Type, Change}) ->
        Risk = assess_risk(Type, Change),
        add_to_risk_category(RiskAnalysis, Risk, {Type, Change})
    end, flatten_changes(Changes)),
    
    %% 生成迁移计划
    #{
        changes => Changes,
        risk_analysis => RiskAnalysis,
        migration_plan => generate_migration_plan(Changes, RiskAnalysis),
        estimated_time => estimate_migration_time(Changes),
        requires_downtime => requires_downtime(Changes)
    }.
```

## 5. DDL 生成器

### 5.1 多数据库 DDL 生成

```erlang
-module(eorm_ddl_generator).
-export([generate_create_table/3, generate_alter_table/3]).

%% 生成 CREATE TABLE 语句
generate_create_table(Adapter, TableName, Schema) ->
    case Adapter of
        postgres -> generate_postgres_create(TableName, Schema);
        mysql -> generate_mysql_create(TableName, Schema);
        sqlite -> generate_sqlite_create(TableName, Schema)
    end.

%% PostgreSQL CREATE TABLE
generate_postgres_create(TableName, Schema) ->
    Columns = generate_column_definitions(postgres, Schema),
    Constraints = generate_constraints(postgres, Schema),
    
    [
        "CREATE TABLE IF NOT EXISTS ", atom_to_list(TableName), " (\n",
        string:join(Columns ++ Constraints, ",\n"),
        "\n)"
    ].

%% 生成 ALTER TABLE 语句
generate_alter_table(Adapter, TableName, Changes) ->
    lists:flatmap(fun(Change) ->
        generate_alter_statement(Adapter, TableName, Change)
    end, Changes).
```

### 5.2 类型映射系统

```erlang
-module(eorm_type_mapper).
-export([map_type/3]).

%% 智能类型映射
map_type(postgres, Type, Options) ->
    case Type of
        {string, Length} -> io_lib:format("VARCHAR(~p)", [Length]);
        string -> "VARCHAR(255)";
        text -> "TEXT";
        integer -> "INTEGER";
        {decimal, P, S} -> io_lib:format("DECIMAL(~p,~p)", [P, S]);
        boolean -> "BOOLEAN";
        datetime -> "TIMESTAMP";
        json -> "JSONB";
        {enum, Values} -> generate_enum_type(postgres, Values);
        _ -> error({unsupported_type, Type})
    end;

map_type(mysql, Type, Options) ->
    case Type of
        {string, Length} -> io_lib:format("VARCHAR(~p)", [Length]);
        text -> "TEXT";
        integer -> if_primary_key(Options, "INT AUTO_INCREMENT", "INT");
        boolean -> "TINYINT(1)";
        datetime -> "DATETIME";
        json -> "JSON";
        {enum, Values} -> generate_enum_type(mysql, Values);
        _ -> error({unsupported_type, Type})
    end.
```

## 6. 迁移执行器

### 6.1 安全迁移执行

```erlang
-module(eorm_migration_executor).
-export([execute_migration/4]).

%% 执行迁移计划
execute_migration(Adapter, MigrationPlan, Options, Callbacks) ->
    %% 开启事务
    transaction(Adapter, fun() ->
        %% 迁移前回调
        run_callback(Callbacks, before_migrate, MigrationPlan),
        
        %% 备份（如果需要）
        maybe_backup(Options, Adapter),
        
        %% 执行迁移步骤
        Results = lists:map(fun(Step) ->
            execute_migration_step(Adapter, Step, Options)
        end, MigrationPlan),
        
        %% 验证迁移结果
        validate_migration(Adapter, Results),
        
        %% 记录迁移历史
        record_migration_history(Adapter, MigrationPlan, Results),
        
        %% 迁移后回调
        run_callback(Callbacks, after_migrate, Results),
        
        {ok, Results}
    end).

%% 执行单个迁移步骤
execute_migration_step(Adapter, Step, Options) ->
    try
        SQL = Step#migration_step.sql,
        Result = execute_ddl(Adapter, SQL),
        {ok, Step, Result}
    catch
        Type:Error ->
            handle_migration_error(Type, Error, Step, Options)
    end.
```

### 6.2 迁移历史管理

```erlang
-module(eorm_migration_history).
-export([init_history_table/1, record_migration/3]).

%% 初始化迁移历史表
init_history_table(Adapter) ->
    SQL = "
        CREATE TABLE IF NOT EXISTS eorm_migrations (
            id SERIAL PRIMARY KEY,
            model VARCHAR(255) NOT NULL,
            version VARCHAR(255) NOT NULL,
            checksum VARCHAR(64) NOT NULL,
            applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            execution_time INTEGER,
            status VARCHAR(20),
            changes TEXT,
            rollback_sql TEXT
        )
    ",
    execute_ddl(Adapter, SQL).

%% 记录迁移
record_migration(Adapter, Model, MigrationData) ->
    insert(Adapter, eorm_migrations, #{
        model => Model,
        version => generate_version(),
        checksum => calculate_checksum(Model),
        execution_time => MigrationData#migration.execution_time,
        status => success,
        changes => encode_changes(MigrationData#migration.changes),
        rollback_sql => generate_rollback_sql(MigrationData)
    }).
```

## 7. 回滚支持

### 7.1 自动回滚生成

```erlang
-module(eorm_rollback).
-export([generate_rollback/2, execute_rollback/2]).

%% 为每个迁移生成回滚 SQL
generate_rollback(MigrationPlan) ->
    lists:map(fun(Step) ->
        case Step#migration_step.type of
            create_table -> 
                {drop_table, Step#migration_step.table};
            add_column -> 
                {drop_column, Step#migration_step.table, Step#migration_step.column};
            modify_column -> 
                {modify_column, Step#migration_step.table, 
                 Step#migration_step.column, 
                 Step#migration_step.old_definition};
            add_index -> 
                {drop_index, Step#migration_step.index_name};
            _ -> 
                {custom, Step#migration_step.rollback_sql}
        end
    end, MigrationPlan).

%% 执行回滚
execute_rollback(Adapter, Version) ->
    %% 获取迁移历史
    Migration = get_migration(Adapter, Version),
    
    %% 解析回滚 SQL
    RollbackSteps = decode_rollback_sql(Migration#migration.rollback_sql),
    
    %% 执行回滚
    transaction(Adapter, fun() ->
        lists:foreach(fun(Step) ->
            execute_ddl(Adapter, Step)
        end, RollbackSteps),
        
        %% 更新迁移历史
        update_migration_status(Adapter, Version, rolled_back)
    end).
```

## 8. 使用示例

### 8.1 基础使用

```erlang
%% 定义模型
-module(product).
-behaviour(eorm_model).

schema() ->
    #{
        fields => [
            {id},
            {name, string(100), [not_null]},
            {price, decimal(10, 2), [not_null]},
            {stock, integer, [default(0)]},
            {description, text},
            {category_id, integer},
            {is_active, boolean, [default(true)]},
            timestamps()
        ],
        
        indexes => [
            {idx_name, [name]},
            {idx_category_active, [category_id, is_active]}
        ]
    }.

%% 应用启动时
start() ->
    %% 自动创建/更新表
    eorm:auto_migrate([product]).
```

### 8.2 高级迁移控制

```erlang
%% 预览迁移计划
{ok, Plan} = eorm:migration_plan([user, product]),
io:format("Migration plan: ~p~n", [Plan]),

%% Dry run 模式
eorm:auto_migrate([user, product], #{mode => dry_run}),

%% 强制迁移（危险操作）
eorm:auto_migrate([user, product], #{
    mode => force,
    drop_columns => true,
    allow_data_loss => true,
    backup_before => true
}),

%% 分步迁移
eorm:auto_migrate([user], #{mode => safe}),
eorm:auto_migrate([product], #{mode => safe}),

%% 回滚到特定版本
eorm:rollback(user, "20240101120000").
```

## 9. 安全机制

### 9.1 迁移保护

```erlang
-module(eorm_migration_guard).
-export([check_safety/2]).

check_safety(MigrationPlan, Options) ->
    Checks = [
        {data_loss, check_data_loss(MigrationPlan)},
        {index_impact, check_index_impact(MigrationPlan)},
        {foreign_keys, check_foreign_keys(MigrationPlan)},
        {table_locks, estimate_lock_time(MigrationPlan)},
        {disk_space, check_disk_space(MigrationPlan)}
    ],
    
    case Options of
        #{mode := force} -> 
            {ok, Checks};
        _ ->
            case lists:any(fun({_, Risk}) -> Risk =:= high end, Checks) of
                true -> {error, {unsafe_migration, Checks}};
                false -> {ok, Checks}
            end
    end.
```

### 9.2 并发迁移锁

```erlang
-module(eorm_migration_lock).
-export([acquire_lock/1, release_lock/1]).

%% 获取迁移锁，防止并发迁移
acquire_lock(Model) ->
    global:set_lock({eorm_migration, Model}, [node()], 1).

release_lock(Model) ->
    global:del_lock({eorm_migration, Model}).
```

## 10. 监控和日志

```erlang
-module(eorm_migration_monitor).
-export([monitor_migration/2]).

monitor_migration(MigrationPlan, Options) ->
    %% 开始监控
    StartTime = erlang:system_time(millisecond),
    
    %% 注册监控进程
    MonitorPid = spawn(fun() -> 
        migration_monitor_loop(MigrationPlan, StartTime)
    end),
    
    %% 执行迁移
    Result = eorm_migration_executor:execute_migration(
        adapter(), MigrationPlan, Options, callbacks()
    ),
    
    %% 停止监控
    MonitorPid ! stop,
    
    %% 生成报告
    generate_migration_report(Result, StartTime).

migration_monitor_loop(Plan, StartTime) ->
    receive
        stop -> ok
    after 1000 ->
        %% 每秒报告进度
        Progress = calculate_progress(Plan, StartTime),
        log_progress(Progress),
        migration_monitor_loop(Plan, StartTime)
    end.
```

这个设计实现了像 GORM 一样的自动迁移功能，核心特性包括：

1. **零配置迁移** - 只需定义模型，系统自动处理表创建和更新
2. **智能 Schema 比较** - 自动检测模型和数据库的差异
3. **安全保护** - 多种安全模式，防止意外数据丢失
4. **回滚支持** - 自动生成回滚脚本
5. **多数据库支持** - 适配不同数据库的 DDL 语法
6. **迁移历史** - 完整的迁移记录和版本管理
7. **并发控制** - 防止多实例同时迁移
8. **监控和日志** - 实时迁移进度和详细日志