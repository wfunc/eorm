# EORM - Erlang ORM Framework Design

## 1. 架构概览

EORM 是一个功能完整的 Erlang ORM 框架，提供类似 GORM 的功能和 API 设计。

### 1.1 核心特性
- **链式 API**: 流畅的查询构建接口
- **自动迁移**: 基于模型定义自动管理数据库结构
- **关联管理**: 支持各种关联关系
- **事务支持**: ACID 事务保证
- **钩子系统**: 生命周期钩子
- **多数据库**: 支持多种数据库后端

### 1.2 系统架构

```
┌─────────────────────────────────────────────────┐
│                  用户应用层                      │
├─────────────────────────────────────────────────┤
│                  EORM API 层                     │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐       │
│  │  Model   │ │  Query   │ │Migration │       │
│  │  DSL     │ │ Builder  │ │  System  │       │
│  └──────────┘ └──────────┘ └──────────┘       │
├─────────────────────────────────────────────────┤
│                 核心服务层                       │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐       │
│  │  Hook    │ │  Cache   │ │Connection│       │
│  │ Manager  │ │ Manager  │ │   Pool   │       │
│  └──────────┘ └──────────┘ └──────────┘       │
├─────────────────────────────────────────────────┤
│                数据库适配器层                    │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐       │
│  │PostgreSQL│ │  MySQL   │ │  SQLite  │       │
│  └──────────┘ └──────────┘ └──────────┘       │
└─────────────────────────────────────────────────┘
```

## 2. 模型定义系统

### 2.1 模型定义 DSL

```erlang
-module(user_model).
-behaviour(eorm_model).

-export([definition/0]).

definition() ->
    #{
        table => users,
        fields => [
            {id, integer, [primary_key, auto_increment]},
            {name, string, [not_null]},
            {email, string, [unique, not_null]},
            {age, integer, [default, 0]},
            {created_at, datetime, [auto_now_add]},
            {updated_at, datetime, [auto_now]}
        ],
        indexes => [
            {idx_email, [email]},
            {idx_name_age, [name, age]}
        ],
        associations => [
            {has_many, posts, post_model, #{foreign_key => user_id}},
            {has_one, profile, profile_model, #{foreign_key => user_id}},
            {many_to_many, tags, tag_model, #{
                join_table => user_tags,
                foreign_key => user_id,
                association_foreign_key => tag_id
            }}
        ],
        hooks => [
            {before_create, fun validate_email/1},
            {after_create, fun send_welcome_email/1},
            {before_update, fun update_timestamp/1}
        ]
    }.
```

### 2.2 字段类型映射

```erlang
%% EORM 类型 -> 数据库类型映射
-define(TYPE_MAPPINGS, #{
    integer => "INTEGER",
    string => "VARCHAR(255)",
    text => "TEXT",
    boolean => "BOOLEAN",
    float => "FLOAT",
    decimal => "DECIMAL",
    datetime => "TIMESTAMP",
    date => "DATE",
    time => "TIME",
    binary => "BLOB",
    json => "JSON",
    uuid => "UUID"
}).
```

## 3. 查询构建器 API

### 3.1 基础查询操作

```erlang
%% 创建
User = eorm:create(user_model, #{
    name => "John Doe",
    email => "john@example.com",
    age => 25
}).

%% 查询单条
User = eorm:first(user_model, #{email => "john@example.com"}).

%% 查询多条
Users = eorm:find(user_model, #{age => {gt, 18}}).

%% 更新
eorm:update(user_model, #{id => 1}, #{age => 26}).

%% 删除
eorm:delete(user_model, #{id => 1}).
```

### 3.2 链式查询 API

```erlang
%% 复杂查询示例
Results = eorm:new(user_model)
    |> eorm:where(#{age => {gte, 18}})
    |> eorm:where(#{status => active})
    |> eorm:order_by([{age, desc}, {name, asc}])
    |> eorm:limit(10)
    |> eorm:offset(20)
    |> eorm:preload([posts, profile])
    |> eorm:find_all().

%% 聚合查询
Count = eorm:new(user_model)
    |> eorm:where(#{age => {gt, 18}})
    |> eorm:count().

%% 分组查询
Stats = eorm:new(user_model)
    |> eorm:group_by([age])
    |> eorm:having(#{age => {gt, 18}})
    |> eorm:select([age, {count, id, as, user_count}])
    |> eorm:find_all().
```

### 3.3 关联查询

```erlang
%% 预加载关联
User = eorm:new(user_model)
    |> eorm:preload([posts, {posts, comments}])
    |> eorm:first(#{id => 1}).

%% 关联条件查询
Users = eorm:new(user_model)
    |> eorm:joins(posts)
    |> eorm:where(posts, #{status => published})
    |> eorm:find_all().
```

## 4. 数据库适配器接口

### 4.1 适配器行为定义

```erlang
-module(eorm_adapter).

-callback init(Config :: map()) -> {ok, State :: term()} | {error, Reason :: term()}.

-callback execute(State :: term(), Query :: binary(), Params :: list()) -> 
    {ok, Result :: term()} | {error, Reason :: term()}.

-callback transaction(State :: term(), Fun :: fun()) -> 
    {ok, Result :: term()} | {error, Reason :: term()}.

-callback migrate(State :: term(), Migrations :: list()) -> 
    ok | {error, Reason :: term()}.

-callback close(State :: term()) -> ok.
```

### 4.2 PostgreSQL 适配器示例

```erlang
-module(eorm_postgres_adapter).
-behaviour(eorm_adapter).

-export([init/1, execute/3, transaction/2, migrate/2, close/1]).

init(Config) ->
    {ok, Conn} = epgsql:connect(Config),
    {ok, #{conn => Conn}}.

execute(#{conn := Conn}, Query, Params) ->
    case epgsql:equery(Conn, Query, Params) of
        {ok, _Columns, Rows} -> {ok, Rows};
        {ok, Count} -> {ok, Count};
        {error, Error} -> {error, Error}
    end.

transaction(#{conn := Conn}, Fun) ->
    epgsql:with_transaction(Conn, Fun).

migrate(State, Migrations) ->
    %% 实现迁移逻辑
    ok.

close(#{conn := Conn}) ->
    epgsql:close(Conn).
```

## 5. 事务支持

```erlang
%% 事务 API
eorm:transaction(fun() ->
    User = eorm:create(user_model, #{name => "John"}),
    Profile = eorm:create(profile_model, #{
        user_id => User#{id},
        bio => "Software Developer"
    }),
    {ok, User, Profile}
end).

%% 嵌套事务
eorm:transaction(fun() ->
    User = eorm:create(user_model, #{name => "Jane"}),
    
    eorm:transaction(fun() ->
        %% 嵌套事务中的操作
        eorm:create(post_model, #{
            user_id => User#{id},
            title => "First Post"
        })
    end),
    
    User
end).
```

## 6. 钩子系统

```erlang
%% 钩子定义
-module(user_hooks).

-export([before_create/1, after_create/1, before_update/2]).

before_create(User) ->
    %% 验证邮箱格式
    case validate_email(maps:get(email, User)) of
        true -> {ok, User};
        false -> {error, invalid_email}
    end.

after_create(User) ->
    %% 发送欢迎邮件（异步）
    spawn(fun() -> send_welcome_email(User) end),
    {ok, User}.

before_update(OldUser, NewUser) ->
    %% 记录变更
    log_changes(OldUser, NewUser),
    {ok, NewUser}.
```

## 7. 迁移系统

```erlang
%% 迁移文件示例
-module(migration_20240101_create_users).
-behaviour(eorm_migration).

-export([up/0, down/0]).

up() ->
    eorm_migration:create_table(users, [
        {id, integer, [primary_key, auto_increment]},
        {name, string, [not_null]},
        {email, string, [unique, not_null]},
        {created_at, datetime}
    ]),
    
    eorm_migration:add_index(users, idx_email, [email]).

down() ->
    eorm_migration:drop_table(users).
```

## 8. 配置管理

```erlang
%% config/sys.config
[
    {eorm, [
        {default_adapter, postgres},
        {databases, [
            {postgres, #{
                adapter => eorm_postgres_adapter,
                host => "localhost",
                port => 5432,
                database => "myapp_dev",
                username => "postgres",
                password => "postgres",
                pool_size => 10
            }},
            {mysql, #{
                adapter => eorm_mysql_adapter,
                host => "localhost",
                port => 3306,
                database => "myapp_dev",
                username => "root",
                password => "root",
                pool_size => 5
            }}
        ]},
        {cache, #{
            enabled => true,
            ttl => 3600
        }},
        {log_queries => true}
    ]}
].
```

## 9. 使用示例

### 9.1 完整的 CRUD 示例

```erlang
-module(blog_app).
-export([demo/0]).

demo() ->
    %% 创建用户
    {ok, User} = eorm:create(user_model, #{
        name => "Alice",
        email => "alice@example.com",
        age => 28
    }),
    
    %% 创建文章
    {ok, Post} = eorm:create(post_model, #{
        user_id => maps:get(id, User),
        title => "My First Post",
        content => "Hello, World!",
        status => draft
    }),
    
    %% 查询用户的所有文章
    UserWithPosts = eorm:new(user_model)
        |> eorm:preload([posts])
        |> eorm:first(#{id => maps:get(id, User)}),
    
    %% 更新文章状态
    eorm:update(post_model, 
        #{id => maps:get(id, Post)}, 
        #{status => published}),
    
    %% 复杂查询
    ActiveUsers = eorm:new(user_model)
        |> eorm:joins(posts)
        |> eorm:where(posts, #{status => published})
        |> eorm:group_by([id])
        |> eorm:having({count, posts.id}, {gte, 5})
        |> eorm:order_by([{created_at, desc}])
        |> eorm:find_all(),
    
    %% 事务操作
    eorm:transaction(fun() ->
        eorm:update(user_model, #{id => 1}, #{credits => {inc, 10}}),
        eorm:create(transaction_model, #{
            user_id => 1,
            amount => 10,
            type => credit
        })
    end).
```

### 9.2 高级特性示例

```erlang
%% 批量插入
Users = [
    #{name => "User1", email => "user1@example.com"},
    #{name => "User2", email => "user2@example.com"},
    #{name => "User3", email => "user3@example.com"}
],
eorm:insert_all(user_model, Users).

%% 原生 SQL 查询
Results = eorm:raw_query(
    "SELECT * FROM users WHERE age > $1 AND status = $2",
    [18, active]
).

%% 软删除
eorm:soft_delete(user_model, #{id => 1}).

%% 查询作用域
PublishedPosts = eorm:new(post_model)
    |> eorm:apply_scope(published)  %% 预定义的查询条件
    |> eorm:find_all().
```

## 10. 性能优化

### 10.1 查询缓存
- 自动缓存频繁查询
- 基于查询指纹的缓存键
- TTL 和 LRU 淘汰策略

### 10.2 连接池管理
- 每个数据库配置独立连接池
- 动态调整池大小
- 连接健康检查

### 10.3 预编译语句
- 自动识别和缓存常用查询
- 参数化查询防止 SQL 注入

## 11. 测试支持

```erlang
%% 测试助手
-module(eorm_test).

%% 事务回滚测试
test_with_rollback(Fun) ->
    eorm:transaction(fun() ->
        Result = Fun(),
        throw(rollback_test),
        Result
    end).

%% 测试数据工厂
create_test_user(Attrs) ->
    DefaultAttrs = #{
        name => "Test User",
        email => "test@example.com",
        age => 25
    },
    eorm:create(user_model, maps:merge(DefaultAttrs, Attrs)).
```