# EORM - Erlang ORM Framework

一个功能完整的 Erlang ORM 框架，提供类似 GORM 的自动迁移功能和流畅的 API。

## 核心特性

### 🚀 自动迁移 - 像 GORM 一样简单
- **零配置迁移**: 只需定义模型结构，系统自动创建和更新数据库表
- **智能 Schema 比较**: 自动检测模型定义与数据库结构的差异
- **安全保护**: 多种迁移模式（safe/force/dry_run），防止意外数据丢失
- **自动类型推导**: `id` 自动识别为主键，`*_id` 识别为外键，`*_at` 识别为时间戳

### 💡 主要功能
- **链式查询 API**: 流畅的查询构建接口
- **多数据库支持**: PostgreSQL、MySQL、SQLite
- **关联管理**: has_one、has_many、belongs_to、many_to_many
- **事务支持**: ACID 事务保证，支持嵌套事务
- **钩子系统**: 完整的生命周期钩子
- **批量操作**: 高效的批量插入、更新、删除
- **查询缓存**: 自动缓存优化性能

## 快速开始

### 1. 定义模型

```erlang
-module(user).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => users,        % 可选，默认使用模块名
        fields => [
            {id},              % 自动推导为 integer + primary_key + auto_increment
            {name, string},
            {email, string, [unique]},
            {age, integer, [default(18)]},
            {created_at, timestamp},
            {updated_at, timestamp, [auto_update]}
        ],
        indexes => [
            {idx_email, [email]}
        ]
    }.
```

### 2. 自动迁移

```erlang
%% 启动应用
application:start(eorm),

%% 注册模型
eorm:register_models([user, post, comment]),

%% 自动创建/更新表结构 - 就像 GORM 一样！
eorm:auto_migrate([user, post, comment]).
```

### 3. CRUD 操作

```erlang
%% 创建
{ok, User} = eorm:create(user, #{
    name => "Alice",
    email => "alice@example.com",
    age => 25
}),

%% 查询
{ok, Users} = eorm:find(user, #{age => {gt, 18}}),

%% 更新
eorm:update(user, #{id => 1}, #{age => 26}),

%% 删除
eorm:delete(user, #{id => 1}).
```

### 4. 链式查询

```erlang
%% 复杂查询
Results = eorm:new(user)
    |> eorm:where(#{age => {gte, 18}})
    |> eorm:order_by([{age, desc}])
    |> eorm:limit(10)
    |> eorm:preload([posts, comments])
    |> eorm_query:execute().
```

## 自动迁移详解

### 迁移模式

```erlang
%% Safe 模式 - 默认，拒绝危险操作
eorm:auto_migrate([user], #{mode => safe}),

%% Dry Run 模式 - 只显示将要执行的 SQL，不实际执行
eorm:auto_migrate([user], #{mode => dry_run}),

%% Force 模式 - 强制执行所有变更（谨慎使用）
eorm:auto_migrate([user], #{mode => force, allow_data_loss => true}).
```

### 智能类型推导

EORM 会自动推导字段类型和约束：

```erlang
{id}                    % → integer, primary_key, auto_increment
{user_id}               % → integer (外键)
{created_at}            % → timestamp
{is_active}             % → boolean, default(false)
timestamps()            % → created_at + updated_at
```

### Schema 比较和风险评估

```erlang
%% 生成迁移计划（不执行）
{ok, Plan} = eorm:migration_plan([user, post]),

%% Plan 包含：
%% - 需要创建的表
%% - 需要添加/修改/删除的列
%% - 风险评估（safe/warning/dangerous）
```

## 高级特性

### 关联查询

```erlang
%% 定义关联
schema() ->
    #{
        associations => [
            {has_many, posts, post},
            {belongs_to, category, category}
        ]
    }.

%% 预加载关联
User = eorm:new(user)
    |> eorm:preload([posts, {posts, comments}])
    |> eorm:first(#{id => 1}).
```

### 事务

```erlang
eorm:transaction(fun() ->
    {ok, User} = eorm:create(user, #{name => "Bob"}),
    {ok, Post} = eorm:create(post, #{
        user_id => User#{id},
        title => "My Post"
    }),
    {ok, User, Post}
end).
```

### 批量操作

```erlang
%% 批量插入
Users = [
    #{name => "User1", email => "user1@example.com"},
    #{name => "User2", email => "user2@example.com"}
],
eorm:insert_all(user, Users).
```

### 原生 SQL

```erlang
%% 当 ORM 不够用时
{ok, Results} = eorm:raw_query(
    "SELECT * FROM users WHERE age > $1",
    [18]
).
```

## 快速运行示例

### 自动运行脚本
```bash
# 一键运行示例（自动启动Docker、编译、运行）
./run_example.sh
```

### 手动运行
```bash
# 1. 启动PostgreSQL容器
docker run --name postgres-dev \
  -e POSTGRES_DB=eorm_test \
  -e POSTGRES_USER=dev \
  -e POSTGRES_PASSWORD=123 \
  -p 5432:5432 -d postgres

# 2. 启动Erlang Shell
rebar3 shell

# 3. 运行简单示例
simple_usage:demo().
```

## 完整示例

### 简单使用示例 (`examples/simple_usage.erl`)
展示基本的EORM操作：
- 数据库连接配置  
- 模型注册和自动迁移
- 基本CRUD API演示（当前为stub实现）
- 错误处理和异常处理

运行方式：
```bash
# 启动PostgreSQL容器
docker run --name postgres-dev -e POSTGRES_DB=eorm_test -e POSTGRES_USER=dev -e POSTGRES_PASSWORD=123 -p 5432:5432 -d postgres

# 编译并运行
rebar3 compile
cd examples && erlc -pa ../_build/default/lib/*/ebin -I ../include simple_usage.erl
cd .. && erl -pa examples -pa _build/default/lib/*/ebin -eval "simple_usage:demo()." -s init stop -noshell
```

### 博客模型示例 (`examples/blog_models.erl`)
完整的博客应用模型定义，包含：
- 用户、文章、评论、标签模型定义
- 复杂字段类型和约束  
- 动态模块创建和编译
- 模型关系设计最佳实践

运行方式：
```erlang
% 在Erlang shell中
blog_models:load_all_models().
% 然后可以使用eorm_registry:register_model/1注册模型
```

**注意**: 当前EORM框架主要完成了自动迁移功能，CRUD操作为开发中的stub实现。完整的数据库CRUD功能正在开发中。

## 项目结构

```
eorm/
├── src/
│   ├── eorm.erl                 # 主 API 接口
│   ├── eorm_auto_migrate.erl    # 自动迁移核心
│   ├── eorm_schema_diff.erl     # Schema 比较引擎
│   ├── eorm_ddl_generator.erl   # DDL 生成器
│   ├── eorm_query.erl           # 查询构建器
│   ├── eorm_registry.erl        # 模型注册表
│   └── ...
├── include/
│   └── eorm.hrl                 # 公共头文件
├── examples/
│   └── blog_app.erl             # 完整示例应用
└── test/
    └── eorm_test.erl            # 测试套件
```

## 安装

### 使用 rebar3

在 `rebar.config` 中添加：

```erlang
{deps, [
    {eorm, {git, "https://github.com/wfunc/eorm.git", {branch, "main"}}}
]}.
```

### 配置

在 `config/sys.config` 中：

```erlang
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
            }}
        ]}
    ]}
].
```

## 测试

```bash
# 运行所有测试
rebar3 test

# 运行特定测试
rebar3 eunit --module=eorm_test
```

## 性能优化

- **连接池**: 自动管理数据库连接池
- **查询缓存**: 基于查询指纹的智能缓存
- **批量操作**: 减少数据库往返
- **预编译语句**: 自动缓存常用查询

## 对比 GORM

| 特性 | EORM | GORM |
|-----|------|------|
| 自动迁移 | ✅ | ✅ |
| 链式 API | ✅ | ✅ |
| 关联管理 | ✅ | ✅ |
| 事务支持 | ✅ | ✅ |
| 钩子系统 | ✅ | ✅ |
| 多数据库 | ✅ | ✅ |
| 类型推导 | ✅ | ✅ |
| 安全模式 | ✅ | ✅ |

## 路线图

- [ ] 更多数据库适配器（MongoDB、Cassandra）
- [ ] 查询优化器
- [ ] 分片支持
- [ ] GraphQL 集成
- [ ] 实时变更订阅
- [ ] 迁移版本控制
- [ ] Web 管理界面

## 贡献

欢迎贡献代码！请查看 [CONTRIBUTING.md](CONTRIBUTING.md) 了解详情。

## 许可证

Apache License 2.0

## 致谢

灵感来自：
- [GORM](https://gorm.io) - Go 语言 ORM
- [Ecto](https://hexdocs.pm/ecto) - Elixir 数据库工具
- [ActiveRecord](https://guides.rubyonrails.org/active_record_basics.html) - Ruby on Rails ORM

---

**EORM - 让 Erlang 数据库操作像 GORM 一样简单！** 🚀