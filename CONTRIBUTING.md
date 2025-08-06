# Contributing to EORM

感谢您对 EORM 项目的关注！我们欢迎各种形式的贡献，包括但不限于：

- 🐛 Bug 报告
- 💡 功能建议
- 📖 文档改进
- 🔧 代码贡献
- 🧪 测试用例
- 🌍 国际化

## 开发环境设置

### 前置要求

- Erlang/OTP 24 或更高版本
- Rebar3
- PostgreSQL 12+ (用于测试)
- Git

### 克隆仓库

```bash
git clone https://github.com/yourusername/eorm.git
cd eorm
```

### 安装依赖

```bash
rebar3 get-deps
rebar3 compile
```

### 运行测试

```bash
# 运行所有测试
rebar3 eunit

# 运行特定测试
rebar3 eunit --module=eorm_auto_migrate_test

# 运行覆盖率测试
rebar3 eunit --cover
rebar3 cover --verbose
```

## 贡献流程

### 1. 创建 Issue

在开始工作之前，请先创建或查找相关的 Issue：

- **Bug 报告**: 使用 Bug 报告模板
- **功能请求**: 使用功能请求模板
- **讨论**: 对于较大的变更，先开启讨论

### 2. Fork 和分支

1. Fork 本仓库
2. 创建功能分支：

```bash
git checkout -b feature/your-feature-name
# 或
git checkout -b fix/issue-number
```

### 3. 开发

#### 代码风格

- 使用 4 个空格缩进
- 函数名使用 snake_case
- 模块名使用 lowercase
- 变量名首字母大写（CamelCase）
- 每行不超过 120 个字符
- 添加适当的注释和文档

#### 提交规范

我们使用语义化提交信息：

```
<type>(<scope>): <subject>

<body>

<footer>
```

**Type 类型**:
- `feat`: 新功能
- `fix`: Bug 修复
- `docs`: 文档更新
- `style`: 代码格式调整
- `refactor`: 代码重构
- `perf`: 性能优化
- `test`: 测试相关
- `chore`: 构建过程或辅助工具的变动

**示例**:

```bash
feat(migration): add support for MySQL DDL generation

- Implement MySQL-specific type mapping
- Add MySQL constraint generation
- Update tests for MySQL adapter

Closes #123
```

### 4. 测试

确保您的代码通过所有测试：

```bash
# 运行测试
rebar3 eunit

# 检查代码风格
rebar3 dialyzer

# 运行性能测试（如果有）
rebar3 ct
```

### 5. 提交 Pull Request

1. 推送到您的 Fork：

```bash
git push origin feature/your-feature-name
```

2. 创建 Pull Request：
   - 填写 PR 模板
   - 关联相关 Issue
   - 等待 CI 通过

## 开发指南

### 项目结构

```
eorm/
├── src/                    # 源代码
│   ├── eorm.erl           # 主 API
│   ├── eorm_*.erl         # 核心模块
│   └── adapters/          # 数据库适配器
├── include/               # 头文件
├── test/                  # 测试文件
├── examples/              # 示例代码
└── docs/                  # 文档
```

### 核心模块说明

- **eorm.erl**: 主要 API 接口
- **eorm_auto_migrate.erl**: 自动迁移核心
- **eorm_schema_diff.erl**: Schema 比较引擎
- **eorm_ddl_generator.erl**: DDL 生成器
- **eorm_query.erl**: 查询构建器
- **eorm_registry.erl**: 模型注册表

### 添加新功能

1. **新增数据库适配器**:

```erlang
%% src/adapters/eorm_mysql_adapter.erl
-module(eorm_mysql_adapter).
-behaviour(eorm_adapter).

-export([connect/1, execute/2, ...]).

%% 实现适配器接口
```

2. **新增字段类型**:

```erlang
%% 在 eorm_ddl_generator.erl 中添加
postgres_type(your_type, _Options) ->
    "YOUR_SQL_TYPE";
```

3. **新增迁移功能**:

```erlang
%% 在 eorm_auto_migrate.erl 中扩展
handle_migration_option(your_option, Value, State) ->
    %% 处理新选项
    {ok, State}.
```

## 测试指南

### 单元测试

```erlang
%% test/your_module_test.erl
-module(your_module_test).
-include_lib("eunit/include/eunit.hrl").

your_feature_test() ->
    %% 测试代码
    ?assertEqual(Expected, Actual).
```

### 集成测试

使用 Docker 进行数据库测试：

```bash
# 启动测试数据库
docker run -d -p 5432:5432 \
  -e POSTGRES_USER=test \
  -e POSTGRES_PASSWORD=test \
  postgres:13

# 运行集成测试
./test_docker_only.sh
```

## 文档贡献

### API 文档

使用 EDoc 格式：

```erlang
%% @doc 函数说明
%% @spec function_name(Type1, Type2) -> ReturnType
%% @end
function_name(Arg1, Arg2) ->
    %% 实现
    ok.
```

### README 更新

- 保持示例代码可运行
- 更新功能列表
- 添加新的使用场景

## 发布流程

### 版本号规范

遵循语义化版本 2.0.0：

- **主版本号**: 不兼容的 API 修改
- **次版本号**: 向下兼容的功能性新增
- **修订号**: 向下兼容的问题修正

### 发布检查清单

- [ ] 所有测试通过
- [ ] 更新 CHANGELOG.md
- [ ] 更新版本号
- [ ] 更新文档
- [ ] 创建 Git 标签
- [ ] 发布到 Hex.pm

## 社区准则

### 行为准则

- 尊重所有贡献者
- 建设性的批评和讨论
- 帮助新手入门
- 保持专业和友好

### 获取帮助

- 📧 Email: your-email@example.com
- 💬 Discussions: [GitHub Discussions](https://github.com/yourusername/eorm/discussions)
- 🐛 Issues: [GitHub Issues](https://github.com/yourusername/eorm/issues)

## 贡献者

感谢所有贡献者！

<!-- ALL-CONTRIBUTORS-LIST:START -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

## 许可证

通过贡献代码，您同意您的贡献将按照 [MIT 许可证](LICENSE) 进行许可。

---

**感谢您的贡献！** 🎉