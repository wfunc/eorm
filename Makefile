# EORM Makefile

.PHONY: all compile test clean docker-test eunit ct shell docs

# 默认目标
all: compile

# 编译项目
compile:
	@echo "编译 EORM..."
	@rebar3 compile

# 运行所有测试
test: eunit ct docker-test

# 运行 EUnit 单元测试
eunit:
	@echo "运行 EUnit 测试..."
	@rebar3 eunit

# 运行 Common Test
ct:
	@echo "运行 Common Test..."
	@rebar3 ct

# 运行 Docker PostgreSQL 测试
docker-test:
	@echo "运行 Docker PostgreSQL 集成测试..."
	@./test_docker_only.sh

# 快速测试（只运行单元测试）
quick-test:
	@rebar3 eunit --module=eorm_auto_migrate_test

# 启动 Erlang Shell
shell:
	@rebar3 shell

# 清理编译文件
clean:
	@echo "清理..."
	@rebar3 clean
	@rm -rf _build
	@rm -f rebar.lock

# 格式化代码
format:
	@rebar3 format

# 类型检查
dialyzer:
	@rebar3 dialyzer

# 代码覆盖率
cover:
	@rebar3 cover

# 生成文档
docs:
	@rebar3 edoc

# 完整检查（编译、测试、类型检查）
check: compile eunit dialyzer

# 安装依赖
deps:
	@rebar3 get-deps

# 发布
release:
	@rebar3 release

# 帮助信息
help:
	@echo "EORM Makefile 使用方法:"
	@echo ""
	@echo "  make              - 编译项目"
	@echo "  make test         - 运行所有测试"
	@echo "  make eunit        - 运行单元测试"
	@echo "  make docker-test  - 运行 Docker 集成测试"
	@echo "  make quick-test   - 快速运行单元测试"
	@echo "  make shell        - 启动 Erlang Shell"
	@echo "  make clean        - 清理编译文件"
	@echo "  make format       - 格式化代码"
	@echo "  make dialyzer     - 类型检查"
	@echo "  make cover        - 代码覆盖率"
	@echo "  make docs         - 生成文档"
	@echo "  make check        - 完整检查"
	@echo "  make help         - 显示帮助"
	@echo ""