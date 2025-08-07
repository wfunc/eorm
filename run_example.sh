#!/bin/bash

# EORM 示例运行脚本
# 自动设置环境并运行EORM示例

set -e

echo "🚀 EORM 示例运行脚本"
echo "===================="

# 检查Docker是否运行
if ! docker ps &> /dev/null; then
    echo "❌ Docker未运行，请先启动Docker"
    exit 1
fi

# 检查postgres-dev容器是否存在和运行
if docker ps -a --format "table {{.Names}}" | grep -q "postgres-dev"; then
    if ! docker ps --format "table {{.Names}}" | grep -q "postgres-dev"; then
        echo "📦 启动现有的postgres-dev容器..."
        docker start postgres-dev
    else
        echo "✅ postgres-dev容器已在运行"
    fi
else
    echo "📦 创建并启动postgres-dev容器..."
    docker run --name postgres-dev \
        -e POSTGRES_DB=eorm_test \
        -e POSTGRES_USER=dev \
        -e POSTGRES_PASSWORD=123 \
        -p 5432:5432 \
        -d postgres:13
fi

# 等待数据库启动
echo "⏳ 等待数据库启动..."
sleep 3

# 检查数据库连接
echo "🔍 检查数据库连接..."
if ! timeout 10s docker exec postgres-dev pg_isready -U dev -d eorm_test &> /dev/null; then
    echo "❌ 数据库连接失败，请检查配置"
    exit 1
fi

echo "✅ 数据库连接成功"

# 编译项目
echo "🔨 编译EORM项目..."
if ! rebar3 compile; then
    echo "❌ 编译失败"
    exit 1
fi

echo "✅ 编译成功"

# 运行简单示例
echo ""
echo "🎯 运行简单使用示例..."
echo "======================="
rebar3 shell --eval "simple_usage:demo(), init:stop()." --sname eorm_demo

echo ""
echo "🎉 示例运行完成！"
echo ""
echo "💡 提示:"
echo "   1. 可以运行 'rebar3 shell' 然后执行 'simple_usage:demo().' 进行交互式测试"
echo "   2. 可以查看 examples/blog_example.erl 了解更复杂的用法"
echo "   3. 运行 'rebar3 eunit --cover' 查看测试覆盖率"
echo ""
echo "📚 更多信息请查看 README.md"