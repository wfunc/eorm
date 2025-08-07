#!/bin/bash

# EORM 简洁测试脚本
# 只显示测试结果，过滤掉进程终止的错误信息

echo "🧪 运行 EORM 单元测试..."
echo ""

# 运行测试并过滤输出
output=$(rebar3 eunit 2>&1)

# 检查是否有编译错误
if echo "$output" | grep -q "===> Compilation failed"; then
    echo "❌ 编译失败！"
    echo "$output" | grep -A5 "===> Compilation failed"
    exit 1
fi

# 提取测试结果
test_result=$(echo "$output" | grep -E "tests?, [0-9]+ failures?" | tail -1)

# 提取测试数量和失败数
if [[ $test_result =~ ([0-9]+)\ tests?,\ ([0-9]+)\ failures? ]]; then
    total_tests="${BASH_REMATCH[1]}"
    failures="${BASH_REMATCH[2]}"
    
    echo "📊 测试结果："
    echo "   总测试数: $total_tests"
    echo "   失败数: $failures"
    echo ""
    
    if [ "$failures" -eq 0 ]; then
        echo "✅ 所有测试通过！"
    else
        echo "❌ 有 $failures 个测试失败"
        echo ""
        echo "失败详情："
        echo "$output" | grep -A10 "Failure/Error:"
    fi
else
    echo "⚠️  无法解析测试结果"
    echo ""
    echo "原始输出："
    echo "$output" | grep -E "tests?, [0-9]+ failures?|Pending:|cancelled"
fi

# 如果有参数 -v 或 --verbose，显示完整输出
if [ "$1" = "-v" ] || [ "$1" = "--verbose" ]; then
    echo ""
    echo "=== 完整输出 ==="
    echo "$output"
fi