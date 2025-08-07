#!/bin/bash

# EORM 单元测试运行脚本

echo "==========================================="
echo "EORM 单元测试运行器"
echo "==========================================="
echo ""

# 默认操作
ACTION=${1:-all}

case "$ACTION" in
    all)
        echo "运行所有测试..."
        rebar3 eunit
        echo ""
        echo "测试完成！"
        ;;
    
    module)
        MODULE=$2
        if [ -z "$MODULE" ]; then
            echo "错误：请指定模块名"
            echo "用法：./run_tests.sh module <module_name>"
            exit 1
        fi
        echo "运行模块 $MODULE 的测试..."
        rebar3 eunit --module=$MODULE
        ;;
    
    cover)
        echo "运行测试并生成覆盖率报告..."
        rebar3 eunit --cover
        rebar3 cover --verbose
        echo ""
        echo "覆盖率报告已生成到 _build/test/cover/"
        ;;
    
    example)
        echo "运行示例测试..."
        rebar3 eunit --module=example_test
        ;;
    
    quick)
        echo "快速测试（只编译和运行基本测试）..."
        rebar3 compile
        rebar3 eunit --module=example_test
        ;;
    
    clean)
        echo "清理并重新运行测试..."
        rebar3 clean
        rebar3 compile
        rebar3 eunit
        ;;
    
    watch)
        echo "监视模式（需要安装 entr）..."
        echo "按 Ctrl+C 退出"
        if ! command -v entr &> /dev/null; then
            echo "错误：需要安装 entr"
            echo "运行：brew install entr"
            exit 1
        fi
        find src test -name "*.erl" | entr -c bash -c "clear && rebar3 eunit"
        ;;
    
    help|*)
        echo "用法："
        echo "  ./run_tests.sh [命令] [参数]"
        echo ""
        echo "命令："
        echo "  all              - 运行所有测试（默认）"
        echo "  module <name>    - 运行指定模块的测试"
        echo "  cover            - 运行测试并生成覆盖率报告"
        echo "  example          - 运行示例测试"
        echo "  quick            - 快速测试"
        echo "  clean            - 清理并重新测试"
        echo "  watch            - 监视文件变化并自动测试"
        echo "  help             - 显示此帮助"
        echo ""
        echo "示例："
        echo "  ./run_tests.sh                          # 运行所有测试"
        echo "  ./run_tests.sh module eorm_auto_migrate_test  # 运行特定模块"
        echo "  ./run_tests.sh cover                    # 生成覆盖率报告"
        echo "  ./run_tests.sh watch                    # 监视模式"
        ;;
esac