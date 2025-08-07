%%% @doc EORM 单元测试示例
%%% 展示如何编写 EUnit 测试
%%% @end
-module(example_test).

%% 引入 EUnit 测试框架
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试套件
%%====================================================================

%% 简单测试 - 使用 _test 后缀
simple_addition_test() ->
    ?assertEqual(4, 2 + 2).

simple_subtraction_test() ->
    ?assertEqual(3, 5 - 2).

%% 测试生成器 - 使用 _test_ 后缀可以生成多个测试
basic_operations_test_() ->
    [
        {"加法测试", ?_assertEqual(5, 3 + 2)},
        {"乘法测试", ?_assertEqual(6, 2 * 3)},
        {"除法测试", ?_assertEqual(2, 6 div 3)},
        {"模运算测试", ?_assertEqual(1, 7 rem 3)}
    ].

%% 带 setup 和 teardown 的测试
database_operations_test_() ->
    {setup,
     fun setup/0,           % 测试前执行
     fun cleanup/1,         % 测试后执行
     fun(Data) ->           % 实际测试
         [
             {"插入数据", ?_test(test_insert(Data))},
             {"查询数据", ?_test(test_query(Data))},
             {"更新数据", ?_test(test_update(Data))}
         ]
     end}.

%% Setup 函数
setup() ->
    %% 初始化测试数据
    #{db => test_db, table => test_table}.

%% Cleanup 函数
cleanup(_Data) ->
    %% 清理测试数据
    ok.

%% 实际测试函数
test_insert(#{db := Db, table := Table}) ->
    %% 模拟插入操作
    Result = {ok, 1},
    ?assertEqual({ok, 1}, Result).

test_query(#{db := Db, table := Table}) ->
    %% 模拟查询操作
    Result = {ok, []},
    ?assertEqual({ok, []}, Result).

test_update(#{db := Db, table := Table}) ->
    %% 模拟更新操作
    Result = {ok, 0},
    ?assertEqual({ok, 0}, Result).

%%====================================================================
%% 断言示例
%%====================================================================

assertions_demo_test_() ->
    [
        %% 相等断言
        {"assertEqual", ?_assertEqual(expected, expected)},
        
        %% 不相等断言
        {"assertNotEqual", ?_assertNotEqual(1, 2)},
        
        %% 匹配断言
        {"assertMatch", ?_assertMatch({ok, _}, {ok, 123})},
        
        %% 异常断言
        {"assertError", ?_assertError(badarith, 1 div 0)},
        {"assertExit", ?_assertExit(normal, exit(normal))},
        {"assertThrow", ?_assertThrow(ball, throw(ball))},
        
        %% 布尔断言
        {"assert true", ?_assert(true)},
        {"assertNot false", ?_assertNot(false)}
    ].

%%====================================================================
%% 参数化测试
%%====================================================================

%% 使用列表生成多个测试用例
parameterized_test_() ->
    TestCases = [
        {1, 1, 2},
        {2, 3, 5},
        {10, 20, 30},
        {-5, 5, 0}
    ],
    [
        {lists:flatten(io_lib:format("~p + ~p = ~p", [A, B, Expected])),
         ?_assertEqual(Expected, A + B)}
        || {A, B, Expected} <- TestCases
    ].

%%====================================================================
%% 测试超时
%%====================================================================

timeout_test_() ->
    %% 设置 5 秒超时
    {timeout, 5,
     fun() ->
         %% 模拟长时间运行的操作
         timer:sleep(100),
         ?assert(true)
     end}.

%%====================================================================
%% 测试模块级别的 setup/teardown
%%====================================================================

%% 这会为整个测试模块运行一次
all_test_() ->
    {setup,
     fun() -> 
         %% 模块级初始化
         %% 暂时注释掉应用启动，因为没有完整的应用配置
         %% application:start(eorm),
         ok
     end,
     fun(_) -> 
         %% 模块级清理
         %% application:stop(eorm),
         ok
     end,
     fun(_) ->
         [
             ?_test(simple_addition_test()),
             ?_test(simple_subtraction_test()),
             basic_operations_test_()
         ]
     end}.

%%====================================================================
%% 运行测试的方法
%%====================================================================

%% 方法 1: 运行所有测试
%% $ rebar3 eunit

%% 方法 2: 只运行这个模块的测试
%% $ rebar3 eunit --module=example_test

%% 方法 3: 运行特定的测试函数
%% $ rebar3 eunit --module=example_test --test=simple_addition_test

%% 方法 4: 在 Erlang shell 中运行
%% eunit:test(example_test).
%% eunit:test({example_test, simple_addition_test}).

%% 方法 5: 生成覆盖率报告
%% $ rebar3 eunit --cover
%% $ rebar3 cover --verbose