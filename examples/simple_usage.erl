%%% @doc EORM简单使用示例
%%% 快速上手指南，展示基本的EORM操作
%%% @end
-module(simple_usage).

%% API exports
-export([demo/0]).

%%====================================================================
%% 简单演示
%%====================================================================

demo() ->
    io:format("=== EORM 简单使用演示 ===~n~n"),
    
    %% 1. 启动应用
    io:format("1. 启动EORM应用...~n"),
    application:ensure_all_started(eorm),
    
    %% 2. 配置数据库
    io:format("2. 配置数据库连接...~n"),
    configure_database(),
    
    %% 3. 模型注册
    io:format("3. 注册模型...~n"),
    register_test_model(),
    
    %% 4. 自动迁移
    io:format("4. 执行自动迁移...~n"),
    run_migration(),
    
    %% 5. 基本CRUD操作演示
    io:format("5. 演示基本操作...~n"),
    demo_basic_operations(),
    
    io:format("~n=== 演示完成 ===~n").

%%====================================================================
%% 配置和初始化
%%====================================================================

configure_database() ->
    %% 使用postgres-dev Docker容器
    eorm_adapter:set_adapter(postgres),
    application:set_env(eorm, default_adapter, postgres),
    application:set_env(eorm, databases, [
        {postgres, #{
            adapter => eorm_postgres_adapter,
            host => "localhost", 
            port => 5432,
            database => "eorm_test",
            username => "dev",
            password => "123",
            pool_size => 3
        }}
    ]),
    io:format("   ✓ 数据库配置完成~n").

register_test_model() ->
    case eorm_registry:register_model(test_model) of
        ok -> 
            io:format("   ✓ test_model 注册成功~n");
        {error, Reason} -> 
            io:format("   ✗ 模型注册失败: ~p~n", [Reason])
    end.

run_migration() ->
    case eorm_auto_migrate:auto_migrate([test_model], #{adapter => postgres, mode => force}) of
        {ok, Plan} -> 
            io:format("   ✓ 迁移成功: ~p~n", [Plan]);
        {error, Reason} -> 
            io:format("   ✗ 迁移失败: ~p~n", [Reason])
    end.

%%====================================================================
%% 基本操作演示
%%====================================================================

demo_basic_operations() ->
    try
        %% 创建测试数据
        io:format("   创建测试数据...~n"),
        create_sample_users(),
        
        %% 查询演示
        io:format("   查询操作演示...~n"),
        query_examples(),
        
        %% 更新演示
        io:format("   更新操作演示...~n"),
        update_examples(),
        
        ok
    catch
        Error:Reason ->
            io:format("   ✗ 操作失败: ~p:~p~n", [Error, Reason])
    end.

create_sample_users() ->
    Users = [
        {<<"Alice">>, <<"alice@example.com">>, 25},
        {<<"Bob">>, <<"bob@example.com">>, 30}, 
        {<<"Carol">>, <<"carol@example.com">>, 28}
    ],
    
    lists:foreach(fun({Name, Email, Age}) ->
        try
            SQL = <<"INSERT INTO test_users (name, email, age) VALUES ($1, $2, $3)">>,
            case eorm_adapter:query(postgres, SQL, [Name, Email, Age]) of
                {ok, _} -> 
                    io:format("     ✓ 创建用户: ~s~n", [Name]);
                {error, Reason} -> 
                    io:format("     ✗ 创建用户失败 ~s: ~p~n", [Name, Reason])
            end
        catch
            _:Error -> 
                io:format("     ✗ 创建用户异常 ~s: ~p~n", [Name, Error])
        end
    end, Users).

query_examples() ->
    try
        %% 查询所有用户
        SQL1 = <<"SELECT name, email, age FROM test_users ORDER BY age">>,
        case eorm_adapter:query(postgres, SQL1, []) of
            {ok, Rows} ->
                io:format("     查询到 ~p 个用户:~n", [length(Rows)]),
                lists:foreach(fun({Name, Email, Age}) ->
                    io:format("       - ~s (~s), 年龄: ~p~n", [Name, Email, Age])
                end, Rows);
            {error, Reason} ->
                io:format("     ✗ 查询失败: ~p~n", [Reason])
        end,
        
        %% 条件查询
        SQL2 = <<"SELECT name FROM test_users WHERE age > $1">>,
        case eorm_adapter:query(postgres, SQL2, [26]) of
            {ok, OlderUsers} ->
                Names = [Name || {Name} <- OlderUsers],
                io:format("     年龄>26的用户: ~p~n", [Names]);
            {error, Reason} ->
                io:format("     ✗ 条件查询失败: ~p~n", [Reason])
        end
    catch
        _:Error ->
            io:format("     ✗ 查询操作异常: ~p~n", [Error])
    end.

update_examples() ->
    try
        %% 更新用户年龄
        SQL = <<"UPDATE test_users SET age = age + 1 WHERE name = $1">>,
        case eorm_adapter:query(postgres, SQL, [<<"Alice">>]) of
            {ok, _} ->
                io:format("     ✓ Alice年龄更新成功~n");
            {error, Reason} ->
                io:format("     ✗ 更新失败: ~p~n", [Reason])
        end,
        
        %% 验证更新结果
        VerifySQL = <<"SELECT age FROM test_users WHERE name = $1">>,
        case eorm_adapter:query(postgres, VerifySQL, [<<"Alice">>]) of
            {ok, [{NewAge}]} ->
                io:format("     Alice的新年龄: ~p~n", [NewAge]);
            _ ->
                io:format("     验证更新失败~n")
        end
    catch
        _:Error ->
            io:format("     ✗ 更新操作异常: ~p~n", [Error])
    end.

%%====================================================================
%% 使用说明
%%====================================================================

%% 如何运行这个示例:
%% 
%% 1. 确保PostgreSQL Docker容器正在运行:
%%    docker run --name postgres-dev -e POSTGRES_DB=eorm_test -e POSTGRES_USER=dev -e POSTGRES_PASSWORD=123 -p 5432:5432 -d postgres
%%
%% 2. 启动Erlang shell:
%%    rebar3 shell
%%
%% 3. 运行示例:
%%    simple_usage:demo().
%%
%% 预期输出:
%% === EORM 简单使用演示 ===
%% 
%% 1. 启动EORM应用...
%% 2. 配置数据库连接...
%%    ✓ 数据库配置完成
%% 3. 注册模型...
%%    ✓ test_model 注册成功
%% 4. 执行自动迁移...
%%    ✓ 迁移成功: #{...}
%% 5. 演示基本操作...
%%    创建测试数据...
%%      ✓ 创建用户: Alice
%%      ✓ 创建用户: Bob
%%      ✓ 创建用户: Carol
%%    查询操作演示...
%%      查询到 3 个用户:
%%        - Alice (alice@example.com), 年龄: 25
%%        - Carol (carol@example.com), 年龄: 28
%%        - Bob (bob@example.com), 年龄: 30
%%      年龄>26的用户: [<<"Carol">>, <<"Bob">>]
%%    更新操作演示...
%%      ✓ Alice年龄更新成功
%%      Alice的新年龄: 26
%% 
%% === 演示完成 ===