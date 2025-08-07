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
    
    %% 5. 基本操作演示
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
        create_sample_records(),
        
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

create_sample_records() ->
    %% 使用EORM API创建记录，根据test_model的实际字段
    Records = [
        #{name => <<"Alice">>, created_at => calendar:universal_time()},
        #{name => <<"Bob">>, created_at => calendar:universal_time()},
        #{name => <<"Carol">>, created_at => calendar:universal_time()}
    ],
    
    lists:foreach(fun(Record) ->
        try
            Name = maps:get(name, Record),
            case eorm:create(test_model, Record) of
                {ok, _CreatedRecord} -> 
                    io:format("     ✓ 创建记录: ~s~n", [Name]);
                {error, Reason} -> 
                    io:format("     ✗ 创建记录失败 ~s: ~p~n", [Name, Reason])
            end
        catch
            _:Error -> 
                SafeName = maps:get(name, Record, <<"unknown">>),
                io:format("     ✗ 创建记录异常 ~s: ~p~n", [SafeName, Error])
        end
    end, Records).

query_examples() ->
    try
        %% 查询所有记录
        case eorm:find_all(test_model, #{}) of
            {ok, Records} ->
                io:format("     查询到 ~p 个记录:~n", [length(Records)]),
                lists:foreach(fun(Record) ->
                    Name = maps:get(name, Record, <<"unknown">>),
                    Id = maps:get(id, Record, undefined),
                    io:format("       - ID:~p, Name:~s~n", [Id, Name])
                end, Records);
            {error, Reason} ->
                io:format("     ✗ 查询失败: ~p~n", [Reason])
        end,
        
        %% 条件查询 - 查找第一个记录
        case eorm:first(test_model, #{name => <<"Alice">>}) of
            {ok, AliceRecord} ->
                Id = maps:get(id, AliceRecord, undefined),
                io:format("     找到Alice记录，ID: ~p~n", [Id]);
            {error, not_found} ->
                io:format("     Alice记录不存在~n");
            {error, QueryReason} ->
                io:format("     ✗ 条件查询失败: ~p~n", [QueryReason])
        end
    catch
        _:Error ->
            io:format("     ✗ 查询操作异常: ~p~n", [Error])
    end.

update_examples() ->
    try
        %% 更新记录 - 查找Alice并更新
        case eorm:first(test_model, #{name => <<"Alice">>}) of
            {ok, AliceRecord} ->
                Id = maps:get(id, AliceRecord),
                UpdatedData = #{name => <<"Alice Updated">>},
                case eorm:update(test_model, #{id => Id}, UpdatedData) of
                    {ok, _} ->
                        io:format("     ✓ Alice记录更新成功~n");
                    {error, Reason} ->
                        io:format("     ✗ 更新失败: ~p~n", [Reason])
                end;
            {error, not_found} ->
                io:format("     Alice记录不存在，无法更新~n");
            {error, FindReason} ->
                io:format("     ✗ 查找Alice失败: ~p~n", [FindReason])
        end,
        
        %% 验证更新结果
        case eorm:first(test_model, #{name => <<"Alice Updated">>}) of
            {ok, UpdatedRecord} ->
                Name = maps:get(name, UpdatedRecord),
                io:format("     验证更新结果: ~s~n", [Name]);
            {error, not_found} ->
                io:format("     更新验证失败：找不到更新后的记录~n");
            {error, VerifyReason} ->
                io:format("     更新验证失败: ~p~n", [VerifyReason])
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
%%      ✓ 创建记录: Alice
%%      ✓ 创建记录: Bob
%%      ✓ 创建记录: Carol
%%    查询操作演示...
%%      查询到 3 个记录:
%%        - ID:1, Name:Alice
%%        - ID:2, Name:Bob
%%        - ID:3, Name:Carol
%%      找到Alice记录，ID: 1
%%    更新操作演示...
%%      ✓ Alice记录更新成功
%%      验证更新结果: Alice Updated
%% 
%% === 演示完成 ===
%%
%% 这个示例展示了:
%% - EORM应用启动和配置
%% - 使用现有test_model进行模型注册
%% - 自动迁移功能
%% - 使用eorm:create/2创建记录
%% - 使用eorm:find_all/2和eorm:first/2查询记录
%% - 使用eorm:update/3更新记录
%% - 所有操作都使用正确的EORM API，而不是直接SQL