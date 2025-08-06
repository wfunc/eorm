%%% @doc EORM 自动迁移核心模块
%%% 提供类似 GORM 的自动迁移功能，根据模型定义自动创建和更新数据库表
%%% @end
-module(eorm_auto_migrate).

-export([
    auto_migrate/1,
    auto_migrate/2,
    migration_plan/1,
    migration_plan/2,
    rollback/2
]).

-include("eorm.hrl").

%% @doc 自动迁移模型列表
-spec auto_migrate([module()]) -> ok | {error, term()}.
auto_migrate(Models) ->
    auto_migrate(Models, #{}).

%% @doc 带选项的自动迁移
-spec auto_migrate([module()], map()) -> ok | {error, term()}.
auto_migrate(Models, Options) ->
    try
        %% 获取数据库适配器
        Adapter = get_adapter(Options),
        
        %% 确保迁移历史表存在
        ok = eorm_migration_history:ensure_table(Adapter),
        
        %% 处理每个模型
        Results = lists:map(fun(Model) ->
            migrate_model(Adapter, Model, Options)
        end, Models),
        
        %% 检查所有结果
        case lists:all(fun(R) -> R =:= ok end, Results) of
            true -> ok;
            false -> {error, {migration_failed, Results}}
        end
    catch
        Type:Error:Stacktrace ->
            error_logger:error_msg("Auto migration failed: ~p:~p~n~p~n", 
                                   [Type, Error, Stacktrace]),
            {error, {migration_error, Error}}
    end.

%% @doc 生成迁移计划但不执行
-spec migration_plan([module()]) -> {ok, list()} | {error, term()}.
migration_plan(Models) ->
    migration_plan(Models, #{}).

-spec migration_plan([module()], map()) -> {ok, list()} | {error, term()}.
migration_plan(Models, Options) ->
    try
        Adapter = get_adapter(Options),
        Plans = lists:map(fun(Model) ->
            generate_migration_plan(Adapter, Model, Options)
        end, Models),
        {ok, Plans}
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc 回滚到指定版本
-spec rollback(module(), binary()) -> ok | {error, term()}.
rollback(Model, Version) ->
    Adapter = get_adapter(#{}),
    eorm_rollback:execute(Adapter, Model, Version).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private 迁移单个模型
migrate_model(Adapter, Model, Options) ->
    %% 获取迁移锁
    case eorm_migration_lock:acquire(Model, 5000) of
        ok ->
            try
                do_migrate_model(Adapter, Model, Options)
            after
                eorm_migration_lock:release(Model)
            end;
        {error, locked} ->
            case maps:get(wait_for_lock, Options, true) of
                true ->
                    timer:sleep(1000),
                    migrate_model(Adapter, Model, Options);
                false ->
                    {error, {locked, Model}}
            end
    end.

%% @private 执行模型迁移
do_migrate_model(Adapter, Model, Options) ->
    %% 获取模型定义
    Schema = get_model_schema(Model),
    
    %% 检查表是否存在
    TableName = maps:get(table, Schema, Model),
    case eorm_schema_inspector:table_exists(Adapter, TableName) of
        false ->
            %% 创建新表
            create_table(Adapter, TableName, Schema, Options);
        true ->
            %% 更新现有表
            update_table(Adapter, TableName, Schema, Options)
    end.

%% @private 创建新表
create_table(Adapter, TableName, Schema, Options) ->
    io:format("Creating table ~p...~n", [TableName]),
    
    %% 生成 CREATE TABLE DDL
    DDL = eorm_ddl_generator:generate_create_table(Adapter, TableName, Schema),
    
    %% 执行 DDL
    case execute_ddl(Adapter, DDL, Options) of
        ok ->
            %% 创建索引
            create_indexes(Adapter, TableName, Schema, Options),
            
            %% 记录迁移历史
            eorm_migration_history:record(Adapter, TableName, create_table, DDL),
            
            io:format("Table ~p created successfully~n", [TableName]),
            ok;
        Error ->
            Error
    end.

%% @private 更新现有表
update_table(Adapter, TableName, Schema, Options) ->
    %% 获取当前数据库 schema
    CurrentSchema = eorm_schema_inspector:get_table_schema(Adapter, TableName),
    
    %% 比较 schemas
    Changes = eorm_schema_diff:compare(Schema, CurrentSchema),
    
    case Changes of
        #{changes := []} ->
            %% 无需变更
            ok;
        _ ->
            apply_changes(Adapter, TableName, Changes, Options)
    end.

%% @private 应用表结构变更
apply_changes(Adapter, TableName, Changes, Options) ->
    Mode = maps:get(mode, Options, safe),
    
    %% 检查变更安全性
    case check_change_safety(Changes, Mode) of
        ok ->
            do_apply_changes(Adapter, TableName, Changes, Options);
        {error, Reason} ->
            handle_unsafe_changes(Adapter, TableName, Changes, Options, Reason)
    end.

%% @private 执行变更
do_apply_changes(Adapter, TableName, Changes, Options) ->
    io:format("Updating table ~p...~n", [TableName]),
    
    %% 生成 ALTER TABLE 语句
    DDLs = eorm_ddl_generator:generate_alter_table(Adapter, TableName, Changes),
    
    %% 在事务中执行所有变更
    Result = eorm_transaction:transaction(Adapter, fun() ->
        lists:foreach(fun(DDL) ->
            case execute_ddl(Adapter, DDL, Options) of
                ok -> ok;
                Error -> throw(Error)
            end
        end, DDLs)
    end),
    
    case Result of
        {atomic, _} ->
            %% 记录迁移历史
            eorm_migration_history:record(Adapter, TableName, alter_table, DDLs),
            io:format("Table ~p updated successfully~n", [TableName]),
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @private 检查变更安全性
check_change_safety(Changes, Mode) ->
    case Mode of
        force ->
            %% 强制模式，跳过安全检查
            ok;
        dry_run ->
            %% Dry run 模式，总是安全的
            ok;
        safe ->
            %% 安全模式，检查危险操作
            RiskAnalysis = eorm_schema_diff:analyze_risks(Changes),
            case maps:get(dangerous, RiskAnalysis, []) of
                [] -> ok;
                Dangerous -> {error, {dangerous_changes, Dangerous}}
            end
    end.

%% @private 处理不安全的变更
handle_unsafe_changes(Adapter, TableName, Changes, Options, Reason) ->
    Mode = maps:get(mode, Options, safe),
    case Mode of
        dry_run ->
            %% Dry run 模式，只打印计划
            io:format("~n[DRY RUN] Table ~p would be modified:~n", [TableName]),
            print_changes(Changes),
            io:format("Warning: ~p~n", [Reason]),
            ok;
        _ ->
            %% 其他模式，返回错误
            io:format("~nRefusing to apply unsafe changes to ~p:~n", [TableName]),
            print_changes(Changes),
            {error, Reason}
    end.

%% @private 创建索引
create_indexes(Adapter, TableName, Schema, Options) ->
    Indexes = maps:get(indexes, Schema, []),
    lists:foreach(fun(Index) ->
        DDL = eorm_ddl_generator:generate_create_index(Adapter, TableName, Index),
        execute_ddl(Adapter, DDL, Options)
    end, Indexes).

%% @private 生成迁移计划
generate_migration_plan(Adapter, Model, Options) ->
    Schema = get_model_schema(Model),
    TableName = maps:get(table, Schema, Model),
    
    case eorm_schema_inspector:table_exists(Adapter, TableName) of
        false ->
            %% 需要创建表
            #{
                model => Model,
                table => TableName,
                action => create_table,
                ddl => eorm_ddl_generator:generate_create_table(Adapter, TableName, Schema)
            };
        true ->
            %% 需要更新表
            CurrentSchema = eorm_schema_inspector:get_table_schema(Adapter, TableName),
            Changes = eorm_schema_diff:compare(Schema, CurrentSchema),
            #{
                model => Model,
                table => TableName,
                action => alter_table,
                changes => Changes,
                ddl => eorm_ddl_generator:generate_alter_table(Adapter, TableName, Changes)
            }
    end.

%% @private 获取模型 schema
get_model_schema(Model) ->
    case erlang:function_exported(Model, schema, 0) of
        true ->
            Model:schema();
        false ->
            %% 兼容旧的 definition/0 函数
            case erlang:function_exported(Model, definition, 0) of
                true ->
                    Model:definition();
                false ->
                    error({invalid_model, Model})
            end
    end.

%% @private 获取数据库适配器
get_adapter(Options) ->
    case maps:get(adapter, Options, undefined) of
        undefined ->
            %% 使用默认适配器
            case application:get_env(eorm, default_adapter) of
                {ok, Adapter} -> Adapter;
                undefined -> error(no_adapter_configured)
            end;
        Adapter ->
            Adapter
    end.

%% @private 执行 DDL
execute_ddl(Adapter, DDL, Options) ->
    Mode = maps:get(mode, Options, safe),
    case Mode of
        dry_run ->
            %% Dry run 模式，只打印 SQL
            io:format("~s~n", [DDL]),
            ok;
        _ ->
            %% 实际执行
            eorm_adapter:execute_ddl(Adapter, DDL)
    end.

%% @private 打印变更
print_changes(Changes) ->
    maps:foreach(fun(Type, Items) ->
        case Items of
            [] -> ok;
            _ ->
                io:format("  ~p:~n", [Type]),
                lists:foreach(fun(Item) ->
                    io:format("    - ~p~n", [Item])
                end, Items)
        end
    end, Changes).