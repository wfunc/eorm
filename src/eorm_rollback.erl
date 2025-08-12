%%% @doc EORM Rollback 模块
%%% 提供数据库迁移回滚功能
%%% @end
-module(eorm_rollback).

-export([
    execute/3
]).

%% @doc 执行回滚操作
-spec execute(atom(), module(), binary()) -> ok | {error, term()}.
execute(_Adapter, _Model, _Version) ->
    %% 简单实现用于测试覆盖率
    {error, rollback_not_implemented}.