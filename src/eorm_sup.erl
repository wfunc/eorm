%%% @doc EORM 顶层监督器
%%% @end
-module(eorm_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    Children = [
        %% 模型注册表
        #{
            id => eorm_registry,
            start => {eorm_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [eorm_registry]
        },
        
        %% 连接池监督器
        #{
            id => eorm_pool_sup,
            start => {eorm_pool_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [eorm_pool_sup]
        },
        
        %% 迁移锁管理器
        #{
            id => eorm_migration_lock,
            start => {eorm_migration_lock, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [eorm_migration_lock]
        }
    ],
    
    {ok, {SupFlags, Children}}.