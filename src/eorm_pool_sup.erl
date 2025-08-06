%%% @doc 连接池监督器
%%% @end
-module(eorm_pool_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% 简化版本，暂不启动实际的连接池
    Children = [],
    
    {ok, {SupFlags, Children}}.