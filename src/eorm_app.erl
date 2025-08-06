%%% @doc EORM 应用启动模块
%%% @end
-module(eorm_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% 启动监督树
    eorm_sup:start_link().

stop(_State) ->
    ok.