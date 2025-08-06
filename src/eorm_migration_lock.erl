%%% @doc 迁移锁管理器
%%% 防止并发迁移
%%% @end
-module(eorm_migration_lock).
-behaviour(gen_server).

-export([start_link/0]).
-export([acquire/1, acquire/2, release/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    locks = #{} :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 获取迁移锁
-spec acquire(atom()) -> ok | {error, locked}.
acquire(Model) ->
    acquire(Model, 5000).

-spec acquire(atom(), integer()) -> ok | {error, locked}.
acquire(Model, Timeout) ->
    gen_server:call(?MODULE, {acquire, Model, self()}, Timeout).

%% @doc 释放迁移锁
-spec release(atom()) -> ok.
release(Model) ->
    gen_server:cast(?MODULE, {release, Model, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% 监控持有锁的进程
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({acquire, Model, Pid}, _From, State = #state{locks = Locks}) ->
    case maps:get(Model, Locks, undefined) of
        undefined ->
            %% 没有锁，可以获取
            monitor(process, Pid),
            NewLocks = maps:put(Model, Pid, Locks),
            {reply, ok, State#state{locks = NewLocks}};
        CurrentPid when CurrentPid =:= Pid ->
            %% 同一个进程已经持有锁
            {reply, ok, State};
        _OtherPid ->
            %% 其他进程持有锁
            {reply, {error, locked}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({release, Model, Pid}, State = #state{locks = Locks}) ->
    case maps:get(Model, Locks, undefined) of
        Pid ->
            %% 释放锁
            NewLocks = maps:remove(Model, Locks),
            {noreply, State#state{locks = NewLocks}};
        _ ->
            %% 不是持有者或锁不存在
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State = #state{locks = Locks}) ->
    %% 进程退出，自动释放其持有的所有锁
    NewLocks = maps:filter(fun(_Model, LockPid) -> LockPid =/= Pid end, Locks),
    {noreply, State#state{locks = NewLocks}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.