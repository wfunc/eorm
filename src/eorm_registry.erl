%%% @doc 模型注册表
%%% 管理所有已注册的模型元数据
%%% @end
-module(eorm_registry).
-behaviour(gen_server).

-export([start_link/0]).
-export([register_model/1, get_model/1, list_models/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eorm.hrl").

-record(state, {
    models = #{} :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 注册模型
-spec register_model(module()) -> ok | {error, term()}.
register_model(Module) ->
    gen_server:call(?MODULE, {register, Module}).

%% @doc 获取模型元数据
-spec get_model(module()) -> {ok, #eorm_model{}} | {error, not_found}.
get_model(Module) ->
    gen_server:call(?MODULE, {get, Module}).

%% @doc 列出所有已注册模型
-spec list_models() -> [module()].
list_models() ->
    gen_server:call(?MODULE, list).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({register, Module}, _From, State = #state{models = Models}) ->
    case parse_model(Module) of
        {ok, ModelMeta} ->
            NewModels = maps:put(Module, ModelMeta, Models),
            {reply, ok, State#state{models = NewModels}};
        Error ->
            {reply, Error, State}
    end;

handle_call({get, Module}, _From, State = #state{models = Models}) ->
    case maps:get(Module, Models, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Model -> {reply, {ok, Model}, State}
    end;

handle_call(list, _From, State = #state{models = Models}) ->
    {reply, maps:keys(Models), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private 解析模型定义
parse_model(Module) ->
    try
        %% 检查模块是否实现了必要的回调
        case erlang:function_exported(Module, schema, 0) orelse 
             erlang:function_exported(Module, definition, 0) of
            false ->
                {error, {invalid_model, Module}};
            true ->
                %% 获取模型定义
                Schema = get_model_schema(Module),
                
                %% 解析模型元数据
                Model = #eorm_model{
                    module = Module,
                    table = maps:get(table, Schema, Module),
                    fields = parse_fields(maps:get(fields, Schema, [])),
                    indexes = maps:get(indexes, Schema, []),
                    associations = parse_associations(maps:get(associations, Schema, [])),
                    constraints = maps:get(constraints, Schema, []),
                    hooks = maps:get(hooks, Schema, []),
                    options = maps:get(options, Schema, [])
                },
                
                {ok, Model}
        end
    catch
        Type:Error ->
            {error, {parse_error, Type, Error}}
    end.

%% @private 获取模型 schema
get_model_schema(Module) ->
    case erlang:function_exported(Module, schema, 0) of
        true ->
            Module:schema();
        false ->
            case erlang:function_exported(Module, definition, 0) of
                true ->
                    Module:definition();
                false ->
                    error({no_schema_function, Module})
            end
    end.

%% @private 解析字段定义
parse_fields(Fields) ->
    lists:map(fun parse_field/1, expand_special_fields(Fields)).

%% @private 展开特殊字段
expand_special_fields(Fields) ->
    lists:flatmap(fun(Field) ->
        case Field of
            timestamps ->
                [
                    {created_at, timestamp, [auto_now_add]},
                    {updated_at, timestamp, [auto_now]}
                ];
            _ ->
                [Field]
        end
    end, Fields).

%% @private 解析单个字段
parse_field(#{name := Name} = Field) when is_map(Field) ->
    %% 处理 map 格式的字段定义
    Type = maps:get(type, Field, string),
    Options = maps:get(opts, Field, maps:get(options, Field, [])),
    #eorm_field{
        name = Name,
        type = Type,
        options = Options,
        primary_key = lists:member(primary_key, Options),
        nullable = not lists:member(not_null, Options) andalso not lists:member(required, Options),
        default = proplists:get_value(default, Options)
    };

parse_field({Name}) ->
    %% 自动推导类型
    {Type, Options} = infer_field_type_and_options(Name),
    #eorm_field{
        name = Name,
        type = Type,
        options = Options,
        primary_key = lists:member(primary_key, Options),
        nullable = not lists:member(not_null, Options),
        default = proplists:get_value(default, Options)
    };

parse_field({Name, Type}) ->
    #eorm_field{
        name = Name,
        type = Type,
        options = [],
        nullable = true
    };

parse_field({Name, Type, Options}) ->
    #eorm_field{
        name = Name,
        type = Type,
        options = Options,
        primary_key = lists:member(primary_key, Options),
        nullable = not lists:member(not_null, Options),
        default = proplists:get_value(default, Options)
    }.

%% @private 推导字段类型和选项
infer_field_type_and_options(id) ->
    {integer, [primary_key, auto_increment]};
infer_field_type_and_options(Name) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    case {lists:suffix("_id", NameStr), 
          lists:suffix("_at", NameStr),
          lists:prefix("is_", NameStr)} of
        {true, _, _} -> {integer, []};
        {_, true, _} -> {timestamp, []};
        {_, _, true} -> {boolean, [default, false]};
        _ -> {string, []}
    end.

%% @private 解析关联定义
parse_associations(Associations) ->
    lists:map(fun parse_association/1, Associations).

%% @private 解析单个关联
parse_association({Type, Name, Target}) ->
    parse_association({Type, Name, Target, #{}});

parse_association({Type, Name, Target, Options}) ->
    #eorm_association{
        name = Name,
        type = Type,
        target = Target,
        foreign_key = maps:get(foreign_key, Options, infer_foreign_key(Type, Name, Target)),
        association_foreign_key = maps:get(association_foreign_key, Options),
        join_table = maps:get(join_table, Options),
        options = maps:to_list(Options)
    }.

%% @private 推导外键名称
infer_foreign_key(belongs_to, _Name, Target) ->
    list_to_atom(atom_to_list(Target) ++ "_id");
infer_foreign_key(_, Name, _Target) ->
    list_to_atom(atom_to_list(Name) ++ "_id").