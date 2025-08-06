%%% @doc EORM 核心模块实现
%%% @author EORM Team
%%% @copyright 2024 EORM Project

-module(eorm).
-export([
    %% 基础 CRUD
    create/2, 
    find/2, 
    first/2, 
    update/3, 
    delete/2,
    %% 链式查询
    new/1,
    where/2,
    order_by/2,
    limit/2,
    offset/2,
    preload/2,
    joins/2,
    group_by/2,
    having/2,
    select/2,
    find_all/1,
    count/1,
    %% 事务
    transaction/1,
    %% 原生查询
    raw_query/2,
    %% 批量操作
    insert_all/2,
    %% 软删除
    soft_delete/2
]).

-record(query, {
    model :: module(),
    conditions = [] :: list(),
    order = [] :: list(),
    limit :: integer() | undefined,
    offset :: integer() | undefined,
    preloads = [] :: list(),
    joins = [] :: list(),
    group = [] :: list(),
    having = [] :: list(),
    select = [] :: list()
}).

%%% ===================================================================
%%% 基础 CRUD 操作
%%% ===================================================================

%% @doc 创建记录
create(Model, Attrs) ->
    Query = build_insert_query(Model, Attrs),
    case execute_query(Query) of
        {ok, Id} ->
            %% 触发 after_create 钩子
            Record = maps:put(id, Id, Attrs),
            run_hooks(Model, after_create, [Record]),
            {ok, Record};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 查询多条记录
find(Model, Conditions) ->
    Query = new(Model),
    Query1 = where(Query, Conditions),
    find_all(Query1).

%% @doc 查询第一条记录
first(Model, Conditions) ->
    Query = new(Model),
    Query1 = where(Query, Conditions),
    Query2 = limit(Query1, 1),
    case find_all(Query2) of
        {ok, [Record|_]} -> {ok, Record};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

%% @doc 更新记录
update(Model, Conditions, Updates) ->
    %% 运行 before_update 钩子
    case run_hooks(Model, before_update, [Conditions, Updates]) of
        {ok, Updates1} ->
            Query = build_update_query(Model, Conditions, Updates1),
            execute_query(Query);
        Error ->
            Error
    end.

%% @doc 删除记录
delete(Model, Conditions) ->
    Query = build_delete_query(Model, Conditions),
    execute_query(Query).

%%% ===================================================================
%%% 链式查询 API
%%% ===================================================================

%% @doc 创建新的查询
new(Model) ->
    #query{model = Model}.

%% @doc 添加 WHERE 条件
where(#query{conditions = Conds} = Query, NewConds) ->
    Query#query{conditions = Conds ++ [NewConds]}.

%% @doc 设置排序
order_by(Query, Orders) ->
    Query#query{order = Orders}.

%% @doc 设置限制
limit(Query, Limit) ->
    Query#query{limit = Limit}.

%% @doc 设置偏移
offset(Query, Offset) ->
    Query#query{offset = Offset}.

%% @doc 预加载关联
preload(Query, Associations) ->
    Query#query{preloads = Associations}.

%% @doc 添加 JOIN
joins(#query{joins = Joins} = Query, Association) ->
    Query#query{joins = Joins ++ [Association]}.

%% @doc 设置分组
group_by(Query, Fields) ->
    Query#query{group = Fields}.

%% @doc 设置 HAVING 条件
having(Query, Conditions) ->
    Query#query{having = Conditions}.

%% @doc 设置查询字段
select(Query, Fields) ->
    Query#query{select = Fields}.

%% @doc 执行查询并返回所有结果
find_all(#query{} = Query) ->
    SQL = build_select_query(Query),
    case execute_query(SQL) of
        {ok, Rows} ->
            Records = [row_to_record(Query#query.model, Row) || Row <- Rows],
            %% 处理预加载
            Records1 = handle_preloads(Query, Records),
            {ok, Records1};
        Error ->
            Error
    end.

%% @doc 计数查询
count(#query{} = Query) ->
    SQL = build_count_query(Query),
    case execute_query(SQL) of
        {ok, [{Count}]} -> {ok, Count};
        Error -> Error
    end.

%%% ===================================================================
%%% 事务支持
%%% ===================================================================

%% @doc 事务执行
transaction(Fun) ->
    Adapter = get_adapter(),
    Adapter:transaction(get_connection(), Fun).

%%% ===================================================================
%%% 原生查询
%%% ===================================================================

%% @doc 执行原生 SQL 查询
raw_query(SQL, Params) ->
    execute_query({raw, SQL, Params}).

%%% ===================================================================
%%% 批量操作
%%% ===================================================================

%% @doc 批量插入
insert_all(Model, RecordsList) ->
    Query = build_batch_insert_query(Model, RecordsList),
    execute_query(Query).

%%% ===================================================================
%%% 软删除
%%% ===================================================================

%% @doc 软删除（设置 deleted_at 字段）
soft_delete(Model, Conditions) ->
    Updates = #{deleted_at => calendar:universal_time()},
    update(Model, Conditions, Updates).

%%% ===================================================================
%%% 内部函数
%%% ===================================================================

%% @private
build_insert_query(Model, Attrs) ->
    TableName = get_table_name(Model),
    Fields = maps:keys(Attrs),
    Values = maps:values(Attrs),
    FieldsStr = string:join([atom_to_list(F) || F <- Fields], ", "),
    PlaceholdersStr = string:join(["$" ++ integer_to_list(I) || I <- lists:seq(1, length(Fields))], ", "),
    SQL = io_lib:format("INSERT INTO ~s (~s) VALUES (~s) RETURNING id", 
                        [TableName, FieldsStr, PlaceholdersStr]),
    {sql, iolist_to_binary(SQL), Values}.

%% @private
build_select_query(#query{} = Query) ->
    TableName = get_table_name(Query#query.model),
    SelectClause = build_select_clause(Query),
    FromClause = build_from_clause(TableName, Query),
    WhereClause = build_where_clause(Query),
    GroupClause = build_group_clause(Query),
    HavingClause = build_having_clause(Query),
    OrderClause = build_order_clause(Query),
    LimitClause = build_limit_clause(Query),
    
    SQL = iolist_to_binary([
        SelectClause, " ",
        FromClause,
        WhereClause,
        GroupClause,
        HavingClause,
        OrderClause,
        LimitClause
    ]),
    
    {sql, SQL, []}.

%% @private
build_select_clause(#query{select = []}) ->
    "SELECT *";
build_select_clause(#query{select = Fields}) ->
    FieldsStr = string:join([field_to_string(F) || F <- Fields], ", "),
    "SELECT " ++ FieldsStr.

%% @private
field_to_string({Func, Field, as, Alias}) ->
    io_lib:format("~s(~s) AS ~s", [Func, Field, Alias]);
field_to_string(Field) when is_atom(Field) ->
    atom_to_list(Field).

%% @private
build_from_clause(TableName, #query{joins = []}) ->
    io_lib:format("FROM ~s", [TableName]);
build_from_clause(TableName, #query{joins = Joins}) ->
    JoinsStr = build_joins_string(Joins),
    io_lib:format("FROM ~s ~s", [TableName, JoinsStr]).

%% @private
build_where_clause(#query{conditions = []}) ->
    "";
build_where_clause(#query{conditions = Conditions}) ->
    WhereStr = build_conditions_string(Conditions),
    " WHERE " ++ WhereStr.

%% @private
build_conditions_string(Conditions) ->
    CondStrs = [condition_to_string(C) || C <- Conditions],
    string:join(CondStrs, " AND ").

%% @private
condition_to_string(#{} = Map) ->
    Pairs = maps:to_list(Map),
    PairStrs = [pair_to_string(K, V) || {K, V} <- Pairs],
    string:join(PairStrs, " AND ").

%% @private
pair_to_string(Field, {Op, Value}) ->
    OpStr = operator_to_string(Op),
    io_lib:format("~s ~s ~p", [Field, OpStr, Value]);
pair_to_string(Field, Value) ->
    io_lib:format("~s = ~p", [Field, Value]).

%% @private
operator_to_string(eq) -> "=";
operator_to_string(neq) -> "!=";
operator_to_string(gt) -> ">";
operator_to_string(gte) -> ">=";
operator_to_string(lt) -> "<";
operator_to_string(lte) -> "<=";
operator_to_string(like) -> "LIKE";
operator_to_string(in) -> "IN".

%% @private
get_table_name(Model) ->
    ModelDef = Model:definition(),
    maps:get(table, ModelDef).

%% @private
get_adapter() ->
    %% 从配置中获取适配器
    {ok, Adapter} = application:get_env(eorm, default_adapter),
    list_to_atom("eorm_" ++ atom_to_list(Adapter) ++ "_adapter").

%% @private
get_connection() ->
    %% 从连接池获取连接
    eorm_db:get_connection().

%% @private
execute_query(Query) ->
    Adapter = get_adapter(),
    Connection = get_connection(),
    Adapter:execute(Connection, Query).

%% @private
row_to_record(Model, Row) ->
    %% 将数据库行转换为记录
    ModelDef = Model:definition(),
    Fields = maps:get(fields, ModelDef),
    maps:from_list(lists:zip([F || {F, _, _} <- Fields], Row)).

%% @private
handle_preloads(#query{preloads = []}, Records) ->
    Records;
handle_preloads(#query{preloads = Preloads, model = Model}, Records) ->
    %% 处理关联预加载
    lists:map(fun(Record) ->
        lists:foldl(fun(Preload, Acc) ->
            load_association(Model, Preload, Acc)
        end, Record, Preloads)
    end, Records).

%% @private
load_association(Model, Association, Record) ->
    %% 加载单个关联
    ModelDef = Model:definition(),
    Associations = maps:get(associations, ModelDef, []),
    case lists:keyfind(Association, 2, Associations) of
        {Type, Name, AssocModel, Options} ->
            load_association_by_type(Type, Name, AssocModel, Options, Record);
        false ->
            Record
    end.

%% @private
load_association_by_type(has_many, Name, AssocModel, Options, Record) ->
    ForeignKey = maps:get(foreign_key, Options),
    RecordId = maps:get(id, Record),
    {ok, AssocRecords} = find(AssocModel, #{ForeignKey => RecordId}),
    maps:put(Name, AssocRecords, Record);

load_association_by_type(has_one, Name, AssocModel, Options, Record) ->
    ForeignKey = maps:get(foreign_key, Options),
    RecordId = maps:get(id, Record),
    case first(AssocModel, #{ForeignKey => RecordId}) of
        {ok, AssocRecord} -> maps:put(Name, AssocRecord, Record);
        {error, not_found} -> maps:put(Name, undefined, Record)
    end;

load_association_by_type(belongs_to, Name, AssocModel, Options, Record) ->
    ForeignKey = maps:get(foreign_key, Options),
    AssocId = maps:get(ForeignKey, Record),
    case first(AssocModel, #{id => AssocId}) of
        {ok, AssocRecord} -> maps:put(Name, AssocRecord, Record);
        {error, not_found} -> maps:put(Name, undefined, Record)
    end.

%% @private
run_hooks(Model, HookType, Args) ->
    ModelDef = Model:definition(),
    Hooks = maps:get(hooks, ModelDef, []),
    case lists:keyfind(HookType, 1, Hooks) of
        {HookType, Fun} ->
            apply(Fun, Args);
        false ->
            {ok, hd(Args)}
    end.

%% @private
build_update_query(Model, Conditions, Updates) ->
    TableName = get_table_name(Model),
    SetClause = build_set_clause(Updates),
    WhereClause = build_where_clause(#query{conditions = [Conditions]}),
    SQL = io_lib:format("UPDATE ~s ~s~s", [TableName, SetClause, WhereClause]),
    {sql, iolist_to_binary(SQL), []}.

%% @private
build_set_clause(Updates) ->
    Pairs = maps:to_list(Updates),
    SetStrs = [io_lib:format("~s = ~p", [K, V]) || {K, V} <- Pairs],
    "SET " ++ string:join(SetStrs, ", ").

%% @private
build_delete_query(Model, Conditions) ->
    TableName = get_table_name(Model),
    WhereClause = build_where_clause(#query{conditions = [Conditions]}),
    SQL = io_lib:format("DELETE FROM ~s~s", [TableName, WhereClause]),
    {sql, iolist_to_binary(SQL), []}.

%% @private
build_count_query(Query) ->
    TableName = get_table_name(Query#query.model),
    FromClause = build_from_clause(TableName, Query),
    WhereClause = build_where_clause(Query),
    SQL = io_lib:format("SELECT COUNT(*) ~s~s", [FromClause, WhereClause]),
    {sql, iolist_to_binary(SQL), []}.

%% @private
build_batch_insert_query(Model, RecordsList) ->
    TableName = get_table_name(Model),
    [First|_] = RecordsList,
    Fields = maps:keys(First),
    FieldsStr = string:join([atom_to_list(F) || F <- Fields], ", "),
    
    ValueRows = lists:map(fun(Record) ->
        Values = [maps:get(F, Record) || F <- Fields],
        "(" ++ string:join([io_lib:format("~p", [V]) || V <- Values], ", ") ++ ")"
    end, RecordsList),
    
    ValuesStr = string:join(ValueRows, ", "),
    SQL = io_lib:format("INSERT INTO ~s (~s) VALUES ~s", 
                        [TableName, FieldsStr, ValuesStr]),
    {sql, iolist_to_binary(SQL), []}.

%% @private
build_order_clause(#query{order = []}) ->
    "";
build_order_clause(#query{order = Orders}) ->
    OrderStrs = [order_to_string(O) || O <- Orders],
    " ORDER BY " ++ string:join(OrderStrs, ", ").

%% @private
order_to_string({Field, Dir}) ->
    io_lib:format("~s ~s", [Field, string:to_upper(atom_to_list(Dir))]);
order_to_string(Field) when is_atom(Field) ->
    atom_to_list(Field).

%% @private
build_limit_clause(#query{limit = undefined, offset = undefined}) ->
    "";
build_limit_clause(#query{limit = Limit, offset = undefined}) ->
    io_lib:format(" LIMIT ~p", [Limit]);
build_limit_clause(#query{limit = Limit, offset = Offset}) ->
    io_lib:format(" LIMIT ~p OFFSET ~p", [Limit, Offset]).

%% @private
build_group_clause(#query{group = []}) ->
    "";
build_group_clause(#query{group = Fields}) ->
    FieldsStr = string:join([atom_to_list(F) || F <- Fields], ", "),
    " GROUP BY " ++ FieldsStr.

%% @private
build_having_clause(#query{having = []}) ->
    "";
build_having_clause(#query{having = Conditions}) ->
    CondStr = build_conditions_string(Conditions),
    " HAVING " ++ CondStr.

%% @private
build_joins_string(Joins) ->
    JoinStrs = lists:map(fun(Join) ->
        %% 这里需要根据关联定义构建 JOIN 语句
        io_lib:format("JOIN ~s ON ...", [Join])
    end, Joins),
    string:join(JoinStrs, " ").