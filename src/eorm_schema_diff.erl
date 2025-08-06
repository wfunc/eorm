%%% @doc Schema 比较引擎
%%% 负责比较模型定义和数据库结构的差异，生成变更计划
%%% @end
-module(eorm_schema_diff).

-export([
    compare/2,
    analyze_risks/1,
    generate_changes/2
]).

-include("eorm.hrl").

%% @doc 比较模型 Schema 和数据库 Schema
-spec compare(map(), map()) -> map().
compare(ModelSchema, DbSchema) ->
    #{
        changes => generate_all_changes(ModelSchema, DbSchema),
        to_add => find_additions(ModelSchema, DbSchema),
        to_modify => find_modifications(ModelSchema, DbSchema),
        to_drop => find_deletions(ModelSchema, DbSchema)
    }.

%% @doc 分析变更风险
-spec analyze_risks(map()) -> map().
analyze_risks(Changes) ->
    AllChanges = maps:get(changes, Changes, []),
    
    RiskMap = #{
        safe => [],
        warning => [],
        dangerous => []
    },
    
    lists:foldl(fun(Change, Acc) ->
        RiskLevel = assess_change_risk(Change),
        CurrentList = maps:get(RiskLevel, Acc, []),
        maps:put(RiskLevel, [Change | CurrentList], Acc)
    end, RiskMap, AllChanges).

%% @doc 生成具体的变更操作
-spec generate_changes(map(), map()) -> list().
generate_changes(ModelSchema, DbSchema) ->
    generate_all_changes(ModelSchema, DbSchema).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private 生成所有变更
generate_all_changes(ModelSchema, DbSchema) ->
    ColumnChanges = generate_column_changes(ModelSchema, DbSchema),
    IndexChanges = generate_index_changes(ModelSchema, DbSchema),
    ConstraintChanges = generate_constraint_changes(ModelSchema, DbSchema),
    
    ColumnChanges ++ IndexChanges ++ ConstraintChanges.

%% @private 生成列变更
generate_column_changes(ModelSchema, DbSchema) ->
    ModelFields = maps:get(fields, ModelSchema, []),
    DbColumns = maps:get(columns, DbSchema, []),
    
    %% 转换为 map 方便比较
    ModelFieldMap = fields_to_map(ModelFields),
    DbColumnMap = columns_to_map(DbColumns),
    
    %% 找出需要添加的列
    ToAdd = lists:filtermap(fun({Name, Def}) ->
        case maps:is_key(Name, DbColumnMap) of
            false -> {true, {add_column, Name, Def}};
            true -> false
        end
    end, maps:to_list(ModelFieldMap)),
    
    %% 找出需要修改的列
    ToModify = lists:filtermap(fun({Name, ModelDef}) ->
        case maps:get(Name, DbColumnMap, undefined) of
            undefined -> false;
            DbDef ->
                case column_needs_update(ModelDef, DbDef) of
                    true -> {true, {modify_column, Name, ModelDef, DbDef}};
                    false -> false
                end
        end
    end, maps:to_list(ModelFieldMap)),
    
    %% 找出需要删除的列（只在非安全模式下）
    ToDrop = lists:filtermap(fun({Name, _}) ->
        case maps:is_key(Name, ModelFieldMap) of
            false -> {true, {drop_column, Name}};
            true -> false
        end
    end, maps:to_list(DbColumnMap)),
    
    ToAdd ++ ToModify ++ ToDrop.

%% @private 生成索引变更
generate_index_changes(ModelSchema, DbSchema) ->
    ModelIndexes = maps:get(indexes, ModelSchema, []),
    DbIndexes = maps:get(indexes, DbSchema, []),
    
    %% 转换为 map
    ModelIndexMap = indexes_to_map(ModelIndexes),
    DbIndexMap = indexes_to_map(DbIndexes),
    
    %% 需要添加的索引
    ToAdd = lists:filtermap(fun({Name, Def}) ->
        case maps:is_key(Name, DbIndexMap) of
            false -> {true, {add_index, Name, Def}};
            true -> false
        end
    end, maps:to_list(ModelIndexMap)),
    
    %% 需要删除的索引
    ToDrop = lists:filtermap(fun({Name, _}) ->
        case maps:is_key(Name, ModelIndexMap) of
            false -> {true, {drop_index, Name}};
            true -> false
        end
    end, maps:to_list(DbIndexMap)),
    
    ToAdd ++ ToDrop.

%% @private 生成约束变更
generate_constraint_changes(ModelSchema, DbSchema) ->
    ModelConstraints = maps:get(constraints, ModelSchema, []),
    DbConstraints = maps:get(constraints, DbSchema, []),
    
    %% 外键约束
    ModelFKs = filter_foreign_keys(ModelConstraints),
    DbFKs = filter_foreign_keys(DbConstraints),
    
    %% 需要添加的外键
    ToAdd = lists:filtermap(fun(FK) ->
        case lists:member(FK, DbFKs) of
            false -> {true, {add_foreign_key, FK}};
            true -> false
        end
    end, ModelFKs),
    
    %% 需要删除的外键
    ToDrop = lists:filtermap(fun(FK) ->
        case lists:member(FK, ModelFKs) of
            false -> {true, {drop_foreign_key, FK}};
            true -> false
        end
    end, DbFKs),
    
    ToAdd ++ ToDrop.

%% @private 字段转换为 map
fields_to_map(Fields) ->
    lists:foldl(fun(Field, Acc) ->
        {Name, Spec} = parse_field(Field),
        maps:put(Name, Spec, Acc)
    end, #{}, Fields).

%% @private 解析字段定义
parse_field({Name}) ->
    %% 简单字段，自动推导类型
    {Name, #{type => infer_type(Name), options => []}};
parse_field({Name, Type}) ->
    {Name, #{type => Type, options => []}};
parse_field({Name, Type, Options}) ->
    {Name, #{type => Type, options => Options}};
parse_field(timestamps) ->
    %% 特殊处理 timestamps
    [
        {created_at, #{type => datetime, options => [auto_now_add]}},
        {updated_at, #{type => datetime, options => [auto_now]}}
    ].

%% @private 推导字段类型
infer_type(id) -> {integer, [primary_key, auto_increment]};
infer_type(Name) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    case lists:suffix("_id", NameStr) of
        true -> integer;
        false ->
            case lists:suffix("_at", NameStr) of
                true -> datetime;
                false -> string
            end
    end.

%% @private 数据库列转换为 map
columns_to_map(Columns) ->
    lists:foldl(fun(Column, Acc) ->
        Name = maps:get(name, Column),
        maps:put(Name, Column, Acc)
    end, #{}, Columns).

%% @private 检查列是否需要更新
column_needs_update(ModelDef, DbDef) ->
    %% 比较类型
    ModelType = normalize_type(maps:get(type, ModelDef)),
    DbType = normalize_type(maps:get(type, DbDef)),
    
    TypeChanged = ModelType =/= DbType,
    
    %% 比较约束
    ModelOptions = maps:get(options, ModelDef, []),
    DbOptions = maps:get(options, DbDef, []),
    
    OptionsChanged = not options_equal(ModelOptions, DbOptions),
    
    TypeChanged orelse OptionsChanged.

%% @private 标准化类型
normalize_type({string, Length}) -> {varchar, Length};
normalize_type(string) -> {varchar, 255};
normalize_type(text) -> text;
normalize_type(integer) -> integer;
normalize_type({decimal, P, S}) -> {decimal, P, S};
normalize_type(boolean) -> boolean;
normalize_type(datetime) -> timestamp;
normalize_type(Type) -> Type.

%% @private 比较选项是否相等
options_equal(Options1, Options2) ->
    %% 忽略顺序，只比较内容
    lists:sort(Options1) =:= lists:sort(Options2).

%% @private 索引转换为 map
indexes_to_map(Indexes) ->
    lists:foldl(fun(Index, Acc) ->
        {Name, Def} = parse_index(Index),
        maps:put(Name, Def, Acc)
    end, #{}, Indexes).

%% @private 解析索引定义
parse_index({Name, Columns}) ->
    {Name, #{columns => Columns, options => []}};
parse_index({Name, Columns, Options}) ->
    {Name, #{columns => Columns, options => Options}}.

%% @private 过滤外键约束
filter_foreign_keys(Constraints) ->
    lists:filter(fun(C) ->
        case C of
            {foreign_key, _, _, _, _} -> true;
            _ -> false
        end
    end, Constraints).

%% @private 评估变更风险级别
assess_change_risk({add_column, _, _}) ->
    safe;
assess_change_risk({add_index, _, _}) ->
    safe;
assess_change_risk({add_foreign_key, _}) ->
    warning;
assess_change_risk({modify_column, _, NewDef, OldDef}) ->
    %% 检查是否会导致数据丢失
    case will_lose_data(NewDef, OldDef) of
        true -> dangerous;
        false -> warning
    end;
assess_change_risk({drop_column, _}) ->
    dangerous;
assess_change_risk({drop_index, _}) ->
    warning;
assess_change_risk({drop_foreign_key, _}) ->
    warning;
assess_change_risk(_) ->
    safe.

%% @private 检查类型变更是否会导致数据丢失
will_lose_data(NewDef, OldDef) ->
    NewType = maps:get(type, NewDef),
    OldType = maps:get(type, OldDef),
    
    %% 检查类型转换是否安全
    case {normalize_type(OldType), normalize_type(NewType)} of
        {Same, Same} -> false;
        {{varchar, OldLen}, {varchar, NewLen}} when NewLen >= OldLen -> false;
        {integer, {decimal, _, _}} -> false;
        {integer, float} -> false;
        {float, {decimal, _, _}} -> false;
        _ -> true
    end.

%% @private 查找需要添加的元素
find_additions(ModelSchema, DbSchema) ->
    #{
        columns => find_missing_columns(ModelSchema, DbSchema),
        indexes => find_missing_indexes(ModelSchema, DbSchema),
        constraints => find_missing_constraints(ModelSchema, DbSchema)
    }.

%% @private 查找需要修改的元素
find_modifications(ModelSchema, DbSchema) ->
    #{
        columns => find_modified_columns(ModelSchema, DbSchema)
    }.

%% @private 查找需要删除的元素
find_deletions(ModelSchema, DbSchema) ->
    #{
        columns => find_extra_columns(ModelSchema, DbSchema),
        indexes => find_extra_indexes(ModelSchema, DbSchema),
        constraints => find_extra_constraints(ModelSchema, DbSchema)
    }.

%% @private 查找缺失的列
find_missing_columns(ModelSchema, DbSchema) ->
    ModelFields = maps:get(fields, ModelSchema, []),
    DbColumns = maps:get(columns, DbSchema, []),
    DbColumnNames = [maps:get(name, C) || C <- DbColumns],
    
    lists:filter(fun(Field) ->
        {Name, _} = parse_field(Field),
        not lists:member(Name, DbColumnNames)
    end, ModelFields).

%% @private 查找修改的列
find_modified_columns(ModelSchema, DbSchema) ->
    ModelFields = maps:get(fields, ModelSchema, []),
    DbColumns = maps:get(columns, DbSchema, []),
    
    ModelFieldMap = fields_to_map(ModelFields),
    DbColumnMap = columns_to_map(DbColumns),
    
    lists:filtermap(fun({Name, ModelDef}) ->
        case maps:get(Name, DbColumnMap, undefined) of
            undefined -> false;
            DbDef ->
                case column_needs_update(ModelDef, DbDef) of
                    true -> {true, {Name, ModelDef, DbDef}};
                    false -> false
                end
        end
    end, maps:to_list(ModelFieldMap)).

%% @private 查找多余的列
find_extra_columns(ModelSchema, DbSchema) ->
    ModelFields = maps:get(fields, ModelSchema, []),
    DbColumns = maps:get(columns, DbSchema, []),
    
    ModelFieldNames = [element(1, parse_field(F)) || F <- ModelFields],
    
    lists:filter(fun(Column) ->
        Name = maps:get(name, Column),
        not lists:member(Name, ModelFieldNames)
    end, DbColumns).

%% @private 查找缺失的索引
find_missing_indexes(ModelSchema, DbSchema) ->
    ModelIndexes = maps:get(indexes, ModelSchema, []),
    DbIndexes = maps:get(indexes, DbSchema, []),
    DbIndexNames = [element(1, parse_index(I)) || I <- DbIndexes],
    
    lists:filter(fun(Index) ->
        {Name, _} = parse_index(Index),
        not lists:member(Name, DbIndexNames)
    end, ModelIndexes).

%% @private 查找多余的索引
find_extra_indexes(ModelSchema, DbSchema) ->
    ModelIndexes = maps:get(indexes, ModelSchema, []),
    DbIndexes = maps:get(indexes, DbSchema, []),
    ModelIndexNames = [element(1, parse_index(I)) || I <- ModelIndexes],
    
    lists:filter(fun(Index) ->
        {Name, _} = parse_index(Index),
        not lists:member(Name, ModelIndexNames)
    end, DbIndexes).

%% @private 查找缺失的约束
find_missing_constraints(ModelSchema, DbSchema) ->
    ModelConstraints = maps:get(constraints, ModelSchema, []),
    DbConstraints = maps:get(constraints, DbSchema, []),
    
    lists:filter(fun(Constraint) ->
        not lists:member(Constraint, DbConstraints)
    end, ModelConstraints).

%% @private 查找多余的约束
find_extra_constraints(ModelSchema, DbSchema) ->
    ModelConstraints = maps:get(constraints, ModelSchema, []),
    DbConstraints = maps:get(constraints, DbSchema, []),
    
    lists:filter(fun(Constraint) ->
        not lists:member(Constraint, ModelConstraints)
    end, DbConstraints).