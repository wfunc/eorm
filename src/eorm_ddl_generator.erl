%%% @doc DDL 生成器
%%% 根据模型定义和变更生成各种数据库的 DDL 语句
%%% @end
-module(eorm_ddl_generator).

-export([
    generate_create_table/3,
    generate_alter_table/3,
    generate_create_index/3,
    generate_drop_table/2,
    generate_drop_index/2
]).

-include("eorm.hrl").

%% @doc 生成 CREATE TABLE 语句
-spec generate_create_table(atom(), atom(), map()) -> iolist().
generate_create_table(postgres, TableName, Schema) ->
    generate_postgres_create_table(TableName, Schema);
generate_create_table(mysql, TableName, Schema) ->
    generate_mysql_create_table(TableName, Schema);
generate_create_table(sqlite, TableName, Schema) ->
    generate_sqlite_create_table(TableName, Schema);
generate_create_table(Adapter, _, _) ->
    error({unsupported_adapter, Adapter}).

%% @doc 生成 ALTER TABLE 语句
-spec generate_alter_table(atom(), atom(), map()) -> [iolist()].
generate_alter_table(Adapter, TableName, Changes) ->
    ChangeList = maps:get(changes, Changes, []),
    lists:map(fun(Change) ->
        generate_alter_statement(Adapter, TableName, Change)
    end, ChangeList).

%% @doc 生成 CREATE INDEX 语句
-spec generate_create_index(atom(), atom(), tuple()) -> iolist().
generate_create_index(Adapter, TableName, {IndexName, Columns}) ->
    generate_create_index(Adapter, TableName, {IndexName, Columns, []});
generate_create_index(_Adapter, TableName, {IndexName, Columns, Options}) ->
    Unique = lists:member(unique, Options),
    Order = proplists:get_value(order, Options, asc),
    
    UniqueClause = case Unique of
        true -> "UNIQUE ";
        false -> ""
    end,
    
    ColumnList = format_index_columns(Columns, Order),
    
    io_lib:format("CREATE ~sINDEX ~s ON ~s (~s)",
                  [UniqueClause, atom_to_list(IndexName), 
                   atom_to_list(TableName), ColumnList]).

%% @doc 生成 DROP TABLE 语句
-spec generate_drop_table(atom(), atom()) -> iolist().
generate_drop_table(_Adapter, TableName) ->
    io_lib:format("DROP TABLE IF EXISTS ~s", [atom_to_list(TableName)]).

%% @doc 生成 DROP INDEX 语句
-spec generate_drop_index(atom(), atom()) -> iolist().
generate_drop_index(_Adapter, IndexName) ->
    io_lib:format("DROP INDEX IF EXISTS ~s", [atom_to_list(IndexName)]).

%%====================================================================
%% PostgreSQL DDL Generation
%%====================================================================

%% @private
generate_postgres_create_table(TableName, Schema) ->
    Fields = maps:get(fields, Schema, []),
    Constraints = maps:get(constraints, Schema, []),
    
    %% 生成列定义
    ColumnDefs = lists:map(fun(Field) ->
        generate_postgres_column(Field)
    end, expand_fields(Fields)),
    
    %% 生成表级约束
    ConstraintDefs = lists:map(fun(Constraint) ->
        generate_postgres_constraint(Constraint)
    end, Constraints),
    
    %% 组合所有定义
    AllDefs = ColumnDefs ++ ConstraintDefs,
    
    [
        "CREATE TABLE IF NOT EXISTS ", atom_to_list(TableName), " (\n    ",
        string:join(AllDefs, ",\n    "),
        "\n)"
    ].

%% @private
generate_postgres_column(Field) ->
    {Name, Spec} = parse_field_spec(Field),
    Type = maps:get(type, Spec),
    Options = maps:get(options, Spec, []),
    
    %% 基本列定义
    TypeStr = postgres_type(Type, Options),
    NameStr = atom_to_list(Name),
    
    %% 添加列约束
    Constraints = generate_column_constraints(Options),
    
    io_lib:format("~s ~s~s", [NameStr, TypeStr, Constraints]).

%% @private
postgres_type(Type, Options) ->
    IsPrimaryKey = lists:member(primary_key, Options),
    %% Handle both direct types and map-based types
    ActualType = case Type of
        #{type := T} -> T;  % Extract type from map
        T -> T              % Direct type
    end,
    case ActualType of
        integer when IsPrimaryKey -> "SERIAL";
        integer -> "INTEGER";
        bigint -> "BIGINT";
        {string, Length} -> io_lib:format("VARCHAR(~p)", [Length]);
        string -> "VARCHAR(255)";
        text -> "TEXT";
        boolean -> "BOOLEAN";
        float -> "FLOAT";
        {decimal, P, S} -> io_lib:format("DECIMAL(~p,~p)", [P, S]);
        decimal -> "DECIMAL";
        datetime -> "TIMESTAMP";
        timestamp -> "TIMESTAMP";
        date -> "DATE";
        time -> "TIME";
        binary -> "BYTEA";
        json -> "JSONB";
        uuid -> "UUID";
        {enum, Values} -> generate_postgres_enum(Values);
        _ -> error({unsupported_type, ActualType})
    end.

%% @private
generate_postgres_enum(_Values) ->
    %% PostgreSQL 需要先创建 ENUM 类型
    %% 这里返回 VARCHAR 作为临时方案
    "VARCHAR(50)".

%% @private
generate_postgres_constraint({foreign_key, Column, RefTable, RefColumn, Options}) ->
    OnDelete = proplists:get_value(on_delete, Options, restrict),
    OnUpdate = proplists:get_value(on_update, Options, restrict),
    
    io_lib:format("FOREIGN KEY (~s) REFERENCES ~s(~s) ON DELETE ~s ON UPDATE ~s",
                  [atom_to_list(Column), atom_to_list(RefTable), 
                   atom_to_list(RefColumn),
                   on_action_to_sql(OnDelete), on_action_to_sql(OnUpdate)]);
generate_postgres_constraint({check, Expression}) ->
    io_lib:format("CHECK (~s)", [Expression]);
generate_postgres_constraint({unique, Columns}) ->
    ColumnList = string:join([atom_to_list(C) || C <- Columns], ", "),
    io_lib:format("UNIQUE (~s)", [ColumnList]).

%%====================================================================
%% MySQL DDL Generation
%%====================================================================

%% @private
generate_mysql_create_table(TableName, Schema) ->
    Fields = maps:get(fields, Schema, []),
    Constraints = maps:get(constraints, Schema, []),
    Options = maps:get(options, Schema, []),
    
    %% 生成列定义
    ColumnDefs = lists:map(fun(Field) ->
        generate_mysql_column(Field)
    end, expand_fields(Fields)),
    
    %% 生成表级约束
    ConstraintDefs = lists:map(fun(Constraint) ->
        generate_mysql_constraint(Constraint)
    end, Constraints),
    
    %% 组合所有定义
    AllDefs = ColumnDefs ++ ConstraintDefs,
    
    %% 表选项
    TableOptions = generate_mysql_table_options(Options),
    
    [
        "CREATE TABLE IF NOT EXISTS ", atom_to_list(TableName), " (\n    ",
        string:join(AllDefs, ",\n    "),
        "\n) ", TableOptions
    ].

%% @private
generate_mysql_column(Field) ->
    {Name, Spec} = parse_field_spec(Field),
    Type = maps:get(type, Spec),
    Options = maps:get(options, Spec, []),
    
    %% 基本列定义
    TypeStr = mysql_type(Type, Options),
    NameStr = atom_to_list(Name),
    
    %% 添加列约束
    Constraints = generate_column_constraints(Options),
    
    io_lib:format("~s ~s~s", [NameStr, TypeStr, Constraints]).

%% @private
mysql_type(Type, Options) ->
    IsPrimaryKey = lists:member(primary_key, Options),
    %% Handle both direct types and map-based types
    ActualType = case Type of
        #{type := T} -> T;  % Extract type from map
        T -> T              % Direct type
    end,
    case ActualType of
        integer when IsPrimaryKey -> "INT AUTO_INCREMENT";
        integer -> "INT";
        bigint -> "BIGINT";
        {string, Length} -> io_lib:format("VARCHAR(~p)", [Length]);
        string -> "VARCHAR(255)";
        text -> "TEXT";
        boolean -> "TINYINT(1)";
        float -> "FLOAT";
        {decimal, P, S} -> io_lib:format("DECIMAL(~p,~p)", [P, S]);
        decimal -> "DECIMAL";
        datetime -> "DATETIME";
        timestamp -> "TIMESTAMP";
        date -> "DATE";
        time -> "TIME";
        binary -> "BLOB";
        json -> "JSON";
        uuid -> "VARCHAR(36)";
        {enum, Values} -> generate_mysql_enum(Values);
        _ -> error({unsupported_type, ActualType})
    end.

%% @private
generate_mysql_enum(Values) ->
    ValueList = string:join(["'" ++ atom_to_list(V) ++ "'" || V <- Values], ", "),
    io_lib:format("ENUM(~s)", [ValueList]).

%% @private
generate_mysql_constraint(Constraint) ->
    %% MySQL 约束生成与 PostgreSQL 类似
    generate_postgres_constraint(Constraint).

%% @private
generate_mysql_table_options(Options) ->
    Engine = proplists:get_value(engine, Options, innodb),
    Charset = proplists:get_value(charset, Options, utf8mb4),
    Comment = proplists:get_value(comment, Options, ""),
    
    Parts = [
        io_lib:format("ENGINE=~s", [string:to_upper(atom_to_list(Engine))]),
        io_lib:format("DEFAULT CHARSET=~s", [atom_to_list(Charset)])
    ],
    
    Parts2 = case Comment of
        "" -> Parts;
        _ -> Parts ++ [io_lib:format("COMMENT='~s'", [Comment])]
    end,
    
    string:join(Parts2, " ").

%%====================================================================
%% SQLite DDL Generation
%%====================================================================

%% @private
generate_sqlite_create_table(TableName, Schema) ->
    Fields = maps:get(fields, Schema, []),
    
    %% 生成列定义
    ColumnDefs = lists:map(fun(Field) ->
        generate_sqlite_column(Field)
    end, expand_fields(Fields)),
    
    [
        "CREATE TABLE IF NOT EXISTS ", atom_to_list(TableName), " (\n    ",
        string:join(ColumnDefs, ",\n    "),
        "\n)"
    ].

%% @private
generate_sqlite_column(Field) ->
    {Name, Spec} = parse_field_spec(Field),
    Type = maps:get(type, Spec),
    Options = maps:get(options, Spec, []),
    
    %% 基本列定义
    TypeStr = sqlite_type(Type, Options),
    NameStr = atom_to_list(Name),
    
    %% 添加列约束
    Constraints = generate_column_constraints(Options),
    
    io_lib:format("~s ~s~s", [NameStr, TypeStr, Constraints]).

%% @private
sqlite_type(Type, Options) ->
    IsPrimaryKey = lists:member(primary_key, Options),
    %% Handle both direct types and map-based types
    ActualType = case Type of
        #{type := T} -> T;  % Extract type from map
        T -> T              % Direct type
    end,
    case ActualType of
        integer when IsPrimaryKey -> "INTEGER PRIMARY KEY AUTOINCREMENT";
        integer -> "INTEGER";
        bigint -> "INTEGER";
        {string, _} -> "TEXT";
        string -> "TEXT";
        text -> "TEXT";
        boolean -> "INTEGER";
        float -> "REAL";
        {decimal, _, _} -> "REAL";
        decimal -> "REAL";
        datetime -> "TEXT";
        timestamp -> "TEXT";
        date -> "TEXT";
        time -> "TEXT";
        binary -> "BLOB";
        json -> "TEXT";
        uuid -> "TEXT";
        _ -> error({unsupported_type, ActualType})
    end.

%%====================================================================
%% ALTER TABLE Generation
%%====================================================================

%% @private
generate_alter_statement(Adapter, TableName, {add_column, Name, Spec}) ->
    ColumnDef = case Adapter of
        postgres -> generate_postgres_column({Name, Spec});
        mysql -> generate_mysql_column({Name, Spec});
        sqlite -> generate_sqlite_column({Name, Spec})
    end,
    io_lib:format("ALTER TABLE ~s ADD COLUMN ~s", 
                  [atom_to_list(TableName), ColumnDef]);

generate_alter_statement(_Adapter, TableName, {drop_column, Name}) ->
    io_lib:format("ALTER TABLE ~s DROP COLUMN ~s", 
                  [atom_to_list(TableName), atom_to_list(Name)]);

generate_alter_statement(Adapter, TableName, {modify_column, Name, NewSpec, _OldSpec}) ->
    case Adapter of
        postgres ->
            %% PostgreSQL 使用多个 ALTER 语句
            generate_postgres_modify_column(TableName, Name, NewSpec);
        mysql ->
            %% MySQL 使用 MODIFY COLUMN
            ColumnDef = generate_mysql_column({Name, NewSpec}),
            io_lib:format("ALTER TABLE ~s MODIFY COLUMN ~s", 
                          [atom_to_list(TableName), ColumnDef]);
        sqlite ->
            %% SQLite 不支持直接修改列，需要重建表
            error({unsupported_operation, {modify_column, sqlite}})
    end;

generate_alter_statement(_Adapter, TableName, {add_index, Name, Spec}) ->
    generate_create_index(_Adapter, TableName, {Name, Spec});

generate_alter_statement(_Adapter, _TableName, {drop_index, Name}) ->
    generate_drop_index(_Adapter, Name);

generate_alter_statement(Adapter, TableName, {add_foreign_key, FK}) ->
    ConstraintDef = case Adapter of
        postgres -> generate_postgres_constraint(FK);
        mysql -> generate_mysql_constraint(FK);
        sqlite -> error({unsupported_operation, {add_foreign_key, sqlite}})
    end,
    io_lib:format("ALTER TABLE ~s ADD ~s", 
                  [atom_to_list(TableName), ConstraintDef]);

generate_alter_statement(_Adapter, TableName, {drop_foreign_key, Name}) ->
    io_lib:format("ALTER TABLE ~s DROP CONSTRAINT ~s", 
                  [atom_to_list(TableName), atom_to_list(Name)]).

%% @private
generate_postgres_modify_column(TableName, ColumnName, NewSpec) ->
    Type = maps:get(type, NewSpec),
    Options = maps:get(options, NewSpec, []),
    
    TypeStr = postgres_type(Type, Options),
    TableStr = atom_to_list(TableName),
    ColumnStr = atom_to_list(ColumnName),
    
    %% PostgreSQL 需要分别修改类型和约束
    AlterType = io_lib:format("ALTER TABLE ~s ALTER COLUMN ~s TYPE ~s",
                               [TableStr, ColumnStr, TypeStr]),
    
    %% 处理 NOT NULL 约束
    AlterNull = case lists:member(not_null, Options) of
        true ->
            io_lib:format("ALTER TABLE ~s ALTER COLUMN ~s SET NOT NULL",
                          [TableStr, ColumnStr]);
        false ->
            io_lib:format("ALTER TABLE ~s ALTER COLUMN ~s DROP NOT NULL",
                          [TableStr, ColumnStr])
    end,
    
    %% 处理默认值
    AlterDefault = case lists:keyfind(default, 1, Options) of
        {default, Value} ->
            io_lib:format("ALTER TABLE ~s ALTER COLUMN ~s SET DEFAULT ~s",
                          [TableStr, ColumnStr, format_default_value(Value)]);
        false ->
            []
    end,
    
    %% 返回多个 ALTER 语句
    [AlterType, ";\n", AlterNull] ++ 
    case AlterDefault of
        [] -> [];
        _ -> [";\n", AlterDefault]
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private 展开字段定义（处理 timestamps 等特殊字段）
expand_fields(Fields) ->
    lists:flatmap(fun(Field) ->
        case Field of
            timestamps ->
                [
                    {created_at, #{type => timestamp, options => [auto_now_add]}},
                    {updated_at, #{type => timestamp, options => [auto_now]}}
                ];
            _ ->
                [Field]
        end
    end, Fields).

%% @private 解析字段规格
parse_field_spec({Name}) ->
    %% 自动推导类型
    Type = infer_field_type(Name),
    {Name, #{type => Type, options => infer_field_options(Name)}};
parse_field_spec({Name, Type}) ->
    {Name, #{type => Type, options => []}};
parse_field_spec({Name, Type, Options}) ->
    {Name, #{type => Type, options => Options}}.

%% @private 推导字段类型
infer_field_type(id) -> integer;
infer_field_type(Name) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    case {lists:suffix("_id", NameStr), 
          lists:suffix("_at", NameStr),
          lists:prefix("is_", NameStr)} of
        {true, _, _} -> integer;
        {_, true, _} -> timestamp;
        {_, _, true} -> boolean;
        _ -> string
    end.

%% @private 推导字段选项
infer_field_options(id) -> [primary_key, auto_increment];
infer_field_options(created_at) -> [auto_now_add];
infer_field_options(updated_at) -> [auto_now];
infer_field_options(_) -> [].

%% @private 生成列约束
generate_column_constraints(Options) ->
    Constraints = lists:filtermap(fun(Option) ->
        case Option of
            primary_key -> {true, " PRIMARY KEY"};
            auto_increment -> false; % 已在类型中处理
            not_null -> {true, " NOT NULL"};
            unique -> {true, " UNIQUE"};
            {default, Value} -> {true, " DEFAULT " ++ format_default_value(Value)};
            default -> {true, " DEFAULT NULL"};
            auto_now -> false; % 需要触发器处理
            auto_now_add -> false; % 需要触发器处理
            _ -> false
        end
    end, Options),
    
    lists:flatten(Constraints).

%% @private 格式化默认值
format_default_value(null) -> "NULL";
format_default_value(true) -> "TRUE";
format_default_value(false) -> "FALSE";
format_default_value(Value) when is_integer(Value) -> integer_to_list(Value);
format_default_value(Value) when is_float(Value) -> float_to_list(Value);
format_default_value(Value) when is_atom(Value) -> "'" ++ atom_to_list(Value) ++ "'";
format_default_value(Value) when is_list(Value) -> "'" ++ Value ++ "'";
format_default_value(Value) when is_binary(Value) -> "'" ++ binary_to_list(Value) ++ "'".

%% @private 格式化索引列
format_index_columns(Columns, Order) when is_atom(Order), Order =/= asc ->
    %% 只有当不是默认的 asc 时才添加顺序
    ColumnStrs = [atom_to_list(C) ++ " " ++ string:to_upper(atom_to_list(Order)) 
                  || C <- Columns],
    string:join(ColumnStrs, ", ");
format_index_columns(Columns, _) when is_list(Columns) ->
    %% 简单列列表，不添加顺序
    string:join([atom_to_list(C) || C <- Columns], ", ").

%% @private 转换 ON DELETE/UPDATE 动作
on_action_to_sql(cascade) -> "CASCADE";
on_action_to_sql(restrict) -> "RESTRICT";
on_action_to_sql(set_null) -> "SET NULL";
on_action_to_sql(no_action) -> "NO ACTION";
on_action_to_sql(set_default) -> "SET DEFAULT".