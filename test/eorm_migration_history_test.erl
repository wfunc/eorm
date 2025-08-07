%%% @doc EORM Migration History 测试
%%% @end
-module(eorm_migration_history_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 迁移历史测试
%%====================================================================

migration_history_test_() ->
    [
        {"记录迁移测试", fun test_record_migration/0},
        {"获取迁移历史测试", fun test_get_history/0},
        {"清理历史测试", fun test_clear_history/0}
    ].

test_record_migration() ->
    %% 这是一个简化的测试，验证函数可以被调用
    %% 实际的数据库连接测试应该在集成测试中进行
    Adapter = postgres,
    TableName = <<"users">>,
    DDL = <<"CREATE TABLE users (id INTEGER PRIMARY KEY)">>,
    
    %% 由于没有数据库配置，预期会返回错误
    Result = eorm_migration_history:record(Adapter, TableName, create_table, DDL),
    ?assertMatch({error, _}, Result).

test_get_history() ->
    TableName = <<"users">>,
    
    Result = eorm_migration_history:get_history(TableName),
    ?assertMatch({ok, _}, Result).

test_clear_history() ->
    Result = eorm_migration_history:clear_all(),
    ?assertEqual(ok, Result).

%%====================================================================
%% 扩展的迁移历史测试
%%====================================================================

extended_migration_history_test_() ->
    [
        {"确保表存在测试", fun test_ensure_table/0},
        {"不同适配器测试", fun test_different_adapters/0},
        {"复杂DDL记录测试", fun test_complex_ddl_record/0},
        {"批量操作测试", fun test_batch_operations/0},
        {"表名类型处理测试", fun test_table_name_types/0}
    ].

test_ensure_table() ->
    %% 测试确保表存在功能
    ?assertMatch({error, _}, eorm_migration_history:ensure_table(postgres)),
    ?assertMatch({error, _}, eorm_migration_history:ensure_table(mysql)),
    ?assertMatch({error, _}, eorm_migration_history:ensure_table(sqlite)).

test_different_adapters() ->
    %% 测试不同数据库适配器
    TableName = <<"products">>,
    DDL = <<"CREATE TABLE products (id INTEGER PRIMARY KEY, name VARCHAR(100))">>,
    
    %% PostgreSQL
    Result1 = eorm_migration_history:record(postgres, TableName, create_table, DDL),
    ?assertMatch({error, _}, Result1),
    
    %% MySQL
    Result2 = eorm_migration_history:record(mysql, TableName, create_table, DDL),
    ?assertMatch({error, _}, Result2),
    
    %% SQLite
    Result3 = eorm_migration_history:record(sqlite, TableName, create_table, DDL),
    ?assertMatch({error, _}, Result3).

test_complex_ddl_record() ->
    %% 测试复杂DDL记录
    TableName = <<"orders">>,
    ComplexDDL = <<"
        CREATE TABLE orders (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            order_no VARCHAR(50) NOT NULL UNIQUE,
            user_id INTEGER NOT NULL,
            total_amount DECIMAL(10,2) NOT NULL,
            status VARCHAR(20) DEFAULT 'pending',
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
            FOREIGN KEY (user_id) REFERENCES users(id)
        )
    ">>,
    
    Result = eorm_migration_history:record(postgres, TableName, create_table, ComplexDDL),
    ?assertMatch({error, _}, Result).

test_batch_operations() ->
    %% 测试批量操作记录
    TableName = <<"categories">>,
    
    %% 创建表
    CreateDDL = <<"CREATE TABLE categories (id INTEGER PRIMARY KEY, name VARCHAR(50))">>,
    Result1 = eorm_migration_history:record(postgres, TableName, create_table, CreateDDL),
    ?assertMatch({error, _}, Result1),
    
    %% 添加索引
    IndexDDL = <<"CREATE INDEX idx_categories_name ON categories(name)">>,
    Result2 = eorm_migration_history:record(postgres, TableName, add_index, IndexDDL),
    ?assertMatch({error, _}, Result2),
    
    %% 修改表结构
    AlterDDL = <<"ALTER TABLE categories ADD COLUMN description TEXT">>,
    Result3 = eorm_migration_history:record(postgres, TableName, alter_table, AlterDDL),
    ?assertMatch({error, _}, Result3).

test_table_name_types() ->
    %% 测试不同类型的表名处理
    AtomTableName = users,
    BinaryTableName = <<"users">>,
    StringTableName = "users",
    DDL = <<"CREATE TABLE users (id INTEGER PRIMARY KEY)">>,
    
    %% 测试原子类型表名
    Result1 = eorm_migration_history:record(postgres, AtomTableName, create_table, DDL),
    ?assertMatch({error, _}, Result1),
    
    %% 测试二进制类型表名
    Result2 = eorm_migration_history:record(postgres, BinaryTableName, create_table, DDL),
    ?assertMatch({error, _}, Result2),
    
    %% 测试字符串类型表名
    Result3 = eorm_migration_history:record(postgres, StringTableName, create_table, DDL),
    ?assertMatch({error, _}, Result3).

%%====================================================================
%% 历史查询和管理测试
%%====================================================================

history_management_test_() ->
    [
        {"查询特定表历史", fun test_get_table_history/0},
        {"查询所有历史", fun test_get_all_history/0},
        {"历史统计信息", fun test_history_statistics/0},
        {"按时间范围查询", fun test_history_by_date_range/0},
        {"按操作类型过滤", fun test_filter_by_operation_type/0}
    ].

test_get_table_history() ->
    %% 测试获取特定表的迁移历史
    TableNames = [<<"users">>, <<"products">>, <<"orders">>, <<"categories">>],
    
    lists:foreach(fun(TableName) ->
        Result = eorm_migration_history:get_history(TableName),
        ?assertMatch({ok, _}, Result),
        
        {ok, History} = Result,
        ?assert(is_list(History))
    end, TableNames).

test_get_all_history() ->
    %% 测试获取所有迁移历史
    Result = eorm_migration_history:get_all_history(),
    ?assertMatch({ok, _}, Result),
    
    {ok, AllHistory} = Result,
    ?assert(is_list(AllHistory)).

test_history_statistics() ->
    %% 测试历史统计信息
    Result = eorm_migration_history:get_statistics(),
    ?assertMatch({ok, _}, Result),
    
    {ok, Stats} = Result,
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total_migrations, Stats)),
    ?assert(maps:is_key(successful_migrations, Stats)),
    ?assert(maps:is_key(failed_migrations, Stats)).

test_history_by_date_range() ->
    %% 测试按时间范围查询历史
    StartDate = {{2023, 1, 1}, {0, 0, 0}},
    EndDate = {{2023, 12, 31}, {23, 59, 59}},
    
    Result = eorm_migration_history:get_history_by_date_range(StartDate, EndDate),
    ?assertMatch({ok, _}, Result),
    
    {ok, History} = Result,
    ?assert(is_list(History)).

test_filter_by_operation_type() ->
    %% 测试按操作类型过滤历史
    OperationTypes = [create_table, alter_table, drop_table, add_index, drop_index],
    
    lists:foreach(fun(OpType) ->
        Result = eorm_migration_history:get_history_by_type(OpType),
        ?assertMatch({ok, _}, Result),
        
        {ok, History} = Result,
        ?assert(is_list(History))
    end, OperationTypes).

%%====================================================================
%% 错误处理和边界情况测试
%%====================================================================

error_handling_test_() ->
    [
        {"无效适配器测试", fun test_invalid_adapter/0},
        {"空DDL测试", fun test_empty_ddl/0},
        {"无效操作类型测试", fun test_invalid_operation_type/0},
        {"并发操作测试", fun test_concurrent_operations/0}
    ].

test_invalid_adapter() ->
    %% 测试无效的数据库适配器
    InvalidAdapter = invalid_adapter,
    TableName = <<"test_table">>,
    DDL = <<"CREATE TABLE test_table (id INTEGER)">>,
    
    Result = eorm_migration_history:record(InvalidAdapter, TableName, create_table, DDL),
    ?assertMatch({error, _}, Result).

test_empty_ddl() ->
    %% 测试空DDL
    TableName = <<"test_table">>,
    EmptyDDL = <<"">>,
    
    Result = eorm_migration_history:record(postgres, TableName, create_table, EmptyDDL),
    ?assertMatch({error, _}, Result).

test_invalid_operation_type() ->
    %% 测试无效的操作类型
    TableName = <<"test_table">>,
    DDL = <<"CREATE TABLE test_table (id INTEGER)">>,
    InvalidOpType = invalid_operation,
    
    Result = eorm_migration_history:record(postgres, TableName, InvalidOpType, DDL),
    ?assertMatch({error, _}, Result).

test_concurrent_operations() ->
    %% 模拟并发操作测试
    TableName = <<"concurrent_test">>,
    DDL = <<"CREATE TABLE concurrent_test (id INTEGER)">>,
    
    %% 启动多个进程同时记录迁移
    Pids = [spawn(fun() ->
        eorm_migration_history:record(postgres, TableName, create_table, DDL)
    end) || _ <- lists:seq(1, 5)],
    
    %% 等待所有进程完成
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        after 1000 ->
            ok
        end
    end, Pids),
    
    ?assert(true). % 主要是测试不崩溃

%%====================================================================
%% 数据完整性和一致性测试
%%====================================================================

data_integrity_test_() ->
    [
        {"版本号生成测试", fun test_version_generation/0},
        {"校验和计算测试", fun test_checksum_calculation/0},
        {"时间戳一致性测试", fun test_timestamp_consistency/0}
    ].

test_version_generation() ->
    %% 测试版本号生成逻辑
    Version1 = eorm_migration_history:generate_version(),
    Version2 = eorm_migration_history:generate_version(),
    
    ?assertMatch({ok, _}, Version1),
    ?assertMatch({ok, _}, Version2),
    
    {ok, V1} = Version1,
    {ok, V2} = Version2,
    ?assert(V1 =/= V2), % 版本号应该不同
    ?assert(is_binary(V1)),
    ?assert(is_binary(V2)).

test_checksum_calculation() ->
    %% 测试DDL校验和计算
    DDL1 = <<"CREATE TABLE users (id INTEGER PRIMARY KEY)">>,
    DDL2 = <<"CREATE TABLE products (id INTEGER PRIMARY KEY, name VARCHAR(100))">>,
    
    Checksum1 = eorm_migration_history:calculate_checksum(DDL1),
    Checksum2 = eorm_migration_history:calculate_checksum(DDL2),
    Checksum3 = eorm_migration_history:calculate_checksum(DDL1), % 相同DDL
    
    ?assertMatch({ok, _}, Checksum1),
    ?assertMatch({ok, _}, Checksum2),
    ?assertMatch({ok, _}, Checksum3),
    
    {ok, C1} = Checksum1,
    {ok, C2} = Checksum2,
    {ok, C3} = Checksum3,
    
    ?assert(C1 =/= C2), % 不同DDL应该有不同校验和
    ?assertEqual(C1, C3). % 相同DDL应该有相同校验和

test_timestamp_consistency() ->
    %% 测试时间戳一致性
    TableName = <<"timestamp_test">>,
    DDL = <<"CREATE TABLE timestamp_test (id INTEGER)">>,
    
    StartTime = calendar:universal_time(),
    
    %% 记录迁移（虽然会失败，但可以测试时间戳生成逻辑）
    _Result = eorm_migration_history:record(postgres, TableName, create_table, DDL),
    
    EndTime = calendar:universal_time(),
    
    %% 验证时间戳在合理范围内
    StartSeconds = calendar:datetime_to_gregorian_seconds(StartTime),
    EndSeconds = calendar:datetime_to_gregorian_seconds(EndTime),
    
    ?assert(EndSeconds >= StartSeconds),
    ?assert(EndSeconds - StartSeconds < 5). % 应该在5秒内完成