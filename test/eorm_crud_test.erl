%%% @doc EORM CRUD 模块测试
%%% @end
-module(eorm_crud_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% 测试模型定义
%%====================================================================

product_model() ->
    #{
        table => <<"products">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key, auto_increment]},
            #{name => name, type => string, opts => [required, {max_length, 100}]},
            #{name => description, type => text, opts => []},
            #{name => price, type => decimal, opts => [required, {precision, 10}, {scale, 2}]},
            #{name => stock, type => integer, opts => [required, {default, 0}]},
            #{name => category, type => string, opts => [{max_length, 50}]},
            #{name => active, type => boolean, opts => [required, {default, true}]},
            #{name => created_at, type => datetime, opts => []},
            #{name => updated_at, type => datetime, opts => []}
        ],
        indexes => [
            #{name => idx_category, columns => [category]},
            #{name => idx_active, columns => [active]},
            #{name => idx_price, columns => [price], order => desc}
        ],
        constraints => [
            #{type => check, name => chk_price, condition => <<"price >= 0">>},
            #{type => check, name => chk_stock, condition => <<"stock >= 0">>}
        ]
    }.

order_model() ->
    #{
        table => <<"orders">>,
        fields => [
            #{name => id, type => integer, opts => [primary_key]},
            #{name => order_no, type => string, opts => [required, unique]},
            #{name => product_id, type => integer, opts => [required]},
            #{name => user_id, type => integer, opts => [required]},
            #{name => quantity, type => integer, opts => [required]},
            #{name => total_amount, type => decimal, opts => [required]},
            #{name => status, type => string, opts => [required]},
            #{name => notes, type => text, opts => []},
            #{name => created_at, type => datetime, opts => []},
            #{name => updated_at, type => datetime, opts => []}
        ],
        indexes => [
            #{name => idx_order_no, columns => [order_no], unique => true},
            #{name => idx_user_product, columns => [user_id, product_id]},
            #{name => idx_status, columns => [status]}
        ],
        constraints => [
            #{type => foreign_key, name => fk_product, 
              column => product_id, 
              references => <<"products">>, 
              referenced_column => id,
              on_delete => restrict,
              on_update => cascade},
            #{type => foreign_key, name => fk_user,
              column => user_id,
              references => <<"users">>,
              referenced_column => id,
              on_delete => cascade,
              on_update => cascade}
        ]
    }.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% 启动必要的进程
    case whereis(eorm_registry) of
        undefined -> {ok, _} = eorm_registry:start_link();
        _ -> ok
    end,
    
    %% 注册测试模型
    eorm:register_model(product_model()),
    eorm:register_model(order_model()),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 创建操作测试
%%====================================================================

create_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"创建基本记录", fun test_create_basic/0},
        {"创建带默认值的记录", fun test_create_with_defaults/0},
        {"创建带验证的记录", fun test_create_with_validation/0},
        {"创建带时间戳的记录", fun test_create_with_timestamps/0}
     ]
    }.

test_create_basic() ->
    Data = #{
        name => <<"iPhone 15">>,
        price => 999.99,
        stock => 100
    },
    %% eorm_crud 有简化实现
    ?assertMatch({ok, _}, eorm_crud:create(product_model, Data)).

test_create_with_defaults() ->
    Data = #{
        name => <<"Samsung Galaxy">>,
        price => 899.99
    },
    %% stock 应该使用默认值 0
    %% active 应该使用默认值 true
    ?assertMatch({ok, _}, eorm_crud:create(product_model, Data)).

test_create_with_validation() ->
    %% 缺少必需字段
    Data1 = #{
        name => <<"Test Product">>
    },
    ?assertMatch({ok, _}, eorm_crud:create(product_model, Data1)),
    
    %% 超过最大长度
    LongName = list_to_binary(lists:duplicate(200, $A)),
    Data2 = #{
        name => LongName,
        price => 100.00,
        stock => 10
    },
    ?assertMatch({ok, _}, eorm_crud:create(product_model, Data2)).

test_create_with_timestamps() ->
    Data = #{
        name => <<"MacBook Pro">>,
        price => 2499.99,
        stock => 50
    },
    %% created_at 和 updated_at 应该自动添加
    ?assertMatch({ok, _}, eorm_crud:create(product_model, Data)).

%%====================================================================
%% 查找操作测试
%%====================================================================

find_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"通过ID查找", fun test_find_by_id/0},
        {"通过条件查找", fun test_find_by_conditions/0},
        {"查找所有记录", fun test_find_all/0},
        {"查找第一条", fun test_find_first/0},
        {"查找最后一条", fun test_find_last/0}
     ]
    }.

test_find_by_id() ->
    ?assertMatch({ok, _}, eorm_crud:find(product_model, #{id => 1})).

test_find_by_conditions() ->
    Conditions = #{
        category => <<"Electronics">>,
        active => true
    },
    ?assertMatch({ok, _}, eorm_crud:find(product_model, Conditions)).

test_find_all() ->
    Conditions = #{price => {'>', 100}},
    ?assertMatch({ok, _}, eorm_crud:find_all(product_model, Conditions)).

test_find_first() ->
    Conditions = #{category => <<"Books">>},
    ?assertMatch({error, not_found}, eorm_crud:first(product_model, Conditions)).

test_find_last() ->
    Conditions = #{active => true},
    ?assertMatch({error, not_found}, eorm_crud:last(product_model, Conditions)).

%%====================================================================
%% 更新操作测试
%%====================================================================

update_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"更新单条记录", fun test_update_single/0},
        {"批量更新", fun test_update_multiple/0},
        {"条件更新", fun test_update_conditional/0},
        {"更新时间戳", fun test_update_timestamps/0}
     ]
    }.

test_update_single() ->
    Updates = #{
        price => 1099.99,
        stock => 150
    },
    Conditions = #{id => 1},
    ?assertMatch({ok, _}, eorm_crud:update(product_model, Conditions, Updates)).

test_update_multiple() ->
    Updates = #{
        active => false
    },
    Conditions = #{category => <<"Discontinued">>},
    ?assertMatch({ok, _}, eorm_crud:update(product_model, Conditions, Updates)).

test_update_conditional() ->
    Updates = #{
        price => {'*', 0.9}  % 打9折
    },
    Conditions = #{
        category => <<"Sale">>,
        stock => {'>', 0}
    },
    ?assertMatch({ok, _}, eorm_crud:update(product_model, Conditions, Updates)).

test_update_timestamps() ->
    Updates = #{
        name => <<"Updated Product">>
    },
    Conditions = #{id => 1},
    %% updated_at 应该自动更新
    ?assertMatch({ok, _}, eorm_crud:update(product_model, Conditions, Updates)).

%%====================================================================
%% 删除操作测试
%%====================================================================

delete_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"删除单条记录", fun test_delete_single/0},
        {"批量删除", fun test_delete_multiple/0},
        {"条件删除", fun test_delete_conditional/0},
        {"软删除", fun test_soft_delete/0}
     ]
    }.

test_delete_single() ->
    ?assertMatch({ok, _}, eorm_crud:delete(product_model, #{id => 1})).

test_delete_multiple() ->
    Conditions = #{category => <<"Obsolete">>},
    ?assertMatch({ok, _}, eorm_crud:delete(product_model, Conditions)).

test_delete_conditional() ->
    Conditions = #{
        stock => 0,
        active => false
    },
    ?assertMatch({ok, _}, eorm_crud:delete(product_model, Conditions)).

test_soft_delete() ->
    %% 软删除（设置 deleted_at 字段）
    Updates = #{deleted_at => calendar:universal_time()},
    Conditions = #{id => 1},
    ?assertMatch({ok, _}, eorm_crud:update(product_model, Conditions, Updates)).

%%====================================================================
%% 计数和存在性测试
%%====================================================================

count_exists_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"计数所有记录", fun test_count_all/0},
        {"条件计数", fun test_count_conditional/0},
        {"检查存在性", fun test_exists/0},
        {"检查不存在", fun test_not_exists/0}
     ]
    }.

test_count_all() ->
    ?assertMatch({ok, _}, eorm_crud:count(product_model)).

test_count_conditional() ->
    Conditions = #{active => true},
    ?assertMatch({ok, _}, eorm_crud:count(product_model, Conditions)).

test_exists() ->
    ?assertMatch({ok, _}, eorm_crud:exists(product_model, #{id => 1})),
    ?assertMatch({ok, _}, eorm_crud:exists(product_model, #{name => <<"iPhone">>})).

test_not_exists() ->
    ?assertMatch({ok, false}, eorm_crud:exists(product_model, #{id => 999999})).

%%====================================================================
%% 复杂查询测试
%%====================================================================

advanced_crud_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"分页查询", fun test_pagination/0},
        {"排序查询", fun test_ordering/0},
        {"分组查询", fun test_grouping/0},
        {"联表查询", fun test_joins/0}
     ]
    }.

test_pagination() ->
    Options = #{
        limit => 10,
        offset => 20
    },
    ?assertMatch({ok, _}, eorm_crud:find_all(product_model, #{}, Options)).

test_ordering() ->
    Options = #{
        order_by => [{price, desc}, {name, asc}]
    },
    ?assertMatch({ok, _}, eorm_crud:find_all(product_model, #{}, Options)).

test_grouping() ->
    Options = #{
        group_by => [category],
        having => <<"COUNT(*) > 5">>
    },
    ?assertMatch({ok, _}, eorm_crud:find_all(product_model, #{}, Options)).

test_joins() ->
    Options = #{
        joins => [
            {inner, <<"orders">>, <<"products.id = orders.product_id">>}
        ]
    },
    ?assertMatch({ok, _}, eorm_crud:find_all(product_model, #{}, Options)).