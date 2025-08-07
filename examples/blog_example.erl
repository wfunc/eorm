%%% @doc EORM博客应用完整示例
%%% 展示真实的EORM使用场景，包括模型定义、迁移、CRUD操作等
%%% @end
-module(blog_example).

%% API exports
-export([
    start/0,
    setup_database/0,
    create_sample_data/0,
    run_queries/0,
    cleanup/0
]).

-include_lib("eorm/include/eorm.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc 启动博客应用示例
start() ->
    io:format("=== EORM 博客应用示例 ===~n~n"),
    
    %% 1. 启动EORM应用
    io:format("1. 启动EORM应用...~n"),
    application:ensure_all_started(eorm),
    
    %% 2. 配置数据库连接
    setup_database(),
    
    %% 3. 注册模型并进行自动迁移
    io:format("2. 注册模型并自动迁移...~n"),
    register_models(),
    auto_migrate(),
    
    %% 4. 创建示例数据
    io:format("3. 创建示例数据...~n"),
    create_sample_data(),
    
    %% 5. 演示各种查询操作
    io:format("4. 演示查询操作...~n"),
    run_queries(),
    
    %% 6. 演示事务操作
    io:format("5. 演示事务操作...~n"),
    transaction_example(),
    
    io:format("~n=== 示例完成！ ===~n"),
    ok.

%% @doc 设置数据库连接
setup_database() ->
    %% 配置PostgreSQL连接（使用Docker容器）
    application:set_env(eorm, default_adapter, postgres),
    application:set_env(eorm, databases, [
        {postgres, #{
            adapter => eorm_postgres_adapter,
            host => "localhost",
            port => 5432,
            database => "eorm_blog_example",
            username => "dev",
            password => "123",
            pool_size => 5
        }}
    ]),
    
    %% 设置适配器
    eorm_adapter:set_adapter(postgres),
    
    io:format("   ✓ 数据库连接配置完成~n").

%% @doc 注册所有模型
register_models() ->
    Models = [user_model, post_model, comment_model, tag_model],
    lists:foreach(fun(Model) ->
        case eorm_registry:register_model(Model) of
            ok -> 
                io:format("   ✓ 模型 ~p 注册成功~n", [Model]);
            {error, Reason} -> 
                io:format("   ✗ 模型 ~p 注册失败: ~p~n", [Model, Reason])
        end
    end, Models).

%% @doc 执行自动迁移
auto_migrate() ->
    Models = [user_model, post_model, comment_model, tag_model],
    Options = #{adapter => postgres, mode => force},
    
    case eorm_auto_migrate:auto_migrate(Models, Options) of
        {ok, Plan} ->
            io:format("   ✓ 自动迁移成功~n"),
            io:format("   执行的操作: ~p~n", [Plan]);
        {error, Reason} ->
            io:format("   ✗ 自动迁移失败: ~p~n", [Reason])
    end.

%% @doc 创建示例数据
create_sample_data() ->
    try
        %% 创建用户
        {ok, User1} = create_user("Alice Johnson", "alice@example.com", 28),
        {ok, User2} = create_user("Bob Smith", "bob@example.com", 35),
        {ok, User3} = create_user("Carol Davis", "carol@example.com", 24),
        
        io:format("   ✓ 创建了3个用户~n"),
        
        %% 创建标签
        {ok, Tag1} = create_tag("Erlang", "函数式编程语言"),
        {ok, Tag2} = create_tag("数据库", "数据存储和管理"),
        {ok, Tag3} = create_tag("ORM", "对象关系映射"),
        
        io:format("   ✓ 创建了3个标签~n"),
        
        %% 创建文章
        {ok, Post1} = create_post(1, "EORM使用指南", "详细介绍如何使用EORM进行数据库操作", true),
        {ok, Post2} = create_post(2, "Erlang并发编程", "探讨Erlang的并发特性和Actor模型", true),
        {ok, Post3} = create_post(1, "数据库设计原则", "分享数据库设计的最佳实践", false),
        
        io:format("   ✓ 创建了3篇文章~n"),
        
        %% 创建评论
        {ok, _Comment1} = create_comment(2, 1, "很棒的文章！学到了很多。"),
        {ok, _Comment2} = create_comment(3, 1, "感谢分享，期待更多内容。"),
        {ok, _Comment3} = create_comment(1, 2, "并发编程确实很重要。"),
        
        io:format("   ✓ 创建了3个评论~n"),
        
        ok
    catch
        _:Error ->
            io:format("   ✗ 创建示例数据失败: ~p~n", [Error])
    end.

%% @doc 演示各种查询操作
run_queries() ->
    try
        %% 1. 简单查询 - 查找所有已发布的文章
        io:format("   查询1: 查找所有已发布的文章~n"),
        Posts = simple_query_published_posts(),
        io:format("   结果: 找到 ~p 篇已发布文章~n", [length(Posts)]),
        
        %% 2. 复杂查询 - 查找特定用户的文章和评论数
        io:format("   查询2: 查找用户Alice的文章统计~n"),
        UserStats = query_user_stats("Alice Johnson"),
        io:format("   结果: ~p~n", [UserStats]),
        
        %% 3. 关联查询 - 查找文章及其评论
        io:format("   查询3: 查找文章及其评论~n"),
        PostsWithComments = query_posts_with_comments(),
        io:format("   结果: 找到 ~p 篇文章的评论信息~n", [length(PostsWithComments)]),
        
        %% 4. 聚合查询 - 统计信息
        io:format("   查询4: 获取博客统计信息~n"),
        Stats = get_blog_stats(),
        io:format("   统计: ~p~n", [Stats]),
        
        ok
    catch
        _:Error ->
            io:format("   ✗ 查询演示失败: ~p~n", [Error])
    end.

%% @doc 演示事务操作
transaction_example() ->
    try
        %% 事务示例：创建用户、文章和标签的关联
        Result = eorm:transaction(fun() ->
            %% 在事务中创建新用户
            {ok, NewUser} = create_user("David Wilson", "david@example.com", 30),
            
            %% 创建新文章
            {ok, NewPost} = create_post(NewUser, "事务处理示例", "演示EORM事务功能", true),
            
            %% 创建新标签
            {ok, NewTag} = create_tag("事务", "数据库事务处理"),
            
            %% 返回创建的对象
            {NewUser, NewPost, NewTag}
        end),
        
        case Result of
            {ok, {User, Post, Tag}} ->
                io:format("   ✓ 事务成功: 创建用户 ~p, 文章 ~p, 标签 ~p~n", 
                         [User, Post, Tag]);
            {error, Reason} ->
                io:format("   ✗ 事务失败: ~p~n", [Reason])
        end
    catch
        _:Error ->
            io:format("   ✗ 事务演示失败: ~p~n", [Error])
    end.

%% @doc 清理示例数据
cleanup() ->
    try
        %% 删除所有测试数据
        eorm_adapter:execute_ddl(postgres, <<"DROP TABLE IF EXISTS comments CASCADE">>),
        eorm_adapter:execute_ddl(postgres, <<"DROP TABLE IF EXISTS posts CASCADE">>),
        eorm_adapter:execute_ddl(postgres, <<"DROP TABLE IF EXISTS users CASCADE">>),
        eorm_adapter:execute_ddl(postgres, <<"DROP TABLE IF EXISTS tags CASCADE">>),
        eorm_adapter:execute_ddl(postgres, <<"DROP TABLE IF EXISTS eorm_migration_history CASCADE">>),
        
        io:format("✓ 清理完成~n")
    catch
        _:Error ->
            io:format("✗ 清理失败: ~p~n", [Error])
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc 创建用户
create_user(Name, Email, Age) ->
    try
        SQL = <<"INSERT INTO users (name, email, age, created_at) VALUES ($1, $2, $3, NOW()) RETURNING id">>,
        case eorm_adapter:query(postgres, SQL, [Name, Email, Age]) of
            {ok, [Row]} ->
                Id = element(1, Row),
                {ok, #{id => Id, name => Name, email => Email, age => Age}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end.

%% @doc 创建标签
create_tag(Name, Description) ->
    try
        SQL = <<"INSERT INTO tags (name, description, created_at) VALUES ($1, $2, NOW()) RETURNING id">>,
        case eorm_adapter:query(postgres, SQL, [Name, Description]) of
            {ok, [Row]} ->
                Id = element(1, Row),
                {ok, #{id => Id, name => Name, description => Description}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end.

%% @doc 创建文章
create_post(UserId, Title, Content, Published) when is_integer(UserId) ->
    try
        SQL = <<"INSERT INTO posts (user_id, title, content, published, created_at) VALUES ($1, $2, $3, $4, NOW()) RETURNING id">>,
        case eorm_adapter:query(postgres, SQL, [UserId, Title, Content, Published]) of
            {ok, [Row]} ->
                Id = element(1, Row),
                {ok, #{id => Id, user_id => UserId, title => Title, content => Content, published => Published}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end;
create_post(User, Title, Content, Published) when is_map(User) ->
    UserId = maps:get(id, User),
    create_post(UserId, Title, Content, Published).

%% @doc 创建评论
create_comment(UserId, PostId, Content) ->
    try
        SQL = <<"INSERT INTO comments (user_id, post_id, content, created_at) VALUES ($1, $2, $3, NOW()) RETURNING id">>,
        case eorm_adapter:query(postgres, SQL, [UserId, PostId, Content]) of
            {ok, [Row]} ->
                Id = element(1, Row),
                {ok, #{id => Id, user_id => UserId, post_id => PostId, content => Content}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end.

%% @doc 查询已发布的文章
simple_query_published_posts() ->
    try
        SQL = <<"SELECT id, title, content FROM posts WHERE published = true ORDER BY created_at DESC">>,
        case eorm_adapter:query(postgres, SQL, []) of
            {ok, Rows} ->
                [#{id => Id, title => Title, content => Content} || {Id, Title, Content} <- Rows];
            {error, _} ->
                []
        end
    catch
        _:_ -> []
    end.

%% @doc 查询用户统计信息
query_user_stats(UserName) ->
    try
        SQL = <<"
            SELECT u.name, COUNT(p.id) as post_count, COUNT(c.id) as comment_count
            FROM users u
            LEFT JOIN posts p ON u.id = p.user_id
            LEFT JOIN comments c ON u.id = c.user_id
            WHERE u.name = $1
            GROUP BY u.id, u.name
        ">>,
        case eorm_adapter:query(postgres, SQL, [UserName]) of
            {ok, [Row]} ->
                {Name, PostCount, CommentCount} = Row,
                #{name => Name, posts => PostCount, comments => CommentCount};
            _ ->
                #{name => UserName, posts => 0, comments => 0}
        end
    catch
        _:_ -> #{name => UserName, posts => 0, comments => 0}
    end.

%% @doc 查询文章及其评论
query_posts_with_comments() ->
    try
        SQL = <<"
            SELECT p.id, p.title, COUNT(c.id) as comment_count
            FROM posts p
            LEFT JOIN comments c ON p.id = c.post_id
            WHERE p.published = true
            GROUP BY p.id, p.title
            ORDER BY comment_count DESC
        ">>,
        case eorm_adapter:query(postgres, SQL, []) of
            {ok, Rows} ->
                [#{id => Id, title => Title, comment_count => Count} || {Id, Title, Count} <- Rows];
            _ ->
                []
        end
    catch
        _:_ -> []
    end.

%% @doc 获取博客统计信息
get_blog_stats() ->
    try
        %% 统计用户数
        {ok, [{UserCount}]} = eorm_adapter:query(postgres, <<"SELECT COUNT(*) FROM users">>, []),
        
        %% 统计文章数
        {ok, [{PostCount}]} = eorm_adapter:query(postgres, <<"SELECT COUNT(*) FROM posts">>, []),
        
        %% 统计已发布文章数
        {ok, [{PublishedCount}]} = eorm_adapter:query(postgres, <<"SELECT COUNT(*) FROM posts WHERE published = true">>, []),
        
        %% 统计评论数
        {ok, [{CommentCount}]} = eorm_adapter:query(postgres, <<"SELECT COUNT(*) FROM comments">>, []),
        
        %% 统计标签数
        {ok, [{TagCount}]} = eorm_adapter:query(postgres, <<"SELECT COUNT(*) FROM tags">>, []),
        
        #{
            users => UserCount,
            posts => PostCount,
            published_posts => PublishedCount,
            comments => CommentCount,
            tags => TagCount
        }
    catch
        _:_ -> 
            #{users => 0, posts => 0, published_posts => 0, comments => 0, tags => 0}
    end.

%%====================================================================
%% Model Definitions
%%====================================================================

%% 这些模型定义需要在实际使用中创建为独立的模块文件
%% 这里作为示例展示结构

%% user_model.erl
-module(user_model).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => users,
        fields => [
            {id},                                    % integer, primary_key, auto_increment
            {name, {string, 100}, [not_null]},      % 用户姓名
            {email, {string, 255}, [unique, not_null]}, % 邮箱，唯一
            {age, integer, [{default, 18}]},        % 年龄，默认18
            {bio, text, []},                        % 个人简介
            {is_active, boolean, [{default, true}]}, % 是否激活
            {created_at, timestamp, []},            % 创建时间
            {updated_at, timestamp, []}             % 更新时间
        ],
        indexes => [
            {idx_users_email, [email], [unique]},
            {idx_users_name, [name], []}
        ],
        associations => [
            {has_many, posts, post_model},
            {has_many, comments, comment_model}
        ]
    }.

%% post_model.erl  
-module(post_model).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => posts,
        fields => [
            {id},
            {user_id, integer, [not_null]},         % 外键，关联用户
            {title, {string, 255}, [not_null]},     % 标题
            {content, text, []},                    % 内容
            {published, boolean, [{default, false}]}, % 是否发布
            {view_count, integer, [{default, 0}]},  % 浏览量
            {created_at, timestamp, []},
            {updated_at, timestamp, []}
        ],
        indexes => [
            {idx_posts_user_id, [user_id], []},
            {idx_posts_published, [published], []},
            {idx_posts_created, [created_at], []}
        ],
        associations => [
            {belongs_to, user, user_model},
            {has_many, comments, comment_model}
        ]
    }.

%% comment_model.erl
-module(comment_model).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => comments,
        fields => [
            {id},
            {user_id, integer, [not_null]},
            {post_id, integer, [not_null]},
            {content, text, [not_null]},
            {is_approved, boolean, [{default, false}]},
            {created_at, timestamp, []},
            {updated_at, timestamp, []}
        ],
        indexes => [
            {idx_comments_user_id, [user_id], []},
            {idx_comments_post_id, [post_id], []},
            {idx_comments_approved, [is_approved], []}
        ],
        associations => [
            {belongs_to, user, user_model},
            {belongs_to, post, post_model}
        ]
    }.

%% tag_model.erl
-module(tag_model).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => tags,
        fields => [
            {id},
            {name, {string, 50}, [unique, not_null]},
            {description, text, []},
            {color, {string, 7}, [{default, "#000000"}]}, % 十六进制颜色值
            {created_at, timestamp, []},
            {updated_at, timestamp, []}
        ],
        indexes => [
            {idx_tags_name, [name], [unique]}
        ]
    }.