%%% @doc 博客应用示例
%%% 展示 EORM 的完整使用
%%% @end
-module(blog_app).

-export([
    setup/0,
    demo/0,
    create_user/2,
    create_post/3,
    find_user_posts/1,
    publish_post/1,
    find_popular_posts/1
]).

%%====================================================================
%% 模型定义
%%====================================================================

%% 用户模型
-module(user).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => users,
        fields => [
            {id},
            {username, string(50), [unique, not_null]},
            {email, string(100), [unique, not_null]}, 
            {password_hash, string(255), [not_null]},
            {full_name, string(100)},
            {bio, text},
            {avatar_url, string(500)},
            {is_active, boolean, [default(true)]},
            {role, string, [default("user")]},
            timestamps()
        ],
        indexes => [
            {idx_username, [username]},
            {idx_email, [email]},
            {idx_created_at, [created_at], [desc]}
        ],
        associations => [
            {has_many, posts, post},
            {has_many, comments, comment}
        ]
    }.

%% 文章模型
-module(post).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => posts,
        fields => [
            {id},
            {user_id, integer, [not_null]},
            {title, string(200), [not_null]},
            {slug, string(200), [unique]},
            {content, text},
            {excerpt, string(500)},
            {status, string, [default("draft")]},
            {view_count, integer, [default(0)]},
            {like_count, integer, [default(0)]},
            {published_at, datetime},
            {featured, boolean, [default(false)]},
            timestamps()
        ],
        indexes => [
            {idx_user_id, [user_id]},
            {idx_slug, [slug]},
            {idx_status, [status]},
            {idx_published_at, [published_at], [desc]},
            {idx_featured_published, [featured, published_at]}
        ],
        associations => [
            {belongs_to, author, user, #{foreign_key => user_id}},
            {has_many, comments, comment},
            {many_to_many, tags, tag, #{
                join_table => post_tags,
                foreign_key => post_id,
                association_foreign_key => tag_id
            }}
        ],
        constraints => [
            {foreign_key, user_id, users, id, [on_delete(cascade)]}
        ]
    }.

%% 评论模型
-module(comment).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => comments,
        fields => [
            {id},
            {post_id, integer, [not_null]},
            {user_id, integer, [not_null]},
            {parent_id, integer},
            {content, text, [not_null]},
            {is_approved, boolean, [default(true)]},
            {like_count, integer, [default(0)]},
            timestamps()
        ],
        indexes => [
            {idx_post_id, [post_id]},
            {idx_user_id, [user_id]},
            {idx_parent_id, [parent_id]},
            {idx_approved_created, [is_approved, created_at]}
        ],
        associations => [
            {belongs_to, post, post},
            {belongs_to, user, user},
            {belongs_to, parent, comment, #{foreign_key => parent_id}},
            {has_many, replies, comment, #{foreign_key => parent_id}}
        ],
        constraints => [
            {foreign_key, post_id, posts, id, [on_delete(cascade)]},
            {foreign_key, user_id, users, id, [on_delete(cascade)]},
            {foreign_key, parent_id, comments, id, [on_delete(cascade)]}
        ]
    }.

%% 标签模型
-module(tag).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => tags,
        fields => [
            {id},
            {name, string(50), [unique, not_null]},
            {slug, string(50), [unique, not_null]},
            {description, text},
            {color, string(7), [default("#000000")]},
            {post_count, integer, [default(0)]},
            timestamps()
        ],
        indexes => [
            {idx_name, [name]},
            {idx_slug, [slug]},
            {idx_post_count, [post_count], [desc]}
        ],
        associations => [
            {many_to_many, posts, post, #{
                join_table => post_tags,
                foreign_key => tag_id,
                association_foreign_key => post_id
            }}
        ]
    }.

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 设置数据库和迁移
setup() ->
    %% 启动 EORM
    application:start(eorm),
    
    %% 配置数据库
    application:set_env(eorm, databases, [
        {default, #{
            adapter => postgres,
            host => "localhost",
            port => 5432,
            database => "blog_dev",
            username => "postgres",
            password => "postgres",
            pool_size => 10
        }}
    ]),
    
    %% 注册模型
    eorm:register_models([user, post, comment, tag]),
    
    %% 执行自动迁移
    io:format("Running migrations...~n"),
    case eorm:auto_migrate([user, post, comment, tag]) of
        ok ->
            io:format("Migrations completed successfully!~n"),
            ok;
        Error ->
            io:format("Migration failed: ~p~n", [Error]),
            Error
    end.

%% @doc 运行演示
demo() ->
    io:format("~n=== EORM Blog Application Demo ===~n~n"),
    
    %% 创建用户
    io:format("Creating users...~n"),
    {ok, Alice} = create_user("alice", "alice@example.com"),
    {ok, Bob} = create_user("bob", "bob@example.com"),
    io:format("Created users: ~p, ~p~n", [
        maps:get(username, Alice),
        maps:get(username, Bob)
    ]),
    
    %% 创建文章
    io:format("~nCreating posts...~n"),
    {ok, Post1} = create_post(
        maps:get(id, Alice),
        "Getting Started with EORM",
        "EORM is a powerful ORM for Erlang..."
    ),
    {ok, Post2} = create_post(
        maps:get(id, Alice),
        "Advanced EORM Features",
        "Let's explore advanced features..."
    ),
    {ok, Post3} = create_post(
        maps:get(id, Bob),
        "Building Apps with Erlang",
        "Erlang is great for building..."
    ),
    
    %% 发布文章
    io:format("~nPublishing posts...~n"),
    publish_post(maps:get(id, Post1)),
    publish_post(maps:get(id, Post2)),
    
    %% 创建标签
    io:format("~nCreating and assigning tags...~n"),
    {ok, ErlangTag} = eorm:create(tag, #{
        name => "Erlang",
        slug => "erlang",
        description => "Erlang programming language"
    }),
    {ok, ORMTag} = eorm:create(tag, #{
        name => "ORM",
        slug => "orm",
        description => "Object-Relational Mapping"
    }),
    
    %% 查询用户的文章
    io:format("~nFinding Alice's posts...~n"),
    AlicePosts = find_user_posts(maps:get(id, Alice)),
    lists:foreach(fun(P) ->
        io:format("  - ~s (status: ~s)~n", [
            maps:get(title, P),
            maps:get(status, P)
        ])
    end, AlicePosts),
    
    %% 创建评论
    io:format("~nCreating comments...~n"),
    {ok, Comment1} = eorm:create(comment, #{
        post_id => maps:get(id, Post1),
        user_id => maps:get(id, Bob),
        content => "Great article! Very helpful."
    }),
    {ok, _Reply} = eorm:create(comment, #{
        post_id => maps:get(id, Post1),
        user_id => maps:get(id, Alice),
        parent_id => maps:get(id, Comment1),
        content => "Thank you for your feedback!"
    }),
    
    %% 复杂查询示例
    io:format("~nPerforming complex queries...~n"),
    
    %% 1. 查找热门文章
    PopularPosts = find_popular_posts(5),
    io:format("Popular posts:~n"),
    lists:foreach(fun(P) ->
        io:format("  - ~s (views: ~p)~n", [
            maps:get(title, P),
            maps:get(view_count, P)
        ])
    end, PopularPosts),
    
    %% 2. 查找有评论的文章
    PostsWithComments = eorm:new(post)
        |> eorm:joins(comments)
        |> eorm:group_by([id])
        |> eorm:having({count, comments.id}, {gt, 0})
        |> eorm:select([id, title, {count, comments.id, as, comment_count}])
        |> eorm_query:execute(),
    
    case PostsWithComments of
        {ok, Results} ->
            io:format("~nPosts with comments:~n"),
            lists:foreach(fun(R) ->
                io:format("  - ~s (~p comments)~n", [
                    maps:get(title, R),
                    maps:get(comment_count, R)
                ])
            end, Results);
        _ ->
            io:format("No posts with comments found~n")
    end,
    
    %% 3. 统计信息
    io:format("~nStatistics:~n"),
    {ok, UserCount} = eorm:count(user),
    {ok, PostCount} = eorm:count(post),
    {ok, CommentCount} = eorm:count(comment),
    {ok, PublishedCount} = eorm:count(
        eorm:new(post) |> eorm:where(#{status => "published"})
    ),
    
    io:format("  Total users: ~p~n", [UserCount]),
    io:format("  Total posts: ~p~n", [PostCount]),
    io:format("  Published posts: ~p~n", [PublishedCount]),
    io:format("  Total comments: ~p~n", [CommentCount]),
    
    %% 事务示例
    io:format("~nTransaction example...~n"),
    Result = eorm:transaction(fun() ->
        %% 在事务中创建文章和自动增加用户文章计数
        {ok, NewPost} = eorm:create(post, #{
            user_id => maps:get(id, Alice),
            title => "Transaction Test Post",
            content => "This post was created in a transaction"
        }),
        
        %% 模拟更新用户统计
        eorm:raw_query(
            "UPDATE users SET post_count = post_count + 1 WHERE id = $1",
            [maps:get(id, Alice)]
        ),
        
        {ok, NewPost}
    end),
    
    case Result of
        {ok, TransPost} ->
            io:format("Transaction successful! Created post: ~s~n", 
                      [maps:get(title, TransPost)]);
        {error, Reason} ->
            io:format("Transaction failed: ~p~n", [Reason])
    end,
    
    io:format("~n=== Demo completed! ===~n"),
    ok.

%% @doc 创建用户
create_user(Username, Email) ->
    eorm:create(user, #{
        username => Username,
        email => Email,
        password_hash => crypto:hash(sha256, Username), %% 简化的密码哈希
        full_name => string:titlecase(Username),
        bio => "I love blogging!"
    }).

%% @doc 创建文章
create_post(UserId, Title, Content) ->
    Slug = string:lowercase(string:replace(Title, " ", "-", all)),
    eorm:create(post, #{
        user_id => UserId,
        title => Title,
        slug => Slug,
        content => Content,
        excerpt => string:slice(Content, 0, 100) ++ "..."
    }).

%% @doc 发布文章
publish_post(PostId) ->
    eorm:update(post, PostId, #{
        status => "published",
        published_at => calendar:local_time()
    }).

%% @doc 查找用户的所有文章
find_user_posts(UserId) ->
    Query = eorm:new(post)
        |> eorm:where(#{user_id => UserId})
        |> eorm:order_by([{created_at, desc}]),
    
    case eorm_query:execute(Query) of
        {ok, Posts} -> Posts;
        _ -> []
    end.

%% @doc 查找热门文章
find_popular_posts(Limit) ->
    Query = eorm:new(post)
        |> eorm:where(#{status => "published"})
        |> eorm:order_by([{view_count, desc}, {like_count, desc}])
        |> eorm:limit(Limit),
    
    case eorm_query:execute(Query) of
        {ok, Posts} -> Posts;
        _ -> []
    end.

%%====================================================================
%% 高级用法示例
%%====================================================================

%% 预加载关联
demo_preload() ->
    %% 预加载用户的所有文章和评论
    Query = eorm:new(user)
        |> eorm:preload([posts, {posts, comments}])
        |> eorm:where(#{username => "alice"}),
    
    case eorm_query:execute(Query) of
        {ok, [User]} ->
            Posts = maps:get(posts, User, []),
            io:format("User ~s has ~p posts~n", 
                      [maps:get(username, User), length(Posts)]);
        _ ->
            io:format("User not found~n")
    end.

%% 批量操作
demo_bulk_operations() ->
    %% 批量创建标签
    Tags = [
        #{name => "Tutorial", slug => "tutorial"},
        #{name => "Database", slug => "database"},
        #{name => "Web", slug => "web"},
        #{name => "API", slug => "api"}
    ],
    {ok, Count} = eorm:insert_all(tag, Tags),
    io:format("Created ~p tags~n", [Count]),
    
    %% 批量更新
    {ok, Updated} = eorm:update_all(
        post,
        #{status => "draft"},
        #{status => "archived"}
    ),
    io:format("Archived ~p draft posts~n", [Updated]).

%% 原生 SQL 查询
demo_raw_sql() ->
    %% 自定义聚合查询
    SQL = "
        SELECT 
            u.username,
            COUNT(p.id) as post_count,
            COUNT(c.id) as comment_count
        FROM users u
        LEFT JOIN posts p ON p.user_id = u.id
        LEFT JOIN comments c ON c.user_id = u.id
        GROUP BY u.id, u.username
        ORDER BY post_count DESC
    ",
    
    case eorm:raw_query(SQL) of
        {ok, Results} ->
            io:format("User statistics:~n"),
            lists:foreach(fun([Username, PostCount, CommentCount]) ->
                io:format("  ~s: ~p posts, ~p comments~n", 
                          [Username, PostCount, CommentCount])
            end, Results);
        Error ->
            io:format("Query failed: ~p~n", [Error])
    end.