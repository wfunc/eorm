%%% @doc 博客应用模型定义示例
%%% 展示如何定义复杂的模型关系
%%% @end
-module(blog_models).

%% API exports - 用于创建模型模块的辅助函数
-export([
    create_user_model/0,
    create_post_model/0, 
    create_comment_model/0,
    create_tag_model/0,
    load_all_models/0
]).

%%====================================================================
%% 模型创建函数
%%====================================================================

%% @doc 加载所有博客模型
load_all_models() ->
    create_user_model(),
    create_post_model(),
    create_comment_model(),
    create_tag_model(),
    io:format("✓ 所有博客模型已加载~n").

%% @doc 创建用户模型
create_user_model() ->
    ModuleCode = [
        "-module(blog_user).",
        "-export([schema/0]).",
        "",
        "schema() ->",
        "    #{",
        "        table => blog_users,",
        "        fields => [",
        "            #{name => id, type => integer, opts => [primary_key, auto_increment]},",
        "            #{name => username, type => string, opts => [required, unique]},",
        "            #{name => email, type => string, opts => [required, unique]},", 
        "            #{name => password_hash, type => string, opts => [required]},",
        "            #{name => full_name, type => string, opts => []},",
        "            #{name => bio, type => text, opts => []},",
        "            #{name => avatar_url, type => string, opts => []},",
        "            #{name => is_active, type => boolean, opts => [{default, true}]},",
        "            #{name => created_at, type => datetime, opts => []},",
        "            #{name => updated_at, type => datetime, opts => []}",
        "        ]",
        "    }."
    ],
    
    compile_and_load_module(blog_user, ModuleCode).

%% @doc 创建文章模型
create_post_model() ->
    ModuleCode = [
        "-module(blog_post).",
        "-export([schema/0]).",
        "",
        "schema() ->",
        "    #{",
        "        table => blog_posts,",
        "        fields => [",
        "            #{name => id, type => integer, opts => [primary_key, auto_increment]},",
        "            #{name => user_id, type => integer, opts => [required]},",
        "            #{name => title, type => string, opts => [required]},",
        "            #{name => slug, type => string, opts => [unique]},",
        "            #{name => content, type => text, opts => []},",
        "            #{name => excerpt, type => text, opts => []},",
        "            #{name => status, type => string, opts => [{default, draft}]},", % draft, published, archived
        "            #{name => published_at, type => datetime, opts => []},",
        "            #{name => view_count, type => integer, opts => [{default, 0}]},",
        "            #{name => featured_image, type => string, opts => []},",
        "            #{name => meta_title, type => string, opts => []},",
        "            #{name => meta_description, type => string, opts => []},",
        "            #{name => created_at, type => datetime, opts => []},",
        "            #{name => updated_at, type => datetime, opts => []}",
        "        ]",
        "    }."
    ],
    
    compile_and_load_module(blog_post, ModuleCode).

%% @doc 创建评论模型
create_comment_model() ->
    ModuleCode = [
        "-module(blog_comment).",
        "-export([schema/0]).",
        "",
        "schema() ->",
        "    #{",
        "        table => blog_comments,",
        "        fields => [",
        "            #{name => id, type => integer, opts => [primary_key, auto_increment]},",
        "            #{name => post_id, type => integer, opts => [required]},",
        "            #{name => user_id, type => integer, opts => []},", % nullable for guest comments
        "            #{name => parent_id, type => integer, opts => []},", % for nested comments
        "            #{name => author_name, type => string, opts => []},", % for guest comments
        "            #{name => author_email, type => string, opts => []},", % for guest comments 
        "            #{name => content, type => text, opts => [required]},",
        "            #{name => status, type => string, opts => [{default, pending}]},", % pending, approved, spam
        "            #{name => ip_address, type => string, opts => []},",
        "            #{name => user_agent, type => string, opts => []},",
        "            #{name => created_at, type => datetime, opts => []},",
        "            #{name => updated_at, type => datetime, opts => []}",
        "        ]",
        "    }."
    ],
    
    compile_and_load_module(blog_comment, ModuleCode).

%% @doc 创建标签模型
create_tag_model() ->
    ModuleCode = [
        "-module(blog_tag).",
        "-export([schema/0]).",
        "",
        "schema() ->",
        "    #{",
        "        table => blog_tags,",
        "        fields => [",
        "            #{name => id, type => integer, opts => [primary_key, auto_increment]},",
        "            #{name => name, type => string, opts => [required, unique]},",
        "            #{name => slug, type => string, opts => [required, unique]},",
        "            #{name => description, type => text, opts => []},",
        "            #{name => color, type => string, opts => [{default, '#000000'}]},", % hex color
        "            #{name => icon, type => string, opts => []},", % icon class or emoji
        "            #{name => post_count, type => integer, opts => [{default, 0}]},", % denormalized count
        "            #{name => is_featured, type => boolean, opts => [{default, false}]},",
        "            #{name => created_at, type => datetime, opts => []},",
        "            #{name => updated_at, type => datetime, opts => []}",
        "        ]",
        "    }."
    ],
    
    compile_and_load_module(blog_tag, ModuleCode).

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 编译并加载模块
compile_and_load_module(ModuleName, ModuleCode) ->
    try
        CodeStr = string:join(ModuleCode, "\n"),
        
        %% 词法分析
        {ok, Tokens, _} = erl_scan:string(CodeStr),
        
        %% 语法分析
        Forms = parse_forms(Tokens, []),
        
        %% 编译
        {ok, ModuleName, Binary} = compile:forms(Forms),
        
        %% 加载
        code:load_binary(ModuleName, atom_to_list(ModuleName) ++ ".erl", Binary),
        
        io:format("✓ ~p 模型创建成功~n", [ModuleName])
    catch
        Error:Reason ->
            io:format("✗ ~p 模型创建失败: ~p:~p~n", [ModuleName, Error, Reason])
    end.

%% @private 解析表单
parse_forms([], Acc) ->
    lists:reverse(Acc);
parse_forms(Tokens, Acc) ->
    case erl_parse:parse_form(Tokens) of
        {ok, Form, RestTokens} ->
            parse_forms(RestTokens, [Form | Acc]);
        {error, _} ->
            lists:reverse(Acc)
    end.

%%====================================================================
%% 使用说明
%%====================================================================

%% 如何使用这些模型:
%%
%% 1. 在Erlang shell中:
%%    blog_models:load_all_models().
%%
%% 2. 注册模型到EORM:
%%    eorm_registry:register_model(blog_user),
%%    eorm_registry:register_model(blog_post),
%%    eorm_registry:register_model(blog_comment),
%%    eorm_registry:register_model(blog_tag).
%%
%% 3. 运行自动迁移:
%%    eorm_auto_migrate:auto_migrate([blog_user, blog_post, blog_comment, blog_tag], #{adapter => postgres, mode => force}).
%%
%% 4. 使用EORM API操作数据:
%%    %% 创建用户
%%    eorm:create(blog_user, #{username => <<"john">>, email => <<"john@example.com">>, password_hash => <<"hashed_password">>}).
%%    
%%    %% 创建文章
%%    eorm:create(blog_post, #{user_id => 1, title => <<"My First Post">>, content => <<"Post content here...">>}).
%%    
%%    %% 查询文章
%%    eorm:find_all(blog_post, #{status => <<"published">>}).
%%
%% 这个示例展示了真实博客应用中的模型设计:
%% - 用户认证和个人资料
%% - 文章内容管理和SEO字段
%% - 嵌套评论支持访客评论
%% - 标签系统with颜色和图标
%% - 合理的数据库关系和约束