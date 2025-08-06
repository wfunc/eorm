#!/bin/bash

# 持久化测试脚本 - 保留测试表供查看

echo "=== EORM 持久化迁移测试 ==="
echo "测试表将保留在 eorm_demo 数据库中"

# 配置
DB_USER="dev"
DB_PASS="123"
DB_HOST="localhost"
DB_NAME="eorm_demo"

# 创建数据库
echo "创建演示数据库..."
export PGPASSWORD=$DB_PASS
psql -h $DB_HOST -U $DB_USER -d postgres <<EOF
DROP DATABASE IF EXISTS $DB_NAME;
CREATE DATABASE $DB_NAME;
EOF

# 创建测试 Erlang 脚本
cat > test_persistent.erl <<'ERLANG'
#!/usr/bin/env escript

main(_) ->
    io:format("=== EORM 持久化迁移测试 ===~n"),
    
    %% 编译项目
    io:format("编译 EORM 项目...~n"),
    case compile:file("src/eorm_ddl_generator.erl", [{outdir, "ebin"}]) of
        {ok, _} -> io:format("DDL Generator 编译成功~n");
        Error -> io:format("编译失败: ~p~n", [Error])
    end,
    
    code:add_path("ebin"),
    
    %% 定义表结构
    UsersSchema = #{
        fields => [
            {id},
            {username, {string, 50}, [unique, not_null]},
            {email, {string, 100}, [unique, not_null]},
            {age, integer, [{default, 18}]},
            {balance, {decimal, 10, 2}},
            {is_active, boolean, [{default, true}]},
            {bio, text},
            {metadata, json},
            {created_at, timestamp},
            {updated_at, timestamp}
        ],
        indexes => [
            {idx_username, [username]},
            {idx_email, [email]},
            {idx_age, [age]}
        ]
    },
    
    PostsSchema = #{
        fields => [
            {id},
            {title, {string, 200}, [not_null]},
            {content, text},
            {user_id, integer},
            {status, {string, 20}, [{default, "draft"}]},
            {view_count, integer, [{default, 0}]},
            {published_at, datetime},
            {tags, json},
            {created_at, timestamp},
            {updated_at, timestamp}
        ],
        indexes => [
            {idx_user_id, [user_id]},
            {idx_status, [status]},
            {idx_published, [published_at]}
        ],
        constraints => [
            {foreign_key, user_id, users, id, [{on_delete, cascade}]}
        ]
    },
    
    %% 生成 DDL
    io:format("~n生成 PostgreSQL DDL...~n"),
    UsersDDL = eorm_ddl_generator:generate_create_table(postgres, users, UsersSchema),
    PostsDDL = eorm_ddl_generator:generate_create_table(postgres, posts, PostsSchema),
    
    io:format("~n=== Users 表 DDL ===~n~s~n", [UsersDDL]),
    io:format("~n=== Posts 表 DDL ===~n~s~n", [PostsDDL]),
    
    %% 保存 DDL 到文件
    file:write_file("users_table.sql", UsersDDL),
    file:write_file("posts_table.sql", PostsDDL),
    
    io:format("~nDDL 已保存到 users_table.sql 和 posts_table.sql~n"),
    io:format("~n测试完成！~n").
ERLANG

# 运行 Erlang 测试
echo "运行迁移测试..."
escript test_persistent.erl

# 执行 DDL
echo ""
echo "在数据库中创建表..."
psql -h $DB_HOST -U $DB_USER -d $DB_NAME < users_table.sql
psql -h $DB_HOST -U $DB_USER -d $DB_NAME < posts_table.sql

# 显示创建的表
echo ""
echo "=== 已创建的表 ==="
psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "\dt"

echo ""
echo "=== Users 表结构 ==="
psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "\d users"

echo ""
echo "=== Posts 表结构 ==="
psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "\d posts"

# 插入测试数据
echo ""
echo "插入测试数据..."
psql -h $DB_HOST -U $DB_USER -d $DB_NAME <<EOF
-- 插入用户数据
INSERT INTO users (username, email, age, balance, is_active, bio) VALUES
    ('alice', 'alice@example.com', 25, 1000.50, true, 'Software developer'),
    ('bob', 'bob@example.com', 30, 2500.00, true, 'Data scientist'),
    ('charlie', 'charlie@example.com', 22, 500.25, false, 'Student');

-- 插入文章数据  
INSERT INTO posts (title, content, user_id, status, view_count) VALUES
    ('Introduction to EORM', 'EORM is a powerful ORM for Erlang...', 1, 'published', 100),
    ('Building Web Apps with Erlang', 'Learn how to build scalable web apps...', 1, 'published', 250),
    ('Data Analysis with Erlang', 'Erlang for data processing...', 2, 'draft', 0);
EOF

echo ""
echo "=== 插入的数据 ==="
echo "Users 表数据："
psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "SELECT * FROM users"

echo ""
echo "Posts 表数据："
psql -h $DB_HOST -U $DB_USER -d $DB_NAME -c "SELECT * FROM posts"

echo ""
echo "========================================="
echo "测试完成！"
echo ""
echo "数据库: $DB_NAME"
echo "表已创建并保留，您可以使用以下命令查看："
echo "  psql -h $DB_HOST -U $DB_USER -d $DB_NAME"
echo ""
echo "清理数据库命令："
echo "  psql -h $DB_HOST -U $DB_USER -d postgres -c 'DROP DATABASE $DB_NAME'"
echo "========================================="

# 清理临时文件
rm -f test_persistent.erl