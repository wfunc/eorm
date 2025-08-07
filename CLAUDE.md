# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

EORM is an Erlang ORM framework inspired by GORM, providing automatic database migrations and a fluent API. The core philosophy is "只要定义了结构就可以自动生成表" (just define the structure and it automatically generates tables).

## Build and Test Commands

```bash
# Compile the project
rebar3 compile

# Run all tests
rebar3 test
# or
rebar3 eunit

# Run specific test module
rebar3 eunit --module=eorm_auto_migrate_test

# Run tests with coverage
rebar3 eunit --cover
rebar3 cover --verbose

# Check code (compile, xref, dialyzer, eunit, ct)
rebar3 check

# Format code
rebar3 format

# Start Erlang shell with EORM loaded
rebar3 shell
```

## Testing with Docker PostgreSQL

```bash
# Test with Docker PostgreSQL (user: dev, password: 123)
./test_docker_only.sh

# Create persistent test database for inspection
./test_persistent_migration.sh
```

## Core Architecture

### Module Organization

The system follows a layered architecture with clear separation of concerns:

1. **API Layer** (`src/eorm.erl`)
   - Main entry point for all EORM operations
   - Provides chain-style query builder API (using `|>` operator workaround)
   - Handles model registration and migration orchestration

2. **Auto-Migration System** (GORM-like behavior)
   - `eorm_auto_migrate.erl`: Core migration orchestrator
   - `eorm_schema_diff.erl`: Compares model definitions with database schema
   - `eorm_ddl_generator.erl`: Generates database-specific DDL statements
   - `eorm_schema_inspector.erl`: Introspects existing database structure
   - `eorm_migration_history.erl`: Tracks migration history

3. **Model Registry** (`src/eorm_registry.erl`)
   - Central registry for all models
   - Handles model metadata parsing and storage
   - Implements intelligent type inference

4. **Query System** (`src/eorm_query.erl`)
   - Query builder implementation
   - Supports chain-style operations (without native `|>` operator)

5. **Database Adapters** (`src/eorm_adapter.erl`)
   - Abstraction layer for different databases
   - Currently supports PostgreSQL, MySQL, SQLite

### Key Design Patterns

#### Automatic Type Inference
The system automatically infers field types and constraints from naming patterns:
- `id` → `integer, primary_key, auto_increment`
- `*_id` → `integer` (foreign key)
- `*_at` → `timestamp`
- `is_*` → `boolean, default(false)`
- `timestamps` → expands to `created_at` and `updated_at`

#### Model Definition Pattern
Models use a behaviour pattern with `schema/0` or `definition/0` callbacks:
```erlang
-module(user).
-behaviour(eorm_model).
-export([schema/0]).

schema() ->
    #{
        table => users,  % optional, defaults to module name
        fields => [
            {id},                               % auto-inferred as primary key
            {username, {string, 50}, [unique]},
            {age, integer, [{default, 18}]},
            timestamps                          % expands to created_at, updated_at
        ],
        indexes => [...],
        associations => [...],
        constraints => [...]
    }.
```

#### Migration Modes
Three migration modes control safety:
- `safe`: Only non-destructive changes (default)
- `force`: All changes including data loss operations
- `dry_run`: Generate plan without execution

### Critical Implementation Details

#### Erlang Syntax Limitations
- **No pipe operator**: Erlang doesn't support `|>`. Use intermediate variables instead
- **Pattern matching**: Use atoms not function calls (e.g., `timestamps` not `timestamps()`)
- **Tuple syntax**: Use `{string, 50}` not `string(50)` for parameterized types
- **Options syntax**: Use `[{default, 18}]` not `[default(18)]`

#### Database Connection
Default PostgreSQL configuration (Docker):
- Host: localhost
- User: dev
- Password: 123
- Container: postgres-dev

#### Test Data Location
Test tables are created in `eorm_test` database, not the main database. This is why users might not see test tables in their default database view.

## Common Development Tasks

### Adding a New Field Type
1. Update type mapping in `eorm_ddl_generator.erl`:
   - Add to `postgres_type/2`, `mysql_type/2`, `sqlite_type/2`
2. Update type inference in `eorm_registry.erl` if needed
3. Add tests in `eorm_auto_migrate_test.erl`

### Adding a New Migration Feature
1. Implement detection in `eorm_schema_diff.erl`
2. Add DDL generation in `eorm_ddl_generator.erl`
3. Update risk assessment in `eorm_schema_diff:assess_risk/1`
4. Add integration in `eorm_auto_migrate.erl`

### Creating a New Database Adapter
1. Implement the `eorm_adapter` behaviour
2. Add type mappings in `eorm_ddl_generator.erl`
3. Register in adapter configuration

## Error Handling Patterns

The codebase uses consistent error handling:
- Success: `{ok, Result}`
- Error: `{error, Reason}`
- Migration errors include risk assessment: `{error, {unsafe_operation, Details}}`

## Testing Approach

Tests are organized by functionality:
- `eorm_auto_migrate_test.erl`: Auto-migration features
- `eorm_complete_test.erl`: Comprehensive integration tests

Tests use mocking (meck) for database interactions when needed.

## Important Files

- `include/eorm.hrl`: Record definitions and types
- `config/sys.config`: Runtime configuration
- `rebar.config`: Build configuration and dependencies
- `test_docker_only.sh`: Docker-based testing script with error handling
- `examples/blog_app.erl`: Complete usage example

## Migration System Flow

1. Model definition → `eorm_registry:parse_model/1`
2. Schema extraction → `eorm_schema_inspector:inspect/2`
3. Diff generation → `eorm_schema_diff:compare/2`
4. Risk assessment → `eorm_schema_diff:assess_risk/1`
5. DDL generation → `eorm_ddl_generator:generate_*/3`
6. Execution → `eorm_auto_migrate:execute_ddl/2`
7. History recording → `eorm_migration_history:record/4`

## Key Behaviors to Maintain

1. **GORM Compatibility**: Maintain similarity to GORM's auto-migration behavior
2. **Safety First**: Default to safe mode, require explicit force for dangerous operations
3. **Type Intelligence**: Continue pattern-based type inference
4. **Zero Configuration**: Models should work with minimal explicit configuration
5. **Database Agnostic**: Keep adapter abstraction clean for multi-database support