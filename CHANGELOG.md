# Changelog

All notable changes to EORM will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial release of EORM framework
- Auto-migration system similar to GORM
- Zero-configuration table creation from model definitions
- Intelligent type inference for common field patterns
  - `id` fields automatically become primary keys
  - `*_id` fields recognized as foreign keys
  - `*_at` fields recognized as timestamps
  - `is_*` fields recognized as booleans
- Multi-database support (PostgreSQL, MySQL, SQLite)
- Schema comparison engine for detecting structural differences
- Migration modes: Safe, Force, and DryRun
- DDL generation for multiple database dialects
- Risk assessment for schema changes
- Migration history tracking
- Comprehensive test suite with EUnit
- Docker-based PostgreSQL testing
- Chain-style query builder API
- CRUD operations (Create, Read, Update, Delete)
- Batch operations for improved performance
- Transaction support
- Association management (belongs_to, has_many, has_one, many_to_many)
- Lifecycle hooks (before_create, after_update, etc.)
- Query caching system
- Connection pooling
- Raw SQL query support

### Features in Development
- [ ] More database adapters (MongoDB, Cassandra)
- [ ] Query optimizer
- [ ] Sharding support
- [ ] GraphQL integration
- [ ] Real-time change subscriptions
- [ ] Migration version control
- [ ] Web-based admin interface
- [ ] Performance monitoring dashboard

## [1.0.0] - 2024-08-06

### Initial Release
- Core ORM functionality
- Auto-migration system
- Basic CRUD operations
- PostgreSQL adapter
- Test coverage framework

---

## Version History

### 1.0.0 (2024-08-06)
**Initial Release**
- 🚀 Launch of EORM with core features
- 📊 Auto-migration inspired by GORM
- 🔧 Complete CRUD operations
- 🧪 Comprehensive test suite

### 0.9.0-beta (2024-08-01)
**Beta Release**
- Schema comparison engine
- DDL generation
- Risk assessment system

### 0.8.0-alpha (2024-07-25)
**Alpha Release**
- Basic model definition
- Simple migrations
- PostgreSQL support