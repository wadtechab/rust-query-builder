# Rust Query Builder

A fluent SQL query builder for constructing PostgreSQL WHERE clauses with automatic parameter binding and some type safety.

## Motivation

I don't want a library wrapping my SQLx queries, or forcing me to redefine my tables
as types from some new framework. I don't really want the dynamic query building for queries
which should be static (e.g. inserts). I just wanted to avoid the `if let Some() = ` and
string building boilerplate when having conditional filters and pagination.

This is thus aimed to be as light as possible.

## Quick Start

Add to your `Cargo.toml`:

```toml
[dependencies]
query-builder = "0.2.0"
sqlx = { version = "0.8", features = ["postgres", "chrono"] }
```

## Basic Usage

```rust
use query_builder::{WhereBuilder, LikePosition};

let query = WhereBuilder::new("SELECT * FROM users")
    .eq("status", Some("active"))
    .gt("age", Some(18))
    .like("name", Some("John".to_string()), LikePosition::Start)
    .into_parts();

// Execute with SQLx
let (sql, args) = query;
let users = sqlx::query_with(&sql, args).fetch_all(&pool).await?;
```

## Complex Conditions with Groups

```rust
use query_builder::{WhereBuilder, LikePosition};

let (sql, args) = WhereBuilder::new("SELECT * FROM products")
    .eq("category", Some("electronics"))
    .or_group(|group| {
        group.gt("price", Some(100))
             .eq("brand", Some("Apple"));
    })
    .active_only(true)
    .into_parts();

// Execute with SQLx
let products = sqlx::query_with(&sql, args).fetch_all(&pool).await?;

// Generated SQL: SELECT * FROM products 
//               WHERE category = $1 
//               AND (price > $2 OR brand = $3) 
//               AND deleted_at IS NULL
```

## Pagination Support

```rust
use query_builder::{WhereBuilder, Paginate, SortDirection};

let pagination = Paginate::desc("created_at", 2, 10);
let (sql, args) = WhereBuilder::new("SELECT * FROM articles")
    .eq("published", Some(true))
    .like("title", Some("rust".to_string()), LikePosition::Contains)
    .paginate(&pagination)
    .into_parts();

// Execute with SQLx
let articles = sqlx::query_with(&sql, args).fetch_all(&pool).await?;

// Generated SQL: SELECT * FROM articles 
//               WHERE published = $1 AND title LIKE $2
//               ORDER BY created_at DESC LIMIT 10 OFFSET 10
```

## Available Operations

### Comparison Operations
- `eq(column, value)` - Equality (`=`)
- `ne(column, value)` - Not equal (`!=`)
- `gt(column, value)` - Greater than (`>`)
- `gte(column, value)` - Greater than or equal (`>=`)
- `lt(column, value)` - Less than (`<`)
- `lte(column, value)` - Less than or equal (`<=`)
- `between(column, min, max)` - Range conditions

### Pattern Matching
- `like(column, value, position)` - Case-sensitive LIKE
- `ilike(column, value, position)` - Case-insensitive LIKE
- `eq_ci(column, value)` - Case-insensitive equality

### Array Operations
- `in_array(column, values)` - Membership (`= ANY()`)
- `not_in(column, values)` - Exclusion (`!= ALL()`)
- `array_overlap(column, values)` - Array overlap (`&&`)
- `array_contains_any(column, values)` - Array contains (`@>`)
- `array_contained_by(column, values)` - Array contained by (`<@`)

### JSON Operations
- `json_eq(column, path, value)` - JSON field equality
- `json_contains(column, value)` - JSON containment (`@>`)

### Full-text Search
- `text_search(column, query)` - PostgreSQL full-text search

### Date Operations
- `date_in_range(column, start, end)` - Date range
- `within_days(column, days)` - Recent records

### Logical Grouping
- `or_group(closure)` - OR conditions in parentheses
- `and_group(closure)` - AND conditions in parentheses

### Utilities
- `is_null(column, check)` - NULL checks
- `is_not_null(column, check)` - NOT NULL checks
- `active_only(check)` - Soft delete filtering
- `exists(subquery)` - EXISTS conditions
- `raw(sql, value)` - Raw SQL escape hatch

## API Integration Example

Perfect for building REST APIs with optional query parameters:

```rust
use query_builder::{WhereBuilder, Paginate};

async fn get_users(
    status: Option<&str>,
    min_age: Option<i32>,
    search: Option<String>,
    page: Option<i64>,
    page_size: Option<i64>,
) -> Result<Vec<User>, sqlx::Error> {
    let pagination = Paginate::desc("created_at", page.unwrap_or(1), page_size.unwrap_or(20));

    let query = WhereBuilder::new("SELECT * FROM users")
        .eq("status", status)
        .gte("age", min_age)
        .like("name", search, LikePosition::Contains)
        .active_only(true)
        .paginate(&pagination)
        .into_parts();

    let (sql, args) = query;
    sqlx::query_with(&sql, args).fetch_all(&pool).await
}
```

## Like Position Options

Control wildcard placement for LIKE operations:

```rust
use query_builder::LikePosition;

// "John%" - starts with John
.like("name", Some("John".to_string()), LikePosition::Start)

// "%John" - ends with John
.like("name", Some("John".to_string()), LikePosition::End)

// "%John%" - contains John
.like("name", Some("John".to_string()), LikePosition::Contains)

// "John" - exact match
.like("name", Some("John".to_string()), LikePosition::Exact)
```

## Advanced Examples

### Complex Nested Groups

```rust
let (sql, args) = WhereBuilder::new("SELECT * FROM orders")
    .eq("status", Some("pending"))
    .or_group(|group| {
        group.and_group(|and_group| {
                and_group.eq("priority", Some("high"))
                         .lt("created_at", Some(yesterday));
            })
            .and_group(|and_group| {
                and_group.eq("customer_tier", Some("premium"))
                         .gt("amount", Some(1000.0));
            });
    })
    .into_parts();

// Execute with SQLx
let orders = sqlx::query_with(&sql, args).fetch_all(&pool).await?;
```

### JSON Queries

```rust
use serde_json::json;

let (sql, args) = WhereBuilder::new("SELECT * FROM users")
    .json_eq("metadata", "department", Some("engineering"))
    .json_contains("preferences", Some(json!({"notifications": true})))
    .into_parts();

// Execute with SQLx
let users = sqlx::query_with(&sql, args).fetch_all(&pool).await?;
```

### PostgreSQL Array Operations

```rust
// Array overlap - checks if arrays have any common elements
let (sql, args) = WhereBuilder::new("SELECT * FROM properties")
    .array_overlap("asset_class", Some(vec!["hotel", "restaurant"]))
    .into_parts();
// Generated SQL: SELECT * FROM properties WHERE asset_class && $1

// Array contains - checks if array contains all specified elements
let (sql, args) = WhereBuilder::new("SELECT * FROM properties")
    .array_contains_any("amenities", Some(vec!["pool", "gym"]))
    .into_parts();
// Generated SQL: SELECT * FROM properties WHERE amenities @> $1

// Array contained by - checks if array is subset of specified elements
let (sql, args) = WhereBuilder::new("SELECT * FROM properties")
    .array_contained_by("categories", Some(vec!["commercial", "residential", "mixed"]))
    .into_parts();
// Generated SQL: SELECT * FROM properties WHERE categories <@ $1

// Execute any of these with SQLx
let properties = sqlx::query_with(&sql, args).fetch_all(&pool).await?;
```

### Full-text Search

```rust
let (sql, args) = WhereBuilder::new("SELECT * FROM articles")
    .text_search("search_vector", Some("rust programming".to_string()))
    .eq("published", Some(true))
    .into_parts();

// Execute with SQLx
let articles = sqlx::query_with(&sql, args).fetch_all(&pool).await?;
```

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE](LICENSE) file for details.
