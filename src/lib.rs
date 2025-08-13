//! A fluent SQL query builder for constructing PostgreSQL WHERE clauses.
//!
//! This crate provides a type-safe, ergonomic way to build complex WHERE clauses
//! for PostgreSQL queries using SQLx. It handles parameter binding automatically
//! and supports common SQL operations like comparisons, pattern matching, array
//! operations, JSON queries, and complex logical groupings.
//!
//! # Examples
//!
//! ## Basic Usage
//!
//! ```rust
//! use query_builder::{WhereBuilder, LikePosition};
//!
//! let query = WhereBuilder::new("SELECT * FROM users")
//!     .eq("status", Some("active"))
//!     .gt("age", Some(18))
//!     .like("name", Some("John".to_string()), LikePosition::Start)
//!     .build();
//! ```
//!
//! ## Complex Conditions with Groups
//!
//! ```rust
//! use query_builder::{WhereBuilder, LikePosition};
//!
//! let query = WhereBuilder::new("SELECT * FROM products")
//!     .eq("category", Some("electronics"))
//!     .or_group(|group| {
//!         group.gt("price", Some(100))
//!              .eq("brand", Some("Apple"));
//!     })
//!     .active_only(true)
//!     .build();
//! ```
//!
//! ## Pagination Support
//!
//! ```rust
//! use query_builder::{WhereBuilder, Paginate, SortDirection, LikePosition};
//!
//! // Basic pagination with sorting
//! let pagination = Paginate::desc("created_at", 2, 10);
//! let query = WhereBuilder::new("SELECT * FROM articles")
//!     .eq("published", Some(true))
//!     .like("title", Some("rust".to_string()), LikePosition::Contains)
//!     .paginate(&pagination)
//!     .build();
//! // Generates: SELECT * FROM articles WHERE published =$1 AND title LIKE$2
//! //           ORDER BY created_at DESC LIMIT 10 OFFSET 10
//! ```
//!
//! ## API Integration Example
//!
//! ```rust
//! use query_builder::{WhereBuilder, Paginate};
//!
//! // From API parameters with defaults
//! let pagination = Some(Paginate::default()).unwrap_or_default();
//! let status_filter: Option<&str> = Some("active");
//! let query = WhereBuilder::new("SELECT * FROM users")
//!     .eq("status", status_filter)
//!     .paginate(&pagination)
//!     .build();
//! ```

use sqlx::{Postgres, QueryBuilder, postgres::PgHasArrayType};

/// Sort direction for ORDER BY clauses.
///
/// Used with [`Paginate`] to specify ascending or descending order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortDirection {
    /// Ascending order (A-Z, 1-9, oldest first)
    Asc,
    /// Descending order (Z-A, 9-1, newest first)
    Desc,
}

impl std::fmt::Display for SortDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SortDirection::Asc => write!(f, "ASC"),
            SortDirection::Desc => write!(f, "DESC"),
        }
    }
}

/// Pagination helper for generating ORDER BY, LIMIT, and OFFSET clauses.
///
/// This struct encapsulates pagination parameters and can generate the appropriate
/// SQL fragment to append to queries. It provides a clean interface for API
/// pagination integration.
///
/// # Examples
///
/// ```rust
/// use query_builder::{Paginate, SortDirection, WhereBuilder};
///
/// // Basic pagination
/// let pagination = Paginate::new("created_at", SortDirection::Desc, 2, 10);
/// let query = WhereBuilder::new("SELECT * FROM users")
///     .eq("status", Some("active"))
///     .paginate(&pagination)
///     .build();
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Paginate {
    /// The column to order by
    pub order_by: String,
    /// The sort direction (ASC or DESC)
    pub direction: SortDirection,
    /// Current page number (1-based)
    pub page: i64,
    /// Number of items per page
    pub page_size: i64,
}

impl Paginate {
    /// Creates a new pagination configuration.
    ///
    /// # Arguments
    ///
    /// * `order_by` - The column name to order by
    /// * `direction` - Sort direction (Asc or Desc)
    /// * `page` - Current page number (1-based)
    /// * `page_size` - Number of items per page
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::{Paginate, SortDirection};
    ///
    /// let pagination = Paginate::new("name", SortDirection::Asc, 1, 20);
    /// ```
    pub fn new(order_by: &str, direction: SortDirection, page: i64, page_size: i64) -> Self {
        Self {
            order_by: order_by.to_string(),
            direction,
            page: page.max(1),           // Ensure page is at least 1
            page_size: page_size.max(1), // Ensure page_size is at least 1
        }
    }

    /// Creates pagination with ascending order.
    ///
    /// Convenience method equivalent to `Paginate::new(order_by, SortDirection::Asc, page, page_size)`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::Paginate;
    ///
    /// let pagination = Paginate::asc("name", 1, 20);
    /// ```
    pub fn asc(order_by: &str, page: i64, page_size: i64) -> Self {
        Self::new(order_by, SortDirection::Asc, page, page_size)
    }

    /// Creates pagination with descending order.
    ///
    /// Convenience method equivalent to `Paginate::new(order_by, SortDirection::Desc, page, page_size)`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::Paginate;
    ///
    /// let pagination = Paginate::desc("created_at", 1, 10);
    /// ```
    pub fn desc(order_by: &str, page: i64, page_size: i64) -> Self {
        Self::new(order_by, SortDirection::Desc, page, page_size)
    }

    /// Calculates the OFFSET value based on current page and page size.
    ///
    /// Uses 1-based page numbering, so page 1 has offset 0.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::Paginate;
    ///
    /// let pagination = Paginate::asc("id", 3, 10);
    /// assert_eq!(pagination.offset(), 20); // (3-1) * 10
    /// ```
    pub fn offset(&self) -> i64 {
        (self.page - 1) * self.page_size
    }

    /// Generates the SQL fragment for ORDER BY, LIMIT, and OFFSET.
    ///
    /// Returns a string that can be appended to a SQL query to add pagination.
    /// The fragment includes a leading space.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::{Paginate, SortDirection};
    ///
    /// let pagination = Paginate::new("name", SortDirection::Desc, 2, 5);
    /// assert_eq!(pagination.to_sql_fragment(), " ORDER BY name DESC LIMIT 5 OFFSET 5");
    /// ```
    pub fn to_sql_fragment(&self) -> String {
        format!(
            " ORDER BY {} {} LIMIT {} OFFSET {}",
            self.order_by,
            self.direction,
            self.page_size,
            self.offset()
        )
    }
}

impl Default for Paginate {
    /// Default pagination: order by "id" ASC, page 1, 20 items per page.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::{Paginate, SortDirection};
    ///
    /// let default_pagination = Paginate::default();
    /// assert_eq!(default_pagination.order_by, "id");
    /// assert_eq!(default_pagination.direction, SortDirection::Asc);
    /// assert_eq!(default_pagination.page, 1);
    /// assert_eq!(default_pagination.page_size, 20);
    /// ```
    fn default() -> Self {
        Self::asc("id", 1, 20)
    }
}

/// A fluent builder for constructing PostgreSQL WHERE clauses with automatic parameter binding.
///
/// The `WhereBuilder` provides a chainable API for building complex WHERE conditions
/// while automatically handling the `WHERE` keyword, `AND` conjunctions, and parameter binding.
/// All methods are conditional - they only add clauses when provided with `Some` values.
pub struct WhereBuilder<'a> {
    builder: QueryBuilder<'a, Postgres>,
    has_where: bool,
    condition_count: usize,
}

impl<'a> WhereBuilder<'a> {
    /// Creates a new `WhereBuilder` with the given base SQL query.
    ///
    /// The base query should be a complete SQL statement up to but not including
    /// the WHERE clause. The builder will automatically add `WHERE` and subsequent
    /// `AND` conjunctions as conditions are added.
    ///
    /// # Arguments
    ///
    /// * `base_query` - The base SQL query (e.g., "SELECT * FROM users")
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let builder = WhereBuilder::new("SELECT * FROM users");
    /// ```
    pub fn new(base_query: &str) -> Self {
        Self {
            builder: QueryBuilder::new(base_query),
            has_where: false,
            condition_count: 0,
        }
    }

    // Core building block - handles the WHERE/AND logic internally
    fn add_condition(&mut self, condition: &str) {
        if !self.has_where {
            self.builder.push(" WHERE ");
            self.has_where = true;
        } else {
            self.builder.push(" AND ");
        }
        self.builder.push(condition);
        self.condition_count += 1;
    }

    /// Core conditional method that adds a WHERE/AND condition only if the value is `Some`.
    ///
    /// This is the building block for all other conditional methods. It automatically
    /// handles adding `WHERE` for the first condition and `AND` for subsequent conditions.
    ///
    /// # Arguments
    ///
    /// * `column` - The column name to compare
    /// * `op` - The SQL operator (e.g., "=", ">", "LIKE")
    /// * `value` - Optional value to bind. Condition is only added if `Some`
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .and_if("age", ">", Some(18))
    ///     .and_if("status", "=", None::<String>) // This won't add anything
    ///     .build();
    /// ```
    pub fn and_if<T>(mut self, column: &str, op: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(val) = value {
            self.add_condition(&format!("{column} {op}"));
            self.builder.push_bind(val);
        }
        self
    }

    /// Adds an equality condition (`column = value`) if value is `Some`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .eq("status", Some("active"))
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE status = $1
    /// ```
    pub fn eq<T>(self, column: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "=", value)
    }

    /// Adds a not-equal condition (`column != value`) if value is `Some`.
    pub fn ne<T>(self, column: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "!=", value)
    }

    /// Adds a greater-than condition (`column > value`) if value is `Some`.
    pub fn gt<T>(self, column: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, ">", value)
    }

    /// Adds a greater-than-or-equal condition (`column >= value`) if value is `Some`.
    pub fn gte<T>(self, column: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, ">=", value)
    }

    /// Adds a less-than condition (`column < value`) if value is `Some`.
    pub fn lt<T>(self, column: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "<", value)
    }

    /// Adds a less-than-or-equal condition (`column <= value`) if value is `Some`.
    pub fn lte<T>(self, column: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "<=", value)
    }

    /// Adds range conditions for a column (both `>= min` and `<= max`) if values are `Some`.
    ///
    /// This is a convenience method for the common pattern of range queries.
    /// Either or both bounds can be `None` to create open-ended ranges.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM products")
    ///     .between("price", Some(10.0), Some(100.0))
    ///     .build();
    /// // Generates: SELECT * FROM products WHERE price >= $1 AND price <= $2
    /// ```
    pub fn between<T>(mut self, column: &str, min: Option<T>, max: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(min_val) = min {
            self.add_condition(&format!("{column} >="));
            self.builder.push_bind(min_val);
        }
        if let Some(max_val) = max {
            self.add_condition(&format!("{column} <="));
            self.builder.push_bind(max_val);
        }
        self
    }

    /// Adds a LIKE condition with configurable wildcard positioning.
    ///
    /// # Arguments
    ///
    /// * `column` - The column to search
    /// * `value` - The search pattern (without wildcards)
    /// * `position` - Where to place wildcards (`Start`, `End`, `Contains`, `Exact`)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::{WhereBuilder, LikePosition};
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .like("name", Some("John".to_string()), LikePosition::Start)
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE name LIKE $1 (with value "John%")
    /// ```
    pub fn like(self, column: &str, value: Option<String>, position: LikePosition) -> Self {
        self.and_if(column, "LIKE", value.map(|v| position.format(v)))
    }

    /// Adds an array membership condition using PostgreSQL's `= ANY()` operator.
    ///
    /// This is more efficient than multiple OR conditions for checking if a column
    /// value matches any value in a list.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .in_array("status", Some(vec!["active", "pending"]))
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE status = ANY($1)
    /// ```
    pub fn in_array<T>(mut self, column: &str, values: Option<Vec<T>>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} = ANY("));
                self.builder.push_bind(vals);
                self.builder.push(")");
            }
        }
        self
    }

    /// Adds an array overlap condition using PostgreSQL's `&&` operator.
    ///
    /// Checks if the array column has any elements in common with the provided array.
    /// This is useful for checking if arrays intersect or have overlapping values.
    ///
    /// # Arguments
    ///
    /// * `column` - The array column name
    /// * `values` - Array of values to check for overlap
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM properties")
    ///     .array_overlap("asset_class", Some(vec!["hotel", "restaurant"]))
    ///     .build();
    /// // Generates: SELECT * FROM properties WHERE asset_class && $1
    /// ```
    pub fn array_overlap<T>(mut self, column: &str, values: Option<Vec<T>>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} &&"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    /// Adds an array contains condition using PostgreSQL's `@>` operator.
    ///
    /// Checks if the array column contains all elements from the provided array.
    /// This is useful for checking if an array field contains specific values.
    ///
    /// # Arguments
    ///
    /// * `column` - The array column name
    /// * `values` - Array of values that should all be contained in the column
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM properties")
    ///     .array_contains_any("asset_class", Some(vec!["hotel", "restaurant"]))
    ///     .build();
    /// // Generates: SELECT * FROM properties WHERE asset_class @> $1
    /// ```
    pub fn array_contains_any<T>(mut self, column: &str, values: Option<Vec<T>>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} @>"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    /// Adds an array contained-by condition using PostgreSQL's `<@` operator.
    ///
    /// Checks if the array column is contained by (is a subset of) the provided array.
    /// This is useful for checking if all values in an array field are within a specific set.
    ///
    /// # Arguments
    ///
    /// * `column` - The array column name
    /// * `values` - Array of values that should contain all elements from the column
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM properties")
    ///     .array_contained_by("asset_class", Some(vec!["hotel", "restaurant", "retail"]))
    ///     .build();
    /// // Generates: SELECT * FROM properties WHERE asset_class <@ $1
    /// ```
    pub fn array_contained_by<T>(mut self, column: &str, values: Option<Vec<T>>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} <@"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    /// Adds a case-insensitive LIKE condition (ILIKE) with configurable wildcard positioning.
    ///
    /// Similar to `like()` but performs case-insensitive matching.
    pub fn ilike(self, column: &str, value: Option<String>, position: LikePosition) -> Self {
        self.and_if(column, "ILIKE", value.map(|v| position.format(v)))
    }

    /// Adds an `IS NULL` condition if `check` is true.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .is_null("deleted_at", true)
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE deleted_at IS NULL
    /// ```
    pub fn is_null(mut self, column: &str, check: bool) -> Self {
        if check {
            self.add_condition(&format!("{column} IS NULL"));
        }
        self
    }

    /// Adds an `IS NOT NULL` condition if `check` is true.
    pub fn is_not_null(mut self, column: &str, check: bool) -> Self {
        if check {
            self.add_condition(&format!("{column} IS NOT NULL"));
        }
        self
    }

    /// Adds a raw SQL condition with parameter binding.
    ///
    /// This is an escape hatch for complex conditions that aren't covered
    /// by the other methods. Use with caution to avoid SQL injection.
    ///
    /// # Arguments
    ///
    /// * `sql` - Raw SQL condition (should include parameter placeholder)
    /// * `value` - Value to bind to the condition
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .raw("EXTRACT(year FROM created_at) =", Some(2024))
    ///     .build();
    /// ```
    pub fn raw<T>(mut self, sql: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(val) = value {
            self.add_condition(sql);
            self.builder.push_bind(val);
        }
        self
    }

    /// Creates a group of OR conditions wrapped in parentheses.
    ///
    /// This allows building complex logical expressions like `(a = 1 OR b = 2)`.
    /// The group is only added if at least one condition is added within the closure.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .eq("status", Some("active"))
    ///     .or_group(|group| {
    ///         group.eq("role", Some("admin"))
    ///              .eq("role", Some("moderator"));
    ///     })
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE status = $1 AND (role = $2 OR role = $3)
    /// ```
    pub fn or_group<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut OrGroup<'_, 'a>),
    {
        self.add_condition("(");
        let mut group = OrGroup::new(&mut self.builder);
        f(&mut group);
        self.builder.push(")");
        self
    }

    /// Creates a group of AND conditions wrapped in parentheses.
    ///
    /// Similar to `or_group()` but with AND logic between conditions.
    pub fn and_group<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut AndGroup<'_, 'a>),
    {
        self.add_condition("(");
        let mut group = AndGroup::new(&mut self.builder);
        f(&mut group);
        self.builder.push(")");
        self
    }

    /// Adds a JSON path equality condition (`column->>'path' = value`).
    ///
    /// Extracts a JSON field as text and compares it to the given value.
    /// This is useful for querying JSONB columns in PostgreSQL.
    ///
    /// # Arguments
    ///
    /// * `column` - The JSONB column name
    /// * `path` - The JSON path key to extract
    /// * `value` - The value to compare against
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .json_eq("metadata", "role", Some("admin"))
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE metadata->>'role' = $1
    /// ```
    pub fn json_eq<T>(self, column: &str, path: &str, value: Option<T>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(&format!("{column}->>'{path}' ="), "", value)
    }

    /// Adds a JSON containment condition using PostgreSQL's `@>` operator.
    ///
    /// Checks if the JSONB column contains the specified JSON value.
    /// This is useful for checking if a JSONB object contains certain key-value pairs.
    ///
    /// # Arguments
    ///
    /// * `column` - The JSONB column name
    /// * `value` - The JSON value that should be contained
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    /// use serde_json::json;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .json_contains("metadata", Some(json!({"active": true})))
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE metadata @> $1
    /// ```
    pub fn json_contains(mut self, column: &str, value: Option<serde_json::Value>) -> Self {
        if let Some(val) = value {
            self.add_condition(&format!("{column} @>"));
            self.builder.push_bind(val);
        }
        self
    }

    /// Adds a full-text search condition using PostgreSQL's `@@` operator.
    ///
    /// Performs full-text search against a tsvector column using `plainto_tsquery`.
    /// This is useful for searching text content with stemming and ranking.
    ///
    /// # Arguments
    ///
    /// * `column` - The tsvector column to search
    /// * `query` - The search query string
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM articles")
    ///     .text_search("search_vector", Some("rust programming".to_string()))
    ///     .build();
    /// // Generates: SELECT * FROM articles WHERE search_vector @@ plainto_tsquery('english', $1)
    /// ```
    pub fn text_search(mut self, column: &str, query: Option<String>) -> Self {
        if let Some(q) = query {
            self.add_condition(&format!("{column} @@ plainto_tsquery('english',"));
            self.builder.push_bind(q);
            self.builder.push(")");
        }
        self
    }

    /// Adds date range conditions for a date column.
    ///
    /// This is a convenience method that delegates to `between()` specifically for date types.
    /// Either or both bounds can be `None` to create open-ended date ranges.
    ///
    /// # Arguments
    ///
    /// * `column` - The date column name
    /// * `start` - Optional start date (inclusive)
    /// * `end` - Optional end date (inclusive)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    /// use chrono::NaiveDate;
    ///
    /// let start = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    /// let end = NaiveDate::from_ymd_opt(2024, 12, 31).unwrap();
    ///
    /// let query = WhereBuilder::new("SELECT * FROM orders")
    ///     .date_in_range("created_at", Some(start), Some(end))
    ///     .build();
    /// ```
    pub fn date_in_range(
        self,
        column: &str,
        start: Option<chrono::NaiveDate>,
        end: Option<chrono::NaiveDate>,
    ) -> Self {
        self.between(column, start, end)
    }

    /// Adds a condition to find records within the last N days.
    ///
    /// Uses PostgreSQL's `INTERVAL` to create a dynamic date range condition.
    /// This is useful for finding recent records without hardcoding dates.
    ///
    /// # Arguments
    ///
    /// * `column` - The date/timestamp column name
    /// * `days` - Number of days to look back from current date
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM logs")
    ///     .within_days("created_at", Some(7))
    ///     .build();
    /// // Generates: SELECT * FROM logs WHERE created_at >= CURRENT_DATE - INTERVAL '7 days'
    /// ```
    pub fn within_days(mut self, column: &str, days: Option<i32>) -> Self {
        if let Some(d) = days {
            self.add_condition(&format!("{column} >= CURRENT_DATE - INTERVAL '{d} days'"));
        }
        self
    }

    /// Adds an EXISTS condition with a subquery.
    ///
    /// Checks if the subquery returns any rows. This is useful for checking
    /// relationships or conditions that require complex logic.
    ///
    /// # Arguments
    ///
    /// * `subquery` - The SQL subquery to check for existence
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .exists("SELECT 1 FROM orders WHERE orders.user_id = users.id")
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE EXISTS (SELECT 1 FROM orders WHERE orders.user_id = users.id)
    /// ```
    pub fn exists(mut self, subquery: &str) -> Self {
        self.add_condition("EXISTS (");
        self.builder.push(subquery);
        self.builder.push(")");
        self
    }

    /// Adds a NOT EXISTS condition with a subquery.
    ///
    /// Checks if the subquery returns no rows. This is the inverse of `exists()`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .not_exists("SELECT 1 FROM orders WHERE orders.user_id = users.id")
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE NOT EXISTS (SELECT 1 FROM orders WHERE orders.user_id = users.id)
    /// ```
    pub fn not_exists(mut self, subquery: &str) -> Self {
        self.add_condition("NOT EXISTS (");
        self.builder.push(subquery);
        self.builder.push(")");
        self
    }

    /// Adds a NOT IN condition using PostgreSQL's `!= ALL()` operator.
    ///
    /// Excludes rows where the column value matches any value in the array.
    /// This is more efficient than multiple AND conditions for exclusion checks.
    ///
    /// # Arguments
    ///
    /// * `column` - The column to check
    /// * `values` - Array of values to exclude
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .not_in("status", Some(vec!["banned", "suspended"]))
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE status != ALL($1)
    /// ```
    pub fn not_in<T>(mut self, column: &str, values: Option<Vec<T>>) -> Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} != ALL("));
                self.builder.push_bind(vals);
                self.builder.push(")");
            }
        }
        self
    }

    /// Adds a case-insensitive equality condition.
    ///
    /// Converts both the column value and comparison value to lowercase before comparing.
    /// This is useful for case-insensitive string matching.
    ///
    /// # Arguments
    ///
    /// * `column` - The column to compare
    /// * `value` - The value to compare (will be lowercased)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .eq_ci("email", Some("JOHN@EXAMPLE.COM".to_string()))
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE LOWER(email) = $1 (with lowercased value)
    /// ```
    pub fn eq_ci(self, column: &str, value: Option<String>) -> Self {
        self.and_if(
            &format!("LOWER({column})"),
            "=",
            value.map(|v| v.to_lowercase()),
        )
    }

    /// Adds a condition to filter out soft-deleted records.
    ///
    /// This is a convenience method for the common soft delete pattern where
    /// `deleted_at IS NULL` indicates an active (non-deleted) record.
    ///
    /// # Arguments
    ///
    /// * `check` - Whether to add the soft delete filter
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .active_only(true)
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE deleted_at IS NULL
    /// ```
    pub fn active_only(mut self, check: bool) -> Self {
        if check {
            self.add_condition("deleted_at IS NULL");
        }
        self
    }

    /// Adds pagination with ORDER BY, LIMIT, and OFFSET clauses.
    ///
    /// This method appends the pagination SQL fragment to the query, including
    /// ORDER BY for sorting and LIMIT/OFFSET for pagination. The pagination
    /// is applied after all WHERE conditions.
    ///
    /// # Arguments
    ///
    /// * `pagination` - The pagination configuration containing order column,
    ///   direction, page number, and page size
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::{WhereBuilder, Paginate, SortDirection};
    ///
    /// let pagination = Paginate::new("created_at", SortDirection::Desc, 2, 10);
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .eq("status", Some("active"))
    ///     .paginate(&pagination)
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE status = $1 ORDER BY created_at DESC LIMIT 10 OFFSET 10
    /// ```
    ///
    /// ```rust
    /// use query_builder::{WhereBuilder, Paginate};
    ///
    /// // Using convenience methods
    /// let pagination = Paginate::desc("name", 1, 5);
    /// let query = WhereBuilder::new("SELECT * FROM products")
    ///     .paginate(&pagination)
    ///     .build();
    /// // Generates: SELECT * FROM products ORDER BY name DESC LIMIT 5 OFFSET 0
    /// ```
    pub fn paginate(mut self, pagination: &Paginate) -> Self {
        self.builder.push(pagination.to_sql_fragment());
        self
    }

    /// Returns the generated SQL string for debugging purposes.
    ///
    /// This is useful for inspecting the generated query without executing it.
    /// Parameter placeholders (`$1`, `$2`, etc.) will be visible in the output.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let builder = WhereBuilder::new("SELECT * FROM users")
    ///     .eq("status", Some("active"));
    ///
    /// println!("{}", builder.to_sql());
    /// // Prints: SELECT * FROM users WHERE status = $1
    /// ```
    pub fn to_sql(&self) -> String {
        self.builder.sql().to_string()
    }

    /// Consumes the builder and returns the underlying SQLx `QueryBuilder`.
    ///
    /// This is the final step that allows you to execute the query using SQLx methods
    /// like `fetch_all()`, `fetch_one()`, etc.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .eq("status", Some("active"))
    ///     .build();
    ///
    /// // Now you can execute with SQLx:
    /// // let users = query.fetch_all(&pool).await?;
    /// ```
    pub fn build(self) -> QueryBuilder<'a, Postgres> {
        self.builder
    }
}

/// Controls wildcard positioning for LIKE and ILIKE operations.
///
/// This enum determines where wildcards (`%`) are placed around the search term:
/// - `Start`: Matches values that start with the term (`term%`)
/// - `End`: Matches values that end with the term (`%term`)
/// - `Contains`: Matches values that contain the term anywhere (`%term%`)
/// - `Exact`: Matches the exact term with no wildcards (`term`)
pub enum LikePosition {
    /// Matches values that start with the pattern (`pattern%`)
    Start,
    /// Matches values that end with the pattern (`%pattern`)
    End,
    /// Matches values that contain the pattern anywhere (`%pattern%`)
    Contains,
    /// Matches the exact pattern with no wildcards
    Exact,
}

impl LikePosition {
    fn format(&self, value: String) -> String {
        match self {
            LikePosition::Start => format!("{value}%"),
            LikePosition::End => format!("%{value}"),
            LikePosition::Contains => format!("%{value}%"),
            LikePosition::Exact => value,
        }
    }
}

/// A builder for creating groups of OR conditions within parentheses.
///
/// This struct is used within `or_group()` closures to build expressions like
/// `(condition1 OR condition2 OR condition3)`. All methods are conditional
/// and only add clauses when provided with `Some` values.
pub struct OrGroup<'b, 'a> {
    builder: &'b mut QueryBuilder<'a, Postgres>,
    condition_count: usize,
}

impl<'b, 'a> OrGroup<'b, 'a> {
    fn new(builder: &'b mut QueryBuilder<'a, Postgres>) -> Self {
        Self {
            builder,
            condition_count: 0,
        }
    }

    // Core building block - handles the OR logic internally
    fn add_condition(&mut self, condition: &str) {
        if self.condition_count > 0 {
            self.builder.push(" OR ");
        }
        self.builder.push(condition);
        self.condition_count += 1;
    }

    /// Core conditional method that adds an OR condition only if the value is `Some`.
    ///
    /// This is the building block for all other conditional methods in the OR group.
    /// It automatically handles adding `OR` between conditions.
    ///
    /// # Arguments
    ///
    /// * `column` - The column name to compare
    /// * `op` - The SQL operator (e.g., "=", ">", "LIKE")
    /// * `value` - Optional value to bind. Condition is only added if `Some`
    pub fn or_if<T>(&mut self, column: &str, op: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(val) = value {
            self.add_condition(&format!("{column} {op}"));
            self.builder.push_bind(val);
        }
        self
    }

    /// Adds an OR equality condition (`column = value`) if value is `Some`.
    pub fn eq<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.or_if(column, "=", value)
    }

    /// Adds an OR not-equal condition (`column != value`) if value is `Some`.
    pub fn ne<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.or_if(column, "!=", value)
    }

    /// Adds an OR greater-than condition (`column > value`) if value is `Some`.
    pub fn gt<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.or_if(column, ">", value)
    }

    /// Adds an OR greater-than-or-equal condition (`column >= value`) if value is `Some`.
    pub fn gte<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.or_if(column, ">=", value)
    }

    /// Adds an OR less-than condition (`column < value`) if value is `Some`.
    pub fn lt<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.or_if(column, "<", value)
    }

    /// Adds an OR less-than-or-equal condition (`column <= value`) if value is `Some`.
    pub fn lte<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.or_if(column, "<=", value)
    }

    // LIKE operations
    pub fn like(
        &mut self,
        column: &str,
        value: Option<String>,
        position: LikePosition,
    ) -> &mut Self {
        self.or_if(column, "LIKE", value.map(|v| position.format(v)))
    }

    pub fn ilike(
        &mut self,
        column: &str,
        value: Option<String>,
        position: LikePosition,
    ) -> &mut Self {
        self.or_if(column, "ILIKE", value.map(|v| position.format(v)))
    }

    // Array operations
    pub fn in_array<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} = ANY("));
                self.builder.push_bind(vals);
                self.builder.push(")");
            }
        }
        self
    }

    pub fn array_overlap<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} &&"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    pub fn array_contains_any<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} @>"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    pub fn array_contained_by<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} <@"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    // NULL handling
    pub fn is_null(&mut self, column: &str, check: bool) -> &mut Self {
        if check {
            self.add_condition(&format!("{column} IS NULL"));
        }
        self
    }

    pub fn is_not_null(&mut self, column: &str, check: bool) -> &mut Self {
        if check {
            self.add_condition(&format!("{column} IS NOT NULL"));
        }
        self
    }

    // Case-insensitive equality
    pub fn eq_ci(&mut self, column: &str, value: Option<String>) -> &mut Self {
        self.or_if(
            &format!("LOWER({column})"),
            "=",
            value.map(|v| v.to_lowercase()),
        )
    }

    // Raw SQL escape hatch
    pub fn raw<T>(&mut self, sql: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(val) = value {
            self.add_condition(sql);
            self.builder.push_bind(val);
        }
        self
    }

    // Nested AND group
    pub fn and_group<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut AndGroup<'_, 'a>),
    {
        self.add_condition("(");
        let mut group = AndGroup::new(self.builder);
        f(&mut group);
        self.builder.push(")");
        self
    }
}

/// A builder for creating groups of AND conditions within parentheses.
///
/// This struct is used within `and_group()` closures to build expressions like
/// `(condition1 AND condition2 AND condition3)`. All methods are conditional
/// and only add clauses when provided with `Some` values.
pub struct AndGroup<'b, 'a> {
    builder: &'b mut QueryBuilder<'a, Postgres>,
    condition_count: usize,
}

impl<'b, 'a> AndGroup<'b, 'a> {
    fn new(builder: &'b mut QueryBuilder<'a, Postgres>) -> Self {
        Self {
            builder,
            condition_count: 0,
        }
    }

    // Core building block - handles the AND logic internally
    fn add_condition(&mut self, condition: &str) {
        if self.condition_count > 0 {
            self.builder.push(" AND ");
        }
        self.builder.push(condition);
        self.condition_count += 1;
    }

    /// Core conditional method that adds an AND condition only if the value is `Some`.
    ///
    /// This is the building block for all other conditional methods in the AND group.
    /// It automatically handles adding `AND` between conditions.
    ///
    /// # Arguments
    ///
    /// * `column` - The column name to compare
    /// * `op` - The SQL operator (e.g., "=", ">", "LIKE")
    /// * `value` - Optional value to bind. Condition is only added if `Some`
    pub fn and_if<T>(&mut self, column: &str, op: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(val) = value {
            self.add_condition(&format!("{column} {op}"));
            self.builder.push_bind(val);
        }
        self
    }

    /// Adds an AND equality condition (`column = value`) if value is `Some`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .and_group(|group| {
    ///         group.eq("status", Some("active"))
    ///              .eq("role", Some("admin"));
    ///     })
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE (status = $1 AND role = $2)
    /// ```
    pub fn eq<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "=", value)
    }

    /// Adds an AND not-equal condition (`column != value`) if value is `Some`.
    pub fn ne<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "!=", value)
    }

    /// Adds an AND greater-than condition (`column > value`) if value is `Some`.
    pub fn gt<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, ">", value)
    }

    /// Adds an AND greater-than-or-equal condition (`column >= value`) if value is `Some`.
    pub fn gte<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, ">=", value)
    }

    /// Adds an AND less-than condition (`column < value`) if value is `Some`.
    pub fn lt<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "<", value)
    }

    /// Adds an AND less-than-or-equal condition (`column <= value`) if value is `Some`.
    pub fn lte<T>(&mut self, column: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        self.and_if(column, "<=", value)
    }

    /// Adds an AND LIKE condition with configurable wildcard positioning.
    ///
    /// Similar to the main `WhereBuilder::like()` method but within an AND group.
    pub fn like(
        &mut self,
        column: &str,
        value: Option<String>,
        position: LikePosition,
    ) -> &mut Self {
        self.and_if(column, "LIKE", value.map(|v| position.format(v)))
    }

    /// Adds an AND case-insensitive LIKE condition (ILIKE) with configurable wildcard positioning.
    ///
    /// Similar to the main `WhereBuilder::ilike()` method but within an AND group.
    pub fn ilike(
        &mut self,
        column: &str,
        value: Option<String>,
        position: LikePosition,
    ) -> &mut Self {
        self.and_if(column, "ILIKE", value.map(|v| position.format(v)))
    }

    /// Adds an AND array membership condition using PostgreSQL's `= ANY()` operator.
    ///
    /// Similar to the main `WhereBuilder::in_array()` method but within an AND group.
    pub fn in_array<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} = ANY("));
                self.builder.push_bind(vals);
                self.builder.push(")");
            }
        }
        self
    }

    /// Adds an AND array overlap condition using PostgreSQL's `&&` operator.
    ///
    /// Similar to the main `WhereBuilder::array_overlap()` method but within an AND group.
    pub fn array_overlap<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} &&"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    /// Adds an AND array contains condition using PostgreSQL's `@>` operator.
    ///
    /// Similar to the main `WhereBuilder::array_contains_any()` method but within an AND group.
    pub fn array_contains_any<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} @>"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    /// Adds an AND array contained-by condition using PostgreSQL's `<@` operator.
    ///
    /// Similar to the main `WhereBuilder::array_contained_by()` method but within an AND group.
    pub fn array_contained_by<T>(&mut self, column: &str, values: Option<Vec<T>>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres> + PgHasArrayType,
    {
        if let Some(vals) = values {
            if !vals.is_empty() {
                self.add_condition(&format!("{column} <@"));
                self.builder.push_bind(vals);
            }
        }
        self
    }

    /// Adds an AND `IS NULL` condition if `check` is true.
    ///
    /// Similar to the main `WhereBuilder::is_null()` method but within an AND group.
    pub fn is_null(&mut self, column: &str, check: bool) -> &mut Self {
        if check {
            self.add_condition(&format!("{column} IS NULL"));
        }
        self
    }

    /// Adds an AND `IS NOT NULL` condition if `check` is true.
    ///
    /// Similar to the main `WhereBuilder::is_not_null()` method but within an AND group.
    pub fn is_not_null(&mut self, column: &str, check: bool) -> &mut Self {
        if check {
            self.add_condition(&format!("{column} IS NOT NULL"));
        }
        self
    }

    /// Adds an AND case-insensitive equality condition.
    ///
    /// Similar to the main `WhereBuilder::eq_ci()` method but within an AND group.
    pub fn eq_ci(&mut self, column: &str, value: Option<String>) -> &mut Self {
        self.and_if(
            &format!("LOWER({column})"),
            "=",
            value.map(|v| v.to_lowercase()),
        )
    }

    /// Adds an AND raw SQL condition with parameter binding.
    ///
    /// Similar to the main `WhereBuilder::raw()` method but within an AND group.
    /// Use with caution to avoid SQL injection.
    pub fn raw<T>(&mut self, sql: &str, value: Option<T>) -> &mut Self
    where
        T: 'a + Send + sqlx::Type<Postgres> + sqlx::Encode<'a, Postgres>,
    {
        if let Some(val) = value {
            self.add_condition(sql);
            self.builder.push_bind(val);
        }
        self
    }

    /// Creates a nested OR group within this AND group.
    ///
    /// This allows building complex logical expressions like `(a = 1 AND (b = 2 OR c = 3))`.
    /// The OR group is only added if at least one condition is added within the closure.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use query_builder::WhereBuilder;
    ///
    /// let query = WhereBuilder::new("SELECT * FROM users")
    ///     .and_group(|and_group| {
    ///         and_group.eq("status", Some("active"))
    ///                  .or_group(|or_group| {
    ///                      or_group.eq("role", Some("admin"))
    ///                              .eq("role", Some("moderator"));
    ///                  });
    ///     })
    ///     .build();
    /// // Generates: SELECT * FROM users WHERE (status = $1 AND (role = $2 OR role = $3))
    /// ```
    pub fn or_group<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut OrGroup<'_, 'a>),
    {
        self.add_condition("(");
        let mut group = OrGroup::new(self.builder);
        f(&mut group);
        self.builder.push(")");
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complex_or_conditions_with_null_checks() {
        // Test the specific example: ((name = "Daniel" AND deleted_at IS NULL) OR (name = "Sven" AND deleted_at IS NULL))
        let query = WhereBuilder::new("SELECT * FROM users")
            .or_group(|group| {
                group
                    .and_group(|and_group| {
                        and_group
                            .eq("name", Some("Daniel"))
                            .is_null("deleted_at", true);
                    })
                    .and_group(|and_group| {
                        and_group
                            .eq("name", Some("Sven"))
                            .is_null("deleted_at", true);
                    });
            })
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE ((name =$1 AND deleted_at IS NULL) OR (name =$2 AND deleted_at IS NULL))"
        );
    }

    #[test]
    fn test_simple_and_conditions() {
        let query = WhereBuilder::new("SELECT * FROM products")
            .eq("category", Some("electronics"))
            .gt("price", Some(100))
            .active_only(true)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM products WHERE category =$1 AND price >$2 AND deleted_at IS NULL"
        );
    }

    #[test]
    fn test_nested_or_within_and_group() {
        // Test: status = "active" AND (role = "admin" OR role = "moderator") AND deleted_at IS NULL
        let query = WhereBuilder::new("SELECT * FROM users")
            .eq("status", Some("active"))
            .or_group(|group| {
                group
                    .eq("role", Some("admin"))
                    .eq("role", Some("moderator"));
            })
            .active_only(true)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE status =$1 AND (role =$2 OR role =$3) AND deleted_at IS NULL"
        );
    }

    #[test]
    fn test_multiple_or_groups() {
        // Test: (name = "John" OR name = "Jane") AND (status = "active" OR status = "pending")
        let query = WhereBuilder::new("SELECT * FROM users")
            .or_group(|group| {
                group.eq("name", Some("John")).eq("name", Some("Jane"));
            })
            .or_group(|group| {
                group
                    .eq("status", Some("active"))
                    .eq("status", Some("pending"));
            })
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE (name =$1 OR name =$2) AND (status =$3 OR status =$4)"
        );
    }

    #[test]
    fn test_complex_nested_groups() {
        // Test: category = "books" AND ((author = "Smith" AND year > 2020) OR (author = "Jones" AND year > 2019))
        let query = WhereBuilder::new("SELECT * FROM books")
            .eq("category", Some("books"))
            .or_group(|group| {
                group
                    .and_group(|and_group| {
                        and_group.eq("author", Some("Smith")).gt("year", Some(2020));
                    })
                    .and_group(|and_group| {
                        and_group.eq("author", Some("Jones")).gt("year", Some(2019));
                    });
            })
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM books WHERE category =$1 AND ((author =$2 AND year >$3) OR (author =$4 AND year >$5))"
        );
    }

    #[test]
    fn test_conditional_values_with_none() {
        // Test that None values don't add conditions
        let query = WhereBuilder::new("SELECT * FROM users")
            .eq("name", Some("Alice"))
            .eq("age", None::<i32>)
            .eq("status", Some("active"))
            .build();

        let sql = query.sql();
        assert_eq!(sql, "SELECT * FROM users WHERE name =$1 AND status =$2");
    }

    #[test]
    fn test_like_operations_in_groups() {
        // Test LIKE operations within OR groups
        let query = WhereBuilder::new("SELECT * FROM articles")
            .or_group(|group| {
                group
                    .like("title", Some("Rust".to_string()), LikePosition::Contains)
                    .like(
                        "content",
                        Some("programming".to_string()),
                        LikePosition::Contains,
                    );
            })
            .active_only(true)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM articles WHERE (title LIKE$1 OR content LIKE$2) AND deleted_at IS NULL"
        );
    }

    #[test]
    fn test_array_operations() {
        // Test array membership conditions
        let query = WhereBuilder::new("SELECT * FROM users")
            .in_array("role", Some(vec!["admin", "moderator", "editor"]))
            .not_in("status", Some(vec!["banned", "suspended"]))
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE role = ANY($1) AND status != ALL($2)"
        );
    }

    #[test]
    fn test_array_overlap_operations() {
        // Test array overlap condition
        let query = WhereBuilder::new("SELECT * FROM properties")
            .array_overlap("asset_class", Some(vec!["hotel", "restaurant"]))
            .eq("status", Some("active"))
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM properties WHERE asset_class &&$1 AND status =$2"
        );
    }

    #[test]
    fn test_array_contains_any_operations() {
        // Test array contains condition
        let query = WhereBuilder::new("SELECT * FROM properties")
            .array_contains_any("asset_class", Some(vec!["hotel", "restaurant"]))
            .gt("price", Some(1000))
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM properties WHERE asset_class @>$1 AND price >$2"
        );
    }

    #[test]
    fn test_array_contained_by_operations() {
        // Test array contained-by condition
        let query = WhereBuilder::new("SELECT * FROM properties")
            .array_contained_by("asset_class", Some(vec!["hotel", "restaurant", "retail"]))
            .active_only(true)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM properties WHERE asset_class <@$1 AND deleted_at IS NULL"
        );
    }

    #[test]
    fn test_combined_array_operations() {
        // Test multiple array operations together
        let query = WhereBuilder::new("SELECT * FROM properties")
            .array_overlap("tags", Some(vec!["luxury", "downtown"]))
            .array_contains_any("amenities", Some(vec!["pool", "gym"]))
            .array_contained_by(
                "categories",
                Some(vec!["commercial", "residential", "mixed"]),
            )
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM properties WHERE tags &&$1 AND amenities @>$2 AND categories <@$3"
        );
    }

    #[test]
    fn test_array_operations_with_empty_arrays() {
        // Test that empty arrays don't add conditions
        let query = WhereBuilder::new("SELECT * FROM properties")
            .array_overlap("asset_class", Some(Vec::<String>::new()))
            .array_contains_any("tags", None::<Vec<String>>)
            .eq("status", Some("active"))
            .build();

        let sql = query.sql();
        assert_eq!(sql, "SELECT * FROM properties WHERE status =$1");
    }

    #[test]
    fn test_array_operations_in_or_groups() {
        // Test array operations within OR groups
        let query = WhereBuilder::new("SELECT * FROM properties")
            .or_group(|group| {
                group
                    .array_overlap("asset_class", Some(vec!["hotel", "restaurant"]))
                    .array_contains_any("amenities", Some(vec!["pool", "spa"]));
            })
            .active_only(true)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM properties WHERE (asset_class &&$1 OR amenities @>$2) AND deleted_at IS NULL"
        );
    }

    #[test]
    fn test_array_operations_in_and_groups() {
        // Test array operations within AND groups
        let query = WhereBuilder::new("SELECT * FROM properties")
            .and_group(|group| {
                group
                    .array_overlap("tags", Some(vec!["luxury"]))
                    .array_contained_by("categories", Some(vec!["residential", "commercial"]));
            })
            .gt("price", Some(500000))
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM properties WHERE (tags &&$1 AND categories <@$2) AND price >$3"
        );
    }

    #[test]
    fn test_json_operations() {
        // Test JSON operations
        let query = WhereBuilder::new("SELECT * FROM users")
            .json_eq("metadata", "department", Some("engineering"))
            .json_contains(
                "preferences",
                Some(serde_json::json!({"notifications": true})),
            )
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE metadata->>'department' = $1 AND preferences @>$2"
        );
    }

    #[test]
    fn test_paginate_basic() {
        // Test basic pagination with ASC order
        let pagination = Paginate::asc("name", 1, 10);
        let query = WhereBuilder::new("SELECT * FROM users")
            .eq("status", Some("active"))
            .paginate(&pagination)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE status =$1 ORDER BY name ASC LIMIT 10 OFFSET 0"
        );
    }

    #[test]
    fn test_paginate_desc() {
        // Test pagination with DESC order
        let pagination = Paginate::desc("created_at", 2, 5);
        let query = WhereBuilder::new("SELECT * FROM posts")
            .paginate(&pagination)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM posts ORDER BY created_at DESC LIMIT 5 OFFSET 5"
        );
    }

    #[test]
    fn test_paginate_with_where_conditions() {
        // Test pagination combined with WHERE conditions
        let pagination = Paginate::new("updated_at", SortDirection::Desc, 3, 20);
        let query = WhereBuilder::new("SELECT * FROM articles")
            .eq("published", Some(true))
            .gt("views", Some(1000))
            .paginate(&pagination)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM articles WHERE published =$1 AND views >$2 ORDER BY updated_at DESC LIMIT 20 OFFSET 40"
        );
    }

    #[test]
    fn test_paginate_default() {
        // Test default pagination
        let pagination = Paginate::default();
        let query = WhereBuilder::new("SELECT * FROM products")
            .paginate(&pagination)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM products ORDER BY id ASC LIMIT 20 OFFSET 0"
        );
    }

    #[test]
    fn test_paginate_offset_calculation() {
        // Test offset calculation for different pages
        let pagination1 = Paginate::asc("id", 1, 10);
        assert_eq!(pagination1.offset(), 0);

        let pagination2 = Paginate::asc("id", 2, 10);
        assert_eq!(pagination2.offset(), 10);

        let pagination3 = Paginate::asc("id", 5, 25);
        assert_eq!(pagination3.offset(), 100);
    }

    #[test]
    fn test_paginate_minimum_values() {
        // Test that page and page_size are clamped to minimum 1
        let pagination = Paginate::new("name", SortDirection::Asc, 0, 0);
        assert_eq!(pagination.page, 1);
        assert_eq!(pagination.page_size, 1);
        assert_eq!(pagination.offset(), 0);
    }

    #[test]
    fn test_paginate_sql_fragment() {
        // Test the SQL fragment generation directly
        let pagination = Paginate::new("title", SortDirection::Desc, 2, 15);
        assert_eq!(
            pagination.to_sql_fragment(),
            " ORDER BY title DESC LIMIT 15 OFFSET 15"
        );
    }

    #[test]
    fn test_sort_direction_display() {
        // Test SortDirection Display implementation
        assert_eq!(format!("{}", SortDirection::Asc), "ASC");
        assert_eq!(format!("{}", SortDirection::Desc), "DESC");
    }

    #[test]
    fn test_paginate_with_complex_query() {
        // Test pagination with complex WHERE conditions and groups
        let pagination = Paginate::desc("score", 1, 5);
        let query = WhereBuilder::new("SELECT * FROM users")
            .eq("active", Some(true))
            .or_group(|group| {
                group
                    .eq("role", Some("admin"))
                    .eq("role", Some("moderator"));
            })
            .between("age", Some(18), Some(65))
            .paginate(&pagination)
            .build();

        let sql = query.sql();
        assert_eq!(
            sql,
            "SELECT * FROM users WHERE active =$1 AND (role =$2 OR role =$3) AND age >=$4 AND age <=$5 ORDER BY score DESC LIMIT 5 OFFSET 0"
        );
    }
}
