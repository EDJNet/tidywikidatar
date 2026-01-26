# Return a connection to be used for caching

Return a connection to be used for caching

## Usage

``` r
tw_connect_to_cache(
  connection = NULL,
  RSQLite = NULL,
  language = tidywikidatar::tw_get_language(),
  cache = NULL
)
```

## Arguments

- connection:

  Defaults to NULL. If NULL, uses local SQLite database. If given, must
  be a connection object or a list with relevant connection settings
  (see example).

- RSQLite:

  Defaults to NULL, expected either NULL or logical. If set to `FALSE`,
  details on the database connection must be given either as a named
  list in the connection parameter, or with
  [`tw_set_cache_db()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_cache_db.md)
  as environment variables.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- cache:

  Defaults to NULL. If given, it should be given either TRUE or FALSE.
  Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

## Value

A connection object.

## Examples

``` r
# \donttest{
if (interactive()) {
  cache_connection <- pool::dbPool(
    RSQLite::SQLite(), # or e.g. odbc::odbc(),
    Driver = ":memory:", # or e.g. "MariaDB",
    Host = "localhost",
    database = "example_db",
    UID = "example_user",
    PWD = "example_pwd"
  )
  tw_connect_to_cache(cache_connection)


  db_settings <- list(
    driver = "MySQL",
    host = "localhost",
    server = "localhost",
    port = 3306,
    database = "tidywikidatar",
    user = "secret_username",
    pwd = "secret_password"
  )

  tw_connect_to_cache(db_settings)
}
# }
```
