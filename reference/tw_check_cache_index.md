# Check if cache table is indexed

Tested only with SQLite and MySql. May work with other drivers. Used to
check if given cache table is indexed (if created with any version of
`tidywikidatar` before 0.6, they are probably not indexed and less
efficient).

## Usage

``` r
tw_check_cache_index(
  table_name = NULL,
  type = "item",
  show_details = FALSE,
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- table_name:

  Name of the table in the database. If given, it takes precedence over
  other parameters.

- type:

  Defaults to "item". Type of cache file to output. Values typically
  used by `tidywikidatar` include "item", "search_item",
  "search_property", and "qualifier".

- show_details:

  Logical, defaults to FALSE. If FALSE, return a logical vector of
  length one (TRUE if the table was indexed, FALSE if it was not). If
  TRUE, returns a data frame with more details about the index.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  "en" if not set. Used to limit the data to be cached. Use
  "all_available" to keep all data. For available values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- response_language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  "en" if not set. Relevant only when type is set to "search_item" or
  "search_property". See
  [`tw_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_search.md)
  for details.

- cache:

  Defaults to `NULL`. If given, it should be given either `TRUE` or
  `FALSE`. Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- cache_connection:

  Defaults to `NULL`. If `NULL`, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to `TRUE`. If `FALSE`, leaves the connection to cache open.

## Value

If `show_details` is set to FALSE, return a logical vector of length one
(TRUE if the table was indexed, FALSE if it was not). If `show_details`
is set to TRUE, returns a data frame with more details about the index.

## Examples

``` r
if (interactive()) {
  tw_enable_cache()
  tw_set_cache_folder(path = fs::path(
    fs::path_home_r(),
    "R",
    "tw_data"
  ))

  tw_set_language(language = "en")

  tw_check_cache_index()
}
```
