# Add index to caching table for search queries for increased speed

Tested only with SQLite and MySql. May work with other drivers.

## Usage

``` r
tw_index_cache_search(
  table_name = NULL,
  check_first = TRUE,
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

- check_first:

  Logical, defaults to `TRUE`. If `TRUE`, then before executing anything
  on the database it checks if the given table has already been indexed.
  If it has, it does nothing and returns only an informative message.

- type:

  Defaults to "item". Type of cache file to output. Values typically
  used by `tidywikidatar` include "item", "search_item",
  "search_property", and "qualifier".

- show_details:

  Logical, defaults to `FALSE`. If `FALSE`, return the function adds the
  index to the database, but does not return anything. If `TRUE`,
  returns a data frame with more details about the index.

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

If `show_details` is set to `FALSE`, nothing, used only for its side
effects (add index to caching table). If `TRUE`, a data frame, same as
the output of `tw_check_cache_index(show_details = TRUE)`.

## Details

To ensure smooth functioning, the search column in the cache table is
transformed into a column of type `varchar` and length 255.

## Examples

``` r
if (interactive()) {
  tw_enable_cache()
  tw_set_cache_folder(path = fs::path(
    fs::path_home_r(),
    "R",
    "tw_data"
  ))

  tw_index_cache_search()
}
```
