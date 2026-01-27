# Writes item to cache

Writes item to cache. Typically used internally, but exported to enable
custom caching solutions.

## Usage

``` r
tw_write_item_to_cache(
  item_df,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- item_df:

  A data frame with three columns typically generated with
  [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md).

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- cache:

  Defaults to `NULL`. If given, it should be given either `TRUE` or
  `FALSE`. Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- overwrite_cache:

  Logical, defaults to `FALSE`. If `TRUE`, it overwrites the table in
  the local sqlite database. Useful if the original Wikidata object has
  been updated.

- cache_connection:

  Defaults to `NULL`. If `NULL`, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to `TRUE`. If `FALSE`, leaves the connection to cache open.

## Value

Nothing, used for its side effects.

## Examples

``` r
tw_set_cache_folder(path = fs::path(tempdir(), paste(sample(letters, 24), collapse = "")))
tw_create_cache_folder(ask = FALSE)
tw_disable_cache()

df_from_api <- tw_get(id = "Q180099", language = "en")

df_from_cache <- tw_get_cached_item(
  id = "Q180099",
  language = "en"
)

is.null(df_from_cache) # expect TRUE, as nothing has yet been stored in cache
#> [1] FALSE

tw_write_item_to_cache(
  item_df = df_from_api,
  language = "en",
  cache = TRUE
)

df_from_cache <- tw_get_cached_item(
  id = "Q180099",
  language = "en",
  cache = TRUE
)

is.null(df_from_cache) # expect a data frame, same as df_from_api
#> [1] FALSE
```
