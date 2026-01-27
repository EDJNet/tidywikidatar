# Writes search to cache

Writes search to cache. Typically used internally, but exported to
enable custom caching solutions.

## Usage

``` r
tw_write_search_to_cache(
  search_df,
  type = "item",
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- search_df:

  A data frame with four columns typically generated with
  `tw_search(include_search = TRUE)`.

- type:

  Defaults to "item". Either "item" or "property".

- language:

  Language to be used for the search. Can be set once per session with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md).
  If not set, defaults to "en". For a full list, see [the dedicated
  Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- response_language:

  Language to be used for the returned labels and descriptions.
  Corresponds to the `uselang` parameter of the MediaWiki API, as
  described [in the official
  documentation](https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities).
  Can be set once per session with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md).
  If not set, defaults to "en". For a full list, see [all available
  language
  codes](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- cache:

  Defaults to `NULL`. If given, it should be given either `TRUE` or
  `FALSE.` Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- overwrite_cache:

  Defaults to `FALSE`. If `TRUE`, overwrites cache.

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

search_from_api <- tw_search(search = "Sylvia Pankhurst", include_search = TRUE)

search_from_cache <- tw_get_cached_search("Sylvia Pankhurst")

nrow(search_from_cache) == 0 # expect TRUE, as nothing has yet been stored in cache
#> [1] TRUE

tw_write_search_to_cache(search_df = search_from_api)

search_from_cache <- tw_get_cached_search("Sylvia Pankhurst")

search_from_cache
#> # A tibble: 0 × 4
#> # ℹ 4 variables: search <chr>, id <chr>, label <chr>, description <chr>
```
