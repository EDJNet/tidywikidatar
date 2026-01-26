# Gets id of Wikipedia pages from local cache

Mostly used internally.

## Usage

``` r
tw_get_cached_wikipedia_page_qid(
  title,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- title:

  Title of a Wikipedia page or final parts of its url. If given, url can
  be left empty, but language must be provided.

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

- cache_connection:

  Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to TRUE. If FALSE, leaves the connection open.

## Value

If data present in cache, returns a data frame with cached data.

## Examples

``` r
if (interactive()) {
  tw_set_cache_folder(path = tempdir())
  tw_enable_cache()
  tw_create_cache_folder(ask = FALSE)

  df_from_api <- tw_get_wikipedia_page_qid(title = "Margaret Mead", language = "en")

  df_from_cache <- tw_get_cached_wikipedia_page_qid(
    title = "Margaret Mead",
    language = "en"
  )

  df_from_cache
}
```
