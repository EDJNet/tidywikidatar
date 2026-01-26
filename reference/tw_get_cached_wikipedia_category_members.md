# Gets members of Wikipedia categories from local cache

Mostly used internally.

## Usage

``` r
tw_get_cached_wikipedia_category_members(
  category,
  type = "page",
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- category:

  Title of a Wikipedia category page or final parts of its url. Must
  include "Category:", or equivalent in other languages. If given, url
  can be left empty, but language must be provided.

- type:

  Defaults to "page", defines which kind of members of a category are
  returned. Valid values include "page", "file", and "subcat" (for
  sub-category). Corresponds to `cmtype`. For details, see
  <https://www.mediawiki.org/wiki/API:Categorymembers>

- language:

  Two-letter language code used to define the Wikipedia version to use.
  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". If url given, this can be left empty.

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

  Defaults to TRUE. If FALSE, leaves the connection to cache open.

## Value

If data present in cache, returns a data frame with cached data.

## Examples

``` r
if (interactive()) {
  tw_set_cache_folder(path = tempdir())
  tw_enable_cache()
  tw_create_cache_folder(ask = FALSE)

  df_from_api <- tw_get_wikipedia_page_qid(category = "Margaret Mead", language = "en")

  df_from_cache <- tw_get_cached_wikipedia_category_members(
    category = "Margaret Mead",
    language = "en"
  )

  df_from_cache
}
```
