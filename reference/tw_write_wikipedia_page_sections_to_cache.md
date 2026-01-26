# Write Wikipedia page links to cache

Mostly used internally by `tidywikidatar`, use with caution to keep
caching consistent.

## Usage

``` r
tw_write_wikipedia_page_sections_to_cache(
  df,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- df:

  A data frame typically generated with
  [`tw_get_wikipedia_page_sections()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_sections.md).

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

- overwrite_cache:

  Logical, defaults to FALSE. If TRUE, it overwrites the table in the
  local sqlite database. Useful if the original Wikidata object has been
  updated.

- cache_connection:

  Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to TRUE. If FALSE, leaves the connection to cache open.

## Value

Silently returns the same data frame provided as input. Mostly used
internally for its side effects.

## Examples

``` r
if (interactive()) {
  df <- tw_get_wikipedia_page_sections(
    title = "Margaret Mead",
    language = "en",
    cache = FALSE
  )

  tw_write_wikipedia_page_sections_to_cache(
    df = df,
    language = "en"
  )
}
```
