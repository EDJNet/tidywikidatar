# Reset Wikipedia page link cache

Removes from cache the table where data typically gathered with
[`tw_get_wikipedia_page_links()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_links.md)
are stored.

## Usage

``` r
tw_reset_wikipedia_page_links_cache(
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE,
  ask = TRUE
)
```

## Arguments

- language:

  Two-letter language code used to define the Wikipedia version to use.
  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". If url given, this can be left empty.

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

- ask:

  Logical, defaults to TRUE. If FALSE, and cache folder does not exist,
  it just creates it without asking (useful for non-interactive
  sessions).

## Value

Nothing, used for its side effects.

## Examples

``` r
if (interactive()) {
  tw_reset_wikipedia_page_links_cache()
}
```
