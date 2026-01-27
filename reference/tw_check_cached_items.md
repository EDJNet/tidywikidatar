# Check if given items are present in cache

Check if given items are present in cache

## Usage

``` r
tw_check_cached_items(
  id,
  language = tidywikidatar::tw_get_language(),
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- id:

  A character vector, must start with Q, e.g. "Q180099" for the
  anthropologist Margaret Mead. Can also be a data frame of one row,
  typically generated with
  [`tw_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_search.md)
  or a combination of
  [`tw_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_search.md)
  and
  [`tw_filter_first()`](https://edjnet.github.io/tidywikidatar/reference/tw_filter_first.md).

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- cache_connection:

  Defaults to `NULL`. If `NULL`, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to `TRUE`. If `FALSE`, leaves the connection to cache open.

## Value

A character vector with IDs of items present in cache. If no item found
in cache, returns `NULL`.

## Examples

``` r
if (interactive()) {
  tw_set_cache_folder(path = tempdir())
  tw_enable_cache()
  tw_create_cache_folder(ask = FALSE)

  # add three items to local cache
  invisible(tw_get(id = "Q180099", language = "en"))
  invisible(tw_get(id = "Q228822", language = "en"))
  invisible(tw_get(id = "Q184992", language = "en"))

  # check if these other items are in cache
  items_in_cache <- tw_check_cached_items(
    id = c(
      "Q180099",
      "Q228822",
      "Q76857"
    ),
    language = "en"
  )
  # it should return only the two items from the current list of id
  # but not other item already in cache
  items_in_cache
}
```
