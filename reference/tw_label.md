# Gets labels for all columns with names such as "id" and "property".

Gets labels for all columns with names such as "id" and "property".

## Usage

``` r
tw_label(
  df,
  value = TRUE,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
)
```

## Arguments

- df:

  A data frame, typically generated with other `tidywikidatar` functions
  such as
  [`tw_get_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property.md)

- value:

  Logical, defaults to TRUE. If TRUE, it tries to get labels for all
  supposed id in the column called value. May break if the columns
  include some value which starts with Q and some digits, but is not a
  wikidata id.

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

- wait:

  In seconds, defaults to 0. Time to wait between queries to Wikidata.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

## Value

A data frame, with the same shape as the input data frame, but with
labels instead of identifiers.

## Examples

``` r
# \donttest{
if (interactive()) {
  tw_get_qualifiers(id = "Q180099", p = "P26", language = "en") %>%
    head(2) %>%
    tw_label()
}
# }
```
