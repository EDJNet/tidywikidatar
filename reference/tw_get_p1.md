# Get Wikidata property of an item as a character vector of the same length as input

This function wraps
[`tw_get_p()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_same_length.md),
but always sets `only_first` and `preferred` to TRUE in order to give
back always a character vector.

## Usage

``` r
tw_get_p1(
  id,
  p,
  latest_start_time = FALSE,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
)
```

## Arguments

- id:

  A character vector, must start with Q, e.g. "Q254" for Wolfgang
  Amadeus Mozart.

- p:

  A character vector, a property. Must always start with the capital
  letter "P", e.g. "P31" for "instance of".

- latest_start_time:

  Logical, defaults to FALSE. If TRUE, returns the property that has the
  most recent start time ("P580") as qualifier. If no such qualifier is
  found, then it is ignored.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- id_df:

  Default to NULL. If given, it should be a dataframe typically
  generated with `tw_get_()`, and is used instead of calling Wikidata or
  replying on cache.

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

A character vector of the same length as the input.

## Examples

``` r
tw_get_p1(id = "Q180099", "P26")
#> [1] "Q594736"
```
