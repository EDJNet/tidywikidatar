# Get URL to a Wikipedia article corresponding to a Wikidata Q identifier in given language

Get URL to a Wikipedia article corresponding to a Wikidata Q identifier
in given language

## Usage

``` r
tw_get_wikipedia(
  id,
  full_link = TRUE,
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

  A character vector of length 1, must start with Q, e.g. "Q254" for
  Wolfgang Amadeus Mozart.

- full_link:

  Logical, defaults to `TRUE`. If `FALSE`, returns only the part of the
  url that corresponds to the title.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- id_df:

  Default to NULL. If given, it should be a dataframe typically
  generated with
  [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md),
  and is used instead of calling Wikidata or using SQLite cache. Ignored
  when `id` is of length more than one.

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

- wait:

  In seconds, defaults to 0. Time to wait between queries to Wikidata.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

## Value

A character vector of the same length as the vector of id given, with
the Wikipedia link in the requested language.

## Examples

``` r
tw_get_wikipedia(id = "Q180099")
#> [1] "https://en.wikipedia.org/wiki/Margaret Mead"
```
