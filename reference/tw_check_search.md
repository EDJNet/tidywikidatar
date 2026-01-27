# Checks if an input is a search; if not, it tries to return a search

Mostly used as a convenience function inside other functions to have
consistent inputs.

## Usage

``` r
tw_check_search(
  search,
  type = "item",
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language(),
  limit = 10,
  include_search = FALSE,
  wait = 0,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE
)
```

## Arguments

- search:

  A string to be searched in Wikidata

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

- limit:

  Maximum numbers of responses to be given.

- include_search:

  Logical, defaults to `FALSE`. If `TRUE`, the search is returned as an
  additional column.

- wait:

  In seconds, defaults to 0. Time to wait between queries to Wikidata.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

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

A data frame with three columns, `id`, `label`, and `description`,
filtered by the above criteria. Four columns if `include_search` is set
to `TRUE`.

## Examples

``` r
# The following two lines should give the same result.

tw_check_search("Sylvia Pankhurst")
#> # A tibble: 4 × 3
#>   id         label                                                   description
#>   <chr>      <chr>                                                   <chr>      
#> 1 Q298213    Sylvia Pankhurst                                        English fe…
#> 2 Q24298894  Sylvia Pankhurst                                        sculpture …
#> 3 Q136209787 Sylvia Pankhurst: Suffragette, Socialist and Scourge o… book 2013  
#> 4 Q136210335 Sylvia Pankhurst: A Life in Radical Politics            book 1999  
tw_check_search(tw_search("Sylvia Pankhurst"))
#> # A tibble: 4 × 3
#>   id         label                                                   description
#>   <chr>      <chr>                                                   <chr>      
#> 1 Q298213    Sylvia Pankhurst                                        English fe…
#> 2 Q24298894  Sylvia Pankhurst                                        sculpture …
#> 3 Q136209787 Sylvia Pankhurst: Suffragette, Socialist and Scourge o… book 2013  
#> 4 Q136210335 Sylvia Pankhurst: A Life in Radical Politics            book 1999  
```
