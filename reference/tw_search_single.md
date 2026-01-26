# Search for Wikidata items or properties and return Wikidata id, label, and description.

This search returns only items, use
[`tw_search_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_search_property.md)
for properties.

## Usage

``` r
tw_search_single(
  search,
  type = "item",
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language(),
  limit = 10,
  include_search = FALSE,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
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
  If not set, defaults to "en". For a full list, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- response_language:

  Language to be used for the returned labels and descriptions.
  Corresponds to the `uselang` parameter of the MediaWiki API:
  https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities.
  Can be set once per session with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md).
  If not set, defaults to "en". For a full list, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- limit:

  Maximum numbers of responses to be given.

- include_search:

  Logical, defaults to FALSE. If TRUE, the search is returned as an
  additional column.

- cache:

  Defaults to NULL. If given, it should be given either TRUE or FALSE.
  Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- overwrite_cache:

  Defaults to FALSE. If TRUE, overwrites cache.

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

A data frame (a tibble) with three columns (id, label, and description),
and as many rows as there are results (by default, limited to 10). Four
columns when `include_search` is set to TRUE.

## Examples

``` r
tidywikidatar:::tw_search_single(search = "Sylvia Pankhurst")
#> # A tibble: 4 × 3
#>   id         label                                                   description
#>   <chr>      <chr>                                                   <chr>      
#> 1 Q298213    Sylvia Pankhurst                                        English fe…
#> 2 Q24298894  Sylvia Pankhurst                                        sculpture …
#> 3 Q136209787 Sylvia Pankhurst: Suffragette, Socialist and Scourge o… book 2013  
#> 4 Q136210335 Sylvia Pankhurst: A Life in Radical Politics            book 1999  
```
