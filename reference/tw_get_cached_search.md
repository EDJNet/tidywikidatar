# Retrieve cached search

Retrieve cached search

## Usage

``` r
tw_get_cached_search(
  search,
  type = "item",
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language(),
  cache = NULL,
  include_search = FALSE,
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

- cache:

  Defaults to NULL. If given, it should be given either TRUE or FALSE.
  Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- include_search:

  Logical, defaults to FALSE. If TRUE, the search is returned as an
  additional column.

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

tw_set_cache_folder(path = tempdir())
tw_enable_cache()
tw_create_cache_folder(ask = FALSE)

search_from_api <- tw_search("Sylvia Pankhurst")
search_from_api
#> # A tibble: 4 × 3
#>   id         label                                                   description
#>   <chr>      <chr>                                                   <chr>      
#> 1 Q298213    Sylvia Pankhurst                                        English fe…
#> 2 Q24298894  Sylvia Pankhurst                                        sculpture …
#> 3 Q136209787 Sylvia Pankhurst: Suffragette, Socialist and Scourge o… book 2013  
#> 4 Q136210335 Sylvia Pankhurst: A Life in Radical Politics            book 1999  

df_from_cache <- tw_get_cached_search("Sylvia Pankhurst")
df_from_cache
#> # A tibble: 0 × 3
#> # ℹ 3 variables: id <chr>, label <chr>, description <chr>
```
