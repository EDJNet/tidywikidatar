# Search for Wikidata items or properties and return Wikidata id, label, and description.

By defaults, this search returns items. Set `type` to property or use
[`tw_search_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_search_property.md)
for properties.

## Usage

``` r
tw_search(
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

A data frame (a tibble) with three columns (`id`, `label`, and
`description`), and as many rows as there are results (by default,
limited to 10). Four columns when `include_search` is set to `TRUE`.

## Examples

``` r
tw_search(search = c("Margaret Mead", "Ruth Benedict"))
#> # A tibble: 15 × 3
#>    id         label                                                  description
#>    <chr>      <chr>                                                  <chr>      
#>  1 Q180099    Margaret Mead                                          American a…
#>  2 Q85724626  Mead & Bateson                                         business o…
#>  3 Q96077616  Margaret Meadows                                       (1718-1781)
#>  4 Q75281958  Lady Margaret Meade-Fetherstonhaugh                    British au…
#>  5 Q76238541  Margaret Meadowe                                       Peerage pe…
#>  6 Q75506638  Margaret Meadows                                       Peerage pe…
#>  7 Q75812372  Margaret Meade-Waldo                                   (died 1954)
#>  8 Q6759717   Margaret Mead Film Festival                            annual fil…
#>  9 Q96617538  Margaret Meador                                        researcher 
#> 10 Q57231017  Margaret Mead Made Me Gay                              2000 hardc…
#> 11 Q228822    Ruth Benedict                                          American a…
#> 12 Q28002866  Ruth Benedict Prize                                    LGBT anthr…
#> 13 Q80896797  Ruth Benedict                                          scientific…
#> 14 Q58589016  Ruth Benedict, Boasian Anthropology, and the Problem … article by…
#> 15 Q124712914 Ruth Benedict on Netherlanders                         journal ar…
```
