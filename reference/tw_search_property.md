# Search for Wikidata properties in Wikidata and return Wikidata id, label, and description.

This search returns only properties, use
[`tw_search_item()`](https://edjnet.github.io/tidywikidatar/reference/tw_search_item.md)
for properties.

## Usage

``` r
tw_search_property(
  search,
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
  If not set, defaults to "en". For a full list, see
  <https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all>all
  available language codes.

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
limited to 10).

## Examples

``` r
tw_search_property(search = "gender")
#> # A tibble: 9 × 3
#>   id     label                                  description                     
#>   <chr>  <chr>                                  <chr>                           
#> 1 P21    sex or gender                          sex or gender identity of human…
#> 2 P5185  grammatical gender                     grammatical gender of the word  
#> 3 P5278  surname for other gender               gender inflection of surname, p…
#> 4 P1560  given name version for other gender    equivalent name (with respect t…
#> 5 P7419  gender educated                        genders educated at this educat…
#> 6 P9827  GSSO ID                                identifier for controlled vocab…
#> 7 P2433  gender of a scientific name of a genus determines the correct form of …
#> 8 P10339 semantic gender                        used when a word is only used t…
#> 9 P9279  Egapro gender equality index           index value for a company per r…
```
