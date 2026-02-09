# Return (most) information from a Wikidata item in a tidy format

Return (most) information from a Wikidata item in a tidy format

## Usage

``` r
tw_get(
  id,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0,
  id_l = NULL,
  user_agent = tidywikidatar::tw_get_user_agent()
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

- id_l:

  Defaults to `NULL`. If given, must be an object or list such as the
  one generated with
  [`tw_get_item()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_item.md).
  If given, and the requested id is actually present in `id_l`, then no
  query to Wikidata servers is made.

- user_agent:

  Defaults to `NULL`. If not given, implicitly defaults to current
  package name (`tidywikidatar`) and version.

## Value

A data.frame (a tibble) with three columns (`id`, `property`, and
`value`).

## Examples

``` r
if (interactive()) {
  tw_get(
    id = c("Q180099", "Q228822"),
    language = "en"
  )
}

## using `tw_test_items` in examples in order to show output without calling
## on Wikidata servers

tw_get(
  id = c("Q180099", "Q228822"),
  language = "en",
  id_l = tw_test_items
)
#> # A tibble: 373 × 4
#>    id      property value                         rank  
#>    <chr>   <chr>    <chr>                         <chr> 
#>  1 Q180099 label_en Margaret Mead                 NA    
#>  2 Q180099 P21      Q6581072                      normal
#>  3 Q180099 P214     44302511                      normal
#>  4 Q180099 P106     Q4773904                      normal
#>  5 Q180099 P373     Margaret Mead                 normal
#>  6 Q180099 P244     n78093416                     normal
#>  7 Q180099 P227     118579789                     normal
#>  8 Q180099 P18      Margaret Mead (1901-1978).jpg normal
#>  9 Q180099 P509     Q212961                       normal
#> 10 Q180099 P19      Q1345                         normal
#> # ℹ 363 more rows
```
