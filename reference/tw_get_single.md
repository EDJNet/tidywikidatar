# Return (most) information from a Wikidata item in a tidy format from a single Wikidata identifier

Return (most) information from a Wikidata item in a tidy format from a
single Wikidata identifier

## Usage

``` r
tw_get_single(
  id,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  read_cache = TRUE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0,
  id_l = NULL
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

- read_cache:

  Logical, defaults to TRUE. Mostly used internally to prevent checking
  if an item is in cache if it is already known that it is not in cache.

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
  [`WikidataR::get_item()`](https://rdrr.io/pkg/WikidataR/man/get_item.html).
  If given, and the requested id is actually present in `id_l`, then no
  query to Wikidata servers is made.

## Value

A data.frame (a tibble) with four columns (id, property, value, and
rank). If item not found or trouble connecting with the server, a data
frame with four columns and zero rows is returned, with the warning as
an attribute, which can be retrieved with `attr(output, "warning"))`

## Examples

``` r
if (interactive()) {
  tidywikidatar:::tw_get_single(
    id = "Q180099",
    language = "en"
  )
}

## using `tw_test_items` in examples in order to show output without calling
## on Wikidata servers

tidywikidatar:::tw_get_single(
  id = "Q180099",
  language = "en",
  id_l = tw_test_items
)
#> # A tibble: 188 × 4
#>    id      property value         rank  
#>    <chr>   <chr>    <chr>         <chr> 
#>  1 Q180099 label_en Margaret Mead NA    
#>  2 Q180099 P21      Q6581072      normal
#>  3 Q180099 P214     44302511      normal
#>  4 Q180099 P106     Q674426       normal
#>  5 Q180099 P106     Q4773904      normal
#>  6 Q180099 P106     Q36180        normal
#>  7 Q180099 P106     Q2526255      normal
#>  8 Q180099 P373     Margaret Mead normal
#>  9 Q180099 P244     n78093416     normal
#> 10 Q180099 P227     118579789     normal
#> # ℹ 178 more rows
```
