# Get Wikidata qualifiers for a given property of a given item

N.B. In order to provide for consistently structured output, this
function outputs either id or value for each qualifier. The user should
keep in mind that some of these come with additional detail (e.g. the
unit, precision, or reference calendar).

## Usage

``` r
tw_get_qualifiers(
  id,
  p,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0,
  id_l = NULL
)
```

## Arguments

- id:

  A character vector of length 1, must start with Q, e.g. "Q254" for
  Wolfgang Amadeus Mozart.

- p:

  A character vector of length 1, a property. Must always start with the
  capital letter "P", e.g. "P31" for "instance of".

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

- id_l:

  Defaults to NULL. If given, must be an object or list such as the one
  generated with
  [`WikidataR::get_item()`](https://rdrr.io/pkg/WikidataR/man/get_item.html).
  If given, and the requested id is actually present in `id_l`, then no
  query to Wikidata servers is made.

## Value

A data frame (a tibble) with eight columns: `id` for the input id,
`property`, `qualifier_id`, `qualifier_property`, `qualifier_value`,
`rank`, `qualifier_value_type`, and `set` (to distinguish sets of data
when a property is present more than once)

## Examples

``` r
if (interactive()) {
  tidywikidatar::tw_get_qualifiers(id = "Q180099", p = "P26", language = "en")
}

#' ## using `tw_test_items` in examples in order to show output without calling
## on Wikidata servers

tidywikidatar::tw_get_qualifiers(
  id = "Q180099",
  p = "P26",
  language = "en",
  id_l = tw_test_items
)
#> # A tibble: 6 × 8
#>   id      property qualifier_id qualifier_property qualifier_value      
#>   <chr>   <chr>    <chr>        <chr>              <chr>                
#> 1 Q180099 P26      Q314252      P580               +1936-00-00T00:00:00Z
#> 2 Q180099 P26      Q314252      P582               +1950-00-00T00:00:00Z
#> 3 Q180099 P26      Q594736      P580               +1923-00-00T00:00:00Z
#> 4 Q180099 P26      Q594736      P582               +1928-00-00T00:00:00Z
#> 5 Q180099 P26      Q2144944     P580               +1928-00-00T00:00:00Z
#> 6 Q180099 P26      Q2144944     P582               +1935-00-00T00:00:00Z
#> # ℹ 3 more variables: qualifier_value_type <chr>, rank <chr>, set <int>
```
