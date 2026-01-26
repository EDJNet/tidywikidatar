# Get Wikidata property of one or more items as a tidy data frame

Get Wikidata property of one or more items as a tidy data frame

## Usage

``` r
tw_get_property(
  id,
  p,
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

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- id_df:

  Default to NULL. If given, it should be a dataframe typically
  generated with `tw_get_()`, and is used instead of calling Wikidata or
  using SQLite cache. Ignored when `id` is of length more than one.

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

A tibble, corresponding to the value for the given property. A tibble of
zero rows if no relevant property found.

## Examples

``` r
# Who were the doctoral advisors - P184 - of Margaret Mead - Q180099?
advisors <- tw_get_property(id = "Q180099", p = "P184")
advisors
#> # A tibble: 2 × 4
#>   id      property value   rank  
#>   <chr>   <chr>    <chr>   <chr> 
#> 1 Q180099 P184     Q228822 normal
#> 2 Q180099 P184     Q76857  normal

# tw_get_label(advisors)

# It is also possible to get one property for many id

if (interactive()) {
  tw_get_property(
    id = c(
      "Q180099",
      "Q228822"
    ),
    p = "P31"
  )

  # Or many properties for a single id

  tw_get_property(
    id = "Q180099",
    p = c("P21", "P31")
  )
}
```
