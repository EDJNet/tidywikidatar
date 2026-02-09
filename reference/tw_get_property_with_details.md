# Gets all details of a property for one or more Wikidata items.

Gets all details of a property for one or more Wikidata items.

## Usage

``` r
tw_get_property_with_details(id, p, wait = 0)
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

- p:

  A character vector, a property. Must always start with the capital
  letter "P", e.g. "P31" for "instance of".

- wait:

  In seconds, defaults to 0. Time to wait between queries to Wikidata.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

## Value

A tibble, corresponding to the details for the given property. `NULL` if
no relevant property found.

## Examples

``` r
# Get "female form of label", including language
tw_get_property_with_details(id = "Q64733534", p = "P2521")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: id <chr>, p <chr>
```
