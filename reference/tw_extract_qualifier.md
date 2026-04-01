# Extract qualifiers from a list created with [`tw_get_item()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_item.md)

This function is mostly used internally and for testing.

## Usage

``` r
tw_extract_qualifier(
  id,
  p,
  w = NULL,
  retry = 10,
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

- p:

  A character vector, a property. Must always start with the capital
  letter "P", e.g. "P31" for "instance of".

- w:

  A list, typically created with
  [`tw_get_item()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_item.md)

- retry:

  Defaults to 10. Maximum number of times to retry if the API throws an
  error, such as "too many requests". Each time, it will wait as much
  time as requested by the API. Notice that this can be a long time,
  e.g. 30 minutes. Set to `FALSE` if you prefer the API to throw an
  error immediately. Consider adjusting the `wait` parameter, or
  customising the `user_agent` if relevant.

- user_agent:

  Defaults to `NULL`. If not given, implicitly defaults to current
  package name (`tidywikidatar`) and version.

## Value

A data frame (a tibble) with eight columns: `id` for the input id,
`property`, `qualifier_id`, `qualifier_property`, `qualifier_value`,
`rank`, `qualifier_value_type`, and `set` (to distinguish sets of data
when a property is present more than once)

## Examples

``` r
# w <- tw_get_item(id = "Q180099")

tw_extract_qualifier(id = "Q180099", p = "P26", w = list(tw_test_items[["Q180099"]]))
#> # A tibble: 6 × 8
#>   id      property qualifier_id qualifier_property qualifier_value      
#>   <chr>   <chr>    <chr>        <chr>              <chr>                
#> 1 Q180099 P26      Q594736      P582               +1928-00-00T00:00:00Z
#> 2 Q180099 P26      Q594736      P580               +1923-00-00T00:00:00Z
#> 3 Q180099 P26      Q2144944     P582               +1935-00-00T00:00:00Z
#> 4 Q180099 P26      Q2144944     P580               +1928-00-00T00:00:00Z
#> 5 Q180099 P26      Q314252      P580               +1936-00-00T00:00:00Z
#> 6 Q180099 P26      Q314252      P582               +1950-00-00T00:00:00Z
#> # ℹ 3 more variables: qualifier_value_type <chr>, rank <chr>, set <int>
```
