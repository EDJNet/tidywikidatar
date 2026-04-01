# Gets all details of a property

Used internally. Users should rely on
[`tw_get_property_with_details()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_with_details.md).

## Usage

``` r
tw_get_property_with_details_single(
  id,
  p,
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

A tibble, corresponding to the details for the given property. `NULL` if
no relevant property found.

## Examples

``` r
# Get "female form of label", including language
tidywikidatar:::tw_get_property_with_details_single(id = "Q64733534", p = "P2521")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: id <chr>, p <chr>
```
