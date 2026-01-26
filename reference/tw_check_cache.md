# Check caching status in the current session, and override it upon request

Mostly used internally in functions, exported for reference.

## Usage

``` r
tw_check_cache(cache = NULL)
```

## Arguments

- cache:

  Defaults to `NULL`. If `NULL`, checks current cache settings. If
  given, returns given value, ignoring cache.

## Value

Either `TRUE` or `FALSE`, depending on current cache settings.

## Examples

``` r
# \donttest{
if (interactive()) {
  tw_check_cache()
}
# }
```
