# Enable caching for the current session

Enable caching for the current session

## Usage

``` r
tw_enable_cache(SQLite = TRUE)
```

## Arguments

- SQLite:

  Logical, defaults to `TRUE`. Set to `FALSE` to use custom database
  options. See
  [`tw_set_cache_db()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_cache_db.md)
  for details.

## Value

Nothing, used for its side effects.

## Examples

``` r
# \donttest{
if (interactive()) {
  tw_enable_cache()
}
# }
```
