# Creates the base cache folder where `tidywikidatar` caches data.

Creates the base cache folder where `tidywikidatar` caches data.

## Usage

``` r
tw_create_cache_folder(ask = TRUE)
```

## Arguments

- ask:

  Logical, defaults to TRUE. If FALSE, and cache folder does not exist,
  it just creates it without asking (useful for non-interactive
  sessions).

## Value

Nothing, used for its side effects.

## Examples

``` r
# \donttest{
if (interactive()) {
  tw_create_cache_folder()
}
# }
```
