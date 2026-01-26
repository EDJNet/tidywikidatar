# Get database connection settings from the environment

Typically set with
[`tw_set_cache_db()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_cache_db.md)

## Usage

``` r
tw_get_cache_db()
```

## Value

A list with all database parameters as stored in environment variables.

## Examples

``` r
tw_get_cache_db()
#> $driver
#> [1] ""
#> 
#> $host
#> [1] ""
#> 
#> $server
#> [1] ""
#> 
#> $port
#> [1] ""
#> 
#> $database
#> [1] ""
#> 
#> $user
#> [1] ""
#> 
#> $pwd
#> [1] ""
#> 
```
