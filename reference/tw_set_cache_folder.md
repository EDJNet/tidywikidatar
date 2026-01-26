# Set folder for caching data

Consider using a folder out of your current project directory, e.g.
`tw_set_cache_folder("~/R/tw_data/")`: you will be able to use the same
cache in different projects, and prevent cached files from being sync-ed
if you use services such as Nextcloud or Dropbox.

## Usage

``` r
tw_set_cache_folder(path = NULL)

tw_get_cache_folder(path = NULL)
```

## Arguments

- path:

  A path to a location used for caching data. If the folder does not
  exist, it will be created.

## Value

The path to the caching folder, if previously set; the same path as
given to the function; or the default, `tw_data` is none is given.

## Examples

``` r
# \donttest{
if (interactive()) {
  tw_set_cache_folder(fs::path(fs::path_home_r(), "R", "tw_data"))
}
# }
tw_get_cache_folder()
```
