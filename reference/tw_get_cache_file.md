# Gets location of cache file

Gets location of cache file

## Usage

``` r
tw_get_cache_file(
  extension = "sqlite",
  language = tidywikidatar::tw_get_language()
)
```

## Arguments

- extension:

  Defaults to `sqlite`. Extension of the database cache file.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

## Value

A character vector of length one with location of item cache file.

## Examples

``` r
tw_set_cache_folder(path = tempdir())
sqlite_cache_file_location <- tw_get_cache_file() # outputs location of cache file
```
