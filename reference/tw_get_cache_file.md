# Gets location of cache file

Gets location of cache file

## Usage

``` r
tw_get_cache_file(type = NULL, language = tidywikidatar::tw_get_language())
```

## Arguments

- type:

  Defaults to NULL. Deprecated. If given, type of cache file to output.
  Values typically used by `tidywikidatar` in versions up to 4.2 include
  "item", "search", and "qualifier".

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

## Value

A character vector of length one with location of item cache file.

## Examples

``` r
tw_set_cache_folder(path = tempdir())
sqlite_cache_file_location <- tw_get_cache_file() # outputs location of cache file
```
