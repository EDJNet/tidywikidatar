# Gets name of table inside the database

Gets name of table inside the database

## Usage

``` r
tw_get_cache_table_name(
  type = "item",
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language()
)
```

## Arguments

- type:

  Defaults to "item". Type of cache file to output. Values typically
  used by `tidywikidatar` include "item", "search_item",
  "search_property", and "qualifier".

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  "en" if not set. Used to limit the data to be cached. Use
  "all_available" to keep all data. For available values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- response_language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  "en" if not set. Relevant only when type is set to "search_item" or
  "search_property". See
  [`tw_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_search.md)
  for details.

## Value

A character vector of length one with the name of the relevant table in
the cache file.

## Examples

``` r
# outputs name of table used in the cache database
tw_get_cache_table_name(type = "item", language = "en")
#> [1] "tw_item_en"
```
