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

  Language to be used for the search. Can be set once per session with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md).
  If not set, defaults to "en". For a full list, see [the dedicated
  Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- response_language:

  Language to be used for the returned labels and descriptions.
  Corresponds to the `uselang` parameter of the MediaWiki API, as
  described [in the official
  documentation](https://www.wikidata.org/w/api.php?action=help&modules=wbsearchentities).
  Can be set once per session with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md).
  If not set, defaults to "en". For a full list, see [all available
  language
  codes](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

## Value

A character vector of length one with the name of the relevant table in
the cache file.

## Examples

``` r
# outputs name of table used in the cache database
tw_get_cache_table_name(type = "item", language = "en")
#> [1] "tw_item_en"
```
