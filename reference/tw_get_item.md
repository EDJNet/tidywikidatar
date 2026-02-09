# Retrieve item from the Wikidata API and returns it as a list

Retrieve item from the Wikidata API and returns it as a list

## Usage

``` r
tw_get_item(id, user_agent = tidywikidatar::tw_get_user_agent())
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

- user_agent:

  Defaults to `NULL`. If not given, implicitly defaults to current
  package name (`tidywikidatar`) and version.

## Value

A list, with as many elements as the unique given id.

## Examples

``` r
if (FALSE) { # \dontrun{
  item_l <- tw_get_item(id = "Q180099")

  tidywikidatar:::tw_extract_single(w = item_l)
} # }
```
