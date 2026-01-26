# Perform simple Wikidata queries

This function aims to facilitate only the most basic type of queries:
return which items have the following property pairs. For more details
on Wikidata queries, consult:
https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples.
For complex queries, use
[`WikidataQueryServiceR::query_wikidata()`](https://rdrr.io/pkg/WikidataQueryServiceR/man/query_wikidata.html).

## Usage

``` r
tw_query(
  query,
  fields = c("item", "itemLabel", "itemDescription"),
  language = tidywikidatar::tw_get_language(),
  return_as_tw_search = TRUE
)
```

## Arguments

- query:

  A list of named vectors, or a data frame (see example and readme).

- fields:

  A character vector of Wikidata fields. Ignored if
  `return_as_tw_search` is set to TRUE (as per default). Defaults to
  `("item", "itemLabel", "itemDescription")`

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". If more than one, can be set in order of preference,
  e.g. `c("it", "fr", "en")`. Use "all_available" to keep all languages.
  For available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- return_as_tw_search:

  Logical, defaults to TRUE. If TRUE, returns a data frame with three
  columns (id, label, and description) that can be piped to other `tw_`
  functions. If FALSE, a data frame with as many columns as fields.

## Value

A data frame

## Details

Consider
[`tw_get_all_with_p()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_all_with_p.md)
if you want to get all items with a given property, irrespective of the
value.

## Examples

``` r
if (interactive()) {
  query <- list(
    c(p = "P106", q = "Q1397808"),
    c(p = "P21", q = "Q6581072")
  )
  tw_query(query)
}
```
