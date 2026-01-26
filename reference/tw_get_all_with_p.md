# Get all items that have a given property (irrespective of the value)

This function does not cache results.

## Usage

``` r
tw_get_all_with_p(
  p,
  fields = c("item", "itemLabel", "itemDescription"),
  language = tidywikidatar::tw_get_language(),
  method = "SPARQL",
  wait = 0.1,
  limit = Inf,
  return_as_tw_search = TRUE,
  user_agent = stringr::str_flatten(c("tidywikidatar/",
    as.character(packageVersion("tidywikidatar"))))
)
```

## Arguments

- p:

  A character vector, a property. Must always start with the capital
  letter "P", e.g. "P31" for "instance of".

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

- method:

  Defaults to "SPARQL". The only accepted alternative value is "JSON",
  to use instead json-based API.

- wait:

  Defaults to 0.1. Used only in method is set to "JSON".

- limit:

  Defaults to `Inf`. Set to smaller values for testing and cache locally
  when possible to reduce load on servers.

- return_as_tw_search:

  Logical, defaults to TRUE. If TRUE, returns a data frame with three
  columns (id, label, and description) that can be piped to other `tw_`
  functions. If FALSE, a data frame with as many columns as fields.

- user_agent:

  Defaults to a combination of `tidywikidatar` and package version
  number. Consider customising, in particular if you are making many
  queries.

## Value

A data frame with three columns is method is set to "SPARQL", or as many
columns as fields if more are given and `return_as_tw_search` is set to
FALSE. A single column with Wikidata identifier if method is set to
"JSON".

## Examples

``` r
if (interactive()) {
  # get all Wikidata items with an ICAO airport code ("P239")
  tw_get_all_with_p(p = "P239", limit = 10)
}
```
