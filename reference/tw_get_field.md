# Gets a field such a label or description from a data frame typically generated with [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md)

Gets a field such a label or description from a data frame typically
generated with
[`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md)

## Usage

``` r
tw_get_field(df, field, id, language = tidywikidatar::tw_get_language())
```

## Arguments

- df:

  A data frame typically generated with
  [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md).
  It should include data for the `id` included in the dedicated
  parameter.

- field:

  A character vector of length one. Typically, either "label" or
  "description".

- id:

  A character vector, typically of Wikidata identifiers. The output will
  be of the same length and in the same order as the identifiers
  provided with this parameter.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

## Value

A character vector of the same length, and with data in the same order,
as `id`.

## Examples

``` r
tw_get("Q180099") %>%
  tw_get_field(field = "label", id = "Q180099")
#> [1] "Margaret Mead"
```
