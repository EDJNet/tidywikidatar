# Extract item data from an object of class Wikidata created with `WikidataR`

This function is mostly used internally and for testing.

## Usage

``` r
tw_extract_single(w, language = tidywikidatar::tw_get_language())
```

## Arguments

- w:

  An object of class Wikidata created with `WikidataR`, typically
  created with `tw_get_item(id = id)`

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

## Value

A data frame (a tibble) with four columns, such as the one created by
[`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md).

## Examples

``` r
#' Retrieving from tests, but normally:
# w <- tw_get_item(id = "Q180099")

tidywikidatar:::tw_extract_single(w = list(tw_test_items[["Q180099"]]))
#> # A tibble: 202 × 4
#>    id      property value                         rank  
#>    <chr>   <chr>    <chr>                         <chr> 
#>  1 Q180099 label_en Margaret Mead                 NA    
#>  2 Q180099 P21      Q6581072                      normal
#>  3 Q180099 P214     44302511                      normal
#>  4 Q180099 P106     Q4773904                      normal
#>  5 Q180099 P373     Margaret Mead                 normal
#>  6 Q180099 P244     n78093416                     normal
#>  7 Q180099 P227     118579789                     normal
#>  8 Q180099 P18      Margaret Mead (1901-1978).jpg normal
#>  9 Q180099 P509     Q212961                       normal
#> 10 Q180099 P19      Q1345                         normal
#> # ℹ 192 more rows
```
