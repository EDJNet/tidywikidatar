# Extract item data from an object of class Wikidata created with `WikidataR`

This function is mostly used internally and for testing.

## Usage

``` r
tw_extract_single(w, language = tidywikidatar::tw_get_language())
```

## Arguments

- w:

  An object of class Wikidata created with `WikidataR`, typically
  created with `WikidataR::get_item(id = id)`

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
item <- tryCatch(WikidataR::get_item(id = "Q180099"),
  error = function(e) {
    as.character(e[[1]])
  }
)

tidywikidatar:::tw_extract_single(w = item)
#> # A tibble: 250 × 4
#>    id      property value                 rank  
#>    <chr>   <chr>    <chr>                 <chr> 
#>  1 Q180099 label_en Margaret Mead         NA    
#>  2 Q180099 alias_en Margaret Bateson      NA    
#>  3 Q180099 alias_en Margaret Mead Bateson NA    
#>  4 Q180099 alias_en Margaret Fortune      NA    
#>  5 Q180099 alias_en Margaret Mead Fortune NA    
#>  6 Q180099 P21      Q6581072              normal
#>  7 Q180099 P214     44302511              normal
#>  8 Q180099 P106     Q4773904              normal
#>  9 Q180099 P106     Q36180                normal
#> 10 Q180099 P106     Q2526255              normal
#> # ℹ 240 more rows
```
