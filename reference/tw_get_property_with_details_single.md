# Gets all details of a property

Gets all details of a property

## Usage

``` r
tw_get_property_with_details_single(id, p)
```

## Arguments

- id:

  A character vector, must start with Q, e.g. "Q254" for Wolfgang
  Amadeus Mozart.

- p:

  A character vector, a property. Must always start with the capital
  letter "P", e.g. "P31" for "instance of".

## Value

A tibble, corresponding to the details for the given property. NULL if
no relevant property found.

## Examples

``` r
# Get "female form of label", including language
tidywikidatar:::tw_get_property_with_details_single(id = "Q64733534", p = "P2521")
#> # A tibble: 11 × 4
#>    id        p     text                                language 
#>    <chr>     <chr> <chr>                               <chr>    
#>  1 Q20551564 P2521 sběratelka pohádek                  cs       
#>  2 Q20551564 P2521 recopiladora de contes fantàstics   ca       
#>  3 Q20551564 P2521 Märchensammlerin                    de       
#>  4 Q20551564 P2521 zbieraczka baśni                    pl       
#>  5 Q20551564 P2521 raccoglitrice di favole             it       
#>  6 Q20551564 P2521 зьбіральніца казак                  be-tarask
#>  7 Q20551564 P2521 collectrice de textes traditionnels fr       
#>  8 Q20551564 P2521 zbiralka pravljic                   sl       
#>  9 Q20551564 P2521 собирательница народных сказок      ru       
#> 10 Q20551564 P2521 збирачка казок                      uk       
#> 11 Q20551564 P2521 recompiladora de contos fantásticos gl       
```
