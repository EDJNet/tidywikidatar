# Ensures that input appears to be a valid Wikidata id

Mostly used internally by other functions.

## Usage

``` r
tw_check_qid(id, logical_vector = FALSE, non_id_as_NA = FALSE)
```

## Arguments

- id:

  A character vector of one or more Wikidata id.

- logical_vector:

  Logical, defaults to FALSE. If TRUE, returns a logical vector of the
  same length as input, where TRUE corresponds to seemingly meaningful Q
  identifiers.

- non_id_as_NA:

  Logical, defaults to FALSE. If TRUE (and if `logical_vector` is set to
  FALSE), a vector of the same length is returned, with NA replacing
  items that are seemingly not meaningful Q identifiers.

## Value

A character vector with only strings appearing to be Wikidata
identifiers; possibly shorter than input

## Examples

``` r
tw_check_qid(id = c("Q180099", "q228822", "Not an id", "00180099", NA, "Q5"))
#> [1] "Q180099" "Q228822" "Q5"     

tw_check_qid(
  id = c("Q180099", "q228822", "Not an id", "00180099", NA, "Q5"),
  logical_vector = TRUE
)
#> [1]  TRUE  TRUE FALSE FALSE FALSE  TRUE

tw_check_qid(
  id = c("Q180099", "q228822", "Not an id", "00180099", NA, "Q5"),
  non_id_as_NA = TRUE
)
#> [1] "Q180099" "Q228822" NA        NA        NA        "Q5"     
```
