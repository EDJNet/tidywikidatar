# Ensures that input appears to be a valid Wikidata property id (i.e. it starts with P and is followed only by digits)

Mostly used internally by other functions.

## Usage

``` r
tw_check_pid(property, logical_vector = FALSE, non_pid_as_NA = FALSE)
```

## Arguments

- property:

  A character vector of one or more Wikidata property identifiers.

- logical_vector:

  Logical, defaults to FALSE. If TRUE, returns a logical vector of the
  same length as input, where TRUE corresponds to seemingly meaningful
  property identifiers.

- non_pid_as_NA:

  Logical, defaults to FALSE. If TRUE (and if `logical_vector` is set to
  FALSE), a vector of the same length is returned, with NA replacing
  items that are seemingly not meaningful property identifiers.

## Value

A character vector with only strings appearing to be Wikidata
identifiers; possibly shorter than input

## Examples

``` r
tw_check_pid(property = c("P19", "p20", "Not an property id", "20", NA, "Q5", ""))
#> [1] "P19" "P20"

tw_check_pid(
  property = c("P19", "p20", "Not an property id", "20", NA, "Q5", ""),
  logical_vector = TRUE
)
#> [1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE

tw_check_pid(
  property = c("P19", "p20", "Not an property id", "20", NA, "Q5", ""),
  non_pid_as_NA = TRUE
)
#> [1] "P19" "P20" NA    NA    NA    NA    NA   
```
