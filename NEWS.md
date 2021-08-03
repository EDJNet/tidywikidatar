# tidywikidatar 0.1.0

* all outputs as data frame or vector
* caches data locally
* facilitates basic queries

# tidywikidatar 0.2.0

* more efficient when input includes repeated id
* better documentation and testing

# tidywikidatar 0.3.0

* much more efficient caching
* introduces partial compatibility with other caching backends via DBI
* introduces progress bars
* report informative error messages from server
* tests fail without throwing errors if Wikidata server cannot be reached
* new caching vignette

# tidywikidatar 0.4.0

* `tw_get_label()` actually returns vector of the same length as input
* introduce `tw_get_property_same_length()` for easier integration with piped operations
* introduce `tw_get_property_with_details()` to extract additional details such as language or unit of a property that are otherwise discarded with `tw_get_property()`. `tw_get_property_with_details()`, however, does not (yet) cache results.

# tidywikidatar 0.4.1

* bug fix release: `tw_get_label()`, now actually always returns vector of the same length as input
* new functions used internally for consistency and preventing the above issue under different scenarios 
* invalid identifiers and `NA` are now ignored by `tw_get()`

