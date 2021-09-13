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

# tidywikidatar 0.4.2

* `tw_get_image()` now returns consistently valid links if format is set to 'embed'; it is now possible to get a direct link to images with a given resolution with the width parameter
* introduce `tw_get_image_metadata()` to obtain adequate credits to be included when images are used
* introduce functions to get Wikidata identifiers from Wikipedia pages
* `tw_get_property()` now consistently returns data frame with properties in the same order as given and does not fail if given invalid values
* more consistent testing, less likely to be impacted to changes in Wikidata items
* updated README with examples using the pipe operator

