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

# tidywikidatar 0.5.0

* new vignette with examples starting from a Wikipedia page, with more examples using piped operations, and clearer references that separate group of functions in official documentation website
* `tw_get()` now keeps rank by default, facilitating retrieving more relevant results
* `tw_get_qualifiers()` now includes ranking such as "preferred", "normal", and "deprecated" associated with each property, as well as value type of the output (new format incompatible with previous cache, reset cache with `tw_reset_qualifiers_cache()`)
* `tw_get_qualifiers()` not returns correctly value when qualifier value type is a string (not a Wikidata identifier, not a date)
* `tw_get_image()` not respects all parameters consistently. `tw_get_image()` and `tw_get_image_metadata()` are now briefly described in the README.
* `tw_get_wikipedia_page_links()` now caches results, and provides more consistent results as a data frame
* introduce `tw_get_p()` as an alias of `tw_get_property_same_length()` for brevity
* introduce new parameters in `tw_get_p()` to deal with common pattern when only "preferred" or most recent property should be returned, rather than whatever Wikidata has first in the list, and add new section in the readme
* add example datasets for illustrative purposes and forthcoming additional vignettes and examples, `tw_qid_meps` and `tw_qid_airports`
* drop legacy `include_id_and_p` parameter `tw_get_qualifiers()`
* add support for setting database connection parameters with `tw_set_cache_db()` for easier use of alternatives to SQLite
* new naming convention for local SQLite database, to reflect change that there is now one database per language
* improve handling of connections to reduce the risk of connections remaining open, aiming at higher efficiency for integration with shiny apps
* bug fix: fix error when Wikidata item has no label in any language

# tidywikidatar 0.5.1

* introduce functions to get list of sections of Wikipedia pages - `tw_get_wikipedia_page_sections()` - and then extract the links from a specific section - `tw_get_wikipedia_page_section_links()`
* introduce convenience function to get all Wikidata items that have a given property, irrespective of the value, `tw_get_all_with_p()`
* `tw_search()` now has separate parameters for the search language, and the language in which label and description and returned (previously, these were always in English)
* fix `tw_get_qualifiers()` when qualifier value is of type quantity
* keep smooth caching also when a Wikidata item has no values and no label in the current language
* introduce additional settings in database connections for drivers that need them
* minor bug fixes
