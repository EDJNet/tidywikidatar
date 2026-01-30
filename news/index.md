# Changelog

## tidywikidatar (development version)

- drop dependency on `WikidataQueryServiceR`
- enable setting default user agent for the session for query functions
- fix response language in `tw_get_property_label` and
  `tw_get_property_description`
- more consistent documentation formatting
- last version supporting legacy pipe `%>%`

## tidywikidatar 0.5.9

CRAN release: 2024-07-29

- drop dependency on `WikipediR`
- report informative error message from the api in `tw_search` and
  related functions
- include “tidywikidatar/version” user agent in `tw_search` and related
  functions
- fix `tw_get_wikipedia_section_links` when url provided as input

## tidywikidatar 0.5.8

CRAN release: 2024-03-28

- consistent results rather than error when searching empty string with
  `tw_search`
- consistent results rather than error when searching empty string with
  `tw_get_property_label`
- drop `usethis` dependency (PR by
  [@olivroy](https://github.com/olivroy))

## tidywikidatar 0.5.7

CRAN release: 2023-03-10

- fix: `tw_get_wikipedia_category_members` now works with categories
  that have more than 1000 member pages and consistently stores data in
  language-appropriate cache file also when language is derived from url
- testing adjustments: more tests now rely on an embedded set of items
  to reduce risks of server timeouts when conducting checks, in
  particular from CRAN

## tidywikidatar 0.5.6

CRAN release: 2023-01-31

- fix: `tw_get` now caches Wikidata items that have been deleted,
  storing the error message as the value of property “error”
- fix: `tw_search` now returns results consistently also when
  description missing in given language (issue stemming from update in a
  dependency)

## tidywikidatar 0.5.5

CRAN release: 2022-10-31

- fix: `tw_search` now checks cache efficiently also when cache settings
  are passed as parameters
- fix: minor adjustments to prevent warnings and error with latest
  `purrr` and `tidyselect`

## tidywikidatar 0.5.4

CRAN release: 2022-08-06

- enhancement:
  [`tw_get_wikipedia_page_links()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_links.md)
  now works more efficiently with cache and deals more graciously when
  given links to non-existing Wikipedia pages
- fix:
  [`tw_get_wikipedia_category_members()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_category_members.md)
  now works as expected also when caching is disabled
- fix: deal consistently with Wikipedia pages that have “&” or other
  special characters in their title
- fix:
  [`tw_get_wikipedia_page_links()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_links.md)
  now works consistently when non-standard characters or non-latin
  alphabets found in url
- fix:
  [`tw_get_wikipedia_page_qid()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_qid.md)
  now works consistently when url given is not in the language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md)

## tidywikidatar 0.5.3

CRAN release: 2022-06-07

- new feature: get identifiers of pages that are members of a category
  on Wikipedia with
  [`tw_get_wikipedia_category_members()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_category_members.md)
- fix: return a data frame of a single row filled with `NA` values when
  [`tw_get_qualifiers()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_qualifiers.md)
  would return no information on a given item/property combination; this
  ensures caching works as expected also in such cases.
- fix: error in caching functions apparently due to `dbplyr`
  update/incompatibility with the `pool` package

## tidywikidatar 0.5.2

CRAN release: 2022-05-18

- new feature:
  [`tw_get_p_wide()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_p_wide.md)
  offers a more efficient way to get wide data frames with a number of
  properties (with relative labels) for a set of Wikidata id; it also
  facilitates collapsing values for ease of export to csv.
- new: introduce
  [`tw_get_p1()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_p1.md)
  as a shorthand for `tw_get_p(only_first = TRUE, preferred = TRUE)`
- new: when pre-cached data is given to functions such as
  [`tw_get_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property.md)
  or
  [`tw_get_label()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_label.md),
  it looks for missing data if not all are present in given data frame
- new: introduce cache indexing convenience functions,
  [`tw_index_cache_item()`](https://edjnet.github.io/tidywikidatar/reference/tw_index_cache_item.md)
  and
  [`tw_index_cache_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_index_cache_search.md).
  These should be significantly improved performance on the caching
  backend, but these are currently introduced as an opt-in as they have
  not been thoroughly tested with different database drivers. Running
  them once drastically improve performance with MySql when the cache
  goes into million of items. See also the vignette on caching.
- fix: don’t look for cache folder when cache disabled in
  [`tw_get_p()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_same_length.md)
- fix: better handling of cases when NA (including, a vector with only
  NA), or NULL, are given as input to various functions
- fix: when only a partial pre-cached data frame is passed to
  [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md)
  and other functions as input, missing items are queried
- fix:
  [`tw_get_wikipedia()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia.md)
  consistently returns vector of the same length as input
- fix: `rank` column mistakenly dropped in some cases when using
  [`tw_get_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property.md)
- fix: when caching language is set manually via parameter (instead of
  via environment variable), a separate SQLite database is used, as
  expected
- new:
  [`tw_check_qid()`](https://edjnet.github.io/tidywikidatar/reference/tw_check_qid.md)
  has more parameters to deal with different use cases
- new: new set of functions to add index to databases to improve
  performance when the local cache grows to many millions rows
  ([`tw_check_cache_index()`](https://edjnet.github.io/tidywikidatar/reference/tw_check_cache_index.md),
  [`tw_index_cache_item()`](https://edjnet.github.io/tidywikidatar/reference/tw_index_cache_item.md),
  and
  [`tw_index_cache_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_index_cache_search.md)).
  Tested with MySql.
- all `_single` functions used internally are now not exported to
  facilitate auto-complete
- new maintainer email address due to CRAN issues

## tidywikidatar 0.5.1

CRAN release: 2022-03-11

- introduce functions to get list of sections of Wikipedia pages -
  [`tw_get_wikipedia_page_sections()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_sections.md) -
  and then extract the links from a specific section -
  [`tw_get_wikipedia_page_section_links()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_section_links.md)
- introduce convenience function to get all Wikidata items that have a
  given property, irrespective of the value,
  [`tw_get_all_with_p()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_all_with_p.md)
- [`tw_search()`](https://edjnet.github.io/tidywikidatar/reference/tw_search.md)
  now has separate parameters for the search language, and the language
  in which label and description and returned (previously, these were
  always in English)
- fix
  [`tw_get_qualifiers()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_qualifiers.md)
  when qualifier value is of type quantity
- keep smooth caching also when a Wikidata item has no values and no
  label in the current language
- introduce additional settings in database connections for drivers that
  need them
- minor bug fixes

## tidywikidatar 0.5.0

CRAN release: 2022-01-26

- new vignette with examples starting from a Wikipedia page, with more
  examples using piped operations, and clearer references that separate
  group of functions in official documentation website
- [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md)
  now keeps rank by default, facilitating retrieving more relevant
  results
- [`tw_get_qualifiers()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_qualifiers.md)
  now includes ranking such as “preferred”, “normal”, and “deprecated”
  associated with each property, as well as value type of the output
  (new format incompatible with previous cache, reset cache with
  [`tw_reset_qualifiers_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_reset_qualifiers_cache.md))
- [`tw_get_qualifiers()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_qualifiers.md)
  not returns correctly value when qualifier value type is a string (not
  a Wikidata identifier, not a date)
- [`tw_get_image()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_image.md)
  not respects all parameters consistently.
  [`tw_get_image()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_image.md)
  and
  [`tw_get_image_metadata()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_image_metadata.md)
  are now briefly described in the README.
- [`tw_get_wikipedia_page_links()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_links.md)
  now caches results, and provides more consistent results as a data
  frame
- introduce
  [`tw_get_p()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_same_length.md)
  as an alias of
  [`tw_get_property_same_length()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_same_length.md)
  for brevity
- introduce new parameters in
  [`tw_get_p()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_same_length.md)
  to deal with common pattern when only “preferred” or most recent
  property should be returned, rather than whatever Wikidata has first
  in the list, and add new section in the readme
- add example datasets for illustrative purposes and forthcoming
  additional vignettes and examples, `tw_qid_meps` and `tw_qid_airports`
- drop legacy `include_id_and_p` parameter
  [`tw_get_qualifiers()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_qualifiers.md)
- add support for setting database connection parameters with
  [`tw_set_cache_db()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_cache_db.md)
  for easier use of alternatives to SQLite
- new naming convention for local SQLite database, to reflect change
  that there is now one database per language
- improve handling of connections to reduce the risk of connections
  remaining open, aiming at higher efficiency for integration with shiny
  apps
- bug fix: fix error when Wikidata item has no label in any language

## tidywikidatar 0.4.2

CRAN release: 2021-09-14

- [`tw_get_image()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_image.md)
  now returns consistently valid links if format is set to ‘embed’; it
  is now possible to get a direct link to images with a given resolution
  with the width parameter
- introduce
  [`tw_get_image_metadata()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_image_metadata.md)
  to obtain adequate credits to be included when images are used
- introduce functions to get Wikidata identifiers from Wikipedia pages
- [`tw_get_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property.md)
  now consistently returns data frame with properties in the same order
  as given and does not fail if given invalid values
- more consistent testing, less likely to be impacted to changes in
  Wikidata items
- updated README with examples using the pipe operator

## tidywikidatar 0.4.1

CRAN release: 2021-08-04

- bug fix release:
  [`tw_get_label()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_label.md),
  now actually always returns vector of the same length as input
- new functions used internally for consistency and preventing the above
  issue under different scenarios
- invalid identifiers and `NA` are now ignored by
  [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md)

## tidywikidatar 0.4.0

CRAN release: 2021-07-31

- [`tw_get_label()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_label.md)
  actually returns vector of the same length as input
- introduce
  [`tw_get_property_same_length()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_same_length.md)
  for easier integration with piped operations
- introduce
  [`tw_get_property_with_details()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_with_details.md)
  to extract additional details such as language or unit of a property
  that are otherwise discarded with
  [`tw_get_property()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property.md).
  [`tw_get_property_with_details()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_property_with_details.md),
  however, does not (yet) cache results.

## tidywikidatar 0.3.0

CRAN release: 2021-06-07

- much more efficient caching
- introduces partial compatibility with other caching backends via DBI
- introduces progress bars
- report informative error messages from server
- tests fail without throwing errors if Wikidata server cannot be
  reached
- new caching vignette

## tidywikidatar 0.2.0

CRAN release: 2021-04-19

- more efficient when input includes repeated id
- better documentation and testing

## tidywikidatar 0.1.0

- all outputs as data frame or vector
- caches data locally
- facilitates basic queries
