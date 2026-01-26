# Get metadata for images from Wikimedia Commons

Please consult the relevant documentation for reusing content outside
Wikimedia:
https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical

## Usage

``` r
tw_get_image_metadata_single(
  id,
  image_filename = NULL,
  only_first = TRUE,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  read_cache = TRUE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10
)
```

## Arguments

- id:

  A character vector of length 1, must start with Q, e.g. "Q254" for
  Wolfgang Amadeus Mozart.

- image_filename:

  Defaults to NULL. If NULL, `image_filename` is obtained from the
  Wikidata id. If given, must be of the same length as id.

- only_first:

  Defaults to TRUE. If TRUE, returns metadata only for the first image
  associated with a given Wikidata id. If FALSE, returns all images
  available.

- language:

  Needed for caching, defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- id_df:

  Default to NULL. If given, it should be a dataframe typically
  generated with `tw_get_()`, and is used instead of calling Wikidata or
  using SQLite cache. Ignored when `id` is of length more than one.

- cache:

  Defaults to NULL. If given, it should be given either TRUE or FALSE.
  Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- overwrite_cache:

  Logical, defaults to FALSE. If TRUE, it overwrites the table in the
  local sqlite database. Useful if the original Wikidata object has been
  updated.

- read_cache:

  Logical, defaults to TRUE. Mostly used internally to prevent checking
  if an item is in cache if it is already known that it is not in cache.

- cache_connection:

  Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to TRUE. If FALSE, leaves the connection to cache open.

- wait:

  In seconds, defaults to 1. Time to wait between queries to the APIs.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

- attempts:

  Defaults to 10. Number of times it re-attempts to reach the API before
  failing.

## Value

A character vector, corresponding to reference to the image in the
requested format.

## Examples

``` r
if (interactive()) {
  tw_get_image_metadata_single("Q180099")
}
```
