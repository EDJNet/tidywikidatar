# Get links from a specific section of a Wikipedia page

Get links from a specific section of a Wikipedia page

## Usage

``` r
tw_get_wikipedia_page_section_links(
  url = NULL,
  title = NULL,
  section_title = NULL,
  section_index = NULL,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10,
  wikipedia_page_qid_df = NULL
)
```

## Arguments

- url:

  Full url to a Wikipedia page. If given, title and language can be left
  empty.

- title:

  Title of a Wikipedia page or final parts of its url. If given, url can
  be left empty, but language must be provided.

- section_title:

  Defaults to `NULL`. If given, it should correspond to the
  human-readable title of a section of the relevant Wikipedia page. See
  also
  [`tw_get_wikipedia_page_sections()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_sections.md)

- section_index:

  Defaults to `NULL`. If given, it should correspond to the ordinal of a
  section of the relevant Wikipedia page. See also
  [`tw_get_wikipedia_page_sections()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_page_sections.md)

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

- cache:

  Defaults to `NULL`. If given, it should be given either `TRUE` or
  `FALSE`. Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- overwrite_cache:

  Logical, defaults to `FALSE`. If `TRUE`, it overwrites the table in
  the local sqlite database. Useful if the original Wikidata object has
  been updated.

- cache_connection:

  Defaults to `NULL`. If `NULL`, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to `TRUE`. If `FALSE`, leaves the connection to cache open.

- wait:

  In seconds, defaults to 0. Time to wait between queries to Wikidata.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

- attempts:

  Defaults to 10. Number of times it re-attempts to reach the API before
  failing.

- wikipedia_page_qid_df:

  Defaults to `NULL`. If given, used to reduce calls to cache. Must be a
  data frame.

## Value

A data frame (a tibble).

## Examples

``` r
if (interactive()) {
  tw_get_wikipedia_page_section_links(title = "Margaret Mead", language = "en", section_index = 1)
}
```
