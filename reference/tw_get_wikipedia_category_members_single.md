# Get all Wikidata Q identifiers of all Wikipedia pages that appear in a given page

Get all Wikidata Q identifiers of all Wikipedia pages that appear in a
given page

## Usage

``` r
tw_get_wikipedia_category_members_single(
  url = NULL,
  category = NULL,
  type = "page",
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10
)
```

## Arguments

- url:

  Full URL to a Wikipedia category page. If given, title and language
  can be left empty.

- category:

  Title of a Wikipedia category page or final parts of its url. Must
  include "Category:", or equivalent in other languages. If given, url
  can be left empty, but language must be provided.

- type:

  Defaults to "page", defines which kind of members of a category are
  returned. Valid values include "page", "file", and "subcat" (for
  sub-category). Corresponds to `cmtype`. For details, see [the relevant
  page of the official
  documentation](https://www.mediawiki.org/wiki/API:Categorymembers).

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

## Value

A data frame (a tibble) with four columns: `wikipedia_title`,
`wikipedia_id`, `wikidata_id`, `wikidata_description`.

## Examples

``` r
if (interactive()) {
  tidywikidatar:::tw_get_wikipedia_category_members_single(
    category = "Category:American women anthropologists",
    type = "subcat"
  )

  tidywikidatar:::tw_get_wikipedia_category_members_single(
    category = "Category:Puerto Rican women anthropologists",
    type = "page"
  )
}
```
