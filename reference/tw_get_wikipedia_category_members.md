# Get all Wikidata Q identifiers of all Wikipedia pages (or files, or subcategories) that are members of the given category,

Get all Wikidata Q identifiers of all Wikipedia pages (or files, or
subcategories) that are members of the given category,

## Usage

``` r
tw_get_wikipedia_category_members(
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
  sub-category). Corresponds to `cmtype`. For details, see
  <https://www.mediawiki.org/wiki/API:Categorymembers>

- language:

  Two-letter language code used to define the Wikipedia version to use.
  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". If url given, this can be left empty.

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

- cache_connection:

  Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar`
  will use a local sqlite database. A custom connection to other
  databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to TRUE. If FALSE, leaves the connection to cache open.

- wait:

  In seconds, defaults to 1 due to time-outs with frequent queries. Time
  to wait between queries to the APIs. If data are cached locally, wait
  time is not applied. If you are running many queries systematically
  you may want to add some waiting time between queries.

- attempts:

  Defaults to 10. Number of times it re-attempts to reach the API before
  failing.

## Value

A data frame (a tibble) with eight columns: `source_title_url`,
`source_wikipedia_title`, `source_qid`, `wikipedia_title`,
`wikipedia_id`, `qid`, `description`, and `language`.

## Examples

``` r
if (interactive()) {
  sub_categories <- tw_get_wikipedia_category_members(
    category = "Category:American women anthropologists",
    type = "subcat"
  )

  sub_categories

  tw_get_wikipedia_category_members(
    category = sub_categories$wikipedia_title,
    type = "page"
  )
}
```
