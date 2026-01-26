# Reset Wikipedia category members cache

Removes from cache the table where data typically gathered with
[`tw_get_wikipedia_category_members()`](https://edjnet.github.io/tidywikidatar/reference/tw_get_wikipedia_category_members.md)
are stored.

## Usage

``` r
tw_reset_wikipedia_category_members_cache(
  language = tidywikidatar::tw_get_language(),
  type = "page",
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE,
  ask = TRUE
)
```

## Arguments

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- type:

  Defaults to "page", defines which kind of members of a category are
  returned. Valid values include "page", "file", and "subcat" (for
  sub-category). Corresponds to `cmtype`. For details, see
  <https://www.mediawiki.org/wiki/API:Categorymembers>

- cache:

  Defaults to NULL. If given, it should be given either TRUE or FALSE.
  Typically set with
  [`tw_enable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_enable_cache.md)
  or
  [`tw_disable_cache()`](https://edjnet.github.io/tidywikidatar/reference/tw_disable_cache.md).

- cache_connection:

  Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar`
  will use a local sqlite database by default. A custom connection to
  other databases can be given (see vignette `caching` for details).

- disconnect_db:

  Defaults to TRUE. If FALSE, leaves the connection to cache open.

- ask:

  Logical, defaults to TRUE. If FALSE, and cache folder does not exist,
  it just creates it without asking (useful for non-interactive
  sessions).

## Value

Nothing, used for its side effects.

## Examples

``` r
if (interactive()) {
  tw_reset_wikipedia_category_members_cache()
}
```
