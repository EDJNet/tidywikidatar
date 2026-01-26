# Facilitates the creation of MediaWiki API base URLs

Mostly used internally

## Usage

``` r
tw_get_wikipedia_base_api_url(
  url = NULL,
  title = NULL,
  language = tidywikidatar::tw_get_language(),
  action = "query",
  type = "page"
)
```

## Arguments

- url:

  A character vector with the full URL to one or more Wikipedia pages.
  If given, title and language can be left empty.

- title:

  Title of a Wikipedia page or final parts of its url. If given, url can
  be left empty, but language must be provided.

- language:

  Two-letter language code used to define the Wikipedia version to use.
  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". If url given, this can be left empty.

- action:

  Defaults to "query". Usually either "query" or "parse". In principle,
  any valid action value, see: <https://www.mediawiki.org/w/api.php>

- type:

  Defaults to "page". Either "page" or "category".

## Value

A character vector of base urls to be used with the MediaWiki API

## Examples

``` r
tw_get_wikipedia_base_api_url(title = "Margaret Mead", language = "en")
#> [1] "https://en.wikipedia.org/w/api.php?action=query&redirects=true&format=json&titles=Margaret%20Mead"
tw_get_wikipedia_base_api_url(
  title = "Category:American women anthropologists",
  type = "category",
  language = "en"
)
#> [1] "https://en.wikipedia.org/w/api.php?action=query&redirects=true&format=json&cmtitle=Category%3AAmerican%20women%20anthropologists&list=categorymembers"
```
