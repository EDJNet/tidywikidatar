# Facilitates the creation of MediaWiki API base URLs to retrieve sections of a page

Mostly used internally

## Usage

``` r
tw_get_wikipedia_sections_api_url(
  url = NULL,
  title = NULL,
  language = tidywikidatar::tw_get_language()
)
```

## Arguments

- url:

  Full url to a Wikipedia page. If given, title and language can be left
  empty.

- title:

  Title of a Wikipedia page or final parts of its url. If given, url can
  be left empty, but language must be provided.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see [the dedicated Wikimedia
  page](https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all).

## Value

A character vector of base urls to be used with the MediaWiki API

## Examples

``` r
tw_get_wikipedia_sections_api_url(title = "Margaret Mead", language = "en")
#> [1] "https://en.wikipedia.org/w/api.php?action=parse&redirects=true&format=json&page=Margaret%20Mead&prop=sections"
```
