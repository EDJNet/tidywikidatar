# Set language to be used by all functions

Defaults to "en".

## Usage

``` r
tw_set_language(language = NULL)

tw_get_language(language = NULL)
```

## Arguments

- language:

  A character vector of length one, with a string of two letters such as
  "en". For a full list of available values, see:
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

## Value

A two letter code for the language, if previously set; the same language
as given to the function; or the default, `en` is none is given.

## Examples

``` r
# \donttest{
if (interactive()) {
  tw_set_language(language = "en")
}
# }

tw_get_language()
```
