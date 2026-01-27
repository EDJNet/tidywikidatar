# Efficiently get a wide table with various properties of a given set of Wikidata identifiers

Efficiently get a wide table with various properties of a given set of
Wikidata identifiers

## Usage

``` r
tw_get_p_wide(
  id,
  p,
  label = FALSE,
  property_label_as_column_name = FALSE,
  both_id_and_label = FALSE,
  only_first = FALSE,
  preferred = FALSE,
  unlist = FALSE,
  collapse = ";",
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  id_df_label = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
)
```

## Arguments

- id:

  A character vector, must start with Q, e.g. "Q254" for Wolfgang
  Amadeus Mozart.

- p:

  A character vector, a property. Must always start with the capital
  letter "P", e.g. "P31" for "instance of".

- label:

  Logical, defaults to `FALSE`. If `TRUE` labels of Wikidata Q
  identifiers are reported instead of the identifiers themselves (or
  labels are presented along of them, if `both_id_and_label` is set to
  `TRUE`)

- property_label_as_column_name:

  Logical, defaults to `FALSE`. If `FALSE`, names of columns with
  properties are the "P" identifiers of the property. If `TRUE`, the
  label of the correspondent property is assigned as column name.

- both_id_and_label:

  Logical, defaults to `FALSE`. Relevant only if `label` is set to
  `TRUE`, otherwise ignored. If `TRUE`, the label is added as a separate
  column along the original one. Column name is the same as the property
  column, followed by "\_label".

- only_first:

  Logical, defaults to `FALSE`. If `TRUE`, it just keeps the first
  relevant property value for each id (or `NA` if none is available),
  and returns a character vector. Warning: this likely discards valid
  values, so make sure this is really what you want. If `FALSE`, returns
  a list of the same length as input, with all values for each id stored
  in a list if more than one is found.

- preferred:

  Logical, defaults to FALSE. If TRUE, returns properties that have rank
  "preferred" if available; if no "preferred" property is found, then it
  is ignored.

- unlist:

  Logical, defaults to `FALSE`. Typically used sharing or exporting data
  as csv files. Collapses all properties in a single string. The
  separator is defined by the `collapse` parameter. Relevant only when
  `only_first` is set to `FALSE`.

- collapse:

  Defaults to ";". Character used to separate results when `unlist` is
  set to `TRUE`.

- language:

  Defaults to language set with
  [`tw_set_language()`](https://edjnet.github.io/tidywikidatar/reference/tw_set_language.md);
  if not set, "en". Use "all_available" to keep all languages. For
  available language values, see
  https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all

- id_df:

  Default to NULL. If given, it should be a dataframe typically
  generated with `tw_get_()`, and is used instead of calling Wikidata or
  replying on cache.

- id_df_label:

  Defaults to NULL. If given, it should be a dataframe typically
  generated with
  [`tw_get()`](https://edjnet.github.io/tidywikidatar/reference/tw_get.md)
  with *all* items for which labels will be requested. It is used
  instead of calling Wikidata or relying on cache.

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

  In seconds, defaults to 0. Time to wait between queries to Wikidata.
  If data are cached locally, wait time is not applied. If you are
  running many queries systematically you may want to add some waiting
  time between queries.

## Value

A data frame, with a column for each given property.

## Examples

``` r
if (interactive()) {
  tw_get_p_wide(
    id = c("Q180099", "Q228822", "Q191095"),
    p = c("P27", "P19", "P20"),
    label = TRUE,
    only_first = TRUE
  )
}
```
