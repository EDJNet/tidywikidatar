#' Gets labels for all columns with names such as "id" and "property".
#'
#' @param df A data frame, typically generated with other `tidywikidatar` functions such as `tw_get_property()`
#' @param value Logical, defaults to TRUE. If TRUE, it tries to get labels for all supposed id in the column called value. May break if the columns include some value which starts with Q and some digits, but is not a wikidata id.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A data frame, with the same shape as the input data frame, but with labels instead of identifiers.
#' @export
#'
#' @examples
#'
#' tw_get_qualifiers(id = "Q180099", p = "P26", language = "en") %>%
#'   head(2) %>%
#'   tw_label()
tw_label <- function(df,
                     value = TRUE,
                     language = tidywikidatar::tw_get_language(),
                     cache = NULL,
                     overwrite_cache = FALSE,
                     cache_connection = NULL,
                     disconnect_db = TRUE,
                     wait = 0) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (is.element("id", colnames(df))) {
    df[["id"]] <- tw_get_label(
      id = df[["id"]],
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait
    )
  }

  if (is.element("qualifier_id", colnames(df))) {
    df[["qualifier_id"]] <- tw_get_label(
      id = df[["qualifier_id"]],
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait
    )
  }


  if (is.element("property", colnames(df))) {
    df[["property"]] <- tw_get_property_label(
      property = df[["property"]],
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait
    )
  }

  if (is.element("qualifier_property", colnames(df))) {
    df[["qualifier_property"]] <- tw_get_property_label(
      property = df[["qualifier_property"]],
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait
    )
  }


  if (isTRUE(value)) {
    if (is.element("value", colnames(df))) {
      df[["value"]] <- purrr::map_chr(
        .x = df[["value"]],
        .f = function(x) {
          if (is.na(x)) {
            output <- x
          } else if (stringr::str_starts(
            string = x,
            pattern = "Q[[:digit:]]+"
          )) {
            output <- tw_get_label(
              id = x,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = db,
              disconnect_db = FALSE,
              wait = wait
            )
          } else {
            output <- x
          }
          output
        }
      )
    }

    if (is.element("qualifier_value", colnames(df))) {
      df[["qualifier_value"]] <- purrr::map_chr(
        .x = df[["qualifier_value"]],
        .f = function(x) {
          if (is.na(x)) {
            output <- x
          } else if (stringr::str_starts(
            string = x,
            pattern = "Q[[:digit:]]+"
          )) {
            output <- tw_get_label(
              id = x,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = db,
              disconnect_db = FALSE,
              wait = wait
            )
          } else {
            output <- x
          }
          output
        }
      )
    }
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db
  )

  df
}
