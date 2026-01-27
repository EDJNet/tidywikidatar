#' Gets labels for all columns with names such as "id" and "property".
#'
#' @param df A data frame, typically generated with other `tidywikidatar`
#'   functions such as [tw_get_property()].
#' @param value Logical, defaults to `TRUE`. If `TRUE`, it tries to get labels
#'   for all supposed id in the column called value. May break if the columns
#'   include some value which starts with Q and some digits, but is not a
#'   Wikidata id.
#' @inheritParams tw_get
#'
#' @return A data frame, with the same shape as the input data frame, but with
#'   labels instead of identifiers.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_get_qualifiers(id = "Q180099", p = "P26", language = "en") %>%
#'     head(2) %>%
#'     tw_label()
#' }
#' }
tw_label <- function(
  df,
  value = TRUE,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
) {
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
          } else if (
            stringr::str_starts(
              string = x,
              pattern = "Q[[:digit:]]+"
            )
          ) {
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
          } else if (
            stringr::str_starts(
              string = x,
              pattern = "Q[[:digit:]]+"
            )
          ) {
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
    disconnect_db = disconnect_db,
    language = language
  )

  df
}
