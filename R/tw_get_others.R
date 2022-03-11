#' Get Wikidata label in given language
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A character vector of the same length as the vector of id given, with the Wikidata label in the requested language.
#' @export
#'
#' @examples
#'
#' tw_get_label(
#'   id = c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   language = "en"
#' )
#'
#' # If a label is not available, a NA value is returned
#' if (interactive()) {
#'   tw_get_label(
#'     id = c(
#'       "Q64733534",
#'       "Q4773904",
#'       "Q220480"
#'     ),
#'     language = "sc"
#'   )
#' }
tw_get_label <- function(id,
                         language = tidywikidatar::tw_get_language(),
                         id_df = NULL,
                         cache = NULL,
                         overwrite_cache = FALSE,
                         cache_connection = NULL,
                         disconnect_db = TRUE,
                         wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  if (is.null(id_df)) {
    id_df <- tw_get(
      id = id,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      language = language,
      wait = wait,
      disconnect_db = disconnect_db
    )
  }

  tw_get_field(
    df = id_df,
    field = "label",
    language = language,
    id = id
  )
}


#' Get Wikidata description in given language
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A character vector of length 1, with the Wikidata description in the requested language.
#' @export
#'
#' @examples
#' tw_get_description(
#'   id = c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   language = "en"
#' )
tw_get_description <- function(id,
                               language = tidywikidatar::tw_get_language(),
                               id_df = NULL,
                               cache = NULL,
                               overwrite_cache = FALSE,
                               cache_connection = NULL,
                               disconnect_db = TRUE,
                               wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  if (is.null(id_df)) {
    id_df <- tw_get(
      id = id,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      language = language,
      wait = wait,
      disconnect_db = disconnect_db
    )
  }

  tw_get_field(
    df = id_df,
    field = "description",
    language = language,
    id = id
  )
}

#' Get Wikipedia article in given language
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A character vector of length 1, with the Wikidata label in the requested language.
#' @export
#'
#' @examples
#' tw_get_wikipedia(id = "Q180099")
tw_get_wikipedia <- function(id,
                             language = tidywikidatar::tw_get_language(),
                             id_df = NULL,
                             cache = NULL,
                             overwrite_cache = FALSE,
                             cache_connection = NULL,
                             disconnect_db = TRUE,
                             wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  base_string <- stringr::str_c("sitelink_", language, "wiki")

  if (is.null(id_df)) {
    id_df <- tw_get(
      id = id,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db,
      language = language,
      wait = wait
    )
  }

  base_link <- id_df %>%
    dplyr::filter(is.element(el = .data$property, set = base_string)) %>%
    dplyr::pull(.data$value)

  if (length(base_link) == 0) {
    as.character(NA)
  } else {
    stringr::str_c("https://", language, ".wikipedia.org/wiki/", base_link)
  }
}
