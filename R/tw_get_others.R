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
  } else {
    current_id <- id
    id_df <- id_df %>%
      dplyr::filter(.data$id %in% current_id)

    missing_id_v <- current_id[!current_id %in% id_df$id]

    if (length(missing_id_v) > 0) {
      id_df <- dplyr::bind_rows(
        id_df,
        tw_get(
          id = missing_id_v,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection,
          language = language,
          wait = wait,
          disconnect_db = disconnect_db
        )
      )
    }
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
#' @return A character vector of the same length as the vector of id given, with the Wikidata description in the requested language.
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
  } else {
    current_id <- id
    id_df <- id_df %>%
      dplyr::filter(.data$id %in% current_id)

    missing_id_v <- current_id[!current_id %in% id_df$id]

    if (length(missing_id_v) > 0) {
      id_df <- dplyr::bind_rows(
        id_df,
        tw_get(
          id = missing_id_v,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection,
          language = language,
          wait = wait,
          disconnect_db = disconnect_db
        )
      )
    }
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
#' @param full_link Logical, defaults to TRUE. If FALSE, returns only the part of the url that corresponds to the title.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A character vector of the same length as the vector of id given, with the Wikipedia link in the requested language.
#' @export
#'
#' @examples
#' tw_get_wikipedia(id = "Q180099")
tw_get_wikipedia <- function(id,
                             full_link = TRUE,
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

  if (sum(is.na(id)) == length(id)) {
    return(rep(as.character(NA), length(id)))
  }

  if (length(tw_check_qid(id = id)) == 0) {
    return(rep(as.character(NA), length(id)))
  }


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
  } else {
    current_id <- id
    id_df <- id_df %>%
      dplyr::filter(.data$id %in% current_id)

    missing_id_v <- current_id[!current_id %in% id_df$id]

    if (length(missing_id_v) > 0) {
      id_df <- dplyr::bind_rows(
        id_df,
        tw_get(
          id = missing_id_v,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection,
          language = language,
          wait = wait,
          disconnect_db = disconnect_db
        )
      )
    }
  }


  base_string <- stringr::str_c("sitelink_", language, "wiki")

  field_df <- id_df %>%
    dplyr::filter(.data$property == base_string) %>%
    dplyr::distinct(.data$id,
      .keep_all = TRUE
    )

  if (nrow(field_df) == 0) {
    base_link <- rep(as.character(NA), length(id))
  } else if (nrow(field_df) < length(id)) {
    base_link <- tibble::tibble(id = id) %>%
      dplyr::left_join(y = field_df, by = "id") %>%
      dplyr::pull(.data$value)
  } else {
    base_link <- field_df %>%
      dplyr::pull(.data$value)
  }

  if (length(base_link) == 0) {
    rep(as.character(NA), length(id))
  } else {
    if (full_link == TRUE) {
      stringr::str_c("https://", language, ".wikipedia.org/wiki/", base_link)
    } else {
      base_link
    }
  }
}
