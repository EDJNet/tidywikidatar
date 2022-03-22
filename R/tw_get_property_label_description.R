#' Get label of a Wikidata property in a given language
#'
#' @param property A character vector. Each element must start with P, e.g. "P31".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A character vector, with the Wikidata label in the requested language.
#' @export
#'
#' @examples
#' tw_get_property_label(property = "P31")
tw_get_property_label <- function(property,
                                  language = tidywikidatar::tw_get_language(),
                                  cache = NULL,
                                  overwrite_cache = FALSE,
                                  cache_connection = NULL,
                                  disconnect_db = TRUE,
                                  wait = 0) {
  if (is.data.frame(property) == TRUE) {
    property <- property$id
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (length(property) > 1) {
    if (length(unique(property)) < length(property)) {
      pre_processed <- tibble::tibble(property = property)

      unique_processed <- purrr::map_dfr(
        .x = unique(property),
        .f = function(x) {
          tibble::tibble(
            property = x,
            label = tw_get_property_label_single(
              property = x,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = db,
              disconnect_db = FALSE,
              wait = wait
            ) %>%
              as.character()
          )
        }
      )
      label_output <- pre_processed %>%
        dplyr::left_join(
          y = unique_processed,
          by = "property"
        ) %>%
        dplyr::pull(.data$label)
    } else {
      label_output <- purrr::map_chr(
        .x = property,
        .f = function(x) {
          tw_get_property_label_single(
            property = x,
            language = language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = db,
            disconnect_db = FALSE,
            wait = wait
          )
        }
      )
    }
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
  } else {
    label_output <- tw_get_property_label_single(
      property = property,
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      wait = wait
    )
  }
  label_output
}

#' Get label of a Wikidata property in a given language
#'
#' @param property A character vector. Each element must start with P, e.g. "P31".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A character vector of length 1, with the Wikidata label in the requested language.
#'
#' @examples
#' tidywikidatar:::tw_get_property_label_single(property = "P31")
tw_get_property_label_single <- function(property,
                                         language = tidywikidatar::tw_get_language(),
                                         cache = NULL,
                                         overwrite_cache = FALSE,
                                         cache_connection = NULL,
                                         disconnect_db = TRUE,
                                         wait = 0) {
  label <- tw_search_property(
    search = property,
    cache = cache,
    language = language,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  ) %>%
    dplyr::filter(.data$id == stringr::str_to_upper(property)) %>%
    dplyr::pull(.data$label)

  if (length(label) == 0) {
    as.character(NA)
  } else {
    label
  }
}


#' Get description of a Wikidata property in a given language
#'
#' @param property A character vector of length 1, must start with P, e.g. "P31".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
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
#' tw_get_property_description(property = "P31")
tw_get_property_description <- function(property,
                                        language = tidywikidatar::tw_get_language(),
                                        cache = NULL,
                                        overwrite_cache = FALSE,
                                        cache_connection = NULL,
                                        disconnect_db = TRUE,
                                        wait = 0) {
  if (is.data.frame(property) == TRUE) {
    property <- property$id
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  description <- purrr::map_chr(
    .x = property,
    .f = function(x) {
      tidywikidatar::tw_search_property(
        search = x,
        cache = cache,
        language = language,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = FALSE,
        wait = wait
      ) %>%
        dplyr::filter(.data$id == stringr::str_to_upper(x)) %>%
        dplyr::pull(.data$description)
    }
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  if (length(description) == 0) {
    as.character(NA)
  } else {
    description
  }
}
