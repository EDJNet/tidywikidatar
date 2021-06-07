#' Get label of a Wikidata property in a given language
#'
#' @param property A characther vector. Each element must start with P, e.g. "P31".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
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

  if (length(property) > 1) {
    if (length(unique(property)) < length(property)) {
      pre_processed <- tibble::tibble(property = property)

      unique_processed <- purrr::map_dfr(
        .x = unique(property),
        .f = function(x) {
          tibble::tibble(
            property = x,
            label = tw_get_property_label(
              property = x,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = cache_connection,
              disconnect_db = FALSE,
              wait = wait
            ) %>%
              as.character()
          )
        }
      )
      pre_processed %>%
        dplyr::left_join(
          y = unique_processed,
          by = "property"
        ) %>%
        dplyr::pull(.data$label)
    } else {
      purrr::map_chr(
        .x = property,
        .f = function(x) {
          tw_get_property_label(
            property = x,
            language = language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = cache_connection,
            disconnect_db = FALSE,
            wait = wait
          )
        }
      )
    }
  } else {
    label <- tw_search_property(
      search = property,
      cache = tw_check_cache(cache),
      language = language,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db,
      wait = wait
    ) %>%
      dplyr::filter(.data$id == stringr::str_to_upper(property)) %>%
      dplyr::pull(.data$label)

    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )

    if (length(label) == 0) {
      as.character(NA)
    } else {
      label
    }
  }
}

#' Get description of a Wikidata property in a given language
#'
#' @param property A characther vector of length 1, must start with P, e.g. "P31".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#' tw_get_property_description(property = "P31")
tw_get_property_description <- function(property,
                                        language = tidywikidatar::tw_get_language(),
                                        cache = NULL,
                                        overwrite_cache = FALSE,
                                        cache_connection = NULL,
                                        wait = 0) {
  if (is.data.frame(property) == TRUE) {
    property <- property$id
  }

  if (length(property) > 1) {
    purrr::map_chr(
      .x = property,
      .f = function(x) {
        tw_get_property_description(
          property = x,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection,
          wait = wait
        )
      }
    )
  } else {
    description <- tidywikidatar::tw_search_property(
      search = property,
      cache = tw_check_cache(cache),
      language = language,
      overwrite_cache = overwrite_cache,
      wait = wait
    ) %>%
      dplyr::filter(.data$id == stringr::str_to_upper(property)) %>%
      dplyr::pull(.data$description)

    if (length(description) == 0) {
      as.character(NA)
    } else {
      description
    }
  }
}
