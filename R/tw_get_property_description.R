#' Get description of a Wikidata property in a given language
#'
#' @param property A character vector. Each element must start with P, e.g.
#'   "P31".
#' @inheritParams tw_get
#' @inheritParams tw_search
#'
#' @return A character vector, with the Wikidata description in the requested
#'   language.
#' @export
#'
#' @examples
#' tw_get_property_description(property = "P31")
tw_get_property_description <- function(
  property,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
) {
  if (is.data.frame(property)) {
    property <- property$id
  }

  if (length(tw_check_pid(property = property)) == 0) {
    return(rep(NA_character_, length(property)))
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
            description = tw_get_property_description_single(
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
      description_output <- pre_processed %>%
        dplyr::left_join(
          y = unique_processed,
          by = "property"
        ) %>%
        dplyr::pull("description")
    } else {
      description_output <- purrr::map_chr(
        .x = property,
        .f = function(x) {
          tw_get_property_description_single(
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
    description_output <- tw_get_property_description_single(
      property = property,
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      wait = wait
    )
  }
  description_output
}

#' Get description of a Wikidata property in a given language
#'
#' @param property A character vector of length 1, must start with P, e.g. "P31".
#' @inheritParams tw_get
#' @inheritParams tw_search
#' @inheritParams tw_get_property
#'
#' @return A character vector of length 1, with the Wikidata description in the
#'   requested language.
#'
#' @examples
#' tidywikidatar:::tw_get_property_description_single(property = "P31")
tw_get_property_description_single <- function(
  property,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
) {
  if (length(tw_check_pid(property = property)) == 0) {
    return(NA_character_)
  }

  description <- tw_search_property(
    search = property,
    cache = cache,
    language = language,
    response_language = language,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  ) %>%
    dplyr::filter(.data$id == stringr::str_to_upper(property)) %>%
    dplyr::pull("description")

  if (length(description) == 0) {
    NA_character_
  } else {
    description
  }
}
