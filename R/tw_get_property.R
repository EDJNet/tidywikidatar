#' Get Wikidata property of one or more items as a tidy data frame
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A tibble, corresponding to the value for the given property. A tibble of zero rows if no relevant property found.
#' @export
#'
#' @examples
#' # Who were the doctoral advisors - P184 - of Margaret Mead - Q180099?
#' advisors <- tw_get_property(id = "Q180099", p = "P184")
#' advisors
#'
#' # tw_get_label(advisors)
#'
#' # It is also possible to get one property for many id
#'
#' if (interactive()) {
#'   tw_get_property(
#'     id = c(
#'       "Q180099",
#'       "Q228822"
#'     ),
#'     p = "P31"
#'   )
#'
#'   # Or many properties for a single id
#'
#'   tw_get_property(
#'     id = "Q180099",
#'     p = c("P21", "P31")
#'   )
#' }
tw_get_property <- function(id,
                            p,
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

  unique_id <- tw_check_qid(id)

  if (length(unique_id) == 0) {
    return(NULL)
  }

  if (is.null(id_df)) {
    id_df <- tw_get(
      id = unique_id,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      language = language,
      wait = wait,
      disconnect_db = disconnect_db
    )
  } else {
    id_df <- id_df %>%
      dplyr::filter(.data$id %in% unique_id)

    missing_id_v <- unique_id[!unique_id %in% id_df$id]

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

  property_df <- id_df %>%
    dplyr::filter(.data$property %in% p)
  if (nrow(property_df) == 0) {
    return(tidywikidatar::tw_empty_item)
  } else {
    if (length(p) > 1) {
      property_df <- tibble::tibble(property = p) %>%
        dplyr::left_join(y = property_df, by = "property") %>%
        dplyr::select("id", "property", "value", "rank")
    }
    if (length(id) > 1) {
      property_df <- tibble::tibble(id = id) %>%
        dplyr::left_join(
          y = property_df,
          by = "id"
        )
    }

    property_df
  }
}


#' Get Wikidata property of an item as a vector or list of the same length as input
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param only_first Logical, defaults to FALSE. If TRUE, it just keeps the first relevant property value for each id (or NA if none is available), and returns a character vector. Warning: this likely discards valid values, so make sure this is really what you want. If FALSE, returns a list of the same length as input, with all values for each id stored in a list if more than one is found.
#' @param preferred Logical, defaults to FALSE. If TRUE, returns properties that have rank "preferred" if available; if no "preferred" property is found, then it is ignored.
#' @param latest_start_time Logical, defaults to FALSE. If TRUE, returns the property that has the most recent start time ("P580") as qualifier. If no such qualifier is found, then it is ignored.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or replying on cache.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A list of the same length of input (or a character vector is only_first is set to TRUE)
#' @export
#'
#' @examples
#'
#'
#' # By default, it returns a list of the same length as input,
#' # no matter how many values for each id/property
#'
#'
#' if (interactive()) {
#'   tw_get_property_same_length(
#'     id = c(
#'       "Q180099",
#'       "Q228822",
#'       "Q76857"
#'     ),
#'     p = "P26"
#'   )
#'   # Notice that if no relevant match is found, it returns a NA
#'   # This is useful for piped operations
#'
#'   tibble::tibble(id = c(
#'     "Q180099",
#'     "Q228822",
#'     "Q76857"
#'   )) %>%
#'     dplyr::mutate(spouse = tw_get_property_same_length(id, "P26"))
#'
#'   # Consider unnesting for further analysis
#'
#'   tibble::tibble(id = c(
#'     "Q180099",
#'     "Q228822",
#'     "Q76857"
#'   )) %>%
#'     dplyr::mutate(spouse = tw_get_property_same_length(id, "P26")) %>%
#'     tidyr::unnest(cols = spouse)
#'
#'   # If you are sure that you are interested only in the first return value,
#'   # consider setting only_first=TRUE to get a character vector rather than a list
#'   # Be mindful: you may well be discarding valid values.
#'   tibble::tibble(id = c(
#'     "Q180099",
#'     "Q228822",
#'     "Q76857"
#'   )) %>%
#'     dplyr::mutate(spouse = tw_get_property_same_length(id, "P26",
#'       only_first = TRUE
#'     ))
#' }
tw_get_property_same_length <- function(id,
                                        p,
                                        only_first = FALSE,
                                        preferred = FALSE,
                                        latest_start_time = FALSE,
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

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  property_df <- tw_get_property(
    id = id,
    p = p,
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = db,
    disconnect_db = FALSE,
    wait = wait
  )

  if (is.null(property_df)) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
    return(rep(as.character(NA), length(id)))
  } else if (nrow(property_df) == 0) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
    if (only_first == TRUE) {
      return(rep(as.character(NA), length(id)))
    } else {
      return(rep(as.character(NA), length(id)) %>%
        as.list())
    }
  }



  if (preferred == TRUE) {
    preferred_df <- property_df %>%
      dplyr::mutate(rank = factor(rank,
        levels = c(
          "preferred",
          "normal",
          "deprecated"
        )
      )) %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(.by_group = TRUE, .data$rank) %>%
      dplyr::ungroup()

    if (nrow(preferred_df) > 0) {
      property_df <- preferred_df
    }
  }

  if (latest_start_time == TRUE) {
    qualifiers_df <- tw_get_qualifiers(
      id = id,
      p = p,
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait
    )

    qualifiers_latest_start_time_df <- qualifiers_df %>%
      dplyr::filter(.data$qualifier_property == "P580") %>%
      dplyr::distinct(.data$id, .data$qualifier_id, .data$qualifier_value, .keep_all = TRUE) %>%
      dplyr::arrange(.data$qualifier_value) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(.data$id,
        .data$property,
        value = .data$qualifier_id
      )

    if (nrow(qualifiers_latest_start_time_df) > 0) {
      qualifiers_latest_start_time_post_df <- purrr::map_dfr(
        .x = id,
        .f = function(current_id) {
          current_latest_start_time_post_df <- qualifiers_latest_start_time_df %>%
            dplyr::filter(.data$id == current_id)
          if (nrow(current_latest_start_time_post_df) > 0) {
            current_latest_start_time_post_df
          } else {
            property_df %>%
              dplyr::filter(.data$id == current_id)
          }
        }
      )

      property_df <- property_df %>%
        dplyr::right_join(
          y = qualifiers_latest_start_time_df %>%
            dplyr::select(-"property"),
          by = c("id", "value")
        )
    }
  }


  if (only_first == TRUE) {
    property_df_post <- property_df %>%
      dplyr::distinct(.data$id, .keep_all = TRUE)
  } else if (only_first == FALSE) {
    property_df_post <- property_df %>%
      dplyr::distinct(.data$id, .data$value) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(value = list(.data$value)) %>%
      dplyr::ungroup()
  }

  property_df_out <- tibble::tibble(id = id) %>%
    dplyr::left_join(
      y = property_df_post,
      by = "id"
    )

  if (is.list(property_df_out$value) == "list") {
    property_df_out$value[purrr::map_lgl(
      .x = property_df_out$value,
      .f = is.null
    )] <- list(as.character(NA))
  }


  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  property_df_out %>%
    dplyr::pull("value")
}


#' @rdname tw_get_property_same_length
#' @examples tw_get_p(id = "Q180099", "P26")
#' @export
tw_get_p <- tw_get_property_same_length


#' Get Wikidata property of an item as a character vector of the same length as input
#'
#' This function wraps `tw_get_p()`, but always sets `only_first` and `preferred` to TRUE in order to give back always a character vector.
#'
#' @inheritParams tw_get_property_same_length
#'
#' @return A character vector of the same length as the input.
#' @export
#'
#' @examples
#' tw_get_p1(id = "Q180099", "P26")
tw_get_p1 <- function(id,
                      p,
                      latest_start_time = FALSE,
                      language = tidywikidatar::tw_get_language(),
                      id_df = NULL,
                      cache = NULL,
                      overwrite_cache = FALSE,
                      cache_connection = NULL,
                      disconnect_db = TRUE,
                      wait = 0) {
  tw_get_property_same_length(
    id = id,
    p = p,
    only_first = TRUE,
    preferred = TRUE,
    latest_start_time = latest_start_time,
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  )
}
