#' Get Wikidata property of one or more items as a tidy data frame
#'
#' @param p A character vector, a property. Must always start with the capital
#'   letter "P", e.g. "P31" for "instance of".
#' @param id_df Default to `NULL`. If given, it should be a dataframe typically
#'   generated with [tw_get()], and is used instead of calling Wikidata or
#'   using SQLite cache. Ignored when `id` is of length more than one.
#' @inheritParams tw_get
#' @inheritParams tw_search
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
tw_get_property <- function(
  id,
  p,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
) {
  if (is.data.frame(id)) {
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


#' Get Wikidata property of an item as a vector or list of the same length as
#' input
#'
#' @param only_first Logical, defaults to `FALSE`. If `TRUE`, it just keeps the
#'   first relevant property value for each id (or `NA` if none is available),
#'   and returns a character vector. Warning: this likely discards valid values,
#'   so make sure this is really what you want. If `FALSE`, returns a list of
#'   the same length as input, with all values for each id stored in a list if
#'   more than one is found.
#' @param preferred Logical, defaults to `FALSE`. If `TRUE`, returns properties
#'   that have rank "preferred" if available; if no "preferred" property is
#' found, then it is ignored.
#' @param latest_start_time Logical, defaults to `FALSE`. If `TRUE`, returns the
#'   property that has the most recent start time ("P580") as qualifier if
#'   `only_first` is set to `TRUE`, or returns a list ordered by start time if
#'   `only_first` is set to `FALSE`. If no such qualifier is found, then it is
#'   ignored.
#' @inheritParams tw_get
#' @inheritParams tw_get_property
#'
#' @return A list of the same length as input (or a character vector is
#'   only_first is set to `TRUE`)
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
tw_get_property_same_length <- function(
  id,
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
  wait = 0
) {
  if (is.data.frame(id)) {
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
    if (only_first) {
      return(rep(NA_character_, length(id)))
    } else {
      return(
        rep(NA_character_, length(id)) %>%
          as.list()
      )
    }
  }

  if (!preferred & !latest_start_time) {
    # do nothing, and keep the retrieved property_df
  }
  if (preferred & !latest_start_time) {
    preferred_df <- property_df %>%
      dplyr::mutate(
        rank = factor(
          rank,
          levels = c(
            "preferred",
            "normal",
            "deprecated"
          )
        )
      ) %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(.by_group = TRUE, .data$rank) %>%
      dplyr::ungroup()

    if (nrow(preferred_df) > 0) {
      property_df <- preferred_df
    }
  } else if (latest_start_time) {
    properties_all_df <- property_df

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

    properties_without_qualifier_df <- properties_all_df %>%
      dplyr::rename(qualifier_id = "value") %>%
      dplyr::anti_join(
        y = qualifiers_df,
        by = c("id", "property", "qualifier_id")
      ) %>%
      dplyr::mutate(
        qualifier_property = "P580",
        qualifier_value = NA_character_
      )

    qualifiers_with_start_time_df <- dplyr::bind_rows(
      qualifiers_df,
      properties_without_qualifier_df
    ) %>%
      dplyr::filter(.data$qualifier_property == "P580") %>%
      dplyr::distinct(
        .data$id,
        .data$qualifier_id,
        .data$qualifier_value,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        year = stringr::str_extract(
          string = .data$qualifier_value,
          pattern = "[[:print:]][[:digit:]]{4}"
        ) %>%
          as.numeric(),
        month = stringr::str_extract(
          string = .data$qualifier_value,
          pattern = "-[[:digit:]]{2}-"
        ) %>%
          stringr::str_remove_all("-") %>%
          as.numeric(),
        day = stringr::str_extract(
          string = .data$qualifier_value,
          pattern = "-[[:digit:]]{2}T"
        ) %>%
          stringr::str_remove("T") %>%
          stringr::str_remove("-") %>%
          as.numeric()
      )

    if (preferred) {
      qualifiers_latest_start_time_df <- qualifiers_with_start_time_df %>%
        dplyr::mutate(
          rank = factor(
            rank,
            levels = c(
              "preferred",
              "normal",
              "deprecated"
            )
          )
        ) %>%
        dplyr::group_by(.data$id) %>%
        dplyr::arrange(
          .data$id,
          .data$rank,
          dplyr::desc(.data$year),
          dplyr::desc(.data$month),
          dplyr::desc(.data$day),
          dplyr::desc(.data$qualifier_value)
        ) %>%
        dplyr::select(-"year", -"month", -"day") %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          .data$id,
          .data$property,
          value = .data$qualifier_id,
          .data$rank
        )
    } else {
      qualifiers_latest_start_time_df <- qualifiers_with_start_time_df %>%
        dplyr::group_by(.data$id) %>%
        dplyr::arrange(
          .data$id,
          dplyr::desc(.data$year),
          dplyr::desc(.data$month),
          dplyr::desc(.data$day),
          dplyr::desc(.data$qualifier_value)
        ) %>%
        dplyr::select(-"year", -"month", -"day") %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          .data$id,
          .data$property,
          value = .data$qualifier_id,
          .data$rank
        )
    }

    property_df <- property_df %>%
      dplyr::distinct(.data$id, .data$property) %>%
      dplyr::right_join(
        y = qualifiers_latest_start_time_df,
        by = c("id", "property")
      )
  }

  if (only_first) {
    property_df_post <- property_df %>%
      dplyr::distinct(.data$id, .keep_all = TRUE)
  } else {
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
    )] <- list(NA_character_)
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


#' Get Wikidata property of an item as a character vector of the same length as
#' input
#'
#' This function wraps [tw_get_p()], but always sets `only_first` and
#' `preferred` to `TRUE` in order to give back always a character vector.
#'
#' @inheritParams tw_get_property_same_length
#'
#' @return A character vector of the same length as the input.
#' @export
#'
#' @examples
#' tw_get_p1(id = "Q180099", "P26")
tw_get_p1 <- function(
  id,
  p,
  latest_start_time = FALSE,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
) {
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
