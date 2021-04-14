#' Get Wikidata qualifiers for a given property of a given item
#'
#' N.B. In order to provide for consistently structured output, this function outputs either id or value for each  qualifier. The user should keep in mind that some of these come with additional detail (e.g. the unit, precision, or reference calendar).
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to "all_available". It should be relevant only for caching purposes. For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param include_id_and_p Logical, defaults to TRUE If TRUE, output includes a column with the wikidata id of the item.
#'
#' @return A data frame (a tibble) with five columns: `id` for the input id, `qualifier_id`, `property`, `value`, and `set` (to distinguish sets of data when a property is present more than once)
#' @export
#'
#' @examples
#' tw_get_qualifiers(id = "Q180099", p = "P26", language = "en")
tw_get_qualifiers <- function(id,
                              p,
                              language = "all_available",
                              cache = NULL,
                              overwrite_cache = FALSE,
                              wait = 0,
                              include_id_and_p = TRUE) {
  if (length(id) > 1 | length(p) > 1) {
    purrr::map2_dfr(
      .x = id,
      .y = p,
      .f = function(x, y) {
        tw_get_qualifiers(
          id = x,
          p = y,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          include_id_and_p = include_id_and_p,
          wait = wait
        )
      }
    )
  } else {
    if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
      db_result <- tw_get_cached_qualifiers(
        id = id,
        p = p,
        language = language
      )

      if (is.data.frame(db_result)) {
        if (isTRUE(include_id_and_p)) {
          return(db_result %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              id = id,
              property = p
            ) %>%
            dplyr::select(.data$id, .data$property, dplyr::everything()))
        } else {
          return(db_result %>%
            tibble::as_tibble())
        }
      }
    }
    Sys.sleep(time = wait)

    claims <- tryCatch(WikidataR::get_item(id = id),
      error = function(e) {
        return(tibble::tibble(id = NA))
      }
    ) %>% purrr::pluck(
      1,
      "claims"
    )

    qualifiers <- claims[[p]] %>%
      tibble::as_tibble()

    if (is.element("qualifiers", colnames(qualifiers)) == FALSE) {
      return(NULL)
    }

    qualifiers_df <- purrr::map_dfr(
      .x = 1:nrow(qualifiers),
      function(i) {
        qualifier_parent_pre <- qualifiers %>%
          dplyr::slice(i) %>%
          dplyr::pull(.data$mainsnak) %>%
          dplyr::pull(.data$datavalue) %>%
          dplyr::pull(.data$value)

        if (is.element("id", colnames(qualifier_parent_pre))) {
          qualifier_parent <- qualifier_parent_pre %>%
            dplyr::pull(.data$id)
        } else {
          qualifier_parent <- qualifier_parent_pre %>%
            dplyr::pull(1)
        }


        qualifiers_set <- qualifiers %>%
          dplyr::slice(i) %>%
          dplyr::pull(.data$qualifiers)


        purrr::map_dfr(
          .x = qualifiers_set,
          .f = function(x) {
            current_qualifier <- x %>%
              purrr::pluck(1) %>%
              tibble::as_tibble()

            p <- current_qualifier[["property"]]
            value_df <- current_qualifier[["datavalue"]][["value"]] %>%
              tibble::as_tibble()
            if (is.element("id", names(value_df)) == TRUE) {
              value <- value_df %>%
                dplyr::pull(.data$id)
            } else if (is.element("time", names(value_df)) == TRUE) {
              value <- value_df %>%
                dplyr::pull(.data$time)
            } else {
              return(NULL)
            }
            tibble::tibble(
              qualifier_id = qualifier_parent,
              qualifier_property = p,
              value = value,
              set = i
            )
          }
        )
      }
    )

    if (tw_check_cache(cache) == TRUE) {
      tw_write_qualifiers_to_cache(
        id = id,
        p = p,
        qualifiers_df = qualifiers_df,
        language = language,
        overwrite_cache = overwrite_cache
      )
    }
    if (isTRUE(include_id_and_p)) {
      qualifiers_df %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          id = id,
          property = p
        ) %>%
        dplyr::select(
          .data$id,
          .data$property,
          dplyr::everything()
        )
    } else {
      qualifiers_df %>%
        tibble::as_tibble()
    }
  }
}



#' Retrieve cached qualifier
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to "all_available". By default, returns dataset with labels in all available languages. If given, only in the chosen language. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
#'
#'
#' tw_set_cache_folder(path = tempdir())
#' tw_enable_cache()
#' tw_create_cache_folder(ask = FALSE)
#'
#' df_from_api <- tw_get_qualifiers(id = "Q180099", p = "P26", language = "en")
#'
#' df_from_cache <- tw_get_cached_qualifiers(
#'   id = "Q180099",
#'   p = "P26",
#'   language = "en"
#' )
#'
#' df_from_cache
tw_get_cached_qualifiers <- function(id,
                                     p,
                                     language = "all_available") {
  tw_check_cache_folder()

  db_file <- tw_get_cache_file(
    type = "qualifiers",
    language = language
  )

  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db_file
  )

  db_table_name <- stringr::str_c(
    stringr::str_to_upper(id),
    "_",
    stringr::str_to_upper(p)
  )

  db_result <- tryCatch(
    DBI::dbReadTable(
      conn = db,
      name = db_table_name
    ),
    error = function(e) {
      logical(1L)
    }
  )
  DBI::dbDisconnect(db)

  if (is.data.frame(db_result)) {
    return(db_result %>% tibble::as_tibble())
  }
}

#' Write qualifiers to cache
#'
#' Mostly to be used internally by `tidywikidatar`, use with caution to keep caching consistent.
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param qualifiers_df A data frame with two columns typically generated with `tw_get(include_id_and_p = FALSE)`.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to "all_available". By default, returns dataset with labels in all available languages. If given, only in the chosen language. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return Silently returns the same data frame provided as input. Mostly used internally for its side effects.
#'
#' @export
#'
#' @examples
#'
#' q_df <- tw_get_qualifiers(
#'   id = "Q180099",
#'   p = "P26",
#'   language = "en",
#'   cache = FALSE,
#'   include_id = FALSE
#' )
#' tw_write_qualifiers_to_cache(
#'   id = "Q180099",
#'   p = "P26",
#'   qualifiers_df = q_df,
#'   language = "en"
#' )
tw_write_qualifiers_to_cache <- function(id,
                                         p,
                                         qualifiers_df,
                                         language = "all_available",
                                         overwrite_cache = FALSE) {
  tw_check_cache_folder()

  db_file <- tw_get_cache_file(
    type = "qualifiers",
    language = language
  )

  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db_file
  )
  RSQLite::sqliteSetBusyHandler(dbObj = db, handler = 5000)

  db_table_name <- stringr::str_c(
    stringr::str_to_upper(id),
    "_",
    stringr::str_to_upper(p)
  )

  if (overwrite_cache == FALSE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = db_table_name,
      value = qualifiers_df
    )
  } else {
    RSQLite::dbWriteTable(
      conn = db,
      name = db_table_name,
      value = qualifiers_df,
      overwrite = TRUE
    )
  }
  DBI::dbDisconnect(db)
  invisible(qualifiers_df)
}
