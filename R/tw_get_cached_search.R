#' Retrieve cached search
#'
#' @inheritParams tw_search
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
#' search_from_api <- tw_search("Sylvia Pankhurst")
#' search_from_api
#'
#' df_from_cache <- tw_get_cached_search("Sylvia Pankhurst")
#' df_from_cache
tw_get_cached_search <- function(search,
                                 type = "item",
                                 language = tidywikidatar::tw_get_language(),
                                 response_language = tidywikidatar::tw_get_language(),
                                 cache = NULL,
                                 include_search = FALSE,
                                 cache_connection = NULL,
                                 disconnect_db = TRUE) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(tidywikidatar::tw_empty_search)
  }

  language_combo <- stringr::str_c(language, "_", response_language)

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language_combo,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = stringr::str_c("search_", type),
    language = language_combo
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language_combo
    )

    search_df <- tidywikidatar::tw_empty_search

    if (include_search == TRUE) {
      return(search_df)
    } else {
      return(search_df %>%
        dplyr::select(-.data$search))
    }
  }

  search_string <- search
  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(search %in% search_string),
    error = function(e) {
      logical(1L)
    }
  )
  if (isFALSE(db_result)) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language_combo
    )
    search_df <- tidywikidatar::tw_empty_search

    if (include_search == TRUE) {
      return(search_df)
    } else {
      return(search_df %>%
        dplyr::select(-.data$search))
    }
  }


  if (include_search == TRUE) {
    cached_items_df <- db_result %>%
      dplyr::collect()
  } else {
    cached_items_df <- db_result %>%
      dplyr::collect() %>%
      dplyr::select(-.data$search)
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language_combo
  )

  cached_items_df
}
