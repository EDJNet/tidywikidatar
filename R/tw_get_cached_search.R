#' Retrieve cached search
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param include_search Logical, defaults to FALSE. If TRUE, the search is returned as an additional column.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
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
                                 cache = NULL,
                                 include_search = FALSE,
                                 cache_connection = NULL,
                                 disconnect_db = TRUE) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(NULL))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = stringr::str_c("search_", type),
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
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
      language = language
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
    language = language
  )

  cached_items_df
}
