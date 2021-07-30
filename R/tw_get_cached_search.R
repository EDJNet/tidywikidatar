#' Retrieve cached search
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
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
                                 include_search = FALSE,
                                 cache_connection = NULL,
                                 disconnect_db = TRUE) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language
  )

  table_name <- tw_get_cache_table_name(
    type = stringr::str_c("search_", type),
    language = language
  )

  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    search_df <- tibble::tibble(
      search = as.character(NA),
      id = as.character(NA),
      label = as.character(NA),
      description = as.character(NA)
    ) %>%
      dplyr::slice(0)

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
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(tibble::tibble(
      id = as.character(NA),
      label = as.character(NA),
      value = as.character(NA)
    ) %>%
      dplyr::slice(0))
  }


  if (include_search == TRUE) {
    cached_items_df <- db_result %>%
      tibble::as_tibble()
  } else {
    cached_items_df <- db_result %>%
      tibble::as_tibble() %>%
      dplyr::select(-.data$search)
  }

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }

  cached_items_df
}
