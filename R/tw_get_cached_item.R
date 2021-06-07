#' Retrieve cached item
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection open.
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
#' df_from_api <- tw_get(id = "Q180099", language = "en")
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
tw_get_cached_item <- function(id,
                               language = tidywikidatar::tw_get_language(),
                               cache_connection = NULL,
                               disconnect_db = TRUE) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language
  )

  table_name <- tw_get_cache_table_name(
    type = "item",
    language = language
  )

  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(tibble::tibble(
      id = as.character(NA),
      property = as.character(NA),
      value = as.character(NA)
    ) %>%
      dplyr::slice(0))
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(.data$id %in% stringr::str_to_upper(id)),
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
      property = as.character(NA),
      value = as.character(NA)
    ) %>%
      dplyr::slice(0))
  }

  cached_items_df <- db_result %>%
    tibble::as_tibble()

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }

  cached_items_df
}



#' Gets location of cache file
#'
#' @param type Defaults to "item". Type of cache file to output. Values typically used by `tidywikidatar` include "item", "search", and "qualifier".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector of length one with location of item cache file.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' sqlite_cache_file_location <- tw_get_cache_file() # outputs location of cache file
tw_get_cache_file <- function(type = "item",
                              language = tidywikidatar::tw_get_language()) {
  fs::path(
    tidywikidatar::tw_get_cache_folder(),
    stringr::str_c(
      "tw_",
      type,
      "_db_",
      language,
      ".sqlite"
    )
  )
}

#' Gets name of table inside the database
#'
#' @param type Defaults to "item". Type of cache file to output. Values typically used by `tidywikidatar` include "item", "search", and "qualifier".
#' @param language Defaults to language set with `tw_set_language()`; "en" if not set. Used to limit the data to be cached. Use "all_available" to keep all data. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector of length one with the name of the relevant table in the cache file.
#' @export
#'
#' @examples
#' # outputs name of table used in  of cache file
#' tw_get_cache_table_name(type = "item", language = "en")
tw_get_cache_table_name <- function(type = "item",
                                    language = tidywikidatar::tw_get_language()) {
  stringr::str_c("tw_", type, "_", language)
}

#' Check if given items are present in cache
#'
#' @param id A characther vector. Each element must start with Q, and correspond to a Wikidata identifier.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return A character vector with IDs of items present in cache. If no item found in cache, returns NULL.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' tw_enable_cache()
#' tw_create_cache_folder(ask = FALSE)
#'
#' # add three items to local cache
#' invisible(tw_get(id = "Q180099", language = "en"))
#' invisible(tw_get(id = "Q228822", language = "en"))
#' invisible(tw_get(id = "Q184992", language = "en"))
#'
#' # check if these other items are in cache
#' items_in_cache <- tw_check_cached_items(
#'   id = c(
#'     "Q180099",
#'     "Q228822",
#'     "Q76857"
#'   ),
#'   language = "en"
#' )
#' # it should return only the two items from the current list of id
#' # but not other item already in cache
#' items_in_cache
tw_check_cached_items <- function(id,
                                  language = tidywikidatar::tw_get_language(),
                                  cache_connection = NULL,
                                  disconnect_db = TRUE) {
  tw_get_cached_item(
    id = id,
    language = language,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db
  ) %>%
    dplyr::distinct(id) %>%
    dplyr::pull(id)
}
