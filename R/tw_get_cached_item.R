#' Retrieve cached item
#'
#' @inheritParams tw_get
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
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
tw_get_cached_item <- function(
  id,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE
) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(tidywikidatar::tw_empty_item)
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "item",
    language = language
  )

  if (!pool::dbExistsTable(conn = db, name = table_name)) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )

    return(tidywikidatar::tw_empty_item)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(.data$id %in% !!stringr::str_to_upper(id)),
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
    return(tidywikidatar::tw_empty_item)
  } else {
    if (
      isFALSE(identical(
        colnames(tidywikidatar::tw_empty_item),
        colnames(db_result)
      ))
    ) {
      cli::cli_abort(c(
        x = "The cache has been generated with a previous version of `tidywikidatar` that is not compatible with the current version.",
        i = "You may want to delete the old cache or reset just this table with {.fn tw_reset_item_cache}"
      ))
    }
  }

  cached_items_df <- db_result %>%
    dplyr::collect()

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  cached_items_df
}


#' Gets location of cache file
#'
#' @param extension Defaults to `sqlite`. Extension of the database cache file.
#' @inheritParams tw_get
#'
#' @return A character vector of length one with location of item cache file.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' sqlite_cache_file_location <- tw_get_cache_file() # outputs location of cache file
tw_get_cache_file <- function(
  extension = "sqlite",
  language = tidywikidatar::tw_get_language()
) {
  fs::path(
    tw_get_cache_folder(),
    stringr::str_c(
      "tw_cache_",
      language
    ),
    ext = extension
  )
}

#' Gets name of table inside the database
#'
#' @param type Defaults to "item". Type of cache file to output. Values
#'   typically used by `tidywikidatar` include "item", "search_item",
#'   "search_property", and "qualifier".
#' @inheritParams tw_search
#' @return A character vector of length one with the name of the relevant table
#'   in the cache file.
#' @export
#'
#' @examples
#' # outputs name of table used in the cache database
#' tw_get_cache_table_name(type = "item", language = "en")
tw_get_cache_table_name <- function(
  type = "item",
  language = tidywikidatar::tw_get_language(),
  response_language = tidywikidatar::tw_get_language()
) {
  if (stringr::str_starts(string = type, pattern = "search")) {
    language <- stringr::str_c(language, "_", response_language)
  }
  stringr::str_c("tw_", type, "_", language)
}

#' Check if given items are present in cache
#'
#' @inheritParams tw_get
#'
#' @return A character vector with IDs of items present in cache. If no item
#'   found in cache, returns `NULL`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_set_cache_folder(path = tempdir())
#'   tw_enable_cache()
#'   tw_create_cache_folder(ask = FALSE)
#'
#'   # add three items to local cache
#'   invisible(tw_get(id = "Q180099", language = "en"))
#'   invisible(tw_get(id = "Q228822", language = "en"))
#'   invisible(tw_get(id = "Q184992", language = "en"))
#'
#'   # check if these other items are in cache
#'   items_in_cache <- tw_check_cached_items(
#'     id = c(
#'       "Q180099",
#'       "Q228822",
#'       "Q76857"
#'     ),
#'     language = "en"
#'   )
#'   # it should return only the two items from the current list of id
#'   # but not other item already in cache
#'   items_in_cache
#' }
tw_check_cached_items <- function(
  id,
  language = tidywikidatar::tw_get_language(),
  cache_connection = NULL,
  disconnect_db = TRUE
) {
  tw_get_cached_item(
    id = id,
    language = language,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db
  ) %>%
    dplyr::distinct(id) %>%
    dplyr::pull(id)
}
