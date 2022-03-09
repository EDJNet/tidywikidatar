#' Writes search to cache
#'
#' Writes search to cache. Typically used internally, but exported to enable custom caching solutions.
#'
#' @param search_df A data frame with four columns typically generated with `tw_search(include_search = TRUE)`.
#' @inheritParams tw_search
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = fs::path(tempdir(), paste(sample(letters, 24), collapse = "")))
#' tw_create_cache_folder(ask = FALSE)
#' tw_disable_cache()
#'
#' search_from_api <- tw_search(search = "Sylvia Pankhurst", include_search = TRUE)
#'
#' search_from_cache <- tw_get_cached_search("Sylvia Pankhurst")
#'
#' nrow(search_from_cache) == 0 # expect TRUE, as nothing has yet been stored in cache
#'
#' tw_write_search_to_cache(search_df = search_from_api)
#'
#' search_from_cache <- tw_get_cached_search("Sylvia Pankhurst")
#'
#' search_from_cache
tw_write_search_to_cache <- function(search_df,
                                     type = "item",
                                     language = tidywikidatar::tw_get_language(),
                                     response_language = tidywikidatar::tw_get_language(),
                                     cache = NULL,
                                     overwrite_cache = FALSE,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE) {
  if (identical(x = colnames(search_df), y = c("search", "id", "label", "description")) == FALSE) {
    usethis::ui_stop('search_df must have exactly four columns: "search", "id", "label", "description"')
  }

  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(NULL))
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
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE search = {search*}",
        search = unique(search_df$search),
        table_name = table_name,
        .con = db
      )
      result <- pool::dbExecute(
        conn = db,
        statement = statement
      )
    }
  }

  pool::dbWriteTable(db,
    name = table_name,
    value = search_df,
    append = TRUE
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language_combo
  )
}
