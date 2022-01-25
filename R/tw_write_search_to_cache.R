#' Writes search to cache
#'
#' Writes search to cache. Typically used internally, but exported to enable custom caching solutions.
#'
#' @param search_df A data frame with four columns typically generated with `tw_search(include_search = TRUE)`.
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it first deletes all rows associated with the item(s) included in the provided `search_df`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
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
    language = language
  )
}
