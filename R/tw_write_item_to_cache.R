#' Writes item to cache
#'
#' Writes item to cache. Typically used internally, but exported to enable custom caching solutions.
#'
#' @param item_df A data frame with three columns typically generated with `tw_get()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it first deletes all rows associated with the item(s) included in `item_df`. Useful if the original Wikidata object has been updated.
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
#' df_from_api <- tw_get(id = "Q180099", language = "en")
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
#'
#' is.null(df_from_cache) # expect TRUE, as nothing has yet been stored in cache
#'
#' tw_write_item_to_cache(
#'   item_df = df_from_api,
#'   language = "en"
#' )
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
#'
#' is.null(df_from_cache) # expect a data frame, same as df_from_api
tw_write_item_to_cache <- function(item_df,
                                   language = tidywikidatar::tw_get_language(),
                                   overwrite_cache = FALSE,
                                   cache_connection = NULL,
                                   disconnect_db = TRUE) {
  db <- tw_connect_to_cache(connection = cache_connection, language = language)

  table_name <- tw_get_cache_table_name(type = "item", language = language)

  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE id = {id*}",
        id = unique(item_df$id),
        table_name = table_name,
        .con = db
      )
      result <- DBI::dbExecute(
        conn = db,
        statement = statement
      )
    }
  }

  DBI::dbWriteTable(db,
    name = table_name,
    value = item_df,
    append = TRUE
  )

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }
}
