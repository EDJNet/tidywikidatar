#' Writes item to cache
#'
#' Writes item to cache. Typically used internally, but exported to enable custom caching solutions.
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param item_df A data frame with two columns typically generated with `tw_get(include_id = FALSE)`.
#' @param language Defaults to "all_available". By default, returns dataset with labels in all available languages. If given, only in the chosen language. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
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
#'   id = "Q180099",
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
tw_write_item_to_cache <- function(id,
                                   item_df,
                                   language = "all_available",
                                   overwrite_cache = FALSE) {
  tw_check_cache_folder()
  db_file <- tw_get_cache_file(
    type = "item",
    language = language
  )
  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db_file
  )
  RSQLite::sqliteSetBusyHandler(dbObj = db, handler = 5000)

  if (overwrite_cache == FALSE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = item_df
    )
  } else {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = item_df,
      overwrite = TRUE
    )
  }
  DBI::dbDisconnect(db)
}
