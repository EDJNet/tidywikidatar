#' Return a connection to be used for caching
#'
#' @param connection Defaults to NULL. If NULL, uses local SQLite database. If given, must be a connection object (see example)
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A connection object.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cache_connection <- DBI::dbConnect(
#'     RSQLite::SQLite(), # or e.g. odbc::odbc(),
#'     Driver =  ":memory:", # or e.g. "MariaDB",
#'     Host = "localhost",
#'     database = "example_db",
#'     UID = "example_user",
#'     PWD = "example_pwd"
#'   )
#'   tw_connect_to_cache(cache_connection)
#' }
#' }
#'
tw_connect_to_cache <- function(connection = NULL,
                                language = NULL) {
  if (is.null(connection)) {
    if (is.null(language) == FALSE) {
      language <- tw_get_language()
    }

    tw_check_cache_folder()
    db_file <- tw_get_cache_file(
      type = "item",
      language = language
    )
    db <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      db_file
    )
    db
  } else {
    connection
  }
}
