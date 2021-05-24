#' Return a connection to be used for caching
#'
#' @param connection Defaults to NULL. If NULL, uses local SQLite database. If given, must be a connection object (see example)
#'
#' @return A connection object.
#' @export
#'
#' @examples
#'
#'
#' cache_connection <- DBI::dbConnect(odbc::odbc(),
#'  Driver = "MariaDB",
#'  Host = "localhost",
#'  database = "example_db",
#'  UID = "example_user",
#'  PWD = "example_pwd"
#'  )
#'
#' tw_connect_to_cache(cache_connection)
#'
tw_connect_to_cache <- function(connection = NULL,
                                language = NULL) {

  if (is.null(connection)) {

    if (is.null(language)==FALSE) {
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
    RSQLite::sqliteSetBusyHandler(dbObj = db, handler = 5000)
    db
  } else {
    connection
  }
}