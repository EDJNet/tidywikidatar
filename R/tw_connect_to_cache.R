#' Return a connection to be used for caching
#'
#' @param connection Defaults to NULL. If NULL, uses local SQLite database. If given, must be a connection object or a list with relevant connection settings (see example).
#' @param RSQLite Defaults to NULL, expected either NULL or logical. If set to `FALSE`, details on the database connection must be given either as a named list in the connection paramater, or with `tw_set_cache_db()` as environment variables.
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
#'
#'
#' db_settings <- list(
#'  driver = "MySQL",
#'  host = "localhost",
#'  port = 3306,
#'  database = "tidywikidatar",
#'  user = "secret_username",
#'  pwd = "secret_password"
#'  )
#'
#'  tw_connect_to_cache(db_settings)
#' }
#' }
#'
tw_connect_to_cache <- function(connection = NULL,
                                RSQLite = NULL,
                                language = NULL) {

  if (is.null(connection)) {
    if (is.null(language) == FALSE) {
      language <- tw_get_language()
    }

    if (is.null(RSQLite)) {
      RSQLite <- Sys.getenv(x = "tw_cache_SQLite", unset = TRUE)
    }

    if (isTRUE(RSQLite)) {
      tw_check_cache_folder()
      db_file <- tw_get_cache_file(
        type = "item",
        language = language
      )
      db <- DBI::dbConnect(
        drv = RSQLite::SQLite(),
        db_file
      )
      return(db)
    } else {

      if (requireNamespace("odbc", quietly = TRUE)==FALSE) {
        usethis::ui_stop(x = "To use custom databases you need to install the package `odbc`.")
      }

      connection <- tw_get_cache_db()

      db <- DBI::dbConnect(
        drv = odbc::odbc(),
        driver = connection[["driver"]],
        host = connection[["host"]],
        port = connection[["port"]],
        database = connection[["database"]],
        user = connection[["user"]],
        pwd = connection[["pwd"]]
      )
      db
    }

  } else {
    if (is.list(connection)) {
      db <- DBI::dbConnect(
        drv = odbc::odbc(),
        driver = connection[["driver"]],
        host = connection[["host"]],
        port = connection[["port"]],
        database = connection[["database"]],
        user = connection[["user"]],
        pwd = connection[["pwd"]]
      )
      db
    } else {
      connection
    }

  }
}
