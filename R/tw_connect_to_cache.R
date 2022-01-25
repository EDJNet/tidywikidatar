#' Return a connection to be used for caching
#'
#' @param connection Defaults to NULL. If NULL, uses local SQLite database. If given, must be a connection object or a list with relevant connection settings (see example).
#' @param RSQLite Defaults to NULL, expected either NULL or logical. If set to `FALSE`, details on the database connection must be given either as a named list in the connection parameter, or with `tw_set_cache_db()` as environment variables.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#'
#' @return A connection object.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cache_connection <- pool::dbPool(
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
#'   db_settings <- list(
#'     driver = "MySQL",
#'     host = "localhost",
#'     port = 3306,
#'     database = "tidywikidatar",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#'
#'   tw_connect_to_cache(db_settings)
#' }
#' }
#'
tw_connect_to_cache <- function(connection = NULL,
                                RSQLite = NULL,
                                language = NULL,
                                cache = NULL) {
  if (isFALSE(x = tw_check_cache(cache))) {
    return(NULL)
  }

  if (is.null(connection) == FALSE & is.list(connection) == FALSE) {
    if (DBI::dbIsValid(connection) == FALSE) {
      connection <- NULL
    }
  }

  if (is.null(connection)) {
    if (is.null(language) == FALSE) {
      language <- tw_get_language()
    }

    if (is.null(RSQLite)) {
      RSQLite <- as.logical(Sys.getenv(x = "tw_cache_SQLite", unset = TRUE))
    }

    if (isTRUE(RSQLite)) {
      tw_check_cache_folder()
      db_file <- tw_get_cache_file(
        language = language
      )

      if (fs::file_exists(db_file) == FALSE) {
        db <- DBI::dbConnect(
          drv = RSQLite::SQLite(),
          db_file
        )
      }

      db <- pool::dbPool(
        drv = RSQLite::SQLite(),
        dbname = db_file
      )
      return(db)
    } else {
      connection <- tw_get_cache_db()

      if (connection[["driver"]] == "SQLite") {
        if (requireNamespace("RSQLite", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use SQLite databases you need to install the package `RSQLite`.")
        }
        drv <- RSQLite::SQLite()
      } else {
        if (requireNamespace("odbc", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use custom databases you need to install the package `odbc`, or provide your connection directly to all functions.")
        }
        drv <- odbc::odbc()
      }

      db <- pool::dbPool(
        drv = drv,
        driver = connection[["driver"]],
        host = connection[["host"]],
        port = as.integer(connection[["port"]]),
        database = connection[["database"]],
        user = connection[["user"]],
        pwd = connection[["pwd"]]
      )
      return(db)
    }
  } else {
    if (is.list(connection)) {
      if (connection[["driver"]] == "SQLite") {
        if (requireNamespace("RSQLite", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use SQLite databases you need to install the package `RSQLite`.")
        }
        drv <- RSQLite::SQLite()
      } else {
        if (requireNamespace("odbc", quietly = TRUE) == FALSE) {
          usethis::ui_stop(x = "To use custom databases you need to install the package `odbc`, or provide your connection directly to all functions.")
        }
        drv <- odbc::odbc()
      }

      db <- pool::dbPool(
        drv = drv,
        driver = connection[["driver"]],
        host = connection[["host"]],
        port = as.integer(connection[["port"]]),
        database = connection[["database"]],
        dbname = connection[["database"]],
        user = connection[["user"]],
        pwd = connection[["pwd"]]
      )
      return(db)
    } else {
      return(connection)
    }
  }
}
