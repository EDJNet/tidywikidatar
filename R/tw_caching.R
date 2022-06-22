#' Creates the base cache folder where `tidywikidatar` caches data.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for non-interactive sessions).
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_create_cache_folder()
#' }
#' }
tw_create_cache_folder <- function(ask = TRUE) {
  if (fs::file_exists(tidywikidatar::tw_get_cache_folder()) == FALSE) {
    if (ask == FALSE) {
      fs::dir_create(path = tidywikidatar::tw_get_cache_folder(), recurse = TRUE)
    } else {
      usethis::ui_info(glue::glue("The cache folder {{usethis::ui_path(tw_get_cache_folder())}} does not exist. If you prefer to cache files elsewhere, reply negatively and set your preferred cache folder with `tw_set_cache_folder()`"))
      check <- usethis::ui_yeah(glue::glue("Do you want to create {{usethis::ui_path(tw_get_cache_folder())}} for caching data?"))
      if (check == TRUE) {
        fs::dir_create(path = tidywikidatar::tw_get_cache_folder(), recurse = TRUE)
      }
    }
    if (fs::file_exists(tidywikidatar::tw_get_cache_folder()) == FALSE) {
      usethis::ui_stop("This function requires a valid cache folder.")
    }
  }
}


#' Set folder for caching data
#'
#' Consider using a folder out of your current project directory, e.g. `tw_set_cache_folder("~/R/tw_data/")`: you will be able to use the same cache in different projects, and prevent cached files from being sync-ed if you use services such as Nextcloud or Dropbox.
#'
#' @param path A path to a location used for caching data. If the folder does not exist, it will be created.
#'
#' @return The path to the caching folder, if previously set; the same path as given to the function; or the default, `tw_data` is none is given.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_set_cache_folder(fs::path(fs::path_home_r(), "R", "tw_data"))
#' }
#' }
tw_set_cache_folder <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("tw_cache_folder")
  } else {
    Sys.setenv(tw_cache_folder = path)
  }
  if (path == "") {
    path <- fs::path("tw_data")
  }
  invisible(path)
}

#' @rdname tw_set_cache_folder
#' @examples
#' tw_get_cache_folder()
#' @export
tw_get_cache_folder <- tw_set_cache_folder



#' Set database connection settings for the session
#'
#'
#' @param db_settings A list of database connection settings (see example)
#' @param driver A database driver. Common database drivers include `MySQL`, `PostgreSQL`, and `MariaDB`. See `unique(odbc::odbcListDrivers()[[1]])` for a list of locally available drivers.
#' @param host Host address, e.g. "localhost". Different drivers use server or host parameter, only one of them is likely needed.
#' @param server Server address, e.g. "localhost". Different drivers use server or host parameter, only one of them is likely needed.
#' @param port Port to use to connect to the database.
#' @param database Database name.
#' @param user Database user name.
#' @param pwd Password for the database user.
#'
#' @return A list with all given parameters (invisibly).
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'
#'   # Settings can be provided either as a list
#'   db_settings <- list(
#'     driver = "MySQL",
#'     host = "localhost",
#'     server = "localhost",
#'     port = 3306,
#'     database = "tidywikidatar",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#'
#'   tw_set_cache_db(db_settings)
#'
#'   # or as parameters
#'
#'   tw_set_cache_db(
#'     driver = "MySQL",
#'     host = "localhost",
#'     server = "localhost",
#'     port = 3306,
#'     database = "tidywikidatar",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#'
#'   # or ignoring fields that can be left to default values, such as "localhost" and port 3306
#'
#'   tw_set_cache_db(
#'     driver = "MySQL",
#'     database = "tidywikidatar",
#'     user = "secret_username",
#'     pwd = "secret_password"
#'   )
#' }
#' }
tw_set_cache_db <- function(db_settings = NULL,
                            driver = NULL,
                            host = NULL,
                            server = NULL,
                            port = NULL,
                            database = NULL,
                            user = NULL,
                            pwd = NULL) {
  if (is.null(db_settings) == TRUE) {
    if (is.null(driver) == FALSE) Sys.setenv(tw_db_driver = driver)
    if (is.null(host) == FALSE) Sys.setenv(tw_db_host = host)
    if (is.null(server) == FALSE) Sys.setenv(tw_db_host = server)
    if (is.null(port) == FALSE) Sys.setenv(tw_db_port = port)
    if (is.null(database) == FALSE) Sys.setenv(tw_db_database = database)
    if (is.null(user) == FALSE) Sys.setenv(tw_db_user = user)
    if (is.null(pwd) == FALSE) Sys.setenv(tw_db_pwd = pwd)
    return(invisible(
      list(
        driver = driver,
        host = host,
        server = server,
        port = port,
        database = database,
        user = user,
        pwd = pwd
      )
    ))
  } else {
    if (is.null(db_settings$driver) == FALSE) Sys.setenv(tw_db_driver = db_settings$driver)
    if (is.null(db_settings$host) == FALSE) Sys.setenv(tw_db_host = db_settings$host)
    if (is.null(db_settings$server) == FALSE) Sys.setenv(tw_db_server = db_settings$server)
    if (is.null(db_settings$port) == FALSE) Sys.setenv(tw_db_port = db_settings$port)
    if (is.null(db_settings$database) == FALSE) Sys.setenv(tw_db_database = db_settings$database)
    if (is.null(db_settings$user) == FALSE) Sys.setenv(tw_db_user = db_settings$user)
    if (is.null(db_settings$pwd) == FALSE) Sys.setenv(tw_db_pwd = db_settings$pwd)
    return(invisible(db_settings))
  }
}

#' Get database connection settings from the environment
#'
#' Typically set with `tw_set_cache_db()`
#'
#' @return A list with all database parameters as stored in environment variables.
#' @export
#'
#' @examples
#'
#' tw_get_cache_db()
tw_get_cache_db <- function() {
  list(
    driver = Sys.getenv("tw_db_driver"),
    host = Sys.getenv("tw_db_host"),
    server = Sys.getenv("tw_db_server"),
    port = Sys.getenv("tw_db_port"),
    database = Sys.getenv("tw_db_database"),
    user = Sys.getenv("tw_db_user"),
    pwd = Sys.getenv("tw_db_pwd")
  )
}


#' Enable caching for the current session
#'
#' @param SQLite Logical, defaults to TRUE. Set to FALSE to use custom database options. See `tw_set_cache_db()` for details.
#'
#' @return Nothing, used for its side effects.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_enable_cache()
#' }
#' }
tw_enable_cache <- function(SQLite = TRUE) {
  Sys.setenv(tw_cache = TRUE)
  Sys.setenv(tw_cache_SQLite = SQLite)
}


#' Disable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_disable_cache()
#' }
#' }
tw_disable_cache <- function() {
  Sys.setenv(tw_cache = FALSE)
}

#' Check caching status in the current session, and override it upon request
#'
#' Mostly used internally in functions, exported for reference.
#'
#' @param cache Defaults to NULL. If NULL, checks current cache settings. If given, returns given value, ignoring cache.
#'
#' @return Either TRUE or FALSE, depending on current cache settings.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_check_cache()
#' }
#' }
tw_check_cache <- function(cache = NULL) {
  if (is.null(cache) == FALSE) {
    return(as.logical(cache))
  }
  current_cache <- Sys.getenv("tw_cache")
  if (current_cache == "") {
    as.logical(FALSE)
  } else {
    as.logical(current_cache)
  }
}

#' Checks if cache folder exists, if not returns an informative message
#'
#' @return If the cache folder exists, returns TRUE. Otherwise throws an error.
#' @export
#'
#' @examples
#'
#' # If cache folder does not exist, it throws an error
#' tryCatch(tw_check_cache_folder(),
#'   error = function(e) {
#'     return(e)
#'   }
#' )
#'
#' # Create cache folder
#' tw_set_cache_folder(path = fs::path(
#'   tempdir(),
#'   "tw_cache_folder"
#' ))
#' tw_create_cache_folder(ask = FALSE)
#'
#' tw_check_cache_folder()
tw_check_cache_folder <- function() {
  if (fs::file_exists(tw_get_cache_folder()) == FALSE) {
    usethis::ui_stop(paste(
      "Cache folder does not exist. Set it with",
      usethis::ui_code("tw_get_cache_folder()"),
      "and create it with",
      usethis::ui_code("tw_create_cache_folder()")
    ))
  }
  TRUE
}


#' Ensure that connection to cache is disconnected consistently
#'
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get(
#'     id = c("Q180099"),
#'     language = "en"
#'   )
#'   tw_disconnect_from_cache()
#' }
tw_disconnect_from_cache <- function(cache = NULL,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE,
                                     language = tidywikidatar::tw_get_language()) {
  if (isFALSE(disconnect_db)) {
    return(invisible(NULL))
  }

  if (isTRUE(tw_check_cache(cache))) {
    db <- tw_connect_to_cache(
      connection = cache_connection,
      language = language,
      cache = cache
    )

    if (pool::dbIsValid(dbObj = db)) {
      if (inherits(db, "Pool")) {
        pool::poolClose(db)
      } else {
        DBI::dbDisconnect(db)
      }
    }
  }
}
