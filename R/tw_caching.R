#' Creates the base cache folder where `tidywikidatar` caches data.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for interactive sessions).
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


#' Enable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_enable_cache()
#' }
#' }
tw_enable_cache <- function() {
  Sys.setenv(tw_cache = TRUE)
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
