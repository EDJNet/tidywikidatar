#' Creates the cache folder where `streetnames` caches data.
#'
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for interactive sessions).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' tw_create_cache_folder
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
#' \dontrun{
#' tw_set_cache_folder("~/R/tw_data/")
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
  path
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
#' \dontrun{
#' tw_enable_cache()
#' }
tw_enable_cache <- function() {
  Sys.setenv(tw_cache = TRUE)
}


#' Disable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \dontrun{
#' tw_disable_cache()
#' }
tw_disable_cache <- function() {
  Sys.setenv(tw_cache = FALSE)
}

#' Enable caching for the current session
#'
#' @return Nothing, used for its side effects.
#' @export

#' @examples
#' \dontrun{
#' tw_check_cache()
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
