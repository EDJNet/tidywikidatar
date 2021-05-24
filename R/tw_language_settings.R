#' Set language to be used by all functions
#'
#' Defaults to "en".
#'
#' @param language A character vector of length one, with a string of two letters such as "en". For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A two letter code for the language, if previously set; the same language as given to the function; or the default, `en` is none is given.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_set_language(language = "en")
#' }
#' }
#'
tw_set_language <- function(language = NULL) {
  if (is.null(language)) {
    language <- Sys.getenv("tw_language")
  } else {
    Sys.setenv(tw_language = language)
  }
  if (language == "") {
    language <- "en"
  }
  invisible(language)
}

#' @rdname tw_set_language
#' @examples
#' tw_get_language()
#' @export
tw_get_language <- tw_set_language
