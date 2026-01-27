#' Checks if an input is a search; if not, it tries to return a search
#'
#' Mostly used as a convenience function inside other functions to have
#' consistent inputs.
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Language to be used for the search. Can be set once per
#'   session with [tw_set_language()]. If not set, defaults to "en". For
#'   available language values, see
#'   \href{https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all}{the
#'   dedicated Wikimedia page}.
#' @param limit Maximum numbers of responses to be given.
#' @param include_search Logical, defaults to `FALSE`. If `TRUE`, the search is
#'   returned as an additional column.
#' @inheritParams tw_get
#'
#' @return A data frame with three columns, `id`, `label`, and `description`,
#'   filtered by the above criteria. Four columns if `include_search` is set to
#'   `TRUE`.
#' @export
#'
#' @examples
#' # The following two lines should give the same result.
#'
#' tw_check_search("Sylvia Pankhurst")
#' tw_check_search(tw_search("Sylvia Pankhurst"))
tw_check_search <- function(
  search,
  type = "item",
  language = tidywikidatar::tw_get_language(),
  limit = 10,
  include_search = FALSE,
  wait = 0,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE
) {
  if (is.data.frame(search)) {
    return(search)
  } else if (length(search) > 1) {
    cli::cli_abort(c(
      x = "{.var search} must be a single query or a data frame generate by {.fn tw_search}, not {.obj_type_friendly {search}}."
    ))
  } else if (is.character(search)) {
    search_result <- tw_search(
      search = search,
      type = type,
      language = language,
      limit = limit,
      include_search = include_search,
      wait = wait,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )
  }
  search_result
}
