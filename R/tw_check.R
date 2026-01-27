#' Checks if an input is a search; if not, it tries to return a search
#'
#' Mostly used as a convenience function inside other functions to have
#' consistent inputs.
#'
#' @inheritParams tw_search
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
  response_language = tidywikidatar::tw_get_language(),
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
      response_language = response_language,
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
