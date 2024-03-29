#' Checks if an input is a search; if not, it tries to return a search
#'
#' Mostly used as a convenience function inside other functions to have consistent inputs.
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Language to be used for the search. Can be set once per session with `tw_set_language()`. If not set, defaults to "en". For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param include_search Logical, defaults to FALSE. If TRUE, the search is returned as an additional column.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return A data frame with three columns, `id`, `label`, and `description`, filtered by the above criteria.
#' @export
#'
#' @examples
#' # The following two lines should give the same result.
#'
#' tw_check_search("Sylvia Pankhurst")
#' tw_check_search(tw_search("Sylvia Pankhurst"))
tw_check_search <- function(search,
                            type = "item",
                            language = tidywikidatar::tw_get_language(),
                            limit = 10,
                            include_search = FALSE,
                            wait = 0,
                            cache = NULL,
                            overwrite_cache = FALSE,
                            cache_connection = NULL,
                            disconnect_db = TRUE) {
  if (is.data.frame(search) == TRUE) {
    return(search)
  } else if (length(search) > 1) {
    cli::cli_abort(c(
      "`search` must be a single query or a data frame generate by `tw_search()`, not {.obj_type_friendly {search}}."
    ))
  } else if (is.character(search) == TRUE) {
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
