#' Get Wikidata property of an item
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A tibble, corresponding to the value for the given property. A tibble of zero rows if no relevant property found.
#' @export
#'
#' @examples
#' # Who were the doctoral advisors - P184 - of Margaret Mead - Q180099?
#' advisors <- tw_get_property(id = "Q180099", p = "P184")
#' advisors
#'
#' # tw_get_label(advisors)
#'
#' # It is also possible to get one property for many id
#'
#' tw_get_property(
#'   id = c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   p = "P31"
#' )
#'
#' # Or many properties for a single id
#'
#' tw_get_property(
#'   id = "Q180099",
#'   p = c("P21", "P31")
#' )
tw_get_property <- function(id,
                            p,
                            language = tidywikidatar::tw_get_language(),
                            id_df = NULL,
                            cache = NULL,
                            overwrite_cache = FALSE,
                            cache_connection = NULL,
                            disconnect_db = TRUE,
                            wait = 0) {
  if (is.null(id_df)) {
    id_df <- tw_get(
      id = id,
      cache = tw_check_cache(cache),
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      language = language,
      wait = wait,
      disconnect_db = disconnect_db
    )
  }

  property_df <- id_df %>%
    dplyr::filter(.data$property %in% p)
  if (nrow(property_df) == 0) {
    tibble::tibble(
      id = as.character(NA),
      property = as.character(NA),
      value = as.character(NA)
    ) %>%
      dplyr::slice(0)
  } else {
    property_df
  }
}
