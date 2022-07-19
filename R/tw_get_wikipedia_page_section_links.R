#' Get links from a specific section of a Wikipedia page
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param section_title Defaults to NULL. If given, it should correspond to the human-readable title of a section of the relevant Wikipedia page. See also `tw_get_wikipedia_page_sections()`
#' @param section_index Defaults to NULL. If given, it should correspond to the ordinal of a section of the relevant Wikipedia page. See also `tw_get_wikipedia_page_sections()`
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 10. Number of times it re-attempts to reach the API before failing.
#' @param wikipedia_page_qid_df Defaults to NULL. If given, used to reduce calls to cache. A data frame
#'
#' @return A data frame (a tibble).
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_section_links(title = "Margaret Mead", language = "en", section_index = 1)
#' }
tw_get_wikipedia_page_section_links <- function(url = NULL,
                                                title = NULL,
                                                section_title = NULL,
                                                section_index = NULL,
                                                language = tidywikidatar::tw_get_language(),
                                                cache = NULL,
                                                overwrite_cache = FALSE,
                                                cache_connection = NULL,
                                                disconnect_db = TRUE,
                                                wait = 1,
                                                attempts = 10,
                                                wikipedia_page_qid_df = NULL) {
  if (is.null(section_index) & is.null(section_title)) {
    usethis::ui_stop("Either {usethis::ui_code('section_index')} or {usethis::ui_code('section_title')} must be given. See also {usethis::ui_code('tw_get_wikipedia_page_sections()')}")
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  sections_df <- tw_get_wikipedia_page_sections(
    url = url,
    title = title,
    language = language,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = db,
    disconnect_db = FALSE,
    wait = wait,
    attempts = attempts
  )

  if (is.null(section_index)) {
    section_index <- sections_df %>%
      dplyr::filter(.data$line == section_title) %>%
      dplyr::pull(.data$index) %>%
      utils::head(1)

    if (length(section_index) == 0) {
      usethis::ui_stop("Section title does not exist. Consider running `tw_get_wikipedia_sections()` with `overwrite_cache` set to TRUE if you believe this may be due to oudated cache.")
    }
  }

  if (is.null(section_title)) {
    section_title <- sections_df %>%
      dplyr::filter(.data$index == as.character(section_index)) %>%
      dplyr::pull(.data$fromtitle) %>%
      utils::head(1)

    if (length(section_index) == 0) {
      usethis::ui_stop("Section index does not exist. Consider running `tw_get_wikipedia_sections()` with `overwrite_cache` set to TRUE if you believe this may be due to oudated cache.")
    }
  }

  wikipedia_page_qid_df <- tw_get_wikipedia_page_qid(
    title = title,
    language = language,
    url = url,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = db,
    disconnect_db = FALSE,
    wait = wait,
    attempts = attempts
  )

  json_url <- tw_get_wikipedia_section_links_api_url(
    url = url,
    title = title,
    language = language,
    section_index = section_index
  )

  api_result <- FALSE

  attempt_n <- 1

  while (isFALSE(api_result) & attempt_n <= attempts) {
    attempt_n <- sum(attempt_n, 1)
    api_result <- tryCatch(
      jsonlite::read_json(path = json_url),
      error = function(e) {
        logical(1L)
      }
    )
    Sys.sleep(time = wait)
  }

  if (isFALSE(api_result)) {
    usethis::ui_stop("It has not been possible to reach the API with {attempts} attempts. Consider increasing the waiting time between calls with the {usethis::ui_code('wait')} parameter or check your internet connection.")
  } else if ("error" %in% names(api_result)) {
    usethis::ui_stop("{api_result[['error']][['code']]}: {api_result[['error']][['info']]} - {json_url}")
    api_result[["error"]]
  } else {
    base_json <- api_result
  }

  links_df <- purrr::map_dfr(
    .x = base_json %>%
      purrr::pluck("parse", "links"),
    .f = tibble::as_tibble_row
  )

  if (nrow(links_df) < 1) {
    return(tidywikidatar::tw_empty_wikipedia_page)
  }


  output_df <- tw_get_wikipedia_page_qid(
    title = links_df[["*"]],
    language = language,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = db,
    disconnect_db = FALSE,
    wait = wait,
    attempts = attempts
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  output_df
}





#' Facilitates the creation of MediaWiki API base URLs to retrieve sections of a page
#'
#' Mostly used internally
#'
#' @param url A character vector with the full URL to one or more Wikipedia pages. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param section_index Required. It should correspond to the ordinal of a section of the relevant Wikipedia page. See also `tw_get_wikipedia_page_sections()`.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#'
#' @return A character vector of base urls to be used with the MediaWiki API
#' @export
#'
#' @examples
#' tw_get_wikipedia_section_links_api_url(title = "Margaret Mead", section_index = 1, language = "en")
tw_get_wikipedia_section_links_api_url <- function(url = NULL,
                                                   title = NULL,
                                                   section_index,
                                                   language = tidywikidatar::tw_get_language()) {
  stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language,
      action = "parse"
    ),
    "&section=",
    section_index
  )
}
