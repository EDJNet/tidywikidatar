#' Get all Wikidata Q identifiers of all Wikipedia pages that appear in a given page
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 5. Number of times it re-attempts to reach the API before failing.
#'
#' @return A data frame (a tibble) with four columns: `wikipedia_title`, `wikipedia_id`, `wikidata_id`, `wikidata_description`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_links_from_wikipedia_page(title = "Margaret Mead", language = "en")
#' }
tw_get_links_from_wikipedia_page <- function(url = NULL,
                                             title = NULL,
                                             language = tidywikidatar::tw_get_language(),
                                             cache = NULL,
                                             overwrite_cache = FALSE,
                                             cache_connection = NULL,
                                             disconnect_db = TRUE,
                                             wait = 1,
                                             attempts = 5) {


  json_url <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops&generator=links&gpllimit=500"
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
  } else {
    base_json <- api_result
  }

  continue_check <- base_json %>%
    purrr::pluck("continue",
                 "gplcontinue")

  all_jsons <- list()

  page_number <- 1

  all_jsons[[page_number]] <- base_json

  while (is.null(continue_check) == FALSE & page_number < 200) {
    page_number <- page_number + 1

    json_url <- stringr::str_c(
      api_url,
      "&gplcontinue=",
      continue_check
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
    } else {
      base_json <- api_result
    }

    all_jsons[[page_number]] <- base_json

    continue_check <- base_json %>%
      purrr::pluck("continue",
                   "gplcontinue")
  }

  all_pages <- purrr::map(.x = all_jsons,
                          .f = purrr::pluck,
                          "query",
                          "pages") %>%
    purrr::flatten()

  purrr::map_dfr(
    .x = all_pages,
    .f = function(current_page) {
      tibble::tibble(
        wikidata_id = current_page %>%
          purrr::pluck(
            "pageprops",
            "wikibase_item"
          ),
        wikidata_description = current_page %>%
          purrr::pluck(
            "pageprops",
            "wikibase-shortdesc"
          ),
        wikipedia_id = current_page %>%
          purrr::pluck(
            "pageid"
          ),
        wikipedia_title = current_page %>%
          purrr::pluck(
            "title"
          )
      )
    }
  ) %>%
    dplyr::select(
      .data$wikipedia_title,
      .data$wikipedia_id,
      .data$wikidata_id,
      .data$wikidata_description
    ) %>%
    dplyr::mutate(language = language)
}
