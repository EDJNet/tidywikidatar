#' Facilitates the creation of MediaWiki API base URLs
#'
#' Mostly used internally
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. If url given, this can be left empty.
#'
#' @return A charachter vector of base urls to be used with the MediaWiki API
#' @export
#'
#' @examples
#'
#' tw_get_wikipedia_base_api_url(title = "Margaret Mead", language = "en")
tw_get_wikipedia_base_api_url <- function(url = NULL,
                                          title = NULL,
                                          language = NULL) {
  if (is.null(url) == TRUE) {
    if (is.null(title) == TRUE) {
      usethis::ui_stop("Either url or title must be provided")
    }
    if (is.null(language) == TRUE) {
      usethis::ui_stop("Either language or full url must be provided")
    }
  } else {
    title <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://[[a-z]][[a-z]].wikipedia.org/wiki/).*"
    )
  }

  if (is.null(language) == TRUE) {
    language <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
    )
  }

  api_url <- stringr::str_c(
    "https://",
    language,
    ".wikipedia.org/w/api.php?action=query&redirects=true&format=json&titles=",
    utils::URLencode(URL = title)
  )

  api_url
}

#' Gets the Wikidata id of a Wikipedia page
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. If url given, this can be left empty.
#'
#' @return A character vector of Wikidata identifiers.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_id_of_wikipedia_page(title = "Margaret Mead", language = "en")
#' }
tw_get_id_of_wikipedia_page <- function(url = NULL,
                                        title = NULL,
                                        language = NULL) {
  wikidata_id <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops"
  ) %>%
    jsonlite::read_json() %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageprops",
      "wikibase_item"
    )

  if (length(wikidata_id) == 0) {
    as.character(NA)
  } else {
    wikidata_id
  }
}


#' Get all Wikidata id of all Wikipedia pages included in a given page
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. If url given, this can be left empty.
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
                                             language = NULL) {
  api_url <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops&generator=links&gpllimit=500"
  )


  base_json <- jsonlite::read_json(api_url)

  continue_check <- base_json %>%
    purrr::pluck("continue", "gplcontinue")

  all_jsons <- list()
  page_number <- 1

  all_jsons[[page_number]] <- base_json

  while (is.null(continue_check) == FALSE & page_number < 100) {
    page_number <- page_number + 1
    base_json <- jsonlite::read_json(stringr::str_c(
      api_url,
      "&gplcontinue=",
      continue_check
    ))

    all_jsons[[page_number]] <- base_json

    continue_check <- base_json %>%
      purrr::pluck("continue", "gplcontinue")
  }

  purrr::map(.x = all_jsons, .f = purrr::pluck, "query", "pages")

  all_pages <- purrr::map(.x = all_jsons, .f = purrr::pluck, "query", "pages") %>%
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
    )
}
