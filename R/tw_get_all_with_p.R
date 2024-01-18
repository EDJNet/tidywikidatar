#' Get all items that have a given property (irrespective of the value)
#'
#' This function does not cache results.
#'
#' @param p A character vector, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param wait Defaults to 0.1. Used only in method is set to "JSON".
#' @param method Defaults to "SPARQL". The only accepted alternative value is "JSON", to use instead json-based API.
#' @param limit Defaults to `Inf`. Set to smaller values for testing and cache locally when possible to reduce load on servers.
#' @inheritParams tw_query
#'
#' @return A data frame with three columns is method is set to "SPARQL", or as many columns as fields if more are given and `return_as_tw_search` is set to FALSE. A single column with Wikidata identifier if method is set to "JSON".
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # get all Wikidata items with an ICAO airport code ("P239")
#'   tw_get_all_with_p(p = "P239", limit = 10)
#' }
tw_get_all_with_p <- function(p,
                              fields = c("item", "itemLabel", "itemDescription"),
                              language = tidywikidatar::tw_get_language(),
                              method = "SPARQL",
                              wait = 0.1,
                              limit = Inf,
                              return_as_tw_search = TRUE) {
  p <- stringr::str_to_upper(string = p)

  if (stringr::str_starts(string = p, pattern = "P", negate = TRUE)) {
    cli::cli_warn("Invalid property. Property should always start with 'P'.")
    if (method == "SPARQL") {
      return(tidywikidatar::tw_empty_search)
    } else if (method == "API") {
      return(tibble::tibble(id = as.character(NA)) %>%
        dplyr::slice(0))
    }
  }

  if (method == "SPARQL") {
    if (is.infinite(limit)) {
      sparql_t <- glue::glue(
        "SELECT
             {stringr::str_c(\"?\", stringr::str_c(fields, collapse = \" ?\"))}
             WHERE{{?item p:{p} ?statement0.
  ?statement0 (ps:{p}) _:anyValue{p}.
             SERVICE wikibase:label {{ bd:serviceParam wikibase:language '{stringr::str_c(language, collapse = ',')},[AUTO_LANGUAGE]' . }}
             }}"
      )
    } else {
      sparql_t <- glue::glue(
        "SELECT
             {stringr::str_c(\"?\", stringr::str_c(fields, collapse = \" ?\"))}
               WHERE{{?item p:{p} ?statement0.
               ?statement0 (ps:{p}) _:anyValue{p}.
               SERVICE wikibase:label {{ bd:serviceParam wikibase:language '{stringr::str_c(language, collapse = ',')},[AUTO_LANGUAGE]' . }}
             }}
      LIMIT {limit}"
      )
    }


    response <- WikidataQueryServiceR::query_wikidata(
      sparql_query = sparql_t,
      format = "simple"
    )

    if (length(fields) == 3 & return_as_tw_search == TRUE) {
      all_items_df <- response %>%
        dplyr::transmute(
          id = stringr::str_extract(.data$item,
            pattern = "Q[[:digit:]]+$"
          ),
          label = .data$itemLabel,
          description = .data$itemDescription
        )
    } else {
      all_items_df <- tibble::as_tibble(response)
    }
  } else if (method == "JSON") {
    api_url <- stringr::str_c(
      "https://www.wikidata.org/w/api.php?action=query&list=backlinks&blnamespace=0&bllimit=500&bltitle=Property:",
      p,
      "&format=json"
    )

    base_json <- jsonlite::read_json(api_url)

    continue_check <- base_json %>%
      purrr::pluck("continue", "blcontinue")

    all_jsons <- list()
    page_number <- 1

    all_jsons[[page_number]] <- base_json

    while (is.null(continue_check) == FALSE & page_number < max(2, (limit / 500))) {
      Sys.sleep(wait)
      cli::cli_alert_info("Page {page_number} extracted")
      page_number <- page_number + 1

      base_json <- jsonlite::read_json(stringr::str_c(
        api_url,
        "&blcontinue=",
        continue_check
      ))

      all_jsons[[page_number]] <- base_json

      continue_check <- base_json %>%
        purrr::pluck("continue", "blcontinue")
    }

    all_pages <- purrr::map(
      .x = all_jsons,
      .f = purrr::pluck, "query", "backlinks"
    ) %>%
      purrr::flatten()

    all_items_df <- purrr::map_dfr(
      .x = all_pages,
      .f = function(current_page) {
        tibble::tibble(
          id = current_page %>%
            purrr::pluck(
              "title"
            )
        )
      }
    )
  }
  all_items_df
}
