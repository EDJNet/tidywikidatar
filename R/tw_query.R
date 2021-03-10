#' Perform simple Wikidata queries
#'
#' This function aims to facilitate only the most basic type of queries: return which items have the following property pairs. For more details on Wikidata queries, consult: https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries/examples. For complex queries, use `WikidataQueryServiceR::query_wikidata()`
#'
#' @param query A list of named vectors, or a data frame (see example and readme).
#' @param fields A character vector of Wikidata fields. Ignored if `return_as_tw_search` is set to TRUE (as per default). Defaults to `("item", "itemLabel", "itemDescription")`
#' @param language Two letter code of preferred language, defaults to "en".
#' @param return_as_tw_search Logical, defaults to TRUE. If TRUE, returns a data frame with three columns (id, label, and description) that can be piped to other `tw_` functions. If FALSE, a data frame with as many columns as fields.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun {
#' query <- list(c(p = "P106", q = "Q1397808"),
#'               c(p = "P21", q = "Q6581072"))
#' tw_query(query, return_as_tw_search = FALSE) %>%
#'   dplyr::transmute(id = stringr::str_extract(item, pattern = "Q[[:digit:]]+$"),
#'                    label = itemLabel,
#'                    description = itemDescription)
#' }
#'
tw_query <- function(query,
                     fields = c("item", "itemLabel", "itemDescription"),
                     language = "en",
                     return_as_tw_search = TRUE) {
  if (is.data.frame(query) == FALSE) {
    query_df <- dplyr::bind_rows(query)
  } else {
    query <- query_df
  }

  query_t <- query_df %>%
    glue::glue_data("wdt:{p} wd:{q}") %>%
    stringr::str_c(collapse = ";\n")


  sparql_t <- glue::glue(
    "SELECT
             「stringr::str_c(\"?\", stringr::str_c(fields, collapse = \" ?\"))」
             WHERE{?item 「query_t」 .
             SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE],「language」' . }
             }",
    .open = "「",
    .close = "」")

  response <- WikidataQueryServiceR::query_wikidata(
    sparql_query = sparql_t,
    format = "simple")

  if (return_as_tw_search==TRUE) {
    response %>%
      dplyr::transmute(
        id = stringr::str_extract(item,
                                                 pattern = "Q[[:digit:]]+$"),
                       label = itemLabel,
                       description = itemDescription
        )
  } else {
    tibble::as_tibble(response)
  }


}
