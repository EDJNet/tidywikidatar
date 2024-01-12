#' Gets all details of a property
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#'
#' @return A tibble, corresponding to the details for the given property. NULL if no relevant property found.
#'
#' @examples
#' # Get "female form of label", including language
#' tidywikidatar:::tw_get_property_with_details_single(id = "Q64733534", p = "P2521")
tw_get_property_with_details_single <- function(id,
                                                p) {
  item <- tryCatch(WikidataR::get_item(id = id),
    error = function(e) {
      as.character(e[[1]])
    }
  )

  if (is.character(item)) {
    cli::cli_alert_danger(item)
    return(NULL)
  }

  if (is.element(
    el = "redirect",
    set = item %>%
      purrr::pluck(1) %>%
      names()
  )) {
    id <- item %>%
      purrr::pluck(1, "redirect")
    item <- tryCatch(WikidataR::get_item(id = id),
      error = function(e) {
        as.character(e[[1]])
      }
    )
  }

  claims <- item %>% purrr::pluck(1, "claims")

  current_claim_l <- claims[names(claims) == p]

  value_pre <- current_claim_l[[unique(p)]][["mainsnak"]][["datavalue"]][["value"]]

  value_pre %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = id, p = p, .before = 0)
}


#' Gets all details of a property
#'
#' @param id A character vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A tibble, corresponding to the details for the given property. `NULL` if no relevant property found.
#' @export
#'
#' @examples
#' # Get "female form of label", including language
#' tw_get_property_with_details(id = "Q64733534", p = "P2521")
tw_get_property_with_details <- function(id,
                                         p,
                                         wait = 0) {
  pb <- progress::progress_bar$new(total = length(unique(id)))
  purrr::map2_dfr(
    .x = unique(id),
    .y = p,
    .f = function(id, p) {
      pb$tick()
      Sys.sleep(wait)
      tw_get_property_with_details_single(
        id = id,
        p = p
      )
    }
  )
}
