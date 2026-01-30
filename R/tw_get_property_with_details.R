#' Gets all details of a property
#'
#' Used internally. Users should rely on [tw_get_property_with_details()].
#'
#' @inheritParams tw_get_property_with_details
#'
#' @return A tibble, corresponding to the details for the given property. `NULL`
#'   if no relevant property found.
#'
#' @examples
#' # Get "female form of label", including language
#' tidywikidatar:::tw_get_property_with_details_single(id = "Q64733534", p = "P2521")
tw_get_property_with_details_single <- function(id, p) {
  item <- tryCatch(WikidataR::get_item(id = id), error = function(e) {
    as.character(e[[1]])
  })

  if (is.character(item)) {
    cli::cli_alert_danger(item)
    return(NULL)
  }

  if (
    is.element(
      el = "redirect",
      set = item %>%
        purrr::pluck(1) %>%
        names()
    )
  ) {
    id <- item %>%
      purrr::pluck(1, "redirect")
    item <- tryCatch(WikidataR::get_item(id = id), error = function(e) {
      as.character(e[[1]])
    })
  }

  claims <- item %>% purrr::pluck(1, "claims")

  current_claim_l <- claims[names(claims) == p]

  value_pre <- current_claim_l[[unique(p)]][["mainsnak"]][["datavalue"]][[
    "value"
  ]]

  value_pre %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = id, p = p, .before = 0)
}


#' Gets all details of a property for one or more Wikidata items.
#'
#' @inheritParams tw_get
#' @inheritParams tw_get_property
#'
#' @return A tibble, corresponding to the details for the given property. `NULL`
#'   if no relevant property found.
#' @export
#'
#' @examples
#' # Get "female form of label", including language
#' tw_get_property_with_details(id = "Q64733534", p = "P2521")
tw_get_property_with_details <- function(id, p, wait = 0) {
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
