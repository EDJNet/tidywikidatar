#' The Wikidata Q identifier of European airports found in Eurostat's `avia_par_` dataset
#'
#'
#' @format A data frame with 429 rows and 1 column:
#' \describe{
#'   \item{id}{Q identifiers}
#' }
#' @source \url{https://www.wikidata.org/wiki/Wikidata:Main_Page}
"tw_qid_airports"


#' The Wikidata Q identifier of all members of the European Parliament since its establishment
#'
#' A dataset with all the Wikidata items that have "Q27169" (member of the European Parliament) for the property "P39" (position held).
#'
#' @format A data frame with 4581 rows and 1 column:
#' \describe{
#'   \item{id}{Q identifiers}
#' }
#' @source \url{https://www.wikidata.org/wiki/Wikidata:Main_Page}
"tw_qid_meps"

#' A zero-rows tibble used internally when `tw_get()` would not return any value.
#'
#' @format A data frame with 0 rows and 3 columns
"tw_empty_item"

#' A zero-rows tibble used internally when `tw_get_qualifiers()` would not return any value.
#'
#' @format A data frame with 0 rows and 8 columns
"tw_empty_qualifiers"

#' A zero-rows tibble used internally when `tw_search()` would not return any value.
#'
#' @format A data frame with 0 rows and 4 columns
"tw_empty_search"

#' A zero-rows tibble used internally when `tw_get_wikipedia_page_qid()` would not return any value.
#'
#' @format A data frame with 0 rows and 6 columns
"tw_empty_wikipedia_page"


#' A zero-rows tibble used internally when `tw_get_wikipedia_page_links()` would not return any value.
#'
#' @format A data frame with 0 rows and 8 columns
"tw_empty_wikipedia_page_links"


#' A zero-rows tibble used internally when `tw_get_image_metadata()` would not return any value.
#'
#' @format A data frame with 0 rows and 19 columns
"tw_empty_image_metadata"
