#' Ensures that input appears to be a valid Wikidata id
#'
#' Mostly used internally by other functions.
#'
#' @param id A character vector of one or more Wikidata id.
#'
#' @return A character vector with only strings appearing to be Wikidata identifiers; possibly shorter than input
#' @export
#'
#' @examples
#'
#' tw_check_qid(id = c("Q180099","q228822","Not an id","00180099", NA))
tw_check_qid <- function(id) {
  tibble::tibble(id = id) %>%
    dplyr::filter(is.na(id)==FALSE) %>%
    dplyr::distinct(id) %>%
    dplyr::mutate(id = stringr::str_to_upper(.data$id)) %>%
    dplyr::filter(stringr::str_starts(string = .data$id,
                                      pattern = "Q[[:digit:]]")) %>%
    dplyr::pull(.data$id)
}
