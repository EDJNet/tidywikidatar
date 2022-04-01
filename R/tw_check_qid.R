#' Ensures that input appears to be a valid Wikidata id
#'
#' Mostly used internally by other functions.
#'
#' @param id A character vector of one or more Wikidata id.
#' @param logical_vector Logical, defaults to FALSE. If TRUE, returns a logical vector of the same length as input, where TRUE corresponds to seemingly meaningful Q identifiers.
#' @param non_id_as_NA Logical, defaults to FALSE. If TRUE (and if `logical_vector` is set to FALSE), a vector of the same length is returned, with NA replacing items that are seemingly not meaningful Q identifiers.
#'
#' @return A character vector with only strings appearing to be Wikidata identifiers; possibly shorter than input
#' @export
#'
#' @examples
#'
#' tw_check_qid(id = c("Q180099", "q228822", "Not an id", "00180099", NA, "Q5"))
#'
#' tw_check_qid(
#'   id = c("Q180099", "q228822", "Not an id", "00180099", NA, "Q5"),
#'   logical_vector = TRUE
#' )
#'
#' tw_check_qid(
#'   id = c("Q180099", "q228822", "Not an id", "00180099", NA, "Q5"),
#'   non_id_as_NA = TRUE
#' )
tw_check_qid <- function(id,
                         logical_vector = FALSE,
                         non_id_as_NA = FALSE) {
  if (is.null(id)) {
    return(character(0L))
  }

  output_v <- tibble::tibble(id = id) %>%
    dplyr::filter(is.na(id) == FALSE) %>%
    dplyr::distinct(id) %>%
    dplyr::mutate(id = stringr::str_to_upper(.data$id)) %>%
    dplyr::filter(stringr::str_starts(
      string = .data$id,
      pattern = "Q[[:digit:]]+$"
    )) %>%
    dplyr::pull(.data$id)

  if (logical_vector == TRUE | non_id_as_NA == TRUE) {
    output_l <- stringr::str_to_upper(id) %in% output_v
    if (non_id_as_NA == TRUE) {
      return(dplyr::if_else(condition = output_l,
        true = stringr::str_to_upper(id),
        false = as.character(NA),
        missing = as.character(NA)
      ))
    } else {
      return(output_l)
    }
  } else {
    output_v
  }
}
