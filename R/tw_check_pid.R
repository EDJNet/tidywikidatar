#' Ensures that input appears to be a valid Wikidata property id (i.e. it starts with P and is followed only by digits)
#'
#' Mostly used internally by other functions.
#'
#' @param property A character vector of one or more Wikidata property identifiers.
#' @param logical_vector Logical, defaults to FALSE. If TRUE, returns a logical vector of the same length as input, where TRUE corresponds to seemingly meaningful property identifiers.
#' @param non_pid_as_NA Logical, defaults to FALSE. If TRUE (and if `logical_vector` is set to FALSE), a vector of the same length is returned, with NA replacing items that are seemingly not meaningful property identifiers.
#'
#' @return A character vector with only strings appearing to be Wikidata identifiers; possibly shorter than input
#' @export
#'
#' @examples
#'
#' tw_check_pid(property = c("P19", "p20", "Not an property id", "20", NA, "Q5", ""))
#'
#' tw_check_pid(
#'   property = c("P19", "p20", "Not an property id", "20", NA, "Q5", ""),
#'   logical_vector = TRUE
#' )
#'
#' tw_check_pid(
#'   property = c("P19", "p20", "Not an property id", "20", NA, "Q5", ""),
#'   non_pid_as_NA = TRUE
#' )
tw_check_pid <- function(property,
                         logical_vector = FALSE,
                         non_pid_as_NA = FALSE) {
  if (is.null(property)) {
    return(character(0L))
  }

  output_v <- tibble::tibble(property = property) %>%
    dplyr::filter(is.na(property) == FALSE) %>%
    dplyr::distinct(property) %>%
    dplyr::mutate(property = stringr::str_to_upper(.data$property)) %>%
    dplyr::filter(stringr::str_starts(
      string = .data$property,
      pattern = "P[[:digit:]]+$"
    )) %>%
    dplyr::pull("property")

  if (logical_vector == TRUE | non_pid_as_NA == TRUE) {
    output_l <- stringr::str_to_upper(property) %in% output_v
    if (non_pid_as_NA == TRUE) {
      return(dplyr::if_else(condition = output_l,
        true = stringr::str_to_upper(property),
        false = NA_character_,
        missing = NA_character_
      ))
    } else {
      return(output_l)
    }
  } else {
    output_v
  }
}
