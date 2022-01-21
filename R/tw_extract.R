#' Extract qualifiers from an object of class Wikidata created with `WikidataR`
#'
#' This function is mostly used internally and for testing.
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param w An object of class Wikidata created with `WikidataR`, typically created with `WikidataR::get_item(id = id)`
#'
#' @return A data frame (a tibble) with eight columns: `id` for the input id, `property`,  `qualifier_id`, `qualifier_property`, `qualifier_value`, `rank`, `qualifier_value_type`, and `set` (to distinguish sets of data when a property is present more than once)
#' @export
#'
#' @examples
#' w <- WikidataR::get_item(id = "Q180099")
#' tw_extract_qualifier(id = "Q180099", p = "P26", w = w)
tw_extract_qualifier <- function(id,
                                 p,
                                 w = NULL) {
  if (is.null(w)) {
    w <- tryCatch(WikidataR::get_item(id = id),
      error = function(e) {
        as.character(e[[1]])
      }
    )

    if (is.character(w)) {
      usethis::ui_oops(w)
      return(tidywikidatar::tw_empty_qualifiers)
    }
  }

  claims <- w %>%
    purrr::pluck(
      1,
      "claims"
    )

  qualifiers <- claims[[p]] %>%
    tibble::as_tibble()

  if (is.element("qualifiers", colnames(qualifiers)) == FALSE) {
    return(tidywikidatar::tw_empty_qualifiers)
  }

  qualifiers_df <- purrr::map_dfr(
    .x = 1:nrow(qualifiers),
    function(i) {
      qualifier_parent_pre <- qualifiers %>%
        dplyr::slice(i) %>%
        dplyr::pull(.data$mainsnak) %>%
        dplyr::pull(.data$datavalue) %>%
        dplyr::pull(.data$value)

      if (is.character(qualifier_parent_pre)) {
        qualifier_parent <- qualifier_parent_pre[[1]]
      } else if (is.element("id", colnames(qualifier_parent_pre))) {
        qualifier_parent <- qualifier_parent_pre %>%
          dplyr::pull(.data$id)
      } else {
        qualifier_parent <- qualifier_parent_pre %>%
          dplyr::pull(1)
      }

      qualifiers_set <- qualifiers %>%
        dplyr::slice(i) %>%
        dplyr::pull(.data$qualifiers)


      purrr::map_dfr(
        .x = qualifiers_set,
        .f = function(x) {
          current_qualifier <- x %>%
            purrr::pluck(1) %>%
            tibble::as_tibble()

          value_df <- current_qualifier[["datavalue"]][["value"]] %>%
            tibble::as_tibble()
          if (is.element("id", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull(.data$id)
          } else if (is.element("value", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull(.data$value)
          } else if (is.element("time", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull(.data$time)
          } else {
            return(tidywikidatar::tw_empty_qualifiers %>%
              dplyr::select(
                -.data$id,
                -.data$property
              ))
          }


          tibble::tibble(
            qualifier_id = qualifier_parent,
            qualifier_property = current_qualifier[["property"]],
            qualifier_value = value,
            qualifier_value_type = current_qualifier[["datavalue"]][["type"]],
            rank = qualifiers[["rank"]][[i]],
            set = i
          )
        }
      )
    }
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      id = id,
      property = p
    ) %>%
    dplyr::select(
      .data$id,
      .data$property,
      dplyr::everything()
    )
  qualifiers_df
}
