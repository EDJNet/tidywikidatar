#' Gets a field such a label or description from a dataframe typically generated with `tw_get()`
#'
#' @param df A data frame typically generated with `tw_get()`. It should include data for the `id` included in the dedicated parameter.
#' @param field A character vector of length one. Typically, either "label" or "description".
#' @param id A character vector, typically of Wikidata identifiers. The output will be of the same length and in the same order as the identifiers provided with this parameter.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector of the same length, and with data in the same order, as `id`.
#' @export
#'
#' @examples
#'
#' tw_get("Q180099") %>%
#'   tw_get_field(field = "label", id = "Q180099")
tw_get_field <- function(df,
                         field,
                         id,
                         language = tidywikidatar::tw_get_language()) {
  if (sum(is.na(id)) == length(id)) {
    return(rep(as.character(NA), length(id)))
  }

  if (length(tw_check_qid(id = id)) == 0) {
    return(rep(as.character(NA), length(id)))
  }

  field_df <- df %>%
    dplyr::filter(
      stringr::str_starts(
        string = .data$property,
        pattern = stringr::str_c(field, "_")
      ),
      stringr::str_ends(
        string = .data$property,
        pattern = stringr::str_c(language,
          collapse = "|"
        )
      )
    ) %>%
    dplyr::distinct(.data$id,
      .keep_all = TRUE
    )

  if (nrow(field_df) == 0) {
    rep(as.character(NA), length(id))
  } else if (nrow(field_df) < length(id)) {
    tibble::tibble(id = id) %>%
      dplyr::left_join(y = field_df, by = "id") %>%
      dplyr::pull("value")
  } else {
    field_df %>%
      dplyr::pull("value")
  }
}
