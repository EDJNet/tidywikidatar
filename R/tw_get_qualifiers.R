#' Get Wikidata qualifiers for a given property of a given item
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to "all_available". It should be relevant only for caching purposes. For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A data frame (a tibble) with three columns: `property`, `value`, and `set` (to distinguish sets of data when a property is present more than once)
#' @export
#'
#' @examples
#' \dontrun{
#' tw_get_qualifiers(id = "Q2391857", p = "P39")
#' }
#'
tw_get_qualifiers <- function(id,
                              p,
                              language = "all_available",
                              cache = NULL,
                              overwrite_cache = FALSE) {
  if (tw_check_cache(cache) == TRUE) {
    tidywikidatar::tw_create_cache_folder()
    db_folder <- fs::path(
      tidywikidatar::tw_get_cache_folder(),
      "wiki_qualifiers_db"
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c("wiki_qualifiers_db_", language, ".sqlite")
    )
    db <- DBI::dbConnect(drv = RSQLite::SQLite(), db_file)
    db_table_name <- stringr::str_c(
      stringr::str_to_upper(id),
      "_",
      stringr::str_to_upper(p)
    )
    db_result <- tryCatch(
      DBI::dbReadTable(
        conn = db,
        name = db_table_name
      ),
      error = function(e) {
        logical(1L)
      }
    )
    if (is.data.frame(db_result) & overwrite_cache == FALSE) {
      DBI::dbDisconnect(db)
      return(db_result %>% tibble::as_tibble())
    }
  }


  claims <- tryCatch(WikidataR::get_item(id = id),
    error = function(e) {
      return(tibble::tibble(id = NA))
    }
  ) %>% purrr::pluck(
    1,
    "claims"
  )

  qualifiers <- claims[[p]] %>%
    tibble::as_tibble()

  qualifiers_df <- purrr::map_dfr(
    .x = 1:nrow(qualifiers),
    function(i) {
      qualifier_parent <- qualifiers %>%
        dplyr::slice(i) %>%
        dplyr::pull(.data$mainsnak) %>%
        dplyr::pull(.data$datavalue) %>%
        dplyr::pull(.data$value) %>%
        dplyr::pull(.data$id)

      qualifiers_set <- qualifiers %>%
        dplyr::slice(i) %>%
        dplyr::pull(.data$qualifiers)


      purrr::map_dfr(
        .x = qualifiers_set,
        .f = function(x) {
          current_qualifier <- x %>%
            purrr::pluck(1) %>%
            tibble::as_tibble()

          p <- current_qualifier[["property"]]
          value_df <- current_qualifier[["datavalue"]][["value"]] %>%
            tibble::as_tibble()
          if (is.element("id", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull(.data$id)
          } else if (is.element("time", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull(.data$time)
          } else {
            return(NULL)
          }
          tibble::tibble(
            id = qualifier_parent,
            property = p,
            value = value,
            set = i
          )
        }
      )
    }
  )

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = db_table_name,
      value = qualifiers_df
    )
    DBI::dbDisconnect(db)
  } else if (tw_check_cache(cache) == TRUE & overwrite_cache == TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = db_table_name,
      value = qualifiers_df,
      overwrite = TRUE
    )
    DBI::dbDisconnect(db)
  }
  qualifiers_df %>%
    tibble::as_tibble()
}
