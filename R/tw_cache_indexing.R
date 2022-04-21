#' Check if cache table is indexed
#'
#' Tested only with SQLite and MySql. May work with other drivers. Used to check if given cache table is indexed (if created with any version of `tidywikidatar` before 0.6, they are probably not indexed and less efficient).
#'
#' @param show_details Logical, defaults to FALSE. If FALSE, return a logical vector of length one (TRUE if the table was indexed, FALSE if it was not). If TRUE, returns a data frame with more details about the index.
#'
#' @inheritParams tw_get_cache_table_name
#' @inheritParams tw_get
#'
#' @return If FALSE, return a logical vector of length one (TRUE if the table was indexed, FALSE if it was not). If TRUE, returns a data frame with more details about the index.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_enable_cache()
#'   tw_set_cache_folder(path = fs::path(
#'     fs::path_home_r(),
#'     "R",
#'     "tw_data"
#'   ))
#'
#'   tw_set_language(language = "en")
#'
#'   tw_check_cache_index()
#' }
tw_check_cache_index <- function(type = "item",
                                 show_details = FALSE,
                                 language = tidywikidatar::tw_get_language(),
                                 response_language = tidywikidatar::tw_get_language(),
                                 cache = NULL,
                                 cache_connection = NULL,
                                 disconnect_db = TRUE) {
  table_name <- tw_get_cache_table_name(
    type = type,
    language = language,
    response_language = response_language
  )

  if (stringr::str_starts(string = type, pattern = "search") & stringr::str_detect(string = language, pattern = "_", negate = TRUE)) {
    language <- stringr::str_c(language, "_", response_language)
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    usethis::ui_warn("Table `{table_name}` does not exist.")
    invisible(return(NULL))
  }

  driver <- db$fetch()

  if (class(driver) == "SQLiteConnection") {
    db_no_pool <- pool::poolCheckout(db)

    statement <- glue::glue_sql("PRAGMA INDEX_LIST({`table_name`});",
      table_name = table_name,
      .con = db_no_pool
    )

    index_odbc_result <- DBI::dbSendQuery(
      conn = db_no_pool,
      statement = statement
    )

    index_odbc_result_fetched <- DBI::dbFetch(index_odbc_result)

    pool::poolReturn(db_no_pool)

    if (show_details == TRUE) {
      output <- tibble::as_tibble(index_odbc_result_fetched)
    } else {
      output <- nrow(tibble::as_tibble(index_odbc_result_fetched)) > 0
    }
  } else {
    if (show_details == TRUE) {
      db_no_pool <- pool::poolCheckout(db)

      statement <- glue::glue_sql("SHOW INDEX FROM {`table_name`}",
        table_name = table_name,
        .con = db_no_pool
      )

      index_odbc_result <- DBI::dbSendQuery(
        conn = db_no_pool,
        statement = statement
      )

      index_odbc_result_fetched <- DBI::dbFetch(index_odbc_result)
      pool::poolReturn(db_no_pool)

      output <- tibble::as_tibble(index_odbc_result_fetched)
    } else {
      statement <- glue::glue_sql("SHOW INDEX FROM {`table_name`}",
        table_name = table_name,
        .con = db
      )
      result <- pool::dbExecute(
        conn = db,
        statement = statement
      )

      output <- result > 0
    }
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db
  )
  output
}
