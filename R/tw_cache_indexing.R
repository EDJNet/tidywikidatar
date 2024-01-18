#' Check if cache table is indexed
#'
#' Tested only with SQLite and MySql. May work with other drivers. Used to check if given cache table is indexed (if created with any version of `tidywikidatar` before 0.6, they are probably not indexed and less efficient).
#'
#' @param table_name Name of the table in the database. If given, it takes precedence over other parameters.
#' @param show_details Logical, defaults to FALSE. If FALSE, return a logical vector of length one (TRUE if the table was indexed, FALSE if it was not). If TRUE, returns a data frame with more details about the index.
#'
#' @inheritParams tw_get_cache_table_name
#' @inheritParams tw_get
#'
#' @return If `show_details` is set to FALSE, return a logical vector of length one (TRUE if the table was indexed, FALSE if it was not). If `show_details` is set to TRUE, returns a data frame with more details about the index.
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
tw_check_cache_index <- function(table_name = NULL,
                                 type = "item",
                                 show_details = FALSE,
                                 language = tidywikidatar::tw_get_language(),
                                 response_language = tidywikidatar::tw_get_language(),
                                 cache = NULL,
                                 cache_connection = NULL,
                                 disconnect_db = TRUE) {
  if (is.null(table_name) == TRUE) {
    table_name <- tw_get_cache_table_name(
      type = type,
      language = language,
      response_language = response_language
    )
  }

  if (stringr::str_starts(string = type, pattern = "search") & stringr::str_detect(string = language, pattern = "_", negate = TRUE)) {
    language <- stringr::str_c(language, "_", response_language)
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (is.null(db)) {
    cli::cli_abort(c(
      "No valid connection found.",
      i = "Enable caching with `tw_enable_cache()` or through the relevant parameters."))
  }

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    cli::cli_warn("Table {.code {table_name}} does not exist.")
    return(invisible())
  }

  driver <- db$fetch()

  if (inherits(x = driver, "SQLiteConnection")) {
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



#' Add index to caching table for search queries for increased speed
#'
#' Tested only with SQLite and MySql. May work with other drivers.
#'
#' To ensure smooth functioning, the search column in the cache table is transformed into a column of type `varchar` and length 255.
#'
#' @param table_name Name of the table in the database. If given, it takes precedence over other parameters.
#' @param check_first Logical, defaults to TRUE. If TRUE, then before executing anything on the database it checks if the given table has already been indexed. If it has, it does nothing and returns only an informative message.
#' @param show_details Logical, defaults to FALSE. If FALSE, return the function adds the index to the database, but does not return anything. If TRUE, returns a data frame with more details about the index.
#'
#' @inheritParams tw_get_cache_table_name
#' @inheritParams tw_get
#'
#' @return If `show_details` is set to FALSE, nothing, used only for its side effects (add index to caching table). If TRUE, a data frame, same as the output of `tw_check_cache_index(show_details = TRUE)`.
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
#'   tw_index_cache_search()
#' }
tw_index_cache_search <- function(table_name = NULL,
                                  check_first = TRUE,
                                  type = "item",
                                  show_details = FALSE,
                                  language = tidywikidatar::tw_get_language(),
                                  response_language = tidywikidatar::tw_get_language(),
                                  cache = NULL,
                                  cache_connection = NULL,
                                  disconnect_db = TRUE) {
  if (is.null(table_name)) {
    table_name <- tw_get_cache_table_name(
      type = stringr::str_c("search_", type),
      language = language,
      response_language = response_language
    )
    language <- stringr::str_c(language, "_", response_language)
  } else {
    language <- stringr::str_extract(string = table_name, "[a-z][a-z]_[a-z][a-z]$")
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (!pool::dbExistsTable(conn = db, name = table_name)) {
    cli::cli_warn("Table `{table_name}` does not exist.")
    return(invisible())
  }

  if (check_first) {
    check <- tw_check_cache_index(
      table_name = table_name,
      type = type,
      show_details = FALSE,
      language = language,
      response_language = response_language,
      cache = cache,
      cache_connection = db,
      disconnect_db = FALSE
    )

    if (check) {
      cli::cli_inform(c(
        "The table {.code {table_name}} is already indexed. No action taken.",
        i = "Use `check = FALSE` to ignore this check."
        ))
      return(invisible(NULL))
    }
  }

  db_no_pool <- pool::poolCheckout(db)
  driver <- db$fetch()

  if (inherits(x = driver, "SQLiteConnection") == FALSE) {
    statement <- glue::glue_sql("ALTER TABLE {`table_name`} MODIFY search VARCHAR(255);",
      table_name = table_name,
      .con = db_no_pool
    )

    index_odbc_result <- DBI::dbSendQuery(
      conn = db_no_pool,
      statement = statement
    )


    statement <- glue::glue_sql("ALTER TABLE {`table_name`} MODIFY id VARCHAR(50);",
      table_name = table_name,
      .con = db_no_pool
    )

    index_odbc_result <- DBI::dbSendQuery(
      conn = db_no_pool,
      statement = statement
    )
  }

  statement <- glue::glue_sql("CREATE INDEX search on {`table_name`} (search);",
    table_name = table_name,
    .con = db_no_pool
  )

  index_odbc_result <- DBI::dbExecute(
    conn = db_no_pool,
    statement = statement
  )

  pool::poolReturn(db_no_pool)
  if (show_details == TRUE) {
    output <- tw_check_cache_index(
      table_name = table_name,
      type = type,
      show_details = TRUE,
      language = language,
      response_language = response_language,
      cache = cache,
      cache_connection = db,
      disconnect_db = FALSE
    )
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db
    )

    return(output)
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db
  )
}



#' Add index to caching table for search queries for increased speed
#'
#' Tested only with SQLite and MySql. May work with other drivers.
#'
#' To ensure smooth functioning, the search column in the cache table is transformed into a column of type `varchar` and length 255.
#'
#' @param table_name Name of the table in the database. If given, it takes precedence over other parameters.
#' @param check_first Logical, defaults to `TRUE`. If `TRUE`, then before executing anything on the database it checks if the given table has already been indexed. If it has, it does nothing and returns only an informative message.
#' @param show_details Logical, defaults to `FALSE`. If `FALSE`, return the function adds the index to the database, but does not return anything. If `TRUE`, returns a data frame with more details about the index.
#'
#' @inheritParams tw_get_cache_table_name
#' @inheritParams tw_get
#'
#' @return If `show_details` is set to FALSE, nothing, used only for its side effects (add index to caching table). If TRUE, a data frame, same as the output of `tw_check_cache_index(show_details = TRUE)`.
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
#'   tw_index_cache_search()
#' }
tw_index_cache_item <- function(table_name = NULL,
                                check_first = TRUE,
                                type = "item",
                                show_details = FALSE,
                                language = tidywikidatar::tw_get_language(),
                                cache = NULL,
                                cache_connection = NULL,
                                disconnect_db = TRUE) {
  if (is.null(table_name)) {
    table_name <- tw_get_cache_table_name(
      type = type,
      language = language
    )
  } else {
    language <- stringr::str_extract(string = table_name, "[a-z][a-z]$")
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    cli::cli_warn("Table `{table_name}` does not exist.")
    return(invisible())
  }

  if (check_first) {
    check <- tw_check_cache_index(
      table_name = table_name,
      type = type,
      show_details = FALSE,
      language = language,
      cache = cache,
      cache_connection = db,
      disconnect_db = FALSE
    )

    if (check) {
      cli::cli_alert_info("The table {.code {table_name}} is already indexed. No action taken. Set `check` to FALSE to ignore this check.")
      return(invisible(NULL))
    }
  }

  db_no_pool <- pool::poolCheckout(db)

  driver <- db$fetch()

  if (inherits(x = driver, "SQLiteConnection") == FALSE) {
    statement <- glue::glue_sql("ALTER TABLE {`table_name`} MODIFY id VARCHAR(50);",
      table_name = table_name,
      .con = db_no_pool
    )

    index_odbc_result <- DBI::dbSendQuery(
      conn = db_no_pool,
      statement = statement
    )


    statement <- glue::glue_sql("ALTER TABLE {`table_name`} MODIFY property VARCHAR(50);",
      table_name = table_name,
      .con = db_no_pool
    )

    index_odbc_result <- DBI::dbSendQuery(
      conn = db_no_pool,
      statement = statement
    )
  }

  statement <- glue::glue_sql("CREATE INDEX qid on {`table_name`} (id, property);",
    table_name = table_name,
    .con = db_no_pool
  )

  index_odbc_result <- DBI::dbSendQuery(
    conn = db_no_pool,
    statement = statement
  )

  pool::poolReturn(db_no_pool)

  if (show_details == TRUE) {
    output <- tw_check_cache_index(
      table_name = table_name,
      type = type,
      show_details = TRUE,
      language = language,
      cache = cache,
      cache_connection = db,
      disconnect_db = FALSE
    )

    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db
    )

    return(output)
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db
  )
}
