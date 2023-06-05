#' Prune unnecessary item properties from the local cache
#'
#' @param p_to_keep A character vector for properties in the format, e.g., `c("P123", "P124")`. Only these properties will be kept in the cache.
#' @param p_to_remove A character vector for properties in the format, e.g., `c("P123", "P124")`. These properties will be removed from cache.
#' @inheritParams tw_get_property
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#'
#' p_to_keep <- c("P31", "P21", "P569", "P570", "P19", "P20")
#'
tw_prune <- function(p_to_keep = NULL,
                     p_to_remove = NULL,
                     language = tidywikidatar::tw_get_language(),
                     cache = NULL,
                     cache_connection = NULL,
                     disconnect_db = TRUE) {
  if (is.null(p_to_keep)&is.null(p_to_remove)) {
    cli::cli_abort("At least one of {.var p_to_keep} and {.var p_to_remove} must be given.")
  }

  if (is.null(p_to_keep)==FALSE) {
    p_to_keep <- p_to_keep[is.na(p_to_keep)==FALSE]
    p_to_keep <- unique(stringr::str_to_upper(p_to_keep))
  }

  if (is.null(p_to_remove)==FALSE) {
    p_to_remove <- p_to_remove[is.na(p_to_remove)==FALSE]
    p_to_remove <- unique(stringr::str_to_upper(p_to_remove))
  }

  if (sum(stringr::str_starts(string = c(p_to_keep, p_to_remove), pattern = "P")) != length(c(p_to_keep, p_to_remove))) {
    cli::cli_abort(c("x" = "All Wikidata properties start with the letter {sQuote('P')}.
                 Check your input to the {.var p_to_keep} and {.var p_to_remove} parameter"))
  }

  if (isFALSE(tw_check_cache(cache = cache))) {
    cli::cli_warn(message = c("!" = "Cache must be enabled for {.fun tw_prune} to do something.",
                              "i" = "Set {.var cache} to {.value TRUE}
                              or run {.fun tw_enable_cache} to enable caching for the current session."))
    return(invisible(NULL))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  #db_no_pool <- pool::poolCheckout(db)
  db_no_pool <- db

  table_name <- tw_get_cache_table_name(
    type = "item",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )

    return(invisible(NULL))
  }

  # get currently existing properties in the database
  previous_p_df <- dplyr::tbl(src = db,
                              table_name) %>%
    dplyr::distinct(property) %>%
    dplyr::collect() %>%
    dplyr::filter(stringr::str_starts(string = property,
                                      pattern = "P[[:digit:]]"))

  to_be_removed_df <- previous_p_df

  if (is.null(p_to_remove)==FALSE) {
    to_be_removed_df <- dplyr::semi_join(x = to_be_removed_df,
                                         y = tibble::tibble(property = p_to_remove),
                                         by = "property")
  }

  if (is.null(p_to_keep)==FALSE) {
    to_be_removed_df <- dplyr::anti_join(x = to_be_removed_df,
                                         y = tibble::tibble(property = p_to_keep),
                                         by = "property")
  }

  to_be_removed_v <- to_be_removed_df %>%
    dplyr::pull(property)

  if (length(to_be_removed_v)==0) {
    invisible(return(NULL))
  }

  sql_statement <- glue::glue_sql("
    DELETE
    FROM {`table_name`}
    WHERE `property`
    IN ({to_be_removed_v*});
    ",
    .con = db)

  before_nrow <- dplyr::tbl(src = db,
                            table_name) %>%
    dplyr::tally(name = "n") %>%
    dplyr::pull(n)

  # Remove rows
  after_nrow <- DBI::dbExecute(conn = db,
                              statement = sql_statement)

# Check if table is indexed

  sql_show_index_statement <- glue::glue_sql("SHOW INDEX FROM {`table_name`}",
                                       table_name = table_name,
                                       .con = db
  )

  show_index_result <- DBI::dbSendStatement(conn = db,
                                      statement = sql_show_index_statement)

  if (index_result==0) {
    statement <- glue::glue_sql("ALTER TABLE {`table_name`} MODIFY id VARCHAR(50);",
                                table_name = table_name,
                                .con = db
    )

    index_odbc_result <- DBI::dbExecute(
      conn = db,
      statement = statement
    )


    statement <- glue::glue_sql("ALTER TABLE {`table_name`} MODIFY property VARCHAR(50);",
                                table_name = table_name,
                                .con = db
    )

    index_odbc_result <- DBI::dbSendQuery(
      conn = db,
      statement = statement
    )

    sql_create_index_statement <- glue::glue_sql("CREATE INDEX id on {`table_name`} (id);",
                                                 table_name = table_name,
                                                 .con = db)

    create_index_result <- DBI::dbExecute(conn = db,
                                          statement = sql_create_index_statement)
  } else {

    ## optimize
    sql_optimize_table_statement <- glue::glue_sql("OPTIMIZE TABLE {`table_name`};",
                                                     table_name = table_name,
                                                     .con = db)

    optimize_table_result <- DBI::dbExecute(conn = db,
                                          statement = sql_optimize_table_statement)


  }






  library("tictoc")
  tic()
  tw_get(id = "Q42",
         cache_connection = db,
         disconnect_db = FALSE)
  toc()

  pb <- progress::progress_bar$new(total = length(to_be_removed_v))

  purrr::walk(.x = to_be_removed_v,
              .f = function(current_p_to_remove) {
                pb$tick()
                sql_statement <- glue::glue_sql("
    DELETE
    FROM {`table_name`}
    WHERE `property`
    IN { glue::glue_collapse(x = to_be_removed_v, sep = ',')} # add other exceptions, basically, starts with P
  ", .con = db)
              }
  )

  ## add exception for labels and descriptions OR
  ## RUN separate queries for all P? e.g.:
  ## delete_queries = paste0("DELETE FROM tablename WHERE (id = '", ids, "');")
  # map(.x = delete_queries, .f = dbExecute, conn = con)
  sql_statement <- glue::glue_sql("
    DELETE
    FROM {`table_name`}
    WHERE `property`
    IN { glue::glue_collapse(x = p, sep = ",")} # add other exceptions, basically, starts with P
  ", .con = con)



  db_result <- tryCatch(
    DBI::dbExecute(conn = db,
                   statement = sql_statement),
    error = function(e) {
      logical(1L)
    }
  )



 # pool::poolReturn(db_no_pool)
  before_nrow-after_nrow

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
}
