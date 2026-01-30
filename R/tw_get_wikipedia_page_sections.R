#' Get sections of a Wikipedia page
#'
#' @inheritParams tw_get_image
#' @inheritParams tw_get_image_metadata
#' @inheritParams tw_get_wikipedia_page_links
#' @inheritParams tw_get_wikipedia_page_links_single
#'
#' @return A data frame (a tibble), with the same columns as
#'   `tw_empty_wikipedia_page_sections`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_sections(title = "Margaret Mead", language = "en")
#' }
tw_get_wikipedia_page_sections <- function(
  url = NULL,
  title = NULL,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10
) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  wikipedia_page_qid_df <- tw_get_wikipedia_page_qid(
    title = title,
    language = language,
    url = url,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = db,
    disconnect_db = FALSE,
    wait = wait,
    attempts = attempts
  )

  source_df <- wikipedia_page_qid_df %>%
    dplyr::transmute(
      source_title_url = .data$title_url,
      source_wikipedia_title = .data$wikipedia_title,
      source_qid = .data$qid,
      language = .data$language
    ) %>%
    dplyr::distinct(.data$source_title_url, .keep_all = TRUE)

  wikipedia_page_sections_df <- purrr::map_dfr(
    .x = seq_along(source_df$source_wikipedia_title),
    .f = function(i) {
      current_slice_df <- source_df %>%
        dplyr::slice(i)

      tw_get_wikipedia_page_sections_single(
        title = current_slice_df$source_title_url,
        language = current_slice_df$language,
        url = NULL,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = FALSE,
        wait = wait,
        attempts = attempts
      )
    }
  )

  if (disconnect_db) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
  }
  wikipedia_page_sections_df
}


#' Get all Wikidata Q identifiers of all Wikipedia pages that appear in a given
#' page
#'
#' @inheritParams tw_get_image
#' @inheritParams tw_get_image_metadata
#' @inheritParams tw_get_wikipedia_page_links
#' @inheritParams tw_get_wikipedia_page_links_single
#' @inheritParams tw_get_wikipedia_page_links_single
#'
#' @return A data frame (a tibble) with four columns: `wikipedia_title`,
#'   `wikipedia_id`, `wikidata_id`, `wikidata_description`.
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_sections_single(title = "Margaret Mead", language = "en")
#' }
tw_get_wikipedia_page_sections_single <- function(
  url = NULL,
  title = NULL,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10,
  wikipedia_page_qid_df = NULL
) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (is.null(wikipedia_page_qid_df)) {
    wikipedia_page_qid_df <- tw_get_wikipedia_page_qid(
      title = title,
      language = language,
      url = url,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait,
      attempts = attempts
    )
  }

  if (tw_check_cache(cache) & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_wikipedia_page_sections(
      title = title,
      language = language,
      cache = cache,
      cache_connection = db,
      disconnect_db = FALSE
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )

      return(
        db_result %>%
          dplyr::collect()
      )
    }
  }

  json_url <- tw_get_wikipedia_sections_api_url(
    url = url,
    title = title,
    language = language
  )

  api_result <- FALSE

  attempt_n <- 1

  while (isFALSE(api_result) & attempt_n <= attempts) {
    attempt_n <- sum(attempt_n, 1)
    api_result <- tryCatch(
      jsonlite::read_json(path = json_url),
      error = function(e) {
        logical(1L)
      }
    )
    Sys.sleep(time = wait)
  }

  if (isFALSE(api_result)) {
    cli::cli_abort(c(
      "Could not reach the API with {attempts} attempts.",
      i = "Consider increasing the waiting time between calls with the {.arg wait} parameter or check your internet connection."
    ))
  } else if ("error" %in% names(api_result)) {
    cli::cli_abort(
      "{api_result[['error']][['code']]}: {api_result[['error']][['info']]} - {json_url}"
    )
    api_result[["error"]]
  } else {
    base_json <- api_result
  }

  sections_df <- purrr::map_dfr(
    .x = base_json %>%
      purrr::pluck("parse", "sections"),
    .f = tibble::as_tibble_row
  )

  if (nrow(sections_df) < 1) {
    return(tidywikidatar::tw_empty_wikipedia_page_sections)
  }

  if (tw_check_cache(cache)) {
    tw_write_wikipedia_page_sections_to_cache(
      df = sections_df,
      cache_connection = db,
      language = language,
      overwrite_cache = overwrite_cache,
      disconnect_db = disconnect_db
    )
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  sections_df
}


#' Gets sections of Wikipedia pages from local cache
#'
#' Mostly used internally.
#'
#' @inheritParams tw_get_image
#' @inheritParams tw_get_image_metadata
#' @inheritParams tw_get_wikipedia_page_links
#' @inheritParams tw_get_wikipedia_page_links_single
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_set_cache_folder(path = tempdir())
#'   tw_enable_cache()
#'   tw_create_cache_folder(ask = FALSE)
#'
#'   df_from_api <- tw_get_wikipedia_page_qid(title = "Margaret Mead", language = "en")
#'
#'   df_from_cache <- tw_get_cached_wikipedia_page_sections(
#'     title = "Margaret Mead",
#'     language = "en"
#'   )
#'
#'   df_from_cache
#' }
tw_get_cached_wikipedia_page_sections <- function(
  title,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE
) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(NULL))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page_sections",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
    }
    return(tidywikidatar::tw_empty_wikipedia_page_sections)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(
        .data$fromtitle %in% !!stringr::str_c(title)
      ),
    error = function(e) {
      logical(1L)
    }
  )

  if (isFALSE(db_result)) {
    if (disconnect_db) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
    }
    return(tidywikidatar::tw_empty_wikipedia_page_sections)
  }

  cached_df <- db_result %>%
    dplyr::collect()

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  cached_df
}


#' Write Wikipedia page links to cache
#'
#' Mostly used internally by `tidywikidatar`, use with caution to keep caching
#' consistent.
#'
#' @param df A data frame typically generated with
#'   [tw_get_wikipedia_page_sections()].
#' @inheritParams tw_get_image
#' @inheritParams tw_get_image_metadata
#' @inheritParams tw_get_wikipedia_page_links
#'
#' @return Silently returns the same data frame provided as input. Mostly used
#'   internally for its side effects.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df <- tw_get_wikipedia_page_sections(
#'     title = "Margaret Mead",
#'     language = "en",
#'     cache = FALSE
#'   )
#'
#'   tw_write_wikipedia_page_sections_to_cache(
#'     df = df,
#'     language = "en"
#'   )
#' }
tw_write_wikipedia_page_sections_to_cache <- function(
  df,
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE
) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(NULL))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page_sections",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache) {
      statement <- glue::glue_sql(
        "DELETE FROM {`table_name`} WHERE fromtitle = {fromtitle*}",
        fromtitle = unique(df$fromtitle),
        table_name = table_name,
        .con = db
      )
      result <- pool::dbExecute(
        conn = db,
        statement = statement
      )
    }
  }

  pool::dbWriteTable(db, name = table_name, value = df, append = TRUE)

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  invisible(df)
}

#' Reset Wikipedia page link cache
#'
#' Removes from cache the table where data typically gathered with
#' [tw_get_wikipedia_page_sections()] are stored.
#'
#' @inheritParams tw_get
#' @inheritParams tw_reset_item_cache
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_reset_wikipedia_page_sections_cache()
#' }
tw_reset_wikipedia_page_sections_cache <- function(
  language = tidywikidatar::tw_get_language(),
  cache = NULL,
  cache_connection = NULL,
  disconnect_db = TRUE,
  ask = TRUE
) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page_sections",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    pool::dbRemoveTable(conn = db, name = table_name)
    cli::cli_alert_info(
      "Wikipedia page sections cache reset for language {.val {language}} completed."
    )
  } else if (
    utils::menu(
      c("Yes", "No"),
      title = paste0(
        "Are you sure you want to remove from cache the Wikipedia page links cache for language: ",
        sQuote(language),
        "?"
      )
    ) ==
      1
  ) {
    pool::dbRemoveTable(conn = db, name = table_name)
    cli::cli_alert_info(
      "Wikipedia page sections cache reset for language {.val {language}} completed."
    )
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
}

#' Facilitates the creation of MediaWiki API base URLs to retrieve sections of a
#' page
#'
#' Mostly used internally
#'
#' @inheritParams tw_get_image
#' @inheritParams tw_get_image_metadata
#' @inheritParams tw_get_wikipedia_page_links
#' @inheritParams tw_get_wikipedia_page_links_single
#'
#' @return A character vector of base urls to be used with the MediaWiki API
#' @export
#'
#' @examples
#' tw_get_wikipedia_sections_api_url(title = "Margaret Mead", language = "en")
tw_get_wikipedia_sections_api_url <- function(
  url = NULL,
  title = NULL,
  language = tidywikidatar::tw_get_language()
) {
  stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language,
      action = "parse"
    ),
    "&prop=sections"
  )
}
