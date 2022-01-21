#' Get all Wikidata Q identifiers of all Wikipedia pages that appear in one or more pages
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 5. Number of times it re-attempts to reach the API before failing.
#'
#' @return A data frame (a tibble) with wight columns: `source_title_url`, `source_wikipedia_title`, `source_qid`, `wikipedia_title`, `wikipedia_id`, `qid`, `description`, and `language`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_links(title = "Margaret Mead", language = "en")
#' }
tw_get_wikipedia_page_links <- function(url = NULL,
                                        language = tidywikidatar::tw_get_language(),
                                        title = NULL,
                                        cache = NULL,
                                        overwrite_cache = FALSE,
                                        cache_connection = NULL,
                                        disconnect_db = TRUE,
                                        wait = 1,
                                        attempts = 5) {
  if (isTRUE(tw_check_cache(cache))) {
    db <- tw_connect_to_cache(
      connection = cache_connection,
      language = language
    )
  }

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
    dplyr::distinct(.data$source_qid, .keep_all = TRUE)

  wikipedia_page_links_df <- purrr::map_dfr(
    .x = seq_along(source_df$source_wikipedia_title),
    .f = function(i) {
      current_slice_df <- source_df %>%
        dplyr::slice(i)

      linked_df <- tw_get_wikipedia_page_links_single(
        title = current_slice_df$source_title_url,
        language = current_slice_df$language,
        url = NULL,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = FALSE,
        wait = wait,
        attempts = attempts,
        wikipedia_page_qid_df = wikipedia_page_qid_df
      )

      linked_df
    }
  )

  if (disconnect_db == TRUE) {
    tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
  }
  wikipedia_page_links_df
}


#' Get all Wikidata Q identifiers of all Wikipedia pages that appear in a given page
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 5. Number of times it re-attempts to reach the API before failing.
#' @param wikipedia_page_qid_df Defaults to NULL. If given, used to reduce calls to cache. A data frame
#'
#' @return A data frame (a tibble) with four columns: `wikipedia_title`, `wikipedia_id`, `wikidata_id`, `wikidata_description`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_links_single(title = "Margaret Mead", language = "en")
#' }
tw_get_wikipedia_page_links_single <- function(url = NULL,
                                               language = tidywikidatar::tw_get_language(),
                                               title = NULL,
                                               cache = NULL,
                                               overwrite_cache = FALSE,
                                               cache_connection = NULL,
                                               disconnect_db = TRUE,
                                               wait = 1,
                                               attempts = 5,
                                               wikipedia_page_qid_df = NULL) {
  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_wikipedia_page_links(
      title = title,
      language = language,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      return(db_result %>%
        dplyr::collect())
    }
  }

  json_url <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops&generator=links&gpllimit=500"
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
    usethis::ui_stop("It has not been possible to reach the API with {attempts} attempts. Consider increasing the waiting time between calls with the {usethis::ui_code('wait')} parameter or check your internet connection.")
  } else {
    base_json <- api_result
  }

  continue_check <- base_json %>%
    purrr::pluck(
      "continue",
      "gplcontinue"
    )

  all_jsons <- list()

  page_number <- 1

  all_jsons[[page_number]] <- base_json

  while (is.null(continue_check) == FALSE & page_number < 200) {
    page_number <- page_number + 1

    json_url <- stringr::str_c(
      json_url,
      "&gplcontinue=",
      continue_check
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
      usethis::ui_stop("It has not been possible to reach the API with {attempts} attempts. Consider increasing the waiting time between calls with the {usethis::ui_code('wait')} parameter or check your internet connection.")
    } else {
      base_json <- api_result
    }

    all_jsons[[page_number]] <- base_json

    continue_check <- base_json %>%
      purrr::pluck(
        "continue",
        "gplcontinue"
      )
  }

  all_pages <- purrr::map(
    .x = all_jsons,
    .f = purrr::pluck,
    "query",
    "pages"
  ) %>%
    purrr::flatten()

  linked_df <- purrr::map_dfr(
    .x = all_pages,
    .f = function(current_page) {
      tibble::tibble(
        qid = current_page %>%
          purrr::pluck(
            "pageprops",
            "wikibase_item"
          ),
        description = current_page %>%
          purrr::pluck(
            "pageprops",
            "wikibase-shortdesc"
          ),
        wikipedia_id = current_page %>%
          purrr::pluck(
            "pageid"
          ),
        wikipedia_title = current_page %>%
          purrr::pluck(
            "title"
          )
      )
    }
  ) %>%
    dplyr::select(
      .data$wikipedia_title,
      .data$wikipedia_id,
      .data$qid,
      .data$description
    ) %>%
    dplyr::mutate(language = language)

  if (is.null(wikipedia_page_qid_df)) {
    wikipedia_page_qid_df <- tw_get_wikipedia_page_qid(
      title = title,
      language = language,
      url = url,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = FALSE,
      wait = wait,
      attempts = attempts
    )
  }

  output_linked_df <- dplyr::bind_cols(
    wikipedia_page_qid_df %>%
      dplyr::transmute(
        source_title_url = .data$title_url,
        source_wikipedia_title = .data$wikipedia_title,
        source_qid = .data$qid
      ) %>%
      dplyr::distinct(.data$source_qid, .keep_all = TRUE),
    linked_df
  )

  if (tw_check_cache(cache) == TRUE) {
    tw_write_wikipedia_page_links_to_cache(
      df = output_linked_df,
      cache_connection = cache_connection,
      language = language,
      overwrite_cache = overwrite_cache,
      disconnect_db = disconnect_db
    )
  }

  if (disconnect_db == TRUE) {
    tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db,
      language = language
    )
  }

  output_linked_df
}





#' Gets links of Wikipedia pages from local cache
#'
#' Mostly used internally.
#'
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection open.
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
#'   df_from_cache <- tw_get_cached_wikipedia_page_links(
#'     title = "Margaret Mead",
#'     language = "en"
#'   )
#'
#'   df_from_cache
#' }
tw_get_cached_wikipedia_page_links <- function(title,
                                               language = tidywikidatar::tw_get_language(),
                                               cache_connection = NULL,
                                               disconnect_db = TRUE) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page_links",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      tw_disconnect_from_cache(
        cache = TRUE,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
    }
    return(tidywikidatar::tw_empty_wikipedia_page_links)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(
        .data$source_title_url %in% stringr::str_c(title)
      ),
    error = function(e) {
      logical(1L)
    }
  )


  if (isFALSE(db_result)) {
    if (disconnect_db == TRUE) {
      tw_disconnect_from_cache(
        cache = TRUE,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
    }
    return(tidywikidatar::tw_empty_wikipedia_page_links)
  }

  cached_df <- db_result %>%
    dplyr::collect()

  if (disconnect_db == TRUE) {
    tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
  }
  cached_df
}


#' Write Wikipedia page links to cache
#'
#' Mostly used internally by `tidywikidatar`, use with caution to keep caching consistent.
#'
#' @param df A data frame typically generated with `tw_get_wikipedia_page_links()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return Silently returns the same data frame provided as input. Mostly used internally for its side effects.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df <- tw_get_wikipedia_page_links(
#'     title = "Margaret Mead",
#'     language = "en",
#'     cache = FALSE
#'   )
#'
#'   tw_write_wikipedia_page_links_to_cache(
#'     df = df,
#'     language = "en"
#'   )
#' }
tw_write_wikipedia_page_links_to_cache <- function(df,
                                                   language = tidywikidatar::tw_get_language(),
                                                   overwrite_cache = FALSE,
                                                   cache_connection = NULL,
                                                   disconnect_db = TRUE) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page_links",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE source_title_url = {source_title_url*}",
        source_title_url = unique(df$source_title_url),
        table_name = table_name,
        .con = db
      )
      result <- pool::dbExecute(
        conn = db,
        statement = statement
      )
    }
  }

  pool::dbWriteTable(db,
    name = table_name,
    value = df,
    append = TRUE
  )

  if (disconnect_db == TRUE) {
    tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = cache_connection,
      disconnect_db = db,
      language = language
    )
  }
  invisible(df)
}

#' Reset Wikipedia page link cache
#'
#' Removes from cache the table where data typically gathered with `tw_get_wikipedia_page_links()` are stored
#'
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database by default. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for non-interactive sessions).
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_reset_wikipedia_page_links_cache()
#' }
tw_reset_wikipedia_page_links_cache <- function(language = tidywikidatar::tw_get_language(),
                                                cache_connection = NULL,
                                                disconnect_db = TRUE,
                                                ask = TRUE) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page_links",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Wikipedia page links cache reset for language ", sQuote(language), " completed"))
  } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from cache the Wikipedia page links cache for language: ", sQuote(language), "?"))) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Wikipedia page links cache reset for language ", sQuote(language), " completed"))
  }

  if (disconnect_db == TRUE) {
    tw_disconnect_from_cache(
      cache = TRUE,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
  }
}