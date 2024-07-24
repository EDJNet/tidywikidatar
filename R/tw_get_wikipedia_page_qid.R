#' Facilitates the creation of MediaWiki API base URLs
#'
#' Mostly used internally
#'
#' @param url A character vector with the full URL to one or more Wikipedia pages. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param action Defaults to "query". Usually either "query" or "parse". In principle, any valid action value, see: \url{https://www.mediawiki.org/w/api.php}
#' @param type Defaults to "page". Either "page" or "category".
#'
#' @return A character vector of base urls to be used with the MediaWiki API
#' @export
#'
#' @examples
#' tw_get_wikipedia_base_api_url(title = "Margaret Mead", language = "en")
#' tw_get_wikipedia_base_api_url(
#'   title = "Category:American women anthropologists",
#'   type = "category",
#'   language = "en"
#' )
tw_get_wikipedia_base_api_url <- function(url = NULL,
                                          title = NULL,
                                          language = tidywikidatar::tw_get_language(),
                                          action = "query",
                                          type = "page") {
  if (is.null(url)) {
    if (is.null(title)) {
      cli::cli_abort("Either {.arg url} or {.arg title} must be provided.")
    }
    if (is.null(language)) {
      cli::cli_abort("Either {.arg language} or full url must be provided.")
    }
  } else {
    check_url_lv <- stringr::str_starts(string = url, pattern = "http")
    if (sum(is.na(check_url_lv)) > 0) {
      url <- url[is.na(check_url_lv) == FALSE]
      cli::cli_warn(c("One or more of the given URLs is actually NA.", i = "Only valid URLs will be processed."))
    }
    check_url_lv <- stringr::str_starts(string = url, pattern = "http")
    if (sum(check_url_lv) != length(check_url_lv)) {
      cli::cli_abort(c(
        "One or more of the Wikipedia URL provided does not start with `http` as expected for a URL.",
        "If you are actually providing Wikipedia page titles, leave the `url` parameter to NULL, and use the `title` parameter instead."
      ))
    }
    title <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://[[a-z]][[a-z]].wikipedia.org/wiki/).*"
    )
  }

  if (is.null(language) == TRUE) {
    language <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
    )
  }

  if (action == "parse") {
    title_reference <- "&page="
  } else {
    title_reference <- "&titles="
  }

  if (type == "page") {
    api_url <- stringr::str_c(
      "https://",
      language,
      ".wikipedia.org/w/api.php?action=",
      action,
      "&redirects=true&format=json",
      title_reference,
      utils::URLencode(URL = title, reserved = TRUE)
    )
  } else if (type == "category") {
    title_reference <- "&cmtitle="

    api_url <- stringr::str_c(
      "https://",
      language,
      ".wikipedia.org/w/api.php?action=",
      action,
      "&redirects=true&format=json",
      title_reference,
      utils::URLencode(URL = title, reserved = TRUE),
      "&list=categorymembers"
    )
  }

  api_url
}


#' Gets the Wikidata Q identifier of one or more Wikipedia pages
#'
#' @param url A character vector with the full URL to one or more Wikipedia pages. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 10. Number of times it re-attempts to reach the API before failing.
#'
#' @return A a data frame with six columns, including `qid` with Wikidata identifiers, and a logical `disambiguation` to flag when disambiguation pages are returned.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_qid(title = "Margaret Mead", language = "en")
#'
#'   # check when Wikipedia returns disambiguation page
#'   tw_get_wikipedia_page_qid(title = c("Rome", "London", "New York", "Vienna"))
#' }
tw_get_wikipedia_page_qid <- function(url = NULL,
                                      title = NULL,
                                      language = tidywikidatar::tw_get_language(),
                                      cache = NULL,
                                      overwrite_cache = FALSE,
                                      cache_connection = NULL,
                                      disconnect_db = TRUE,
                                      wait = 1,
                                      attempts = 10) {
  if (is.null(url) == FALSE) {
    if (is.null(title)) {
      title <- dplyr::case_when(
        stringr::str_starts(string = url, pattern = "http") ~ stringr::str_extract(
          string = url,
          pattern = "(?<=https://[[a-z]][[a-z]].wikipedia.org/wiki/).*"
        ),
        stringr::str_starts(string = url,
                            pattern = stringr::fixed("/wiki/")) ~ stringr::str_remove(
                              string = url,
                              pattern = stringr::fixed("/wiki/")
                            ),
        .default = ""
      )
    } else {
      cli::cli_abort("Either url or title must be provided, not both.")
    }

    if (sum(stringr::str_starts(string = url, pattern = "http"))==length(url)) {
      language <- stringr::str_extract(
        string = url,
        pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
      )
    }
  }

  unique_language <- unique(language)

  if (length(unique_language) == 0) {
    return(tidywikidatar::tw_empty_wikipedia_page)
  } else if (length(unique_language) > 1) {
    cli::cli_abort("{.fn tw_get_wikipedia_page_qid} currently accepts only inputs with one language at a time.")
  }

  unique_title <- unique(title)

  if (length(unique_title) == 0) {
    return(tidywikidatar::tw_empty_wikipedia_page)
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = unique_language,
    cache = cache
  )

  if (length(unique_title) == 1) {
    return(
      dplyr::left_join(
        x = tibble::tibble(title_url = title),
        y = tw_get_wikipedia_page_qid_single(
          url = NULL,
          title = unique_title,
          language = unique_language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          wait = wait,
          attempts = attempts
        ),
        by = "title_url"
      )
    )
  } else if (length(unique_title) > 1) {
    if (overwrite_cache == TRUE | tw_check_cache(cache) == FALSE) {
      pb <- progress::progress_bar$new(total = length(unique_title))

      df <- purrr::map_dfr(
        .x = unique_title,
        .f = function(x) {
          pb$tick()
          tw_get_wikipedia_page_qid_single(
            url = NULL,
            title = x,
            language = unique_language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = db,
            disconnect_db = FALSE,
            wait = wait,
            attempts = attempts
          )
        }
      )
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = unique_language
      )
      return(
        dplyr::left_join(
          x = tibble::tibble(title_url = title),
          y = df %>%
            dplyr::distinct(.data$title_url, .keep_all = TRUE),
          by = "title_url"
        )
      )
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      titles_in_cache_df <- tw_get_cached_wikipedia_page_qid(
        title = unique_title,
        language = unique_language,
        cache_connection = db,
        disconnect_db = FALSE
      )

      titles_not_in_cache <- unique_title[!is.element(unique_title, titles_in_cache_df$title_url)]

      if (length(titles_not_in_cache) == 0) {
        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          language = unique_language
        )
        return(
          dplyr::left_join(
            x = tibble::tibble(title_url = title),
            y = titles_in_cache_df %>%
              dplyr::distinct(.data$title_url, .keep_all = TRUE),
            by = "title_url"
          )
        )
      } else if (length(titles_not_in_cache) > 0) {
        pb <- progress::progress_bar$new(total = length(titles_not_in_cache))

        titles_not_in_cache_df <- purrr::map_dfr(
          .x = titles_not_in_cache,
          .f = function(x) {
            pb$tick()
            tw_get_wikipedia_page_qid_single(
              url = NULL,
              title = x,
              language = unique_language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = db,
              disconnect_db = FALSE,
              wait = wait,
              attempts = attempts
            )
          }
        )

        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          language = unique_language
        )
        dplyr::left_join(
          x = tibble::tibble(title_url = title),
          y = dplyr::bind_rows(
            titles_in_cache_df,
            titles_not_in_cache_df
          ) %>%
            dplyr::distinct(.data$title_url, .keep_all = TRUE),
          by = "title_url"
        )
      }
    }
  }
}

#' Gets the Wikidata id of a Wikipedia page
#'
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 10. Number of times it re-attempts to reach the API before failing.
#'
#' @return A data frame (a tibble) with eight columns: `title`, `wikipedia_title`, `wikipedia_id`, `qid`, `description`, `disambiguation`, and `language`.
#'
#' @examples
#' if (interactive()) {
#'   tw_get_wikipedia_page_qid_single(title = "Margaret Mead", language = "en")
#' }
tw_get_wikipedia_page_qid_single <- function(title = NULL,
                                             url = NULL,
                                             language = tidywikidatar::tw_get_language(),
                                             cache = NULL,
                                             overwrite_cache = FALSE,
                                             cache_connection = NULL,
                                             disconnect_db = TRUE,
                                             wait = 1,
                                             attempts = 10) {
  if (!is.null(url) && !is.function(url)) {
    if (is.null(title) & is.function(title) == FALSE) {
      if (stringr::str_starts(string = url,
                              pattern = stringr::fixed("/wiki/"))) {
        title <- stringr::str_remove(string = url,
                                     pattern = stringr::fixed("/wiki/"))
      } else {
        title <- stringr::str_extract(
          string = url,
          pattern = "(?<=https://[[a-z]][[a-z]].wikipedia.org/wiki/).*"
        )

        language <- stringr::str_extract(
          string = url,
          pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
        )
      }
      url <- NULL
    } else {
      cli::cli_abort("Either url or title must be provided, not both.")
    }
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_wikipedia_page_qid(
      title = title,
      language = language,
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      return(db_result %>%
        tibble::as_tibble())
    }
  }

  json_url <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops"
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
  } else {
    wikidata_id_l <- api_result
  }


  wikipedia_id <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageid"
    )

  if (is.null(wikipedia_id)) {
    wikipedia_id <- NA_real_
  } else {
    wikipedia_id <- as.numeric(wikipedia_id)
  }

  wikidata_id <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageprops",
      "wikibase_item"
    )

  if (is.null(wikidata_id)) {
    wikidata_id <- as.character(NA)
  }

  description <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageprops",
      "wikibase-shortdesc"
    )

  if (is.null(description)) {
    description <- as.character(NA)
  }

  disambiguation <- is.element(
    el = "disambiguation",
    set = wikidata_id_l %>%
      purrr::pluck(
        "query",
        "pages",
        1,
        "pageprops"
      ) %>%
      names()
  )

  normalised <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "normalized",
      1,
      "to"
    )

  if (is.null(normalised)) {
    normalised <- as.character(NA)
  }

  redirected <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "redirects",
      1,
      "to"
    )

  if (is.null(redirected)) {
    redirected <- as.character(NA)
  }

  wikipedia_title <- dplyr::case_when(
    is.na(redirected) == FALSE ~ redirected,
    is.na(normalised) == FALSE ~ normalised,
    is.na(wikipedia_id) == TRUE ~ as.character(NA),
    TRUE ~ title
  )

  df <- tibble::tibble(
    title_url = as.character(title),
    wikipedia_title = as.character(wikipedia_title),
    wikipedia_id = as.numeric(wikipedia_id),
    qid = as.character(wikidata_id),
    description = as.character(description),
    disambiguation = as.logical(disambiguation),
    language = as.character(language)
  )

  if (tw_check_cache(cache) == TRUE) {
    tw_write_qid_of_wikipedia_page_to_cache(
      df = df,
      cache = cache,
      cache_connection = db,
      language = language,
      overwrite_cache = overwrite_cache,
      disconnect_db = disconnect_db
    )
  }

  df
}



#' Gets id of Wikipedia pages from local cache
#'
#' Mostly used internally.
#'
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
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
#'   df_from_cache <- tw_get_cached_wikipedia_page_qid(
#'     title = "Margaret Mead",
#'     language = "en"
#'   )
#'
#'   df_from_cache
#' }
tw_get_cached_wikipedia_page_qid <- function(title,
                                             language = tidywikidatar::tw_get_language(),
                                             cache = NULL,
                                             cache_connection = NULL,
                                             disconnect_db = TRUE) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(NULL))
  }

  title_url <- title

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
    }
    return(tidywikidatar::tw_empty_wikipedia_page)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(
        .data$title_url %in% !!stringr::str_c(title_url)
      ),
    error = function(e) {
      logical(1L)
    }
  )
  if (isFALSE(db_result)) {
    if (disconnect_db == TRUE) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
    }
    return(tidywikidatar::tw_empty_wikipedia_page)
  }

  cached_df <- db_result %>%
    dplyr::collect() %>%
    dplyr::mutate(disambiguation = as.logical(.data$disambiguation))

  if (disconnect_db == TRUE) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
  }
  cached_df
}



#' Write Wikidata identifier (qid) of Wikipedia page to cache
#'
#' Mostly used internally by `tidywikidatar`, use with caution to keep caching consistent.
#'
#' @param df A data frame typically generated with `tw_get_wikipedia_page_qid()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return Silently returns the same data frame provided as input. Mostly used internally for its side effects.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   df <- tw_get_wikipedia_page_qid(
#'     title = "Margaret Mead",
#'     language = "en",
#'     cache = FALSE
#'   )
#'
#'   tw_write_qid_of_wikipedia_page_to_cache(
#'     df = df,
#'     language = "en"
#'   )
#' }
tw_write_qid_of_wikipedia_page_to_cache <- function(df,
                                                    language = tidywikidatar::tw_get_language(),
                                                    cache = NULL,
                                                    overwrite_cache = FALSE,
                                                    cache_connection = NULL,
                                                    disconnect_db = TRUE) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(NULL))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE title_url = {title_url*}",
        title_url = unique(df$title_url),
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

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  invisible(df)
}

#' Reset Wikipedia page cache
#'
#' Removes the table where data typically gathered with `tw_get_wikipedia_page_qid()` from cache
#'
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param ask Logical, defaults to TRUE. If FALSE, and cache folder does not exist, it just creates it without asking (useful for non-interactive sessions).
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_reset_wikipedia_page_cache()
#' }
tw_reset_wikipedia_page_cache <- function(language = tidywikidatar::tw_get_language(),
                                          cache = NULL,
                                          cache_connection = NULL,
                                          disconnect_db = TRUE,
                                          ask = TRUE) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    pool::dbRemoveTable(conn = db, name = table_name)
    cli::cli_alert_info("Wikipedia page cache reset for language {.val {language}} completed.")
  } else if (utils::menu(c("Yes", "No"), title = paste0("Are you sure you want to remove from cache the qualifiers table for language: ", sQuote(language), "?")) == 1) {
    pool::dbRemoveTable(conn = db, name = table_name)
    cli::cli_alert_info("Wikipedia page cache reset for language {.val {language}} completed.")
  }


  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
}
