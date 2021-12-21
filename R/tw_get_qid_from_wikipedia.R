#' Facilitates the creation of MediaWiki API base URLs
#'
#' Mostly used internally
#'
#' @param url A character vector with the full URL to one or more Wikipedia pages. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#'
#' @return A charachter vector of base urls to be used with the MediaWiki API
#' @export
#'
#' @examples
#'
#' tw_get_wikipedia_base_api_url(title = "Margaret Mead", language = "en")
tw_get_wikipedia_base_api_url <- function(url = NULL,
                                          title = NULL,
                                          language = tidywikidatar::tw_get_language()) {
  if (is.null(url) == TRUE) {
    if (is.null(title) == TRUE) {
      usethis::ui_stop("Either url or title must be provided")
    }
    if (is.null(language) == TRUE) {
      usethis::ui_stop("Either language or full url must be provided")
    }
  } else {
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

  api_url <- stringr::str_c(
    "https://",
    language,
    ".wikipedia.org/w/api.php?action=query&redirects=true&format=json&titles=",
    utils::URLencode(URL = title)
  )

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
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A a data frame with six columns, including `qid` with Wikidata identifiers, and a logical `disambiguation` to flag when disambiguation pages are returned.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_qid_of_wikipedia_page(title = "Margaret Mead", language = "en")
#'
#'   # check when Wikipedia returns disambiguation page
#'   tw_get_qid_of_wikipedia_page(title = c("Rome","London", "New York", "Vienna"))
#' }
tw_get_qid_of_wikipedia_page <- function(url = NULL,
                                         title = NULL,
                                         language = tidywikidatar::tw_get_language(),
                                         cache = NULL,
                                         overwrite_cache = FALSE,
                                         cache_connection = NULL,
                                         disconnect_db = TRUE,
                                         wait = 0) {

  if (is.null(url)==FALSE) {
    if (is.null(title)) {
      title <- stringr::str_extract(
        string = url,
        pattern = "(?<=https://[[a-z]][[a-z]].wikipedia.org/wiki/).*"
      )
    } else {
      usethis::ui_stop(x = "Either url or title must be provided, not both.")
    }

    language <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
    )
  }

  unique_language <- unique(language)

  if (length(unique_language) == 0) {
    return(NULL)
  } else if (length(unique_language)>1) {
    usethis::ui_stop(x = "{usethis::ui_code('tw_get_qid_of_wikipedia_page()')} currently accepts only inputs with one language at a time.")
  }

  unique_title <- unique(title)

  if (length(unique_title) == 0) {
    return(NULL)
  }

  if (length(unique_title) == 1) {
    return(
      dplyr::left_join(
        x = tibble::tibble(title = title),
        y = tw_get_qid_of_wikipedia_page_single(
          url = NULL,
          title = unique_title,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection,
          disconnect_db = disconnect_db
        ),
        by = "title"
      )
    )
  } else if (length(unique_title) > 1) {
    if (overwrite_cache == TRUE | tw_check_cache(cache) == FALSE) {
      pb <- progress::progress_bar$new(total = length(unique_title))

      df <- purrr::map_dfr(
        .x = unique_title,
        .f = function(x) {
          pb$tick()
          tw_get_qid_of_wikipedia_page_single(
            url = NULL,
            title = x,
            language = unique_language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = cache_connection,
            disconnect_db = FALSE
          )
        }
      )
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = cache_connection,
        disconnect_db = disconnect_db
      )
      return(
        dplyr::left_join(
          x = tibble::tibble(title = title),
          y = df,
          by = "title"
        )
      )
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      titles_in_cache_df <- tw_get_cached_qid_of_wikipedia_page(
        title = unique_title,
        language = language,
        cache_connection = cache_connection,
        disconnect_db = FALSE
      )

      titles_not_in_cache <- unique_title[!is.element(unique_title, titles_in_cache_df$title)]

      if (length(titles_not_in_cache) == 0) {
        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = cache_connection,
          disconnect_db = disconnect_db
        )
        return(
          dplyr::left_join(
            x = tibble::tibble(title = title),
            y = titles_in_cache_df,
            by = "title"
          )
        )
      } else if (length(titles_not_in_cache) > 0) {
        pb <- progress::progress_bar$new(total = length(titles_not_in_cache))
        titles_not_in_cache_df <- purrr::map_dfr(
          .x = titles_not_in_cache,
          .f = function(x) {
            pb$tick()
            tw_get_qid_of_wikipedia_page_single(
              url = NULL,
              title = x,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = cache_connection,
              disconnect_db = FALSE
            )
          }
        )

        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = cache_connection,
          disconnect_db = disconnect_db
        )
        dplyr::left_join(
          x = tibble::tibble(title = title),
          y = dplyr::bind_rows(
            titles_in_cache_df,
            titles_not_in_cache_df
          ),
          by = "title"
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
#' @param wait In seconds, defaults to 0. Time to wait between queries to the API. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A data frame (a tibble) with eight columns: `title`, `wikipedia_title`, `wikipedia_id`, `qid`, `description`, `disambiguation`, and `language`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_qid_of_wikipedia_page_single(title = "Margaret Mead", language = "en")
#' }
tw_get_qid_of_wikipedia_page_single <- function(title = NULL,
                                                url = NULL,
                                                language = tidywikidatar::tw_get_language(),
                                                cache = NULL,
                                                overwrite_cache = FALSE,
                                                cache_connection = NULL,
                                                disconnect_db = TRUE,
                                                wait = 0) {


  if (is.null(url)==FALSE&is.function(url)==FALSE) {
    if (is.null(title)&is.function(title)==FALSE) {
      title <- stringr::str_extract(
        string = url,
        pattern = "(?<=https://[[a-z]][[a-z]].wikipedia.org/wiki/).*"
      )
    } else {
      usethis::ui_stop(x = "Either url or title must be provided, not both.")
    }

    language <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
    )
  }


  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_qid_of_wikipedia_page(
      title = title,
      language = language,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      return(db_result %>%
               tibble::as_tibble())
    }
  }

  Sys.sleep(time = wait)


  json_url <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops"
  )

  wikidata_id_l <- jsonlite::read_json(path = json_url)


  wikipedia_id <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageid"
    ) %>%
    dplyr::if_else(condition = is.null(.),
                   true = as.integer(NA),
                   false = .)

  wikidata_id <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageprops",
      "wikibase_item"
    ) %>%
    dplyr::if_else(condition = is.null(.),
                   true = as.character(NA),
                   false = .)

  description <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageprops",
      "wikibase-shortdesc"
    ) %>%
    dplyr::if_else(condition = is.null(.),
                   true = as.character(NA),
                   false = .)

  disambiguation <- is.element(el = "disambiguation",
             set = wikidata_id_l %>%
    purrr::pluck(
      "query",
      "pages",
      1,
      "pageprops"
    ) %>%
    names())

  normalised <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "normalized",
      1,
      "to"
    ) %>%
    dplyr::if_else(condition = is.null(.),
                   true = as.character(NA),
                   false = .)

  redirected <- wikidata_id_l %>%
    purrr::pluck(
      "query",
      "redirects",
      1,
      "to"
    ) %>%
    dplyr::if_else(condition = is.null(.),
                   true = as.character(NA),
                   false = .)

  wikipedia_title <- dplyr::case_when(
    is.na(redirected)==FALSE ~ redirected,
    is.na(normalised)==FALSE ~ normalised,
    is.na(wikipedia_id)==TRUE ~ as.character(NA),
    TRUE ~ title)

  df <- tibble::tibble(
    title = as.character(title),
    wikipedia_title = as.character(wikipedia_title),
    wikipedia_id = as.integer(wikipedia_id),
    qid = as.character(wikidata_id),
    description = as.character(description),
    disambiguation = as.logical(disambiguation),
    language = as.character(language)
  )

  if (tw_check_cache(cache) == TRUE) {
    tw_write_qid_of_wikipedia_page_to_cache(
      df = df,
      cache_connection = cache_connection,
      language = language,
      overwrite_cache = overwrite_cache,
      disconnect_db = disconnect_db
    )
  }

  df
}


#' Get all Wikidata Q identifiers of all Wikipedia pages that appear in a given page
#'
#' @param url Full URL to a Wikipedia page. If given, title and language can be left empty.
#' @param title Title of a Wikipedia page or final parts of its url. If given, url can be left empty, but language must be provided.
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#'
#' @return A data frame (a tibble) with four columns: `wikipedia_title`, `wikipedia_id`, `wikidata_id`, `wikidata_description`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_links_from_wikipedia_page(title = "Margaret Mead", language = "en")
#' }
tw_get_links_from_wikipedia_page <- function(url = NULL,
                                             title = NULL,
                                             language = tidywikidatar::tw_get_language()) {
  api_url <- stringr::str_c(
    tw_get_wikipedia_base_api_url(
      url = url,
      title = title,
      language = language
    ),
    "&prop=pageprops&generator=links&gpllimit=500"
  )


  base_json <- jsonlite::read_json(api_url)

  continue_check <- base_json %>%
    purrr::pluck("continue", "gplcontinue")

  all_jsons <- list()
  page_number <- 1

  all_jsons[[page_number]] <- base_json

  while (is.null(continue_check) == FALSE & page_number < 200) {
    page_number <- page_number + 1
    base_json <- jsonlite::read_json(stringr::str_c(
      api_url,
      "&gplcontinue=",
      continue_check
    ))

    all_jsons[[page_number]] <- base_json

    continue_check <- base_json %>%
      purrr::pluck("continue", "gplcontinue")
  }

  purrr::map(.x = all_jsons, .f = purrr::pluck, "query", "pages")

  all_pages <- purrr::map(.x = all_jsons, .f = purrr::pluck, "query", "pages") %>%
    purrr::flatten()

  purrr::map_dfr(
    .x = all_pages,
    .f = function(current_page) {
      tibble::tibble(
        wikidata_id = current_page %>%
          purrr::pluck(
            "pageprops",
            "wikibase_item"
          ),
        wikidata_description = current_page %>%
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
      .data$wikidata_id,
      .data$wikidata_description
    ) %>%
    dplyr::mutate(language = language)
}


#' Gets id of Wikipedia pages from local cache
#'
#' Mostly used internally.
#'
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection open.
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' tw_enable_cache()
#' tw_create_cache_folder(ask = FALSE)
#'
#' df_from_api <- tw_get_qid_of_wikipedia_page(title = "Margaret Mead", language = "en")
#'
#' df_from_cache <- tw_get_cached_qid_of_wikipedia_page(
#'   title = "Margaret Mead",
#'   language = "en"
#' )
#'
#' df_from_cache
tw_get_cached_qid_of_wikipedia_page <- function(title,
                                                language = tidywikidatar::tw_get_language(),
                                                cache_connection = NULL,
                                                disconnect_db = TRUE) {

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language
  )

  table_name <- tw_get_cache_table_name(
    type = "wikipedia_page",
    language = language
  )

  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(tidywikidatar::tw_empty_wikipedia_page)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(
        .data$title %in% stringr::str_c(title)
      ),
    error = function(e) {
      logical(1L)
    }
  )
  if (isFALSE(db_result)) {
    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
    return(tidywikidatar::tw_empty_wikipedia_page)
  }

  cached_df <- db_result %>%
    tibble::as_tibble() %>%
    dplyr::mutate(disambiguation = as.logical(.data$disambiguation))

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }
  cached_df
}



#' Write qualifiers to cache
#'
#' Mostly to be used internally by `tidywikidatar`, use with caution to keep caching consistent.
#'
#' @param df A data frame typically generated with `tw_get_qid_of_wikipedia_page()`.
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
#'
#' df <- tw_get_qid_of_wikipedia_page(
#'   title = "Margaret Mead",
#'   language = "en",
#'   cache = FALSE
#' )
#'
#' tw_write_qid_of_wikipedia_page_to_cache(
#'   df = df,
#'   language = "en"
#' )
tw_write_qid_of_wikipedia_page_to_cache <- function(df,
                                                    language = tidywikidatar::tw_get_language(),
                                                    overwrite_cache = FALSE,
                                                    cache_connection = NULL,
                                                    disconnect_db = TRUE) {
  db <- tw_connect_to_cache(connection = cache_connection,
                            language = language)

  table_name <- tw_get_cache_table_name(type = "wikipedia_page",
                                        language = language)

  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE title = {title*}",
                                  title = unique(df$title),
                                  table_name = table_name,
                                  .con = db
      )
      result <- DBI::dbExecute(
        conn = db,
        statement = statement
      )
    }
  }

  DBI::dbWriteTable(db,
                    name = table_name,
                    value = df,
                    append = TRUE
  )

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }
  invisible(df)
}

#' Reset qualifiers cache
#'
#' Removes the table where qualifiers are cached
#'
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
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
                                          cache_connection = NULL,
                                          disconnect_db = TRUE,
                                          ask = TRUE) {
  db <- tw_connect_to_cache(connection = cache_connection,
                            language = language)

  table_name <- tw_get_cache_table_name(type = "wikipedia_page",
                                        language = language)

  if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    DBI::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Wikipedia page cache reset for language ", sQuote(language), " completed"))
  } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from cache the qualifiers table for language: ", sQuote(language), "?"))) {
    DBI::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Wikipedia page cache reset for language ", sQuote(language), " completed"))
  }

  if (disconnect_db == TRUE) {
    DBI::dbDisconnect(db)
  }
}
