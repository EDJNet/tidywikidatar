#' Get all Wikidata Q identifiers of all Wikipedia pages (or files, or subcategories) that are members of the given category,
#'
#' @param url Full URL to a Wikipedia category page. If given, title and language can be left empty.
#' @param category Title of a Wikipedia category page or final parts of its url. Must include "Category:", or equivalent in other languages. If given, url can be left empty, but language must be provided.
#' @param type Defaults to "page", defines which kind of members of a category are returned. Valid values include "page", "file", and "subcat" (for sub-category). Corresponds to `cmtype`. For details, see \url{https://www.mediawiki.org/wiki/API:Categorymembers}
#' @param language Two-letter language code used to define the Wikipedia version to use. Defaults to language set with `tw_set_language()`; if not set, "en". If url given, this can be left empty.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 1 due to time-outs with frequent queries. Time to wait between queries to the APIs. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param attempts Defaults to 10. Number of times it re-attempts to reach the API before failing.
#'
#' @return A data frame (a tibble) with eight columns: `source_title_url`, `source_wikipedia_title`, `source_qid`, `wikipedia_title`, `wikipedia_id`, `qid`, `description`, and `language`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   sub_categories <- tw_get_wikipedia_category_members(
#'     category = "Category:American women anthropologists",
#'     type = "subcat"
#'   )
#'
#'   sub_categories
#'
#'   tw_get_wikipedia_category_members(
#'     category = sub_categories$wikipedia_title,
#'     type = "page"
#'   )
#' }
tw_get_wikipedia_category_members <- function(url = NULL,
                                              category = NULL,
                                              type = "page",
                                              language = tidywikidatar::tw_get_language(),
                                              cache = NULL,
                                              overwrite_cache = FALSE,
                                              cache_connection = NULL,
                                              disconnect_db = TRUE,
                                              wait = 1,
                                              attempts = 10) {
  if (is.null(category) == TRUE & is.null(url) == FALSE) {
    language <- stringr::str_extract(
      string = url,
      pattern = "(?<=https://)[[a-z]][[a-z]](?=.wikipedia.org/)"
    )
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  wikipedia_page_qid_df <- tw_get_wikipedia_page_qid(
    title = category,
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
      source_wikipedia_category = .data$wikipedia_title,
      source_qid = .data$qid,
      language = .data$language
    ) %>%
    dplyr::distinct(.data$source_title_url, .keep_all = TRUE)

  wikipedia_category_members_df <- purrr::map_dfr(
    .x = seq_along(source_df$source_wikipedia_category),
    .f = function(i) {
      current_slice_df <- source_df %>%
        dplyr::slice(i)

      linked_df <- tw_get_wikipedia_category_members_single(
        category = current_slice_df$source_title_url,
        language = current_slice_df$language,
        url = NULL,
        type = type,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = FALSE,
        wait = wait,
        attempts = attempts
      )

      linked_df
    }
  )

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  wikipedia_category_members_df
}


#' Get all Wikidata Q identifiers of all Wikipedia pages that appear in a given page
#'
#' @inheritParams tw_get_wikipedia_category_members
#'
#' @return A data frame (a tibble) with four columns: `wikipedia_title`, `wikipedia_id`, `wikidata_id`, `wikidata_description`.
#'
#' @examples
#' if (interactive()) {
#'   tidywikidatar:::tw_get_wikipedia_category_members_single(
#'     category = "Category:American women anthropologists",
#'     type = "subcat"
#'   )
#'
#'   tidywikidatar:::tw_get_wikipedia_category_members_single(
#'     category = "Category:Puerto Rican women anthropologists",
#'     type = "page"
#'   )
#' }
tw_get_wikipedia_category_members_single <- function(url = NULL,
                                                     category = NULL,
                                                     type = "page",
                                                     language = tidywikidatar::tw_get_language(),
                                                     cache = NULL,
                                                     overwrite_cache = FALSE,
                                                     cache_connection = NULL,
                                                     disconnect_db = TRUE,
                                                     wait = 1,
                                                     attempts = 10) {
  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (is.null(category)) {
    category <- stringr::str_extract(
      string = url,
      pattern = "wiki/.*$"
    ) %>%
      stringr::str_remove(pattern = stringr::fixed("wiki/"))
  }

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_wikipedia_category_members(
      category = category,
      type = type,
      language = language,
      cache = cache,
      cache_connection = db,
      disconnect_db = FALSE
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      category_df <- db_result %>%
        dplyr::collect() %>%
        dplyr::distinct()
    } else {
      category_df <- tidywikidatar::tw_empty_wikipedia_category_members
    }
  } else {
    category_df <- tidywikidatar::tw_empty_wikipedia_category_members
  }

  if (nrow(category_df) == 0) {
    json_url <- stringr::str_c(
      tw_get_wikipedia_base_api_url(
        url = url,
        title = category,
        language = language,
        type = "category"
      ),
      "&cmlimit=max&cmtype=",
      type
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
    } else if (length(api_result) == 1) {
      usethis::ui_stop("Page not found. Make sure that language parameter is consistent with the language of the input title or url.")
    } else {
      base_json <- api_result
    }

    continue_check <- base_json %>%
      purrr::pluck(
        "continue",
        "cmcontinue"
      )

    all_jsons <- list()

    page_number <- 1

    all_jsons[[page_number]] <- base_json

    while (is.null(continue_check) == FALSE & page_number < 1000) {
      page_number <- page_number + 1

      json_url <- stringr::str_c(
        json_url,
        "&cmcontinue=",
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
          "cmcontinue"
        )
    }

    all_pages <- purrr::map(
      .x = all_jsons,
      .f = purrr::pluck,
      "query",
      "categorymembers"
    ) %>%
      purrr::flatten()

    page_title_df <- purrr::map_dfr(
      .x = all_pages,
      .f = function(current_page) {
        tibble::tibble(
          wikipedia_title = current_page %>%
            purrr::pluck(
              "title"
            ),
          wikipedia_id = current_page %>%
            purrr::pluck(
              "pageid"
            )
        )
      }
    )

    category_df <- page_title_df %>%
      dplyr::transmute(
        category = category,
        .data$wikipedia_title,
        wikipedia_id = as.numeric(.data$wikipedia_id)
      )

    if (tw_check_cache(cache) == TRUE) {
      tw_write_wikipedia_category_members_to_cache(
        df = category_df,
        type = type,
        cache_connection = db,
        language = language,
        overwrite_cache = overwrite_cache,
        disconnect_db = disconnect_db
      )
    }
  }


  wikipedia_page_qid_df <- tw_get_wikipedia_page_qid(
    title = category_df$wikipedia_title,
    language = language,
    url = NULL,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = FALSE,
    wait = wait,
    attempts = attempts
  )


  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  wikipedia_page_qid_df
}





#' Gets members of Wikipedia categories from local cache
#'
#' Mostly used internally.
#'
#' @inheritParams tw_get_wikipedia_category_members
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
#'   df_from_api <- tw_get_wikipedia_page_qid(category = "Margaret Mead", language = "en")
#'
#'   df_from_cache <- tw_get_cached_wikipedia_category_members(
#'     category = "Margaret Mead",
#'     language = "en"
#'   )
#'
#'   df_from_cache
#' }
tw_get_cached_wikipedia_category_members <- function(category,
                                                     type = "page",
                                                     language = tidywikidatar::tw_get_language(),
                                                     cache = NULL,
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
    type = stringr::str_c("wikipedia_category_members_", type),
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
    return(tidywikidatar::tw_empty_wikipedia_category_members)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(
        .data$category %in% !!stringr::str_c(category)
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
    return(tidywikidatar::tw_empty_wikipedia_category_members)
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
#' Mostly used internally by `tidywikidatar`, use with caution to keep caching consistent.
#'
#' @param df A data frame typically generated with `tw_get_wikipedia_category_members()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param type Defaults to "page", defines which kind of members of a category are returned. Valid values include "page", "file", and "subcat" (for sub-category). Corresponds to `cmtype`. For details, see \url{https://www.mediawiki.org/wiki/API:Categorymembers}
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return Silently returns the same data frame provided as input. Mostly used internally for its side effects.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df <- tw_get_wikipedia_category_members(
#'     category = "American women anthropologists",
#'     language = "en",
#'     cache = FALSE
#'   )
#'
#'   tw_write_wikipedia_category_members_to_cache(
#'     df = df,
#'     language = "en"
#'   )
#' }
tw_write_wikipedia_category_members_to_cache <- function(df,
                                                         language = tidywikidatar::tw_get_language(),
                                                         type = "page",
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
    type = stringr::str_c("wikipedia_category_members_", type),
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE category = {category*}",
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


  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  invisible(df)
}

#' Reset Wikipedia category members cache
#'
#' Removes from cache the table where data typically gathered with `tw_get_wikipedia_category_members()` are stored.
#'
#' @param type Defaults to "page", defines which kind of members of a category are returned. Valid values include "page", "file", and "subcat" (for sub-category). Corresponds to `cmtype`. For details, see \url{https://www.mediawiki.org/wiki/API:Categorymembers}
#'
#' @inheritParams tw_reset_wikipedia_page_links_cache
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_reset_wikipedia_category_members_cache()
#' }
tw_reset_wikipedia_category_members_cache <- function(language = tidywikidatar::tw_get_language(),
                                                      type = "page",
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
    type = stringr::str_c("wikipedia_category_members_", type),
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Wikipedia category members cache reset for language ", sQuote(language), " completed"))
  } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from cache the Wikipedia category members cache for language: ", sQuote(language), "?"))) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Wikipedia category members cache reset for language ", sQuote(language), " completed"))
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
}
