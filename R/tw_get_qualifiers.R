#' Get Wikidata qualifiers for a given property of a given item
#'
#' N.B. In order to provide for consistently structured output, this function outputs either id or value for each  qualifier. The user should keep in mind that some of these come with additional detail (e.g. the unit, precision, or reference calendar).
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param id_l Defaults to NULL. If given, must be an object or list such as the one generated with `WikidataR::get_item()`. If given, and the requested id is actually present in `id_l`, then no query to Wikidata servers is made.
#'
#' @return A data frame (a tibble) with eight columns: `id` for the input id, `property`,  `qualifier_id`, `qualifier_property`, `qualifier_value`, `rank`, `qualifier_value_type`, and `set` (to distinguish sets of data when a property is present more than once)
#'
#' @examples
#' if (interactive()) {
#'   tidywikidatar:::tw_get_qualifiers_single(id = "Q180099", p = "P26", language = "en")
#' }
#'
#' #' ## using `tw_test_items` in examples in order to show output without calling
#' ## on Wikidata servers
#'
#' tidywikidatar:::tw_get_qualifiers_single(
#'   id = "Q180099",
#'   p = "P26",
#'   language = "en",
#'   id_l = tw_test_items
#' )
tw_get_qualifiers_single <- function(id,
                                     p,
                                     language = tidywikidatar::tw_get_language(),
                                     cache = NULL,
                                     overwrite_cache = FALSE,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE,
                                     wait = 0,
                                     id_l = NULL) {
  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_qualifiers(
      id = id,
      p = p,
      language = language,
      cache = cache,
      cache_connection = cache_connection,
      disconnect_db = FALSE
    )

    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = cache_connection,
        disconnect_db = disconnect_db,
        language = language
      )


      return(db_result %>%
        tibble::as_tibble())
    }
  }

  Sys.sleep(time = wait)

  if (is.null(id_l) == FALSE) {
    w <- id_l[purrr::map_chr(
      .x = id_l,
      .f = function(x) {
        purrr::pluck(x, "id")
      }
    ) %in% id]

    if (length(w) == 0) {
      w <- tryCatch(WikidataR::get_item(id = id),
        error = function(e) {
          as.character(e[[1]])
        }
      )
    } else if (length(w) > 1) {
      w <- w[1]
    }
  } else {
    w <- tryCatch(WikidataR::get_item(id = id),
      error = function(e) {
        as.character(e[[1]])
      }
    )
  }

  if (is.character(w)) {
    usethis::ui_oops(w)
    return(tidywikidatar::tw_empty_qualifiers)
  } else {
    qualifiers_df <- tw_extract_qualifier(id = id, p = p, w = w)
  }

  if (nrow(qualifiers_df) == 0) {
    # keep one row, otherwise nothing remains in cache, and it will query
    # each time the script is re-run
    qualifiers_df <- tibble::tibble(
      id = as.character(id),
      property = as.character(p),
      qualifier_id = as.character(NA),
      qualifier_property = as.character(NA),
      qualifier_value = as.character(NA),
      qualifier_value_type = as.character(NA),
      rank = as.character(NA),
      set = as.numeric(NA)
    )
  }

  if (tw_check_cache(cache) == TRUE) {
    tw_write_qualifiers_to_cache(
      qualifiers_df = qualifiers_df,
      language = language,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )
  }

  qualifiers_df
}




#' Get Wikidata qualifiers for a given property of a given item
#'
#' N.B. In order to provide for consistently structured output, this function outputs either id or value for each  qualifier. The user should keep in mind that some of these come with additional detail (e.g. the unit, precision, or reference calendar).
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param id_l Defaults to NULL. If given, must be an object or list such as the one generated with `WikidataR::get_item()`. If given, and the requested id is actually present in `id_l`, then no query to Wikidata servers is made.
#'
#' @return A data frame (a tibble) with eight columns: `id` for the input id, `property`,  `qualifier_id`, `qualifier_property`, `qualifier_value`, `rank`, `qualifier_value_type`, and `set` (to distinguish sets of data when a property is present more than once)
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tidywikidatar::tw_get_qualifiers(id = "Q180099", p = "P26", language = "en")
#' }
#'
#' #' ## using `tw_test_items` in examples in order to show output without calling
#' ## on Wikidata servers
#'
#' tidywikidatar::tw_get_qualifiers(
#'   id = "Q180099",
#'   p = "P26",
#'   language = "en",
#'   id_l = tw_test_items
#' )
tw_get_qualifiers <- function(id,
                              p,
                              language = tidywikidatar::tw_get_language(),
                              cache = NULL,
                              overwrite_cache = FALSE,
                              cache_connection = NULL,
                              disconnect_db = TRUE,
                              wait = 0,
                              id_l = NULL) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (length(id) == 0 | length(p) == 0) {
    usethis::ui_stop("`tw_get_qualifiers()` requires `id` and `p` of length 1 or more.")
  } else if (length(id) == 1 & length(p) == 1) {
    return(tw_get_qualifiers_single(
      id = id,
      p = p,
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      wait = wait,
      id_l = id_l
    ))
  } else if (length(id) > 1 | length(p) > 1) {
    if (overwrite_cache == TRUE | tw_check_cache(cache) == FALSE) {
      pb <- progress::progress_bar$new(total = length(id) * length(p))

      qualifiers_df <- purrr::map2_dfr(
        .x = id,
        .y = p,
        .f = function(x, y) {
          pb$tick()
          tw_get_qualifiers_single(
            id = x,
            p = y,
            language = language,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = db,
            disconnect_db = FALSE,
            wait = wait,
            id_l = id_l
          )
        }
      )
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
      return(qualifiers_df)
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      qualifiers_from_cache_df <- tw_get_cached_qualifiers(
        id = id,
        p = p,
        language = language,
        cache_connection = db,
        disconnect_db = FALSE
      )

      not_in_cache_df <- tibble::tibble(id = id, property = p) %>%
        dplyr::anti_join(qualifiers_from_cache_df,
          by = c("id", "property")
        )


      if (nrow(not_in_cache_df) == 0) {
        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          language = language
        )
        return(
          dplyr::left_join(
            x = tibble::tibble(id = id),
            y = qualifiers_from_cache_df,
            by = "id"
          )
        )
      } else if (nrow(not_in_cache_df) > 0) {
        pb <- progress::progress_bar$new(total = nrow(not_in_cache_df))

        qualifiers_not_in_cache_df <- purrr::map2_dfr(
          .x = unique(not_in_cache_df$id),
          .y = unique(not_in_cache_df$property),
          .f = function(x, y) {
            pb$tick()
            tw_get_qualifiers_single(
              id = x,
              p = y,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = db,
              disconnect_db = FALSE,
              wait = wait,
              id_l = id_l
            )
          }
        )

        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          language = language
        )

        dplyr::left_join(
          x = tibble::tibble(id = id),
          y = dplyr::bind_rows(
            qualifiers_from_cache_df,
            qualifiers_not_in_cache_df
          ),
          by = "id"
        )
      }
    }
  }
}



#' Retrieve cached qualifier
#'
#' @param id A character vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
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
#' df_from_api <- tw_get_qualifiers(id = "Q180099", p = "P26", language = "en")
#'
#' df_from_cache <- tw_get_cached_qualifiers(
#'   id = "Q180099",
#'   p = "P26",
#'   language = "en"
#' )
#'
#' df_from_cache
tw_get_cached_qualifiers <- function(id,
                                     p,
                                     language = tidywikidatar::tw_get_language(),
                                     cache = NULL,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(tidywikidatar::tw_empty_qualifiers))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "qualifiers",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
    return(tidywikidatar::tw_empty_qualifiers)
  }

  db_result <- tryCatch(
    dplyr::tbl(src = db, table_name) %>%
      dplyr::filter(
        .data$id %in% !!stringr::str_to_upper(id),
        .data$property %in% !!stringr::str_to_upper(p)
      ),
    error = function(e) {
      logical(1L)
    }
  )
  if (isFALSE(db_result)) {
    tw_disconnect_from_cache(
      cache = cache,
      cache_connection = db,
      disconnect_db = disconnect_db,
      language = language
    )
    return(tidywikidatar::tw_empty_qualifiers)
  } else if (isFALSE(identical(colnames(tidywikidatar::tw_empty_qualifiers), colnames(db_result)))) {
    usethis::ui_stop("The cache has been generated with a previous version of `tidywikidatar` that is not compatible with the current version. You may want to delete the old cache or reset just this table with {usethis::ui_code('tw_reset_qualifiers_cache()()')}")
  }

  cached_qualifiers_df <- db_result %>%
    tibble::as_tibble()

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  cached_qualifiers_df
}

#' Write qualifiers to cache
#'
#' Mostly to be used internally by `tidywikidatar`, use with caution to keep caching consistent.
#'
#' @param qualifiers_df A data frame typically generated with `tw_get_qualifiers()`.
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
#' q_df <- tw_get_qualifiers(
#'   id = "Q180099",
#'   p = "P26",
#'   language = "en",
#'   cache = FALSE
#' )
#'
#' tw_write_qualifiers_to_cache(
#'   qualifiers_df = q_df,
#'   language = "en",
#'   cache = TRUE
#' )
tw_write_qualifiers_to_cache <- function(qualifiers_df,
                                         language = tidywikidatar::tw_get_language(),
                                         cache = NULL,
                                         overwrite_cache = FALSE,
                                         cache_connection = NULL,
                                         disconnect_db = TRUE) {
  if (isFALSE(tw_check_cache(cache = cache))) {
    return(invisible(qualifiers_df))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  table_name <- tw_get_cache_table_name(
    type = "qualifiers",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (overwrite_cache == TRUE) {
      statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE id = {id*} AND property = {p*}",
        id = unique(qualifiers_df$id),
        p = unique(qualifiers_df$property),
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
    value = qualifiers_df,
    append = TRUE
  )


  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  invisible(qualifiers_df)
}

#' Reset qualifiers cache
#'
#' Removes the table where qualifiers are cached
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
#'   tw_reset_qualifiers_cache()
#' }
tw_reset_qualifiers_cache <- function(language = tidywikidatar::tw_get_language(),
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
    type = "qualifiers",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Qualifiers cache reset for language ", sQuote(language), " completed"))
  } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from cache the qualifiers table for language: ", sQuote(language), "?"))) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Qualifiers cache reset for language ", sQuote(language), " completed"))
  }

  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
}
