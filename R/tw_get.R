#' Return (most) information from a Wikidata item in a tidy format from a single Wikidata identifier
#'
#' @param read_cache Logical, defaults to TRUE. Mostly used internally to prevent checking if an item is in cache if it is already known that it is not in cache.
#'
#' @inheritParams tw_get
#'
#' @return A data.frame (a tibble) with four columns (id, property, value, and rank). If item not found or trouble connecting with the server, a data frame with four columns and zero rows is returned, with the warning as an attribute, which can be retrieved with `attr(output, "warning"))`
#'
#' @examples
#' if (interactive()) {
#'   tidywikidatar:::tw_get_single(
#'     id = "Q180099",
#'     language = "en"
#'   )
#' }
#'
#' #' ## using `tw_test_items` in examples in order to show output without calling
#' ## on Wikidata servers
#'
#' tidywikidatar:::tw_get_single(
#'   id = "Q180099",
#'   language = "en",
#'   id_l = tw_test_items
#' )
tw_get_single <- function(id,
                          language = tidywikidatar::tw_get_language(),
                          cache = NULL,
                          overwrite_cache = FALSE,
                          read_cache = TRUE,
                          cache_connection = NULL,
                          disconnect_db = TRUE,
                          wait = 0,
                          id_l = NULL) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  id <- tw_check_qid(id)

  if (length(id) > 1) {
    usethis::ui_stop("`tw_get_single()` requires `id` of length 1. Consider using `tw_get()`.")
  } else if (length(id) == 0) {
    return(tidywikidatar::tw_empty_item)
  }

  if (tw_check_cache(cache) == TRUE) {
    db <- tw_connect_to_cache(
      connection = cache_connection,
      language = language,
      cache = cache
    )
  }

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE & read_cache == TRUE) {
    db_result <- tw_get_cached_item(
      id = id,
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
      return(db_result %>%
        tibble::as_tibble())
    }
  }

  Sys.sleep(time = wait)

  if (is.null(id_l) == FALSE) {
    item <- id_l[purrr::map_chr(
      .x = id_l,
      .f = function(x) {
        purrr::pluck(x, "id")
      }
    ) %in% id]

    if (length(item) == 0) {
      item <- tryCatch(WikidataR::get_item(id = id),
        error = function(e) {
          as.character(e[[1]])
        }
      )
    } else if (length(item) > 1) {
      item <- item[1]
    }
  } else {
    item <- tryCatch(WikidataR::get_item(id = id),
      error = function(e) {
        as.character(e[[1]])
      }
    )
  }


  if (is.character(item)) {
    usethis::ui_oops(item)
    output <- tibble::tibble(
      id = as.character(id),
      property = "error",
      value = stringr::str_remove(
        string = item,
        pattern = "The API returned an error: "
      ),
      rank = NA_character_
    )

    if (tw_check_cache(cache) == TRUE) {
      tw_write_item_to_cache(
        item_df = output,
        language = language,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = disconnect_db
      )
    }

    attr(output, "warning") <- item
    return(output)
  }


  if (is.element(
    el = "redirect",
    set = item %>%
      purrr::pluck(1) %>%
      names()
  )) {
    id <- item %>%
      purrr::pluck(1, "redirect")
    return(
      tw_get(
        id = id,
        language = language,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = disconnect_db,
        wait = wait
      )
    )
  }

  everything_df <- tw_extract_single(
    w = item,
    language = language
  )

  # keep one row, otherwise nothing remains in cache, and it will query
  # each time the script is re-run
  if (nrow(everything_df) == 0) {
    everything_df <- tibble::tibble(
      id = id,
      property = stringr::str_c("label_", language),
      value = as.character(NA),
      rank = as.character(NA)
    )
  }

  if (tw_check_cache(cache) == TRUE) {
    tw_write_item_to_cache(
      item_df = everything_df,
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE
    )
  }
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
  everything_df
}

#' Return (most) information from a Wikidata item in a tidy format
#'
#' @param id A character vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param id_l Defaults to NULL. If given, must be an object or list such as the one generated with `WikidataR::get_item()`. If given, and the requested id is actually present in `id_l`, then no query to Wikidata servers is made.
#'
#' @return A data.frame (a tibble) with three columns (id, property, and value).
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   tw_get(
#'     id = c("Q180099", "Q228822"),
#'     language = "en"
#'   )
#' }
#'
#' ## using `tw_test_items` in examples in order to show output without calling
#' ## on Wikidata servers
#'
#' tw_get(
#'   id = c("Q180099", "Q228822"),
#'   language = "en",
#'   id_l = tw_test_items
#' )
tw_get <- function(id,
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

  unique_id <- tw_check_qid(id)

  if (length(unique_id) == 0) {
    return(tidywikidatar::tw_empty_item)
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (length(unique_id) == 1) {
    return(
      dplyr::left_join(
        x = tibble::tibble(id = id),
        y = tw_get_single(
          id = unique_id,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          wait = wait,
          id_l = id_l
        ),
        by = "id"
      )
    )
  } else if (length(unique_id) > 1) {
    if (overwrite_cache == TRUE | tw_check_cache(cache) == FALSE) {
      pb <- progress::progress_bar$new(total = length(unique_id))

      item_df <- purrr::map_dfr(
        .x = unique_id,
        .f = function(x) {
          pb$tick()
          tw_get_single(
            id = x,
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
      return(
        dplyr::left_join(
          x = tibble::tibble(id = id),
          y = item_df,
          by = "id"
        )
      )
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      items_from_cache_df <- tw_get_cached_item(
        id = unique_id,
        language = language,
        cache = cache,
        cache_connection = db,
        disconnect_db = FALSE
      )

      id_items_not_in_cache <- unique_id[!is.element(unique_id, items_from_cache_df$id)]

      if (length(id_items_not_in_cache) == 0) {
        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = db,
          disconnect_db = disconnect_db,
          language = language
        )
        return(
          dplyr::left_join(
            x = tibble::tibble(id = id),
            y = items_from_cache_df,
            by = "id"
          )
        )
      } else if (length(id_items_not_in_cache) > 0) {
        pb <- progress::progress_bar$new(total = length(id_items_not_in_cache))

        items_not_in_cache_df <- purrr::map_dfr(
          .x = id_items_not_in_cache,
          .f = function(x) {
            pb$tick()
            tw_get_single(
              id = x,
              language = language,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = db,
              read_cache = FALSE,
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
            items_from_cache_df,
            items_not_in_cache_df
          ),
          by = "id"
        ) %>%
          dplyr::filter(is.na(.data$id) == FALSE)
      }
    }
  }
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
#'   tw_reset_item_cache()
#' }
tw_reset_item_cache <- function(language = tidywikidatar::tw_get_language(),
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
    type = "item",
    language = language
  )

  if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
    # do nothing: if table does not exist, nothing to delete
  } else if (isFALSE(ask)) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Item cache reset for language ", sQuote(language), " completed"))
  } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from cache the items table for language: ", sQuote(language), "?"))) {
    pool::dbRemoveTable(conn = db, name = table_name)
    usethis::ui_info(paste0("Items cache reset for language ", sQuote(language), " completed"))
  }


  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )
}
