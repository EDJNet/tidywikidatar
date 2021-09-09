#' Search for Wikidata items or properties and return Wikidata id, label, and description.
#'
#' This search returns only items, use `tw_search_property()` for properties.
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Language to be used for the search. Can be set once per session with `tw_set_language()`. If not set, defaults to "en". For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param include_search Logical, defaults to FALSE. If TRUE, the search is returned as an additional column.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10). Four columns when `include_search` is set to TRUE.
#' @export
#'
#' @examples
#' tw_search_single(search = "Sylvia Pankhurst")
tw_search_single <- function(search,
                             type = "item",
                             language = tidywikidatar::tw_get_language(),
                             limit = 10,
                             include_search = FALSE,
                             cache = NULL,
                             overwrite_cache = FALSE,
                             cache_connection = NULL,
                             disconnect_db = TRUE,
                             wait = 0) {
  if (is.null(search)) {
    usethis::ui_stop("A search string must be given.")
  }

  if (is.null(language)) {
    usethis::ui_stop("A search language must be given.")
  }

  if (length(search) > 1) {
    usethis::ui_stop("`tw_search_single()` requires `search` of length 1. Consider using `tw_search()`.")
  }

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_search(
      search = search,
      type = type,
      include_search = include_search,
      language = language,
      cache_connection = cache_connection
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      return(db_result %>%
        tibble::as_tibble())
    }
  }

  Sys.sleep(time = wait)

  if (type == "item") {
    search_response <- tryCatch(
      WikidataR::find_item(
        search_term = search,
        language = language,
        limit = limit
      ),
      error = function(e) {
        warning(e)
        tibble::tibble(
          id = as.character(NA),
          label = as.character(NA),
          description = as.character(NA)
        )
      }
    )
  } else if (type == "property") {
    search_response <- tryCatch(
      WikidataR::find_property(
        search_term = search,
        language = language,
        limit = limit
      ),
      error = function(e) {
        warning(e)
        tibble::tibble(
          id = as.character(NA),
          label = as.character(NA),
          description = as.character(NA)
        )
      }
    )
  }

  if (length(search_response) == 0) {
    search_response_df <- tibble::tibble(
      id = as.character(NA),
      label = as.character(NA),
      description = as.character(NA)
    )
  } else if (tibble::is_tibble(search_response) == TRUE) {
    search_response_df <- search_response
  } else {
    search_response_df <- purrr::map_dfr(
      .x = search_response,
      .f = function(x) {
        tibble::tibble(
          id = x %>% purrr::pluck("id"),
          label = dplyr::if_else(
            condition = is.null(x %>% purrr::pluck("label")),
            true = as.character(NA),
            false = x %>% purrr::pluck("label")
          ),
          description = dplyr::if_else(
            condition = is.null(x %>% purrr::pluck("description")),
            true = as.character(NA),
            false = x %>% purrr::pluck("description")
          )
        )
      }
    )
  }
  search_response_df <- search_response_df %>%
    dplyr::mutate(search = search) %>%
    dplyr::select(.data$search, .data$id, .data$label, .data$description)

  if (tw_check_cache(cache) == TRUE) {
    tw_write_search_to_cache(
      search_df = search_response_df,
      type = type,
      language = language,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection
    )
  }
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db
  )

  if (include_search == TRUE) {
    search_response_df %>%
      dplyr::filter(is.na(.data$id) == FALSE) %>%
      tibble::as_tibble()
  } else {
    search_response_df %>%
      dplyr::filter(is.na(.data$id) == FALSE) %>%
      dplyr::select(-search) %>%
      tibble::as_tibble()
  }
}




#' Search for Wikidata items or properties and return Wikidata id, label, and description.
#'
#' By defaults, this search returns items. Set `type` to property or use `tw_search_property()` for properties.
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Language to be used for the search. Can be set once per session with `tw_set_language()`. If not set, defaults to "en". For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param include_search Logical, defaults to FALSE. If TRUE, the search is returned as an additional column.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10). Four columns when `include_search` is set to TRUE.
#' @export
#'
#' @examples
#' tw_search(search = c("Margaret Mead", "Ruth Benedict"))
tw_search <- function(search,
                      type = "item",
                      language = tidywikidatar::tw_get_language(),
                      limit = 10,
                      include_search = FALSE,
                      wait = 0,
                      cache = NULL,
                      overwrite_cache = FALSE,
                      cache_connection = NULL,
                      disconnect_db = TRUE) {
  if (is.null(search)) {
    usethis::ui_stop("A search string must be given.")
  }

  if (length(search) == 0) {
    stop("`tw_search()` requires `search` of length 1 or more.")
  }

  if (is.null(language)) {
    usethis::ui_stop("A search language must be given.")
  }

  unique_search <- unique(search)

  if (length(unique_search) == 1) {
    search_df <-
      dplyr::left_join(
        x = tibble::tibble(search = search),
        y = tw_search_single(
          search = search,
          type = type,
          language = language,
          limit = limit,
          include_search = TRUE,
          wait = wait,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection
        ),
        by = "search"
      )

    if (include_search == TRUE) {
      return(search_df)
    } else {
      return(search_df %>%
        dplyr::select(-.data$search))
    }
  } else if (length(unique_search) > 1) {
    if (overwrite_cache == TRUE | tw_check_cache(cache) == FALSE) {
      pb <- progress::progress_bar$new(total = length(unique_search))
      search_df <- dplyr::left_join(
        x = tibble::tibble(search = search),
        y = purrr::map_dfr(
          .x = unique_search,
          .f = function(x) {
            pb$tick()
            tw_search_single(
              search = x,
              type = type,
              language = language,
              limit = limit,
              include_search = TRUE,
              wait = wait,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = cache_connection
            )
          }
        ),
        by = "search"
      )

      if (include_search == TRUE) {
        return(search_df)
      } else {
        return(search_df %>%
          dplyr::select(-.data$search))
      }
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      search_from_cache_df <- tw_get_cached_search(
        search = unique_search,
        type = type,
        language = language,
        include_search = TRUE,
        cache_connection = cache_connection
      )

      search_not_in_cache_v <- unique_search[!is.element(unique_search, search_from_cache_df$search)]

      if (length(search_not_in_cache_v) == 0) {
        search_df <- dplyr::left_join(
          x = tibble::tibble(search = search),
          y = search_from_cache_df,
          by = "search"
        )
        if (include_search == TRUE) {
          return(search_df)
        } else {
          return(search_df %>%
            dplyr::select(-.data$search))
        }
      } else if (length(search_not_in_cache_v) > 0) {
        pb <- progress::progress_bar$new(total = length(search_not_in_cache_v))
        items_not_in_cache_df <- purrr::map_dfr(
          .x = search_not_in_cache_v,
          .f = function(x) {
            pb$tick()
            tw_search_single(
              search = x,
              type = type,
              language = language,
              limit = limit,
              include_search = TRUE,
              wait = wait,
              cache = cache,
              overwrite_cache = overwrite_cache,
              cache_connection = cache_connection
            )
          }
        )

        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = cache_connection,
          disconnect_db = disconnect_db
        )

        search_merged_df <- dplyr::left_join(
          x = tibble::tibble(search = search),
          y = dplyr::bind_rows(
            search_from_cache_df,
            items_not_in_cache_df
          ),
          by = "search"
        )


        if (include_search == TRUE) {
          search_merged_df
        } else {
          search_merged_df %>%
            dplyr::select(-.data$search)
        }
      }
    }
  }
}

#' Search for Wikidata properties in Wikidata and return Wikidata id, label, and description.
#'
#' This search returns only items, use `tw_search_property()` for properties.
#'
#' @param search A string to be searched in Wikidata
#' @param language Language to be used for the search. Can be set once per session with `tw_set_language()`. If not set, defaults to "en". For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param include_search Logical, defaults to FALSE. If TRUE, the search is returned as an additional column.
#' @param wait In seconds, defaults to 0.1. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10).
#' @export
#'
#' @examples
#' tw_search_item(search = "Sylvia Pankhurst")
tw_search_item <- function(search,
                           language = tidywikidatar::tw_get_language(),
                           limit = 10,
                           include_search = FALSE,
                           wait = 0,
                           cache = NULL,
                           overwrite_cache = FALSE,
                           cache_connection = cache_connection) {
  tw_search(
    search = search,
    type = "item",
    language = language,
    limit = limit,
    include_search = include_search,
    wait = wait,
    cache = cache,
    overwrite_cache = overwrite_cache
  )
}



#' Search for Wikidata properties in Wikidata and return Wikidata id, label, and description.
#'
#' This search returns only properties, use `tw_search_items()` for properties.
#'
#' @param search A string to be searched in Wikidata.
#' @param language Language to be used for the search. Can be set once per session with `tw_set_language()`. If not set, defaults to "en". For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param include_search Logical, defaults to FALSE. If TRUE, the search is returned as an additional column.
#' @param wait In seconds, defaults to 0.1. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10).
#' @export
#'
#' @examples
#' tw_search_property(search = "gender")
tw_search_property <- function(search,
                               language = tidywikidatar::tw_get_language(),
                               limit = 10,
                               include_search = FALSE,
                               wait = 0,
                               cache = NULL,
                               overwrite_cache = FALSE,
                               disconnect_db = TRUE,
                               cache_connection = NULL) {
  tw_search(
    search = search,
    type = "property",
    language = language,
    limit = limit,
    include_search = include_search,
    wait = wait,
    cache = cache,
    disconnect_db = disconnect_db,
    overwrite_cache = overwrite_cache
  )
}
