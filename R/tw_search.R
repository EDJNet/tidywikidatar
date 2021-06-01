#' Search for Wikidata items or properties and return Wikidata id, label, and description.
#'
#' This search returns only items, use `tw_search_property()` for properties.
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
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10). Four columns when `include_search` is set to TRUE.
#' @export
#'
#' @examples
#' tw_search(search = "Sylvia Pankhurst")
tw_search <- function(search,
                      type = "item",
                      language = tidywikidatar::tw_get_language(),
                      limit = 10,
                      include_search = FALSE,
                      wait = 0,
                      cache = NULL,
                      overwrite_cache = FALSE,
                      cache_connection = NULL) {
  if (is.null(search)) {
    usethis::ui_stop("A search string must be given.")
  }

  if (is.null(language)) {
    usethis::ui_stop("A search language must be given.")
  }

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_search(
      search = search,
      type= type,
      include_search = include_search,
      language = language,
      cache_connection = cache_connection
    )
    if (is.data.frame(db_result)&nrow(db_result)>0) {
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
    dplyr::select(search, id, label, description)

  if (tw_check_cache(cache) == TRUE) {
    tw_write_search_to_cache(
      search_df = search_response_df,
      type = type,
      language = language,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection
    )
  }

  if (include_search==TRUE) {
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
                               cache_connection = NULL) {
  tw_search(
    search = search,
    type = "property",
    language = language,
    limit = limit,
    include_search = include_search,
    wait = wait,
    cache = cache,
    overwrite_cache = overwrite_cache
  )
}
