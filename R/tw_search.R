#' Search for Wikidata items or properties and return Wikidata id, label, and description.
#'
#' This search returns only items, use `tw_search_property()` for properties.
#'
#' @param search A string to be searched in Wikidata
#' @param type Defaults to "item". Either "item" or "property".
#' @param language Language to be used for the search. For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10).
#' @export
#'
#' @examples
#' tw_search(search = "Sylvia Pankhurst")
tw_search <- function(search,
                      type = "item",
                      language = "en",
                      limit = 10,
                      wait = 0,
                      cache = NULL,
                      overwrite_cache = FALSE) {
  if (is.null(search)) {
    usethis::ui_stop("A search string must be given.")
  }

  if (tw_check_cache(cache) == TRUE) {
    tidywikidatar::tw_create_cache_folder()
    db_folder <- fs::path(
      tidywikidatar::tw_get_cache_folder(),
      stringr::str_c("wiki_search_", type, "_db")
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c(language, ".sqlite")
    )
    db <- DBI::dbConnect(drv = RSQLite::SQLite(), db_file)
    RSQLite::sqliteSetBusyHandler(dbObj = db, handler = 5000)
    db_result <- tryCatch(
      DBI::dbReadTable(
        conn = db,
        name = stringr::str_to_lower(search)
      ),
      error = function(e) {
        logical(1L)
      }
    )
    if (is.data.frame(db_result) & overwrite_cache == FALSE) {
      DBI::dbDisconnect(db)
      return(db_result %>%
        dplyr::filter(is.na(.data$id) == FALSE) %>%
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
          id = NA,
          label = NA,
          description = NA
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
          id = NA,
          label = NA,
          description = NA
        )
      }
    )
  }

  if (length(search_response) == 0) {
    search_response_df <- tibble::tibble(
      id = NA,
      label = NA,
      description = NA
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

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_lower(string = search),
      value = search_response_df
    )
    DBI::dbDisconnect(db)
  } else if (tw_check_cache(cache) == TRUE & overwrite_cache == TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_lower(string = search),
      value = search_response_df,
      overwrite = TRUE
    )
  }
  search_response_df %>%
    dplyr::filter(is.na(.data$id) == FALSE) %>%
    tibble::as_tibble()
}

#' Search for Wikidata properties in Wikidata and return Wikidata id, label, and description.
#'
#' This search returns only items, use `tw_search_property()` for properties.
#'
#' @param search A string to be searched in Wikidata
#' @param language Language to be used for the search. For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param wait In seconds, defaults to 0.1. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10).
#' @export
#'
#' @examples
#' tw_search_item(search = "Sylvia Pankhurst")
tw_search_item <- function(search,
                           language = "en",
                           limit = 10,
                           wait = 0,
                           cache = NULL,
                           overwrite_cache = FALSE) {
  tw_search(
    search = search,
    type = "item",
    language = language,
    limit = limit,
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
#' @param language Language to be used for the search. For a full list, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param limit Maximum numbers of responses to be given.
#' @param wait In seconds, defaults to 0.1. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Defaults to FALSE. If TRUE, overwrites cache.
#'
#' @return A data frame (a tibble) with three columns (id, label, and description), and as many rows as there are results (by default, limited to 10).
#' @export
#'
#' @examples
#' tw_search_property(search = "gender")
tw_search_property <- function(search,
                               language = "en",
                               limit = 10,
                               wait = 0,
                               cache = NULL,
                               overwrite_cache = FALSE) {
  tw_search(
    search = search,
    type = "property",
    language = language,
    limit = limit,
    wait = wait,
    cache = cache,
    overwrite_cache = overwrite_cache
  )
}
