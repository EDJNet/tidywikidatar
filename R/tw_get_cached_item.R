#' Retrieve cached item
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to "all_available". By default, returns dataset with labels in all available languages. If given, only in the chosen language. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return If data present in cache, returns a data frame with cached data.
#' @export
#'
#' @examples
#'
#'
#' tw_set_cache_folder(path = tempdir())
#' tw_enable_cache()
#' tw_create_cache_folder(ask = FALSE)
#'
#' df_from_api <- tw_get(id = "Q180099", language = "en")
#'
#' df_from_cache <- tw_get_cached_item(
#'   id = "Q180099",
#'   language = "en"
#' )
tw_get_cached_item <- function(id,
                               language = "all_available") {
  tw_check_cache_folder()

  db_file <- tw_get_cache_file(
    type = "item",
    language = language
  )

  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db_file
  )
  db_result <- tryCatch(
    DBI::dbReadTable(
      conn = db,
      name = stringr::str_to_upper(id)
    ),
    error = function(e) {
      logical(1L)
    }
  )
  DBI::dbDisconnect(db)
  if (is.data.frame(db_result)) {
    return(db_result %>% tibble::as_tibble())
  }
}



#' Gets location of cache file
#'
#' @param type Defaults to "item". Type of cache file to output. Values typically used by `tidywikidatar` include "item", "search", and "qualifier".
#' @param language Defaults to "all_available". Use to limit the data to be cached. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector of length one with location of item cache file.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' sqlite_cache_file_location <- tw_get_cache_file() # outputs location of cache file
tw_get_cache_file <- function(type = "item",
                              language = "all_available") {
  db_folder <- fs::path(
    tidywikidatar::tw_get_cache_folder(),
    stringr::str_c(
      "tw_",
      type,
      "_db"
    )
  )
  fs::dir_create(db_folder)
  db_file <- fs::path(
    db_folder,
    stringr::str_c(
      "tw_",
      type,
      "_db_",
      language,
      ".sqlite"
    )
  )
  db_file
}


#' Check if given items are present in cache
#'
#' @param id A characther vector. Each element must start with Q, and correspond to a Wikidata identifier.
#' @param language Defaults to "all_available". Use to limit the data to be cached. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A character vector with IDs of items present in cache. If no item found in cache, returns NULL.
#' @export
#'
#' @examples
#'
#' tw_set_cache_folder(path = tempdir())
#' tw_enable_cache()
#' tw_create_cache_folder(ask = FALSE)
#'
#' # add three items to local cache
#' invisible(tw_get(id = "Q180099", language = "en"))
#' invisible(tw_get(id = "Q228822", language = "en"))
#' invisible(tw_get(id = "Q184992", language = "en"))
#'
#' # check if these other items are in cache
#' items_in_cache <- tw_check_cached_items(
#'   id = c(
#'     "Q180099",
#'     "Q228822",
#'     "Q76857"
#'   ),
#'   language = "en"
#' )
#' # it should return only the two items from the current list of id
#' # but not other item already in cache
#' items_in_cache
tw_check_cached_items <- function(id,
                                  language = "all_available") {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  tw_check_cache_folder()

  db_file <- tw_get_cache_file(
    type = "item",
    language = language
  )

  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db_file
  )
  db_result <- tryCatch(
    DBI::dbListTables(
      conn = db
    ),
    error = function(e) {
      NULL
    }
  )
  DBI::dbDisconnect(db)
  if (is.null(db_result) == FALSE) {
    return(db_result[is.element(db_result, id)])
  }
}
