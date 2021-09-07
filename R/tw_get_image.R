
#' Get image from Wikimedia Commons
#'
#' Please consult the relevant documentation for reusing content outside Wikimedia: https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param format A charachter vector, defaults to 'filename'. If set to 'commons', outputs the link to the Wikimedia Commons page. If set to "embed", outputs a link that can be used to embed.
#' @param width A numeric value, defaults to NULL, relevant only if format is set to 'embed'. If not given, defaults to full resolution image.
#' @param language Needed for caching, defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#' tw_get_image("Q180099",
#'   format = "filename"
#' )
#'
#' tw_get_image("Q180099",
#'   format = "commons"
#' )
#'
#' tw_get_image("Q180099",
#'   format = "embed",
#'   width = 300
#' )
tw_get_image <- function(id,
                         format = "filename",
                         width = NULL,
                         language = tidywikidatar::tw_get_language(),
                         id_df = NULL,
                         cache = NULL,
                         overwrite_cache = FALSE,
                         cache_connection = NULL,
                         disconnect_db = TRUE,
                         wait = 0) {
  filename_df <- tw_get_property(
    id = id,
    p = "P18",
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  )

  if (is.null(filename_df)) {
    return(NULL)
  }

  filename <- filename_df %>%
    dplyr::pull(.data$value)

  if (format == "filename") {
    filename_df
  } else if (format == "commons") {
    stringr::str_c("https://commons.wikimedia.org/wiki/File:", filename)
  } else if (format == "embed") {
    if (is.null(width) == TRUE) {
      stringr::str_c(
        "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
        filename
      )
    } else {
      stringr::str_c(
        "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
        filename, "&width=", width
      )
    }
  } else {
    filename
  }
}

#' Get image from Wikimedia Commons
#'
#' Please consult the relevant documentation for reusing content outside Wikimedia: https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param format A charachter vector, defaults to 'filename'. If set to 'commons', outputs the link to the Wikimedia Commons page. If set to "embed", outputs a link that can be used to embed.
#' @param only_first Defaults to TRUE. If TRUE, returns only the first image associated with a given Wikidata id. If FALSE, returns all images available.
#' @param width A numeric value, defaults to NULL, relevant only if format is set to 'embed'. If not given, defaults to full resolution image.
#' @param language Needed for caching, defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#' tw_get_image("Q180099",
#'   format = "filename"
#' )
#'
#' tw_get_image("Q180099",
#'   format = "commons"
#' )
#'
#' tw_get_image("Q180099",
#'   format = "embed",
#'   width = 300
#' )
tw_get_image_same_length <- function(id,
                                     format = "filename",
                                     only_first = TRUE,
                                     width = NULL,
                                     language = tidywikidatar::tw_get_language(),
                                     id_df = NULL,
                                     cache = NULL,
                                     overwrite_cache = FALSE,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE,
                                     wait = 0) {
  filename <- tw_get_property_same_length(
    id = id,
    p = "P18",
    only_first = only_first,
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  )

  purrr::map_chr(
    .x = filename,
    .f = function(current_filename) {
      if (is.na(current_filename)) {
        as.character(NA)
      } else if (format == "filename") {
        current_filename
      } else if (format == "commons") {
        stringr::str_c("https://commons.wikimedia.org/wiki/File:", current_filename)
      } else if (format == "embed") {
        if (is.null(width) == TRUE) {
          stringr::str_c(
            "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
            current_filename
          )
        } else {
          stringr::str_c(
            "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
            current_filename, "&width=", width
          )
        }
      } else {
        current_filename
      }
    }
  )
}

#' Get metadata for image from Wikimedia Commons
#'
#' Please consult the relevant documentation for reusing content outside Wikimedia: https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param image_filename Defaults to NULL. If NULL, `image_filename` is obtained from the Wikidata id. If given, must be of the same length as id.
#' @param only_first Defaults to TRUE. If TRUE, returns metadata only for the first image associated with a given Wikidata id. If FALSE, returns all images available.
#' @param language Needed for caching, defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param id_df Default to NULL. If given, it should be a dataframe typically generated with `tw_get_()`, and is used instead of calling Wikidata or using SQLite cache. Ignored when `id` is of length more than one.
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#' tw_get_image_metadata("Q180099")
tw_get_image_metadata <- function(id,
                                  image_filename = NULL,
                                  only_first = TRUE,
                                  language = tidywikidatar::tw_get_language(),
                                  id_df = NULL,
                                  cache = NULL,
                                  overwrite_cache = FALSE,
                                  cache_connection = NULL,
                                  disconnect_db = TRUE,
                                  wait = 0) {
  if (is.null(image_filename)) {
    image_filename <- tw_get_image_same_length(
      id = id,
      format = "filename",
      only_first = only_first,
      language = language,
      id_df = id_df,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db,
      wait = wait
    )
  }


  purrr::map_dfr(
    .x = image_filename,
    .f = function(current_image_filename) {
      api_link <- stringr::str_c(
        "https://commons.wikimedia.org/w/api.php?action=query&titles=File:",
        utils::URLencode(current_image_filename),
        "&prop=imageinfo&iiprop=metadata%7Ccommonmetadata%7Cextmetadata",
        "&format=json"
      )

      json_as_list <- jsonlite::read_json(api_link)

      images <- json_as_list %>%
        purrr::pluck("query", "pages", 1, "imageinfo")

      extmetadata_list <- json_as_list %>%
        purrr::pluck("query", "pages", 1, "imageinfo", 1, "extmetadata")

      tibble::tibble(
        id = id,
        filename = current_image_filename,
        name = extmetadata_list %>% purrr::pluck("ObjectName", "value"),
        description = extmetadata_list %>% purrr::pluck("ImageDescription", "value"),
        credit = extmetadata_list %>% purrr::pluck("Credit", "value"),
        artist = extmetadata_list %>% purrr::pluck("Artist", "value"),
        permission = extmetadata_list %>% purrr::pluck("Permission", "value"),
        license_short = extmetadata_list %>% purrr::pluck("LicenseShortName", "value"),
        usage_terms = extmetadata_list %>% purrr::pluck("UsageTerms", "value"),
        attribution_required = extmetadata_list %>% purrr::pluck("AttributionRequired", "value") %>% stringr::str_to_upper() %>% as.logical(),
        copyrighted = extmetadata_list %>% purrr::pluck("Copyrighted", "value") %>% stringr::str_to_upper() %>% as.logical(),
        restrictions = extmetadata_list %>% purrr::pluck("Restrictions", "value")
      )
    }
  )
}
