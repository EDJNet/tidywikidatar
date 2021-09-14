
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
#' @return A data frame of two columns, id and image, corresponding to reference to the image in the requested format.
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
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  filename_df <- tw_get_property(
    id = stringr::str_to_upper(id),
    p = "P18",
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  )

  image_df <- purrr::map2_dfr(
    .x = filename_df$value,
    .y = stringr::str_to_upper(filename_df$id),
    .f = function(current_filename, current_id) {
      if (is.na(current_filename)) {
        output_filename <- as.character(NA)
      } else if (format == "filename") {
        output_filename <- current_filename
      } else if (format == "commons") {
        output_filename <- stringr::str_c("https://commons.wikimedia.org/wiki/File:", current_filename)
      } else if (format == "embed") {
        if (is.null(width) == TRUE) {
          output_filename <- stringr::str_c(
            "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
            current_filename
          )
        } else {
          output_filename <- stringr::str_c(
            "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
            current_filename, "&width=", width
          )
        }
      } else {
        output_filename <- current_filename
      }
      tibble::tibble(
        id = current_id,
        image = current_filename
      )
    }
  )
  if (nrow(image_df) > 0) {
    image_df
  }
}

#' Get image from Wikimedia Commons
#'
#' Please consult the relevant documentation for reusing content outside Wikimedia: https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param format A charachter vector, defaults to 'filename'. If set to 'commons', outputs the link to the Wikimedia Commons page. If set to "embed", outputs a link that can be used to embed.
#' @param only_first Defaults to TRUE. If TRUE, returns only the first image associated with a given Wikidata id. If FALSE, returns all images available.
#' @param as_tibble Defaults to FALSE. If TRUE, returns a data frame instead of a character vector.
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
#' tw_get_image_same_length("Q180099",
#'   format = "filename"
#' )
#'
#' tw_get_image_same_length("Q180099",
#'   format = "commons"
#' )
#'
#' tw_get_image_same_length("Q180099",
#'   format = "embed",
#'   width = 300
#' )
tw_get_image_same_length <- function(id,
                                     format = "filename",
                                     as_tibble = FALSE,
                                     only_first = TRUE,
                                     width = NULL,
                                     language = tidywikidatar::tw_get_language(),
                                     id_df = NULL,
                                     cache = NULL,
                                     overwrite_cache = FALSE,
                                     cache_connection = NULL,
                                     disconnect_db = TRUE,
                                     wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  image_df <- tw_get_image(
    id = stringr::str_to_upper(id),
    format = format,
    width = width,
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db,
    wait = wait
  )

  if (is.null(image_df)) {
    return(rep(as.character(NA), length(id)))
  }
  if (as_tibble == TRUE) {
    if (only_first == TRUE) {
      dplyr::left_join(tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::ungroup(),
        by = "id"
      )
    } else {
      dplyr::left_join(tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::summarise(image = list(.data$image)),
        by = "id"
      )
    }
  } else {
    if (only_first == TRUE) {
      dplyr::left_join(tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::ungroup(),
        by = "id"
      ) %>%
        dplyr::pull(.data$image)
    } else {
      dplyr::left_join(tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::summarise(image = list(.data$image)),
        by = "id"
      ) %>%
        dplyr::pull(.data$image)
    }
  }
}

#' Get metadata for images from Wikimedia Commons
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
#' if (interactive()) {
#'   tw_get_image_metadata("Q180099")
#' }
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
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  if (is.null(image_filename)) {
    image_filename <- tw_get_image_same_length(
      id = id,
      format = "filename",
      as_tibble = FALSE,
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

  input_df <- tibble::tibble(
    id = id,
    image_filename = image_filename
  ) %>%
    tidyr::unnest(image_filename)

  input_df_distinct <- dplyr::distinct(input_df)

  if (nrow(input_df_distinct) == 1) {
    return(
      dplyr::left_join(
        x = tibble::tibble(id = id),
        y = tw_get_image_metadata_single(
          id = input_df_distinct$id,
          image_filename = input_df_distinct$image_filename,
          only_first = only_first,
          language = language,
          id_df = id_df,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = cache_connection,
          disconnect_db = disconnect_db,
          wait = wait
        ),
        by = "id"
      )
    )
  } else if (nrow(input_df_distinct) > 1) {
    if (overwrite_cache == TRUE | tw_check_cache(cache) == FALSE) {
      pb <- progress::progress_bar$new(total = nrow(input_df_distinct))
      image_metadata <- purrr::map2_dfr(
        .x = input_df_distinct$image_filename,
        .y = input_df_distinct$id,
        .f = function(current_image_filename, current_id) {
          pb$tick()
          tw_get_image_metadata_single(current_id,
            image_filename = current_image_filename,
            only_first = only_first,
            language = language,
            id_df = id_df,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = cache_connection,
            disconnect_db = FALSE,
            wait = wait
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
          x = tibble::tibble(id = id),
          y = image_metadata,
          by = "id"
        )
      )
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      db <- tw_connect_to_cache(
        connection = cache_connection,
        language = language
      )

      table_name <- tw_get_cache_table_name(
        type = "image_metadata",
        language = language
      )

      if (DBI::dbExistsTable(conn = db, name = table_name) == TRUE) {
        db_result <- tryCatch(
          dplyr::tbl(src = db, table_name) %>%
            dplyr::filter(.data$id %in% stringr::str_to_upper(id)),
          error = function(e) {
            logical(1L)
          }
        )
        if (isFALSE(db_result)) {
          image_metadata_from_cache_df <- logical(1L)
        } else {
          image_metadata_from_cache_df <- db_result %>%
            tibble::as_tibble()
        }
      } else {
        image_metadata_from_cache_df <- logical(1L)
      }
    }

    if (isFALSE(image_metadata_from_cache_df)) {
      image_metadata_not_in_cache <- input_df_distinct
      image_metadata_from_cache_df <- input_df_distinct %>%
        dplyr::slice(0)
    } else {
      image_metadata_not_in_cache <- input_df_distinct %>%
        dplyr::anti_join(image_metadata_from_cache_df, by = "id")
    }

    if (nrow(image_metadata_not_in_cache) == 0) {
      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = cache_connection,
        disconnect_db = disconnect_db
      )
      return(
        dplyr::left_join(
          x = tibble::tibble(id = id),
          y = image_metadata_from_cache_df,
          by = "id"
        )
      )
    } else if (nrow(image_metadata_not_in_cache) > 0) {
      pb <- progress::progress_bar$new(total = nrow(image_metadata_not_in_cache))
      image_metadata_not_in_cache_df <- purrr::map2_dfr(
        .x = image_metadata_not_in_cache$image_filename,
        .y = image_metadata_not_in_cache$id,
        .f = function(current_image_filename, current_id) {
          pb$tick()
          tw_get_image_metadata_single(
            id = current_id,
            image_filename = current_image_filename,
            only_first = only_first,
            language = language,
            id_df = id_df,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = cache_connection,
            disconnect_db = FALSE,
            wait = wait
          )
        }
      )

      tw_disconnect_from_cache(
        cache = cache,
        cache_connection = cache_connection,
        disconnect_db = disconnect_db
      )

      dplyr::left_join(
        x = tibble::tibble(id = id),
        y =
          dplyr::bind_rows(
            image_metadata_from_cache_df,
            image_metadata_not_in_cache_df
          ),
        by = "id"
      )
    }
  }
}

#' Get metadata for images from Wikimedia Commons
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
#' @param read_cache Logical, defaults to TRUE. Mostly used internally to prevent checking if an item is in cache if it is already known that it is not in cache.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_image_metadata_single("Q180099")
#' }
tw_get_image_metadata_single <- function(id,
                                         image_filename = NULL,
                                         only_first = TRUE,
                                         language = tidywikidatar::tw_get_language(),
                                         id_df = NULL,
                                         cache = NULL,
                                         overwrite_cache = FALSE,
                                         read_cache = TRUE,
                                         cache_connection = NULL,
                                         disconnect_db = TRUE,
                                         wait = 0) {
  if (length(id) > 1) {
    usethis::ui_stop("`tw_get_image_metadata_single()` requires `id` of length 1. Consider using `tw_get_image_metadata()`.")
  }
  if (is.null(image_filename)) {
    image_filename <- tw_get_image_same_length(
      id = stringr::str_to_upper(id),
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


  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE & read_cache == TRUE) {
    db <- tw_connect_to_cache(
      connection = cache_connection,
      language = language
    )

    table_name <- tw_get_cache_table_name(
      type = "image_metadata",
      language = language
    )

    if (DBI::dbExistsTable(conn = db, name = table_name) == TRUE) {
      db_result <- tryCatch(
        dplyr::tbl(src = db, table_name) %>%
          dplyr::filter(.data$id %in% stringr::str_to_upper(id)),
        error = function(e) {
          logical(1L)
        }
      )
      if (isFALSE(db_result)) {
        image_metadata_from_cache_df <- logical(1L)
      } else {
        image_metadata_from_cache_df <- db_result %>%
          tibble::as_tibble() %>%
          dplyr::distinct()
        if (nrow(image_metadata_from_cache_df) > 0) {
          return(image_metadata_from_cache_df)
        } else {
          image_metadata_from_cache_df <- logical(1L)
        }
      }
    } else {
      image_metadata_from_cache_df <- logical(1L)
    }
  }

  Sys.sleep(time = wait)

  image_metadata <- purrr::map_dfr(
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
        image_filename = current_image_filename,
        name = extmetadata_list %>% purrr::pluck("ObjectName", "value"),
        description = extmetadata_list %>% purrr::pluck("ImageDescription", "value"),
        categories = extmetadata_list %>% purrr::pluck("Categories", "value"),
        assessments = extmetadata_list %>% purrr::pluck("Assessments", "value"),
        credit = extmetadata_list %>% purrr::pluck("Credit", "value"),
        artist = extmetadata_list %>% purrr::pluck("Artist", "value"),
        permission = extmetadata_list %>% purrr::pluck("Permission", "value"),
        license_short_name = extmetadata_list %>% purrr::pluck("LicenseShortName", "value"),
        license_url = extmetadata_list %>% purrr::pluck("LicenseUrl", "value"),
        license = extmetadata_list %>% purrr::pluck("License", "value"),
        usage_terms = extmetadata_list %>% purrr::pluck("UsageTerms", "value"),
        attribution_required = extmetadata_list %>% purrr::pluck("AttributionRequired", "value") %>% stringr::str_to_upper() %>% as.logical(),
        copyrighted = extmetadata_list %>% purrr::pluck("Copyrighted", "value") %>%
          stringr::str_to_upper() %>% as.logical(),
        restrictions = extmetadata_list %>% purrr::pluck("Restrictions", "value"),
        date_time = extmetadata_list %>% purrr::pluck("DateTime", "value"),
        date_time_original = extmetadata_list %>% purrr::pluck("DateTimeOriginal", "value"),
        commons_metadata_extension = extmetadata_list %>% purrr::pluck("CommonsMetadataExtension", "value")
      )
    }
  )

  if (tw_check_cache(cache) == TRUE) {
    db <- tw_connect_to_cache(connection = cache_connection, language = language)

    table_name <- tw_get_cache_table_name(type = "image_metadata", language = language)

    if (DBI::dbExistsTable(conn = db, name = table_name) == FALSE) {
      # do nothing: if table does not exist, previous data cannot be there
    } else {
      if (overwrite_cache == TRUE) {
        statement <- glue::glue_sql("DELETE FROM {`table_name`} WHERE id = {id*}",
          id = unique(image_metadata$id),
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
      value = image_metadata,
      append = TRUE
    )

    if (disconnect_db == TRUE) {
      DBI::dbDisconnect(db)
    }
  }
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db
  )
  image_metadata
}
