#' Get image from Wikimedia Commons
#'
#' Please consult the
#' \href{https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical}{relevant
#' documentation for reusing content outside Wikimedia}.
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for
#'   Wolfgang Amadeus Mozart.
#' @param format A character vector, defaults to `filename`. If set to
#'   `commons`, outputs the link to the Wikimedia Commons page. If set to
#'   `embed`, outputs a link that can be used to embed.
#' @param width A numeric value, defaults to `NULL`, relevant only if format is
#'   set to 'embed'. If not given, defaults to full resolution image.
#' @inheritParams tw_get
#' @inheritParams tw_get_label
#'
#' @return A data frame of two columns, `id` and `image`, corresponding to
#'   reference to the image in the requested format.
#' @export
#'
#' @examples
#' tw_get_image("Q180099",
#'   format = "filename"
#' )
#'
#' if (interactive()) {
#'   tw_get_image("Q180099",
#'     format = "commons"
#'   )
#'
#'   tw_get_image("Q180099",
#'     format = "embed",
#'     width = 300
#'   )
#' }
tw_get_image <- function(
  id,
  format = "filename",
  width = NULL,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 0
) {
  if (is.data.frame(id)) {
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
        output_filename <- NA_character_
      } else if (format == "filename") {
        output_filename <- current_filename
      } else if (format == "commons") {
        output_filename <- stringr::str_c(
          "https://commons.wikimedia.org/wiki/File:",
          current_filename
        )
      } else if (format == "embed") {
        if (is.null(width)) {
          output_filename <- stringr::str_c(
            "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
            current_filename
          )
        } else {
          output_filename <- stringr::str_c(
            "https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/",
            current_filename,
            "&width=",
            width
          )
        }
      } else {
        output_filename <- current_filename
      }
      tibble::tibble(
        id = current_id,
        image = output_filename
      )
    }
  )
  if (nrow(image_df) > 0) {
    image_df
  }
}

#' Get image from Wikimedia Commons
#'
#' Please consult the
#' \href{https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical}{relevant
#' documentation for reusing content outside Wikimedia}.
#'
#' @param as_tibble Defaults to `FALSE`. If `TRUE`, returns a data frame instead
#'   of a character vector.
#' @inheritParams tw_get_image
#' @inheritParams tw_get_image_metadata
#' @return A character vector, corresponding to reference to the image in the
#'   requested format.
#' @export
#'
#' @examples
#' tw_get_image_same_length("Q180099",
#'   format = "filename"
#' )
#'
#' if (interactive()) {
#'   tw_get_image_same_length("Q180099",
#'     format = "commons"
#'   )
#'
#'   tw_get_image_same_length("Q180099",
#'     format = "embed",
#'     width = 300
#'   )
#' }
tw_get_image_same_length <- function(
  id,
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
  wait = 0
) {
  if (is.data.frame(id)) {
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
  if (as_tibble) {
    if (only_first) {
      dplyr::left_join(
        tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::ungroup(),
        by = "id"
      )
    } else {
      dplyr::left_join(
        tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::summarise(image = list(.data$image)),
        by = "id"
      )
    }
  } else {
    if (only_first) {
      dplyr::left_join(
        tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::ungroup(),
        by = "id"
      ) %>%
        dplyr::pull("image")
    } else {
      dplyr::left_join(
        tibble::tibble(id = id),
        image_df %>%
          dplyr::group_by(.data$id) %>%
          dplyr::summarise(image = list(.data$image)),
        by = "id"
      ) %>%
        dplyr::pull("image")
    }
  }
}

#' Get metadata for images from Wikimedia Commons
#'
#' Please consult the
#' \href{https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical}{relevant
#' documentation for reusing content outside Wikimedia}.
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for
#'   Wolfgang Amadeus Mozart.
#' @param image_filename Defaults to `NULL`. If `NULL`, `image_filename` is
#'   obtained from the Wikidata id. If given, must be of the same length as id.
#' @param only_first Defaults to `TRUE`. If `TRUE`, returns only the first image
#'   associated with a given Wikidata id. If `FALSE`, returns all images
#'   available.
#' @param id_df Default to NULL. If given, it should be a dataframe typically
#'   generated with [tw_get()], and is used instead of calling Wikidata or
#'   using SQLite cache. Ignored when `id` is of length more than one.
#' @param attempts Defaults to 10. Number of times it re-attempts to reach the
#'   API before failing.
#' @inheritParams tw_get_image
#'
#' @return A character vector, corresponding to reference to the image in the
#'   requested format.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   tw_get_image_metadata("Q180099")
#' }
tw_get_image_metadata <- function(
  id,
  image_filename = NULL,
  only_first = TRUE,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10
) {
  if (is.data.frame(id)) {
    id <- id$id
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

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
      cache_connection = db,
      disconnect_db = FALSE,
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
          cache_connection = db,
          disconnect_db = disconnect_db,
          wait = wait,
          attempts = attempts
        ),
        by = "id"
      )
    )
  } else if (nrow(input_df_distinct) > 1) {
    if (overwrite_cache | !tw_check_cache(cache)) {
      pb <- progress::progress_bar$new(total = nrow(input_df_distinct))

      image_metadata <- purrr::map2_dfr(
        .x = input_df_distinct$image_filename,
        .y = input_df_distinct$id,
        .f = function(current_image_filename, current_id) {
          pb$tick()
          tw_get_image_metadata_single(
            current_id,
            image_filename = current_image_filename,
            only_first = only_first,
            language = language,
            id_df = id_df,
            cache = cache,
            overwrite_cache = overwrite_cache,
            cache_connection = db,
            disconnect_db = FALSE,
            wait = wait,
            attempts = attempts
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
          y = image_metadata,
          by = "id"
        )
      )
    }

    if (!overwrite_cache & tw_check_cache(cache)) {
      table_name <- tw_get_cache_table_name(
        type = "image_metadata",
        language = language
      )

      if (pool::dbExistsTable(conn = db, name = table_name)) {
        db_result <- tryCatch(
          dplyr::tbl(src = db, table_name) %>%
            dplyr::filter(.data$id %in% !!stringr::str_to_upper(id)),
          error = function(e) {
            logical(1L)
          }
        )
        if (isFALSE(db_result)) {
          image_metadata_from_cache_df <- logical(1L)
        } else {
          image_metadata_from_cache_df <- db_result %>%
            dplyr::collect()
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
        cache_connection = db,
        disconnect_db = disconnect_db,
        language = language
      )
      return(
        dplyr::left_join(
          x = tibble::tibble(id = id),
          y = image_metadata_from_cache_df,
          by = "id"
        )
      )
    } else if (nrow(image_metadata_not_in_cache) > 0) {
      pb <- progress::progress_bar$new(
        total = nrow(image_metadata_not_in_cache)
      )

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
            cache_connection = db,
            disconnect_db = FALSE,
            wait = wait,
            attempts = attempts
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
#' Please consult the
#' \href{https://commons.wikimedia.org/wiki/Commons:Reusing_content_outside_Wikimedia/technical}{relevant
#' documentation for reusing content outside Wikimedia}.
#'
#' @inheritParams tw_get_image_metadata
#' @inheritParams tw_get
#' @inheritParams tw_get_single
#'
#' @return A character vector, corresponding to reference to the image in the
#'   requested format.
#'
#' @examples
#' if (interactive()) {
#'   tw_get_image_metadata_single("Q180099")
#' }
tw_get_image_metadata_single <- function(
  id,
  image_filename = NULL,
  only_first = TRUE,
  language = tidywikidatar::tw_get_language(),
  id_df = NULL,
  cache = NULL,
  overwrite_cache = FALSE,
  read_cache = TRUE,
  cache_connection = NULL,
  disconnect_db = TRUE,
  wait = 1,
  attempts = 10
) {
  if (length(id) > 1) {
    cli::cli_abort(c(
      x = "id` must have length 1.",
      i = "Consider using {.fn tw_get_image_metadata}."
    ))
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  if (is.null(image_filename)) {
    image_filename <- tw_get_image_same_length(
      id = stringr::str_to_upper(id),
      format = "filename",
      only_first = only_first,
      language = language,
      id_df = id_df,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = db,
      disconnect_db = FALSE,
      wait = wait
    )
  }

  if (
    tw_check_cache(cache) &
      overwrite_cache == FALSE &
      read_cache
  ) {
    table_name <- tw_get_cache_table_name(
      type = "image_metadata",
      language = language
    )

    if (pool::dbExistsTable(conn = db, name = table_name)) {
      db_result <- tryCatch(
        dplyr::tbl(src = db, table_name) %>%
          dplyr::filter(.data$id %in% !!stringr::str_to_upper(id)),
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
          tw_disconnect_from_cache(
            cache = cache,
            cache_connection = db,
            disconnect_db = disconnect_db,
            language = language
          )
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

  empty_df <- data.frame(matrix(
    ncol = 19,
    nrow = 1,
    data = as.character(NA),
    dimnames = list(
      NULL,
      c(
        "id",
        "image_filename",
        "object_name",
        "image_description",
        "categories",
        "assessments",
        "credit",
        "artist",
        "permission",
        "license_short_name",
        "license_url",
        "license",
        "usage_terms",
        "attribution_required",
        "copyrighted",
        "restrictions",
        "date_time",
        "date_time_original",
        "commons_metadata_extension"
      )
    )
  )) %>%
    tibble::as_tibble()

  image_metadata <- purrr::map_dfr(
    .x = image_filename,
    .f = function(current_image_filename) {
      if (is.na(current_image_filename)) {
        empty_df$id[1] <- stringr::str_to_upper(id)
        empty_df$copyrighted <- as.logical(NA)
        empty_df$attribution_required <- as.logical(NA)
        return(empty_df)
      }

      api_link <- stringr::str_c(
        "https://commons.wikimedia.org/w/api.php?action=query&titles=File:",
        utils::URLencode(current_image_filename, reserved = TRUE),
        "&prop=imageinfo&iiprop=metadata%7Ccommonmetadata%7Cextmetadata",
        "&format=json"
      )

      api_result <- FALSE

      attempt_n <- 1

      while (isFALSE(api_result) & attempt_n <= attempts) {
        attempt_n <- sum(attempt_n, 1)
        api_result <- tryCatch(
          jsonlite::read_json(api_link),
          error = function(e) {
            logical(1L)
          }
        )
        Sys.sleep(time = wait)
      }

      if (isFALSE(api_result)) {
        cli::cli_abort(c(
          "Could not reach the API with {attempts} attempt{?s}.",
          i = "Consider increasing the waiting time between calls with the {.arg wait} parameter or check your internet connection."
        ))
      } else {
        json_as_list <- api_result
      }

      images <- json_as_list %>%
        purrr::pluck("query", "pages", 1, "imageinfo")

      extmetadata_list <- json_as_list %>%
        purrr::pluck("query", "pages", 1, "imageinfo", 1, "extmetadata")

      tibble::tibble(
        id = stringr::str_to_upper(id),
        image_filename = current_image_filename %>% as.character(),
        object_name = ifelse(
          test = is.null(
            extmetadata_list %>% purrr::pluck("ObjectName", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("ObjectName", "value")
        ) %>%
          as.character(),
        image_description = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("ImageDescription", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("ImageDescription", "value")
        ) %>%
          as.character(),
        categories = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Categories", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Categories", "value")
        ) %>%
          as.character(),
        assessments = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Assessments", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Assessments", "value")
        ) %>%
          as.character(),
        credit = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Credit", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Credit", "value")
        ),
        artist = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Artist", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Artist", "value")
        ) %>%
          as.character(),
        permission = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Permission", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Permission", "value")
        ) %>%
          as.character(),
        license_short_name = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("LicenseShortName", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("LicenseShortName", "value")
        ) %>%
          as.character(),
        license_url = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("LicenseUrl", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("LicenseUrl", "value")
        ) %>%
          as.character(),
        license = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("License", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>% purrr::pluck("License", "value")
        ) %>%
          as.character(),
        usage_terms = ifelse(
          test = is.null(
            extmetadata_list %>% purrr::pluck("UsageTerms", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck(
              "UsageTerms",
              "value"
            )
        ) %>%
          as.character(),
        attribution_required = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("AttributionRequired", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("AttributionRequired", "value")
        ) %>%
          stringr::str_to_upper() %>%
          as.logical(),
        copyrighted = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Copyrighted", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Copyrighted", "value")
        ) %>%
          stringr::str_to_upper() %>%
          as.logical(),
        restrictions = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("Restrictions", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("Restrictions", "value")
        ) %>%
          as.character(),
        date_time = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("DateTime", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("DateTime", "value")
        ) %>%
          as.character(),
        date_time_original = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("DateTimeOriginal", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("DateTimeOriginal", "value")
        ) %>%
          as.character(),
        commons_metadata_extension = ifelse(
          test = is.null(
            extmetadata_list %>%
              purrr::pluck("CommonsMetadataExtension", "value")
          ),
          yes = as.character(NA),
          no = extmetadata_list %>%
            purrr::pluck("CommonsMetadataExtension", "value")
        ) %>%
          as.character()
      )
    }
  )

  if (tw_check_cache(cache)) {
    table_name <- tw_get_cache_table_name(
      type = "image_metadata",
      language = language
    )

    if (pool::dbExistsTable(conn = db, name = table_name) == FALSE) {
      # do nothing: if table does not exist, previous data cannot be there
    } else {
      if (overwrite_cache) {
        statement <- glue::glue_sql(
          "DELETE FROM {`table_name`} WHERE id = {id*}",
          id = unique(image_metadata$id),
          table_name = table_name,
          .con = db
        )
        result <- pool::dbExecute(
          conn = db,
          statement = statement
        )
      }
    }

    pool::dbWriteTable(
      db,
      name = table_name,
      value = image_metadata,
      append = TRUE
    )
  }
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db
  )
  image_metadata
}
