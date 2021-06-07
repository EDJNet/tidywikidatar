#' Return (most) information from a Wikidata item in a tidy format
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param read_cache Logical, defaults to TRUE. Mostly used internally to prevent checking if an item is in cache if it is already known that it is not in cache.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A data.frame (a tibble) with three columns (id, property, and value). If item not found or trouble connecting with the server, an data frame with three columns and zero rows is returned, with the warning as an attribute, which can be retrieved with `attr(output, "warning"))`
#' @export
#'
#' @examples
#' tw_get_single(
#'   id = "Q180099",
#'   language = "en"
#' )
tw_get_single <- function(id,
                          language = tidywikidatar::tw_get_language(),
                          cache = NULL,
                          overwrite_cache = FALSE,
                          read_cache = TRUE,
                          cache_connection = NULL,
                          disconnect_db = TRUE,
                          wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  if (length(id) > 1) {
    usethis::ui_stop("`tw_get_single()` requires `id` of length 1. Consider using `tw_get()`.")
  }

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE & read_cache == TRUE) {
    db_result <- tw_get_cached_item(
      id = id,
      language = language,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )
    if (is.data.frame(db_result) & nrow(db_result) > 0) {
      return(db_result %>%
        tibble::as_tibble())
    }
  }

  Sys.sleep(time = wait)

  item <- tryCatch(WikidataR::get_item(id = id),
    error = function(e) {
      as.character(e[[1]])
    }
  )

  if (is.character(item)) {
    usethis::ui_oops(item)
    output <- tibble::tibble(
      id = as.character(NA),
      property = as.character(NA),
      value = as.character(NA)
    ) %>%
      dplyr::slice(0)
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
        cache_connection = cache_connection,
        disconnect_db = disconnect_db,
        wait = wait
      )
    )
  }

  labels <- item %>%
    purrr::pluck(1, "labels")

  labels_df <- purrr::map_dfr(
    .x = labels,
    function(current_label_l) {
      tibble::tibble(
        property = paste0("label_", current_label_l$language),
        value = current_label_l$value
      )
    }
  )

  if (language == "all_available") {
    # do nothing
  } else {
    labels_df <- labels_df %>%
      dplyr::filter(.data$property == stringr::str_c("label_", language))
  }

  aliases <- item %>% purrr::pluck(1, "aliases")

  if (is.null(aliases)) {
    aliases_df <- tibble::tibble(
      property = as.character(NA),
      values = as.character(NA)
    ) %>%
      tidyr::drop_na()
  } else {
    aliases_df <- purrr::map_dfr(
      .x = aliases,
      function(current_alias_l) {
        tibble::tibble(
          property = paste0("alias_", current_alias_l$language),
          value = current_alias_l$value
        )
      }
    )

    if (language == "all_available") {
      # do nothing
    } else {
      aliases_df <- aliases_df %>%
        dplyr::filter(.data$property == stringr::str_c("alias_", language))
    }
  }

  descriptions <- item %>% purrr::pluck(1, "descriptions")

  if (is.null(descriptions)) {
    descriptions_df <- tibble::tibble(
      property = as.character(NA),
      values = as.character(NA)
    ) %>%
      tidyr::drop_na()
  } else {
    descriptions_df <- purrr::map_dfr(
      .x = descriptions,
      function(current_description_l) {
        tibble::tibble(
          property = paste0("description_", current_description_l$language),
          value = current_description_l$value
        )
      }
    )

    if (language == "all_available") {
      # do nothing
    } else {
      descriptions_df <- descriptions_df %>%
        dplyr::filter(.data$property == stringr::str_c("description_", language))
    }
  }

  claims <- item %>% purrr::pluck(1, "claims")

  claims_df <- purrr::map_dfr(
    .x = claims,
    .f = function(current_claim_l) {
      property <- current_claim_l$mainsnak$property

      value_pre <- claims[[unique(property)]][["mainsnak"]][["datavalue"]][["value"]]

      if (is.null(value_pre)) {
        value <- as.character("NA")
      } else if (is.data.frame(value_pre)) {
        if (is.element("time", names(value_pre))) {
          value <- value_pre$time
        } else if (is.element("text", names(value_pre))) {
          value <- value_pre$text
        } else if (is.element("amount", names(value_pre))) {
          value <- value_pre$amount
        } else if (is.element("latitude", names(value_pre))) {
          value <- stringr::str_c(value_pre$latitude, value_pre$longitude, sep = ",")
        } else if (is.element("id", names(value_pre))) {
          value <- value_pre$id
        } else if (is.na(value_pre[[1]]) == FALSE) {
          value <- value_pre[[1]]
        } else {
          value <- as.character("NA")
        }
      } else if (is.character(value_pre)) {
        value <- value_pre
      } else {
        value <- as.character("NA")
      }

      tibble::tibble(
        property = property,
        value = value
      )
    }
  )


  sitelinks <- item %>% purrr::pluck(1, "sitelinks")

  sitelinks_df <- purrr::map_dfr(
    .x = sitelinks,
    function(current_sitelink_l) {
      tibble::tibble(
        property = paste0("sitelink_", current_sitelink_l$site),
        value = current_sitelink_l$title
      )
    }
  )

  if (language == "all_available" | nrow(sitelinks_df) == 0) {
    # do nothing
  } else {
    sitelinks_df <- sitelinks_df %>%
      dplyr::filter((.data$property == stringr::str_c(
        "sitelink_",
        language,
        "wiki"
      )) | (.data$property == stringr::str_c(
        "sitelink_",
        language,
        "wikiquote"
      )) | (.data$property == stringr::str_c(
        "sitelink_",
        language,
        "wikisource"
      )) | (.data$property == "sitelink_commonswiki"))
  }

  everything_df <- dplyr::bind_rows(
    labels_df,
    aliases_df,
    claims_df,
    descriptions_df,
    sitelinks_df
  ) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      id = id,
      .data$property,
      .data$value
    )

  if (tw_check_cache(cache) == TRUE) {
    tw_write_item_to_cache(
      item_df = everything_df,
      language = language,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db
    )
  }
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = cache_connection,
    disconnect_db = disconnect_db
  )
  everything_df
}

#' Return (most) information from a Wikidata item in a tidy format
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to language set with `tw_set_language()`; if not set, "en". Use "all_available" to keep all languages. For available language values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param cache_connection Defaults to NULL. If NULL, and caching is enabled, `tidywikidatar` will use a local sqlite database. A custom connection to other databases can be given (see vignette `caching` for details).
#' @param disconnect_db Defaults to TRUE. If FALSE, leaves the connection to cache open.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A data.frame (a tibble) with three columns (id, property, and value).
#' @export
#'
#' @examples
#' tw_get(
#'   id = c("Q180099", "Q228822"),
#'   language = "en"
#' )
tw_get <- function(id,
                   language = tidywikidatar::tw_get_language(),
                   cache = NULL,
                   overwrite_cache = FALSE,
                   cache_connection = NULL,
                   disconnect_db = TRUE,
                   wait = 0) {
  if (length(id) == 0) {
    usethis::ui_stop("`tw_get()` requires `id` of length 1 or more.")
  }

  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  unique_id <- unique(id)

  if (length(unique_id) == 1) {
    return(tw_get_single(
      id = unique_id,
      language = language,
      cache = cache,
      overwrite_cache = overwrite_cache,
      cache_connection = cache_connection,
      disconnect_db = disconnect_db,
      wait = wait
    ) %>%
      dplyr::right_join(tibble::tibble(id = id),
        by = "id"
      ))
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
      return(item_df %>%
        dplyr::right_join(tibble::tibble(id = id),
          by = "id"
        ))
    }

    if (overwrite_cache == FALSE & tw_check_cache(cache) == TRUE) {
      items_from_cache_df <- tw_get_cached_item(
        id = unique_id,
        language = language,
        cache_connection = cache_connection,
        disconnect_db = FALSE
      )

      id_items_not_in_cache <- unique_id[!is.element(unique_id, items_from_cache_df$id)]

      if (length(id_items_not_in_cache) == 0) {
        tw_disconnect_from_cache(
          cache = cache,
          cache_connection = cache_connection,
          disconnect_db = disconnect_db
        )
        return(items_from_cache_df %>%
          dplyr::right_join(tibble::tibble(id = id), by = "id"))
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
              cache_connection = cache_connection,
              read_cache = FALSE,
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

        dplyr::bind_rows(
          items_from_cache_df,
          items_not_in_cache_df
        ) %>%
          dplyr::right_join(tibble::tibble(id = id), by = "id")
      }
    }
  }
}
