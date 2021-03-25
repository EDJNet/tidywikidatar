#' Return (almost) all information from a Wikidata item in a tidy format
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to "all_available". By default, returns dataset with labels in all available languages. If given, only in the chosen language. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A data.frame (a tibble) with two columns: property and value
#' @export
#'
#' @examples
#' \dontrun{
#' tw_get("Q220480")
#' }
#'
tw_get <- function(id,
                   language = "all_available",
                   cache = NULL,
                   overwrite_cache = FALSE) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  if (tw_check_cache(cache) == TRUE) {
    tidywikidatar::tw_create_cache_folder()
    db_folder <- fs::path(
      tidywikidatar::tw_get_cache_folder(),
      "wiki_item_db"
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c("wiki_item_db", language, ".sqlite")
    )
    db <- DBI::dbConnect(drv = RSQLite::SQLite(), db_file)
    db_result <- tryCatch(
      DBI::dbReadTable(
        conn = db,
        name = stringr::str_to_upper(id)
      ),
      error = function(e) {
        logical(1L)
      }
    )
    if (is.data.frame(db_result) & overwrite_cache == FALSE) {
      DBI::dbDisconnect(db)
      return(db_result)
    }
  }


  item <- tryCatch(WikidataR::get_item(id = id),
    error = function(e) {
      return(tibble::tibble(id = NA))
    }
  )
  labels <- item %>% purrr::pluck(1, "labels")

  labels_df <- purrr::map_dfr(
    .x = labels,
    function(current_label_l) {
      tibble::tibble(
        property = paste0("label_", current_label_l$language),
        value = current_label_l$value
      )
    }
  )

  if (language=="all_available") {
    # do nothing
  } else {
    labels_df <- labels_df %>%
      dplyr::filter(property == stringr::str_c("label_", language))
  }

  aliases <- item %>% purrr::pluck(1, "aliases")

  aliases_df <- purrr::map_dfr(
    .x = aliases,
    function(current_alias_l) {
      tibble::tibble(
        property = paste0("alias_", current_alias_l$language),
        value = current_alias_l$value
      )
    }
  )

  if (language=="all_available") {
    # do nothing
  } else {
    aliases_df <- aliases_df %>%
      dplyr::filter(property == stringr::str_c("alias_", language))
  }

  descriptions <- item %>% purrr::pluck(1, "descriptions")

  descriptions_df <- purrr::map_dfr(
    .x = descriptions,
    function(current_description_l) {
      tibble::tibble(
        property = paste0("description_", current_description_l$language),
        value = current_description_l$value
      )
    }
  )

  if (language=="all_available") {
    # do nothing
  } else {
    descriptions_df <- descriptions_df %>%
      dplyr::filter(property == stringr::str_c("description_", language))
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

  everything_df <- dplyr::bind_rows(
    labels_df,
    aliases_df,
    claims_df,
    descriptions_df,
    sitelinks_df
  )
  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = everything_df
    )
    DBI::dbDisconnect(db)
  } else if (tw_check_cache(cache) == TRUE & overwrite_cache == TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = everything_df,
      overwrite = TRUE
    )
    DBI::dbDisconnect(db)
  }
  everything_df %>%
    tibble::as_tibble()
}


#' Get Wikidata label in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en". For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tw_get_label(id = "Q228822")
#' }
#'

tw_get_label <- function(id,
                         language = "en",
                         cache = NULL,
                         overwrite_cache = FALSE) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  label <- tidywikidatar::tw_get(
    id = id,
    cache = tw_check_cache(cache),
    language = language,
    overwrite_cache = overwrite_cache
  ) %>%
    dplyr::filter(
      stringr::str_starts(
        string = .data$property,
        pattern = "label_"
      ),
      stringr::str_ends(
        string = .data$property,
        pattern = stringr::str_c(language,
          collapse = "|"
        )
      )
    ) %>%
    dplyr::pull(.data$value)

  if (length(label) == 0) {
    as.character(NA)
  } else {
    label
  }
}

#' Get Wikidata description in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en". For a full list of available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A charachter vector of length 1, with the Wikidata description in the requested languae.
#' @export
#'
#' @examples
#' \dontrun{
#' tw_get_description(id = "Q228822")
#' }
tw_get_description <- function(id,
                               language = "en",
                               cache = NULL,
                               overwrite_cache = FALSE) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  description <- tidywikidatar::tw_get(
    id = id,
    cache = tw_check_cache(cache),
    overwrite_cache = overwrite_cache,
    language = language
  ) %>%
    dplyr::filter(
      stringr::str_starts(
        string = .data$property,
        pattern = "description_"
      ),
      stringr::str_ends(
        string = .data$property,
        pattern = stringr::str_c(language,
          collapse = "|"
        )
      )
    ) %>%
    dplyr::pull(.data$value)
  if (length(description) == 0) {
    as.character(NA)
  } else {
    description
  }
}

#' Get Wikidata property of an item
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param language Defaults to "all_available". It should be relevant only for caching purposes. For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A charachter vector of length 1, corresponding to the value for the given property.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tw_get_property(id = "Q228822")
#' }
#'
tw_get_property <- function(id,
                            p,
                            language = "all_available",
                            cache = NULL,
                            overwrite_cache = FALSE) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  property <- tidywikidatar::tw_get(
    id = id,
    cache = tw_check_cache(cache),
    overwrite_cache = overwrite_cache,
    language = language
  ) %>%
    dplyr::filter(property == p) %>%
    dplyr::pull(.data$value)
  if (length(property) == 0) {
    as.character(NA)
  } else {
    property
  }
}

#' Get Wikidata image
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param format A charachter vector, defaults to 'filename". If set to 'commons', outputs the link to the Wikimedia Commons page. If set to "embed", outputs a link that can be used to embed.
#' @param language Defaults to "all_available". It should be relevant only for caching purposes. For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#' \dontrun{
#' tw_get_image(id = "Q228822")
#' }
#'
tw_get_image <- function(id,
                         cache = NULL,
                         language = language,
                         overwrite_cache = FALSE,
                         format = "filename") {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  link <- tidywikidatar::tw_get(
    id = id,
    cache = tw_check_cache(cache),
    overwrite_cache = overwrite_cache,
    language = language
  ) %>%
    dplyr::filter(.data$property == "P18") %>%
    dplyr::pull(.data$value)

  if (length(link) == 0) {
    return(as.character(NA))
  }

  if (format == "filename") {
    link
  } else if (format == "commons") {
    stringr::str_c("https://commons.wikimedia.org/wiki/File:", link)
  } else if (format == "embed") {
    stringr::str_c("https://upload.wikimedia.org/wikipedia/commons/d/dc/", link)
  } else {
    link
  }
}


#' Get Wikidata description in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en". For a full list of available languages, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tw_get_wikipedia(id = "Q228822")
#' }
#'
tw_get_wikipedia <- function(id,
                             language = "en",
                             cache = NULL,
                             overwrite_cache = FALSE) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  base_string <- stringr::str_c("sitelink_", language, "wiki")
  base_link <- tidywikidatar::tw_get(
    id = id,
    cache = tw_check_cache(cache),
    overwrite_cache = overwrite_cache,
    language = language
  ) %>%
    dplyr::filter(is.element(el = .data$property, set = base_string)) %>%
    dplyr::pull(.data$value)
  if (length(base_link) == 0) {
    as.character(NA)
  } else {
    stringr::str_c("https://", language, ".wikipedia.org/wiki/", base_link)
  }
}
