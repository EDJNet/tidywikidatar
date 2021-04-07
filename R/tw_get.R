#' Return (most) information from a Wikidata item in a tidy format
#'
#' @param id A characther vector, must start with Q, e.g. "Q180099" for the anthropologist Margaret Mead. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param language Defaults to "all_available". By default, returns dataset with labels in all available languages. If given, only in the chosen language. For available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A data.frame (a tibble) with two columns: property and value
#' @export
#'
#' @examples
#' tw_get(
#'   id = "Q180099",
#'   language = "en"
#' )
tw_get <- function(id,
                   language = "all_available",
                   cache = NULL,
                   overwrite_cache = FALSE,
                   wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  if (tw_check_cache(cache) == TRUE & overwrite_cache == FALSE) {
    db_result <- tw_get_cached_item(
      id = id,
      language = language
    )
    if (is.data.frame(db_result)) {
      return(db_result)
    }
  }

  Sys.sleep(time = wait)

  item <- tryCatch(WikidataR::get_item(id = id),
    error = function(e) {
      return(tibble::tibble(id = NA))
    }
  )


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

  everything_df <- dplyr::bind_rows(
    labels_df,
    aliases_df,
    claims_df,
    descriptions_df,
    sitelinks_df
  ) %>%
    tibble::as_tibble()

  if (tw_check_cache(cache) == TRUE) {
    tw_write_item_to_cache(
      id = id,
      item_df = everything_df,
      language = language,
      overwrite_cache = overwrite_cache
    )
  }
  everything_df
}


#' Get Wikidata label in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param language A character vector of length one, defaults to "en". For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of the same length as the vector of id given, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#'
#' tw_get_label(
#'   id = c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   language = "en"
#' )
tw_get_label <- function(id,
                         language = "en",
                         cache = NULL,
                         overwrite_cache = FALSE,
                         wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  if (length(id) > 1) {
    purrr::map_chr(
      .x = id,
      .f = function(x) {
        tw_get_label(
          id = x,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          wait = wait
        )
      }
    )
  } else {
    label <- tidywikidatar::tw_get(
      id = id,
      cache = tw_check_cache(cache),
      language = language,
      overwrite_cache = overwrite_cache,
      wait = wait
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
}




#' Get Wikidata description in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en". For a full list of available values, see https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, with the Wikidata description in the requested languae.
#' @export
#'
#' @examples
#' tw_get_description(
#'   id = c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   language = "en"
#' )
tw_get_description <- function(id,
                               language = "en",
                               cache = NULL,
                               overwrite_cache = FALSE,
                               wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  if (length(id) > 1) {
    purrr::map_chr(
      .x = id,
      .f = function(x) {
        tw_get_description(
          id = x,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          wait = wait
        )
      }
    )
  } else {
    description <- tidywikidatar::tw_get(
      id = id,
      language = language,
      cache = tw_check_cache(cache),
      overwrite_cache = overwrite_cache,
      wait = wait
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
}

#' Get label of a Wikidata property in a given language
#'
#' @param property A characther vector of length 1, must start with P, e.g. "P31".
#' @param language A character vector of length one, defaults to "en". For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_get_property_label(id = "P31")
#' }
#' }
#'
tw_get_property_label <- function(property,
                                  language = "en",
                                  cache = NULL,
                                  overwrite_cache = FALSE,
                                  wait = 0) {
  if (is.data.frame(property) == TRUE) {
    property <- property$id
  }
  label <- tidywikidatar::tw_search_property(
    search = property,
    cache = tw_check_cache(cache),
    language = language,
    overwrite_cache = overwrite_cache,
    wait = wait
  ) %>%
    dplyr::filter(.data$id == stringr::str_to_upper(property)) %>%
    dplyr::pull(.data$label)

  if (length(label) == 0) {
    as.character(NA)
  } else {
    label
  }
}

#' Get description of a Wikidata property in a given language
#'
#' @param property A characther vector of length 1, must start with P, e.g. "P31".
#' @param language A character vector of length one, defaults to "en". For a full list of available values, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   tw_get_property_description(id = "P31")
#' }
#' }
#'
tw_get_property_description <- function(property,
                                        language = "en",
                                        cache = NULL,
                                        overwrite_cache = FALSE,
                                        wait = 0) {
  if (is.data.frame(property) == TRUE) {
    property <- property$id
  }
  description <- tidywikidatar::tw_search_property(
    search = property,
    cache = tw_check_cache(cache),
    language = language,
    overwrite_cache = overwrite_cache,
    wait = wait
  ) %>%
    dplyr::filter(.data$id == stringr::str_to_upper(property)) %>%
    dplyr::pull(.data$description)

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
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, corresponding to the value for the given property.
#' @export
#'
#' @examples
#' # Who were the doctoral advisors - P184 - of Margaret Mead - Q180099
#' advisors <- tw_get_property(id = "Q180099", p = "P184")
#' advisors
#'
#' tw_get_label(advisors)
tw_get_property <- function(id,
                            p,
                            language = "all_available",
                            cache = NULL,
                            overwrite_cache = FALSE,
                            wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  property <- tidywikidatar::tw_get(
    id = id,
    cache = tw_check_cache(cache),
    overwrite_cache = overwrite_cache,
    language = language,
    wait = wait
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
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#' tw_get_image(
#'   c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   format = "filename"
#' )
#'
#' tw_get_image(
#'   c(
#'     "Q180099",
#'     "Q228822"
#'   ),
#'   format = "commons"
#' )
tw_get_image <- function(id,
                         language = "all_available",
                         cache = NULL,
                         overwrite_cache = FALSE,
                         format = "filename",
                         wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  if (length(id) > 1) {
    purrr::map_chr(
      .x = id,
      .f = function(x) {
        tw_get_image(
          id = x,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          format = format,
          wait = wait
        )
      }
    )
  } else {
    link <- tidywikidatar::tw_get(
      id = id,
      cache = tw_check_cache(cache),
      overwrite_cache = overwrite_cache,
      language = language,
      wait = wait
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
}


#' Get Wikipedia article in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Defaults to NULL. If given, it should be given either TRUE or FALSE. Typically set with `tw_enable_cache()` or `tw_disable_cache()`.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en". For a full list of available languages, see: https://www.wikidata.org/wiki/Help:Wikimedia_language_codes/lists/all
#' @param wait In seconds, defaults to 0. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied. If you are running many queries systematically you may want to add some waiting time between queries.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#' tw_get_wikipedia(id = "Q180099")
tw_get_wikipedia <- function(id,
                             language = "en",
                             cache = NULL,
                             overwrite_cache = FALSE,
                             wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }
  if (length(id) > 1) {
    purrr::map_chr(
      .x = id,
      .f = function(x) {
        tw_get_wikipedia(
          id = x,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          wait = wait
        )
      }
    )
  } else {
    base_string <- stringr::str_c("sitelink_", language, "wiki")
    base_link <- tidywikidatar::tw_get(
      id = id,
      cache = tw_check_cache(cache),
      overwrite_cache = overwrite_cache,
      language = language,
      wait = wait
    ) %>%
      dplyr::filter(is.element(el = .data$property, set = base_string)) %>%
      dplyr::pull(.data$value)
    if (length(base_link) == 0) {
      as.character(NA)
    } else {
      stringr::str_c("https://", language, ".wikipedia.org/wiki/", base_link)
    }
  }
}
