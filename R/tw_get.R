#' Search string in Wikidata and return Wikidata id, label, and description.
#'
#' @param search A string to be searched in Wikidata
#' @param language Language to be used for the search
#' @param limit Maximum numbers of responses to be given.
#' @param wait In seconds, defaults to 0.1. Time to wait between queries to Wikidata. If data are cached locally, wait time is not applied.
#' @param cache Logical, defaults to TRUE. If TRUE, search queries are stored in a local sqlite database located in the `wiki_search_db` folder within the local cache folder.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tw_search(search = "Mihai Eminescu", language = "en")
#' tw_search(search = "Mihai Eminescu", language = "ro")
#' }
#'
tw_search <- function(search,
                      language = "en",
                      limit = 10,
                      wait = 0.1,
                      cache = TRUE) {
  if (is.null(search)) {
    usethis::ui_stop("A search string must be given.")
  }

  if (cache == TRUE) {
    tidywikidatar::tw_create_cache_folder()
    db_folder <- fs::path(
      tidywikidatar::tw_get_cache_folder(),
      "wiki_search_db"
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c(language, ".sqlite")
    )
    db <- DBI::dbConnect(drv = RSQLite::SQLite(), db_file)
    db_result <- tryCatch(
      DBI::dbReadTable(
        conn = db,
        name = stringr::str_to_lower(search)
      ),
      error = function(e) {
        logical(1L)
      }
    )
    if (is.data.frame(db_result)) {
      DBI::dbDisconnect(db)
      return(db_result %>% dplyr::filter(is.na(id) == FALSE))
    }
  }

  Sys.sleep(time = wait)

  search_response <- tryCatch(WikidataR::find_item(
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
  if (length(search_response)==0) {
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
          label = dplyr::if_else(is.null(x %>% purrr::pluck("label")), as.character(NA), x %>% purrr::pluck("label")),
          description = dplyr::if_else(
            condition = is.null(x %>% purrr::pluck("description")),
            true = as.character(NA),
            false = x %>% purrr::pluck("description")
          )
        )
      }
    )
  }

  if (cache == TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_lower(string = search),
      value = search_response_df
    )
    DBI::dbDisconnect(db)
  }
  search_response_df %>%
    dplyr::filter(is.na(id)==FALSE) %>%
    tibble::as_tibble()
}


#' Return (almost) all information from a Wikidata item in a tidy format
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart. Can also be a data frame of one row, typically generated with `tw_search()` or a combination of `tw_search()` and `tw_filter_first()`.
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A data.frame (a tibble) with two columns: property and value
#' @export
#'
#' @examples
#' \dontrun{
#' tw_get("Q254")
#' }
#'
tw_get <- function(id,
                   cache = TRUE,
                   overwrite_cache = FALSE) {

  if (is.data.frame(id)==TRUE) {
    id <- id$id
  }

  if (cache == TRUE) {
    tidywikidatar::tw_create_cache_folder()
    db_folder <- fs::path(
      tidywikidatar::tw_get_cache_folder(),
      "wiki_q_db"
    )
    fs::dir_create(db_folder)
    db_file <- fs::path(
      db_folder,
      stringr::str_c("wiki_q_db", ".sqlite")
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
    if (is.data.frame(db_result)&overwrite_cache==FALSE) {
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
        } else if (is.na(value_pre[[1]])==FALSE) {
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
  if (cache == TRUE&overwrite_cache==FALSE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = everything_df
    )
    DBI::dbDisconnect(db)
  } else if (cache == TRUE&overwrite_cache==TRUE) {
    RSQLite::dbWriteTable(
      conn = db,
      name = stringr::str_to_upper(string = id),
      value = everything_df,
      overwrite = TRUE
    )
    DBI::dbDisconnect(db)
  }
  everything_df
}


#' Get Wikidata label in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en", must correspond to a two-letter language code.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#'
#'
tw_get_label <- function(id, language = "en",
                         cache = TRUE,
                         overwrite_cache = FALSE) {
  if (is.data.frame(id)==TRUE) {
    id <- id$id
  }
  label <- tidywikidatar::tw_get(id = id,
                                 cache = cache,
                                 overwrite_cache = overwrite_cache) %>%
    dplyr::filter(stringr::str_starts(string = property,
                                      pattern = "label_"),
                  stringr::str_ends(string = property,
                                    pattern = stringr::str_c(language,
                                                             collapse = "|"))) %>%
    dplyr::pull(value)

  if (length(label)==0) {
    as.character(NA)
  } else {
    label
  }
}

#' Get Wikidata description in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en", must correspond to a two-letter language code.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#'
#'
tw_get_description <- function(id,
                               language = "en",
                               cache = TRUE,
                               overwrite_cache = FALSE) {
  if (is.data.frame(id)==TRUE) {
    id <- id$id
  }
  description <- tidywikidatar::tw_get(id = id,
                                       cache = cache,
                                       overwrite_cache = overwrite_cache) %>%
    dplyr::filter(stringr::str_starts(string = property,
                                      pattern = "description_"),
                  stringr::str_ends(string = property,
                                    pattern = stringr::str_c(language,
                                                             collapse = "|"))) %>%
    dplyr::pull(value)
  if (length(description)==0) {
    as.character(NA)
  } else {
    description
  }
}

#' Get Wikidata property
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A charachter vector of length 1, corresponding to the value for the given property.
#' @export
#'
#' @examples
#'
#'
tw_get_property <- function(id,
                            p,
                            cache = TRUE,
                            overwrite_cache = FALSE) {
  if (is.data.frame(id)==TRUE) {
    id <- id$id
  }
  property <- tidywikidatar::tw_get(id = id,
                                    cache = cache,
                                    overwrite_cache = overwrite_cache) %>%
    dplyr::filter(property == p) %>%
    dplyr::pull(value)
  if (length(property)==0) {
    as.character(NA)
  } else {
    property
  }
}

#' Get Wikidata image
#'
#' @param id A characther vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param format A charachter vector, defaults to 'filename". If set to 'commons', outputs the link to the Wikimedia Commons page. If set to "embed", outputs a link that can be used to embed.
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#'
#' @return A charachter vector, corresponding to reference to the image in the requested format.
#' @export
#'
#' @examples
#'
#'
tw_get_image <- function(id,
                         cache = TRUE,
                         overwrite_cache = FALSE,
                         format = "filename") {
  if (is.data.frame(id)==TRUE) {
    id <- id$id
  }
  link <- tidywikidatar::tw_get(id = id,
                                cache = cache,
                                overwrite_cache = overwrite_cache) %>%
    dplyr::filter(property == "P18") %>%
    dplyr::pull(value)

  if (length(link)==0) {
    return(as.character(NA))
  }

  if (format=="filename") {
    link
  } else if  (format=="commons") {
    link %>%
      stringr::str_c("https://commons.wikimedia.org/wiki/File:", .)
  } else if (format =="embed") {
    link %>%
      stringr::str_c("https://upload.wikimedia.org/wikipedia/commons/d/dc/", .)
  } else {
    link
  }
}


#' Get Wikidata description in given language
#'
#' @param id A characther vector, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart
#' @param cache Logical, defaults to TRUE. If TRUE, it stores all retrieved data in a local sqlite database.
#' @param overwrite_cache Logical, defaults to FALSE. If TRUE, it overwrites the table in the local sqlite database. Useful if the original Wikidata object has been updated.
#' @param language A character vector of length one, defaults to "en", must correspond to a two-letter language code.
#'
#' @return A charachter vector of length 1, with the Wikidata label in the requested languae.
#' @export
#'
#' @examples
#'
#'
tw_get_wikipedia <- function(id,
                             language = "en",
                             cache = TRUE,
                             overwrite_cache = FALSE) {
  if (is.data.frame(id)==TRUE) {
    id <- id$id
  }
  base_string <- stringr::str_c("sitelink_", language, "wiki")
  base_link <- tidywikidatar::tw_get(id = id,
                                     cache = cache,
                                     overwrite_cache = overwrite_cache) %>%
    dplyr::filter(is.element(el = property, set = base_string)) %>%
    dplyr::pull(value)
  if (length(base_link)==0) {
    as.character(NA)
  } else {
    stringr::str_c("https://", language, ".wikipedia.org/wiki/", base_link)
  }
}
