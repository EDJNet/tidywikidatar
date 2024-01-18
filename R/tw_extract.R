#' Extract item data from an object of class Wikidata created with `WikidataR`
#'
#' This function is mostly used internally and for testing.
#'
#' @param w An object of class Wikidata created with `WikidataR`, typically created with `WikidataR::get_item(id = id)`
#' @inheritParams tw_get
#'
#' @return A data frame (a tibble) with four columns, such as the one created by `tw_get`.
#'
#' @examples
#' item <- tryCatch(WikidataR::get_item(id = "Q180099"),
#'   error = function(e) {
#'     as.character(e[[1]])
#'   }
#' )
#'
#' tidywikidatar:::tw_extract_single(w = item)
tw_extract_single <- function(w,
                              language = tidywikidatar::tw_get_language()) {
  id <- w %>%
    purrr::pluck(1, "id")

  labels <- w %>%
    purrr::pluck(1, "labels")

  if (length(labels) == 0) {
    labels_df <- tibble::tibble(
      property = as.character(NA),
      value = as.character(NA),
      rank = as.character(NA)
    ) %>%
      dplyr::slice(0)
  } else {
    labels_df <- purrr::map_dfr(
      .x = labels,
      function(current_label_l) {
        tibble::tibble(
          property = paste0("label_", current_label_l$language),
          value = current_label_l$value,
          rank = as.character(NA)
        )
      }
    )
  }


  if (language == "all_available") {
    # do nothing
  } else {
    labels_df <- labels_df %>%
      dplyr::filter(.data$property == stringr::str_c("label_", language))
  }

  aliases <- w %>% purrr::pluck(1, "aliases")

  if (length(aliases) == 0) {
    aliases_df <- tibble::tibble(
      property = as.character(NA),
      values = as.character(NA),
      rank = as.character(NA)
    ) %>%
      tidyr::drop_na()
  } else {
    aliases_df <- purrr::map_dfr(
      .x = aliases,
      function(current_alias_l) {
        tibble::tibble(
          property = paste0("alias_", current_alias_l$language),
          value = current_alias_l$value,
          rank = as.character(NA)
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

  descriptions <- w %>%
    purrr::pluck(1, "descriptions")

  if (length(descriptions) == 0) {
    descriptions_df <- tibble::tibble(
      property = as.character(NA),
      values = as.character(NA),
      rank = as.character(NA)
    ) %>%
      tidyr::drop_na()
  } else {
    descriptions_df <- purrr::map_dfr(
      .x = descriptions,
      function(current_description_l) {
        tibble::tibble(
          property = paste0("description_", current_description_l$language),
          value = current_description_l$value,
          rank = as.character(NA)
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

  claims <- w %>%
    purrr::pluck(1, "claims")

  claims_df <- purrr::map_dfr(
    .x = claims,
    .f = function(current_claim_l) {
      property <- current_claim_l$mainsnak$property

      rank <- current_claim_l$rank

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
        value = value,
        rank = rank
      )
    }
  )


  sitelinks <- w %>%
    purrr::pluck(1, "sitelinks")

  sitelinks_df <- purrr::map_dfr(
    .x = sitelinks,
    function(current_sitelink_l) {
      tibble::tibble(
        property = paste0("sitelink_", current_sitelink_l$site),
        value = current_sitelink_l$title,
        rank = as.character(NA)
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
      .data$value,
      .data$rank
    )
  everything_df
}





#' Extract qualifiers from an object of class Wikidata created with `WikidataR`
#'
#' This function is mostly used internally and for testing.
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with the capital letter "P", e.g. "P31" for "instance of".
#' @param w An object of class Wikidata created with `WikidataR`, typically created with `WikidataR::get_item(id = id)`
#'
#' @return A data frame (a tibble) with eight columns: `id` for the input id, `property`,  `qualifier_id`, `qualifier_property`, `qualifier_value`, `rank`, `qualifier_value_type`, and `set` (to distinguish sets of data when a property is present more than once)
#' @export
#'
#' @examples
#' w <- WikidataR::get_item(id = "Q180099")
#' tw_extract_qualifier(id = "Q180099", p = "P26", w = w)
tw_extract_qualifier <- function(id,
                                 p,
                                 w = NULL) {
  if (is.null(w)) {
    w <- tryCatch(WikidataR::get_item(id = id),
      error = function(e) {
        as.character(e[[1]])
      }
    )

    if (is.character(w)) {
      cli::cli_alert_danger(w)
      return(tidywikidatar::tw_empty_qualifiers)
    }
  }

  claims <- w %>%
    purrr::pluck(
      1,
      "claims"
    )

  qualifiers <- claims[[p]] %>%
    tibble::as_tibble()

  if (is.element("qualifiers", colnames(qualifiers)) == FALSE) {
    return(tidywikidatar::tw_empty_qualifiers)
  }

  qualifiers_df <- purrr::map_dfr(
    .x = 1:nrow(qualifiers),
    function(i) {
      qualifier_parent_pre <- qualifiers %>%
        dplyr::slice(i) %>%
        dplyr::pull("mainsnak") %>%
        dplyr::pull("datavalue") %>%
        dplyr::pull("value")

      if (is.character(qualifier_parent_pre)) {
        qualifier_parent <- qualifier_parent_pre[[1]]
      } else if (is.element("id", colnames(qualifier_parent_pre))) {
        qualifier_parent <- qualifier_parent_pre %>%
          dplyr::pull("id")
      } else {
        qualifier_parent <- qualifier_parent_pre %>%
          dplyr::pull(1)
      }

      qualifiers_set <- qualifiers %>%
        dplyr::slice(i) %>%
        dplyr::pull("qualifiers")


      purrr::map_dfr(
        .x = qualifiers_set,
        .f = function(x) {
          current_qualifier <- x %>%
            purrr::pluck(1) %>%
            tibble::as_tibble()

          value_df <- current_qualifier[["datavalue"]][["value"]] %>%
            tibble::as_tibble()
          if (is.element("id", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull("id")
          } else if (is.element("value", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull("value")
          } else if (is.element("amount", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull("amount")
          } else if (is.element("time", names(value_df)) == TRUE) {
            value <- value_df %>%
              dplyr::pull("time")
          } else {
            return(tidywikidatar::tw_empty_qualifiers %>%
              dplyr::select(
                -"id",
                -"property"
              ))
          }

          tibble::tibble(
            qualifier_id = qualifier_parent,
            qualifier_property = current_qualifier[["property"]],
            qualifier_value = value,
            qualifier_value_type = current_qualifier[["datavalue"]][["type"]],
            rank = qualifiers[["rank"]][[i]],
            set = i
          )
        }
      )
    }
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      id = id,
      property = p,
      .before = 0
    )
  qualifiers_df
}
