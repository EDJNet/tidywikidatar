#' Extract item data from an object of class Wikidata created with `WikidataR`
#'
#' This function is mostly used internally and for testing.
#'
#' @param w An object of class Wikidata created with `WikidataR`, typically
#'   created with `tw_get_item(id = id)`
#' @inheritParams tw_get
#'
#' @return A data frame (a tibble) with four columns, such as the one created by
#'   [tw_get()].
#'
#' @examples
#' #' Retrieving from tests, but normally:
#' # w <- tw_get_item(id = "Q180099")
#'
#' tidywikidatar:::tw_extract_single(w = list(tw_test_items[["Q180099"]]))
tw_extract_single <- function(w, language = tidywikidatar::tw_get_language()) {
  id <- w %>%
    purrr::pluck(1, 1, "id")

  labels <- w %>%
    purrr::pluck(1, 1, "labels")

  if (length(labels) == 0) {
    labels_df <- tibble::tibble(
      property = NA_character_,
      value = NA_character_,
      rank = NA_character_
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

  aliases <- w %>% purrr::pluck(1, 1, "aliases")

  if (length(aliases) == 0) {
    aliases_df <- tibble::tibble(
      property = NA_character_,
      values = NA_character_,
      rank = NA_character_
    ) %>%
      dplyr::slice(0)
  } else {
    aliases_df <- purrr::map_dfr(
      .x = aliases,
      function(current_alias_l) {
        tibble::tibble(
          property = paste0("alias_", current_alias_l$language),
          value = current_alias_l$value,
          rank = NA_character_
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
    purrr::pluck(1, 1, "descriptions")

  if (length(descriptions) == 0) {
    descriptions_df <- tibble::tibble(
      property = NA_character_,
      values = NA_character_,
      rank = NA_character_
    ) %>%
      dplyr::slice(0)
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
        dplyr::filter(
          .data$property == stringr::str_c("description_", language)
        )
    }
  }

  claims <- w %>%
    purrr::pluck(1, 1, "claims")

  claims_df <- purrr::map_dfr(
    .x = claims,
    .f = function(current_claim_l) {
      property <- current_claim_l %>%
        purrr::pluck(1, "mainsnak", "property")

      rank <- current_claim_l %>%
        purrr::pluck(1, "rank")

      value_pre <- current_claim_l %>%
        purrr::pluck(1, "mainsnak", "datavalue", "value")

      if (is.null(value_pre)) {
        value <- as.character("NA")
      } else if (is.list(value_pre)) {
        if (is.element("time", names(value_pre))) {
          value <- value_pre$time
        } else if (is.element("text", names(value_pre))) {
          value <- value_pre$text
        } else if (is.element("amount", names(value_pre))) {
          value <- value_pre$amount
        } else if (is.element("latitude", names(value_pre))) {
          value <- stringr::str_c(
            value_pre$latitude,
            value_pre$longitude,
            sep = ","
          )
        } else if (is.element("id", names(value_pre))) {
          value <- value_pre$id
        } else if (!is.na(value_pre[[1]])) {
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
    purrr::pluck(1, 1, "sitelinks")

  sitelinks_df <- purrr::map_dfr(
    .x = sitelinks,
    function(current_sitelink_l) {
      tibble::tibble(
        property = paste0("sitelink_", current_sitelink_l$site),
        value = current_sitelink_l$title,
        rank = NA_character_
      )
    }
  )

  if (language == "all_available" | nrow(sitelinks_df) == 0) {
    # do nothing
  } else {
    sitelinks_df <- sitelinks_df %>%
      dplyr::filter(
        (.data$property ==
          stringr::str_c(
            "sitelink_",
            language,
            "wiki"
          )) |
          (.data$property ==
            stringr::str_c(
              "sitelink_",
              language,
              "wikiquote"
            )) |
          (.data$property ==
            stringr::str_c(
              "sitelink_",
              language,
              "wikisource"
            )) |
          (.data$property == "sitelink_commonswiki")
      )
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


#' Extract qualifiers from a list created with [tw_get_item()]
#'
#' This function is mostly used internally and for testing.
#'
#' @param id A character vector of length 1, must start with Q, e.g. "Q254" for
#'   Wolfgang Amadeus Mozart.
#' @param p A character vector of length 1, a property. Must always start with
#'   the capital letter "P", e.g. "P31" for "instance of".
#' @param w A list, typically created with [tw_get_item()]
#'
#' @return A data frame (a tibble) with eight columns: `id` for the input id,
#'   `property`,  `qualifier_id`, `qualifier_property`, `qualifier_value`,
#'   `rank`, `qualifier_value_type`, and `set` (to distinguish sets of data when
#'   a property is present more than once)
#' @export
#'
#' @examples
#' # w <- tw_get_item(id = "Q180099")
#'
#' tw_extract_qualifier(id = "Q180099", p = "P26", w = list(tw_test_items[["Q180099"]]))
tw_extract_qualifier <- function(id, p, w = NULL) {
  if (is.null(w)) {
    w <- tryCatch(tw_get_item(id = id), error = function(e) {
      as.character(e[[1]])
    })

    if (is.character(w)) {
      cli::cli_alert_danger(w)
      return(tidywikidatar::tw_empty_qualifiers)
    }
  }

  qualifiers <- w %>%
    purrr::pluck(
      1,
      1,
      "claims",
      p
    )

  qualifiers_df <- purrr::imap(
    .x = qualifiers,
    function(current_qualifier, i) {
      if (!is.element("qualifiers", names(current_qualifier))) {
        return(tidywikidatar::tw_empty_qualifiers)
      }

      current_rank <- current_qualifier %>%
        purrr::pluck("rank")

      qualifier_parent_pre <- current_qualifier %>%
        purrr::pluck("mainsnak", "datavalue", "value")

      if (is.character(qualifier_parent_pre)) {
        qualifier_parent <- qualifier_parent_pre[[1]]
      } else if (is.element("id", names(qualifier_parent_pre))) {
        qualifier_parent <- qualifier_parent_pre %>%
          purrr::pluck("id")
      } else {
        qualifier_parent <- qualifier_parent_pre %>%
          purrr::pluck(1)
      }

      qualifiers_set <- current_qualifier %>%
        purrr::pluck("qualifiers")

      purrr::map_dfr(
        .x = qualifiers_set,
        .f = function(x) {
          current_qualifier_set <- x %>%
            purrr::pluck(1) %>%
            tibble::as_tibble()

          value_df <- current_qualifier_set[["datavalue"]][["value"]] %>%
            tibble::as_tibble()
          if (is.element("id", names(value_df))) {
            value <- value_df %>%
              dplyr::pull("id")
          } else if (is.element("value", names(value_df))) {
            value <- value_df %>%
              dplyr::pull("value")
          } else if (is.element("amount", names(value_df))) {
            value <- value_df %>%
              dplyr::pull("amount")
          } else if (is.element("time", names(value_df))) {
            value <- value_df %>%
              dplyr::pull("time")
          } else {
            return(
              tidywikidatar::tw_empty_qualifiers %>%
                dplyr::select(
                  -"id",
                  -"property"
                )
            )
          }

          tibble::tibble(
            qualifier_id = qualifier_parent,
            qualifier_property = current_qualifier_set[["property"]][[1]],
            qualifier_value = value,
            qualifier_value_type = current_qualifier_set[["datavalue"]][[
              "type"
            ]],
            rank = current_rank,
            set = i
          )
        }
      )
    }
  ) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(
      id = id,
      property = p,
      .before = 0
    )
  qualifiers_df
}
