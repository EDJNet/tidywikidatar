#' Efficiently get a wide table with various properties of a given set of
#' Wikidata identifiers
#'
#' @param label Logical, defaults to FALSE. If TRUE labels of Wikidata Q
#'   identifiers are reported instead of the identifiers themselves (or labels
#'   are presented along of them, if `both_id_and_label` is set to TRUE)
#' @param property_label_as_column_name Logical, defaults to FALSE. If FALSE,
#'   names of columns with properties are the "P" identifiers of the property.
#'   If TRUE, the label of the correspondent property is assigned as column
#'   name.
#' @param both_id_and_label Logical, defaults to FALSE. Relevant only if `label`
#'   is set to TRUE, otherwise ignored. If TRUE, the label is added as a
#'   separate column along the original one. Column name is the same as the
#'   property column, followed by "_label".
#' @param id_df_label Defaults to NULL. If given, it should be a dataframe
#'   typically generated with `tw_get()` with *all* items for which labels will
#'   be requested. It is used instead of calling Wikidata or relying on cache.
#' @param unlist Logical, defaults to FALSE. Typically used sharing or exporting
#'   data as csv files. Collapses all properties in a single string. The
#'   separator is defined by the `collapse` parameter. Relevant only when
#'   `only_first` is set to FALSE.
#' @param collapse Defaults to ";". Character used to separate results when
#'   `unlist` is set to TRUE.
#'
#' @inheritParams tw_get_property_same_length
#'
#' @return A data frame, with a column for each given property.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   tw_get_p_wide(
#'     id = c("Q180099", "Q228822", "Q191095"),
#'     p = c("P27", "P19", "P20"),
#'     label = TRUE,
#'     only_first = TRUE
#'   )
#' }
tw_get_p_wide <- function(id,
                          p,
                          label = FALSE,
                          property_label_as_column_name = FALSE,
                          both_id_and_label = FALSE,
                          only_first = FALSE,
                          preferred = FALSE,
                          unlist = FALSE,
                          collapse = ";",
                          language = tidywikidatar::tw_get_language(),
                          id_df = NULL,
                          id_df_label = NULL,
                          cache = NULL,
                          overwrite_cache = FALSE,
                          cache_connection = NULL,
                          disconnect_db = TRUE,
                          wait = 0) {
  if (is.data.frame(id) == TRUE) {
    id <- id$id
  }

  db <- tw_connect_to_cache(
    connection = cache_connection,
    language = language,
    cache = cache
  )

  unique_p <- unique(p)
  property <- as.character(NA)
  value <- as.character(NA)

  property_df <- tw_get_property(
    id = id,
    p = unique_p,
    language = language,
    id_df = id_df,
    cache = cache,
    overwrite_cache = overwrite_cache,
    cache_connection = db,
    disconnect_db = FALSE,
    wait = wait
  )


  if (preferred == TRUE) {
    preferred_df <- property_df %>%
      dplyr::mutate(rank = factor(.data$rank,
        levels = c(
          "preferred",
          "normal",
          "deprecated"
        )
      )) %>%
      dplyr::group_by(id) %>%
      dplyr::arrange(
        .by_group = TRUE,
        .data$rank
      ) %>%
      dplyr::ungroup()

    if (nrow(preferred_df) > 0) {
      property_df <- preferred_df
    }
  }

  if (label == TRUE) {
    property_df <- property_df %>%
      dplyr::mutate(label = dplyr::if_else(
        condition = tw_check_qid(
          id = .data$value,
          logical_vector = TRUE
        ),
        true = tw_get_label(
          id = .data$value,
          language = language,
          cache = cache,
          overwrite_cache = overwrite_cache,
          cache_connection = db,
          disconnect_db = FALSE,
          id_df = id_df_label,
          wait = wait
        ),
        false = .data$value,
        missing = as.character(NA)
      ))
  }

  if (only_first == TRUE) {
    property_df <- property_df %>%
      dplyr::distinct(.data$id,
        .data$property,
        .keep_all = TRUE
      )
  } else if (only_first == FALSE) {
    property_df <- property_df %>%
      dplyr::distinct(.data$id,
        .data$property,
        .data$value,
        .keep_all = TRUE
      )

    if (label == TRUE) {
      property_df <- property_df %>%
        dplyr::group_by(.data$id, .data$property) %>%
        dplyr::summarise(
          value = list(.data$value),
          label = list(.data$label),
          .groups = "drop"
        ) %>%
        dplyr::ungroup()
    } else {
      property_df <- property_df %>%
        dplyr::group_by(.data$id, .data$property) %>%
        dplyr::summarise(
          value = list(.data$value),
          .groups = "drop"
        ) %>%
        dplyr::ungroup()
    }
  }

  if (label == TRUE) {
    if (both_id_and_label == TRUE) {
      if (only_first == TRUE) {
        property_df_wide <- property_df %>%
          tidyr::pivot_wider(
            id_cols = "id",
            names_from = "property",
            values_from = c("value", "label"),
            values_fill = as.character(NA),
            names_glue = "{property}_{.value}"
          )
      } else if (only_first == FALSE) {
        property_df_wide <- property_df %>%
          tidyr::pivot_wider(
            id_cols = "id",
            names_from = "property",
            values_from = c("value", "label"),
            values_fill = list(as.character(NA)),
            names_glue = "{property}_{.value}"
          )
      }
    } else if (both_id_and_label == FALSE) {
      if (only_first == TRUE) {
        property_df_wide <- property_df %>%
          tidyr::pivot_wider(
            id_cols = "id",
            names_from = "property",
            values_from = "label",
            values_fill = as.character(NA)
          )
      } else if (only_first == FALSE) {
        property_df_wide <- property_df %>%
          tidyr::pivot_wider(
            id_cols = "id",
            names_from = "property",
            values_from = "label",
            values_fill = list(as.character(NA))
          )
      }
    } else {
      usethis::ui_stop("The parameter `both_id_and_label` must be either TRUE or FALSE.")
    }


    if (both_id_and_label == TRUE) {
      new_order <- c(
        "id",
        t(matrix(c(
          stringr::str_c(unique_p, "_value"),
          stringr::str_c(unique_p, "_label")
        ),
        ncol = 2
        )) %>%
          as.character()
      )

      property_df_wide_ordered <- property_df_wide[, new_order]

      names(property_df_wide_ordered) <- stringr::str_remove_all(
        string = new_order,
        pattern = "_value$"
      )
    } else {
      new_order <- c(
        "id",
        unique_p
      )

      property_df_wide_ordered <- property_df_wide[, new_order]
    }
  } else if (label == FALSE) {
    if (only_first == TRUE) {
      property_df_wide <- property_df %>%
        tidyr::pivot_wider(
          id_cols = "id",
          names_from = "property",
          values_from = "value",
          values_fill = as.character(NA)
        )
    } else if (only_first == FALSE) {
      property_df_wide <- property_df %>%
        tidyr::pivot_wider(
          id_cols = "id",
          names_from = "property",
          values_from = "value",
          values_fill = list(as.character(NA))
        )
    }

    new_order <- c(
      "id",
      unique_p
    )

    property_df_wide_ordered <- property_df_wide[, new_order]
  }



  if (property_label_as_column_name == TRUE) {
    p_labels <- names(property_df_wide_ordered)[-1] %>%
      stringr::str_extract(pattern = "P[[:digit:]]+") %>%
      tw_get_property_label(
        language = language,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = FALSE,
        wait = wait
      )

    if (both_id_and_label == TRUE) {
      p_labels[!seq_along(p_labels) %% 2] <- stringr::str_c(
        p_labels[!seq_along(p_labels) %% 2],
        "_label"
      )
    }

    new_col_names <- p_labels %>%
      vctrs::vec_as_names(repair = "universal", quiet = TRUE) %>%
      stringr::str_replace_all(
        pattern = stringr::fixed("."),
        replacement = "_"
      )

    names(property_df_wide_ordered) <- c("id", new_col_names)
  }

  if (label == TRUE) {
    output_df <- tibble::tibble(id = id) %>%
      dplyr::mutate(label = tw_get_label(
        id = .data$id,
        language = language,
        cache = cache,
        overwrite_cache = overwrite_cache,
        cache_connection = db,
        disconnect_db = FALSE,
        id_df = id_df_label,
        wait = wait
      )) %>%
      dplyr::left_join(
        y = property_df_wide_ordered,
        by = "id"
      )
  } else {
    output_df <- tibble::tibble(id = id) %>%
      dplyr::left_join(
        y = property_df_wide_ordered,
        by = "id"
      )
  }
  tw_disconnect_from_cache(
    cache = cache,
    cache_connection = db,
    disconnect_db = disconnect_db,
    language = language
  )

  if (unlist == TRUE & only_first == FALSE) {
    output_df %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.list),
          function(x) {
            stringr::str_c(unique(unlist(x)),
              collapse = collapse
            )
          }
        )
      ) %>%
      dplyr::ungroup()
  } else {
    output_df
  }
}
