library("testthat")

test_that("tw_get_p_wide works with labels", {
  testthat::skip_if_offline()
  testthat::skip_on_cran()

  tw_set_cache_folder(path = tempdir())
  tw_enable_cache()

  expect_equal(
    object = {
      df <- tw_get_p_wide(
        id = c("Q180099", "Q228822", "Q191095"),
        p = c("P27", "P19", "P20"),
        both_id_and_label = FALSE,
        label = TRUE,
        property_label_as_column_name = TRUE,
        only_first = TRUE
      )

      l <- list(
        ncol = ncol(df),
        nrow = nrow(df),
        colnames = colnames(df)
      )

      l
    },
    expected = {
      list(
        ncol = 5,
        nrow = 3,
        colnames = c(
          "id",
          "label",
          "country_of_citizenship",
          "place_of_birth",
          "place_of_death"
        )
      )
    }
  )

  expect_equal(
    object = {
      df <- tw_get_p_wide(
        id = c("Q180099", "Q228822", "Q191095"),
        p = c("P27", "P19", "P20"),
        both_id_and_label = FALSE,
        label = TRUE,
        property_label_as_column_name = FALSE,
        only_first = TRUE
      )

      l <- list(
        ncol = ncol(df),
        nrow = nrow(df),
        colnames = colnames(df)
      )

      l
    },
    expected = {
      list(
        ncol = 5,
        nrow = 3,
        colnames = c(
          "id",
          "label",
          "P27",
          "P19",
          "P20"
        )
      )
    }
  )

  expect_equal(
    object = {
      df <- tw_get_p_wide(
        id = c("Q180099", "Q228822", "Q191095"),
        p = c("P27", "P19", "P20"),
        both_id_and_label = FALSE,
        label = TRUE,
        property_label_as_column_name = FALSE,
        only_first = FALSE
      )

      l <- list(
        ncol = ncol(df),
        nrow = nrow(df),
        colnames = colnames(df)
      )

      l
    },
    expected = {
      list(
        ncol = 5,
        nrow = 3,
        colnames = c(
          "id",
          "label",
          "P27",
          "P19",
          "P20"
        )
      )
    }
  )

  expect_equal(
    object = {
      df <- tw_get_p_wide(
        id = c("Q180099", "Q228822", "Q191095"),
        p = c("P27", "P19", "P20"),
        both_id_and_label = TRUE,
        label = TRUE,
        property_label_as_column_name = FALSE,
        only_first = FALSE
      )

      l <- list(
        ncol = ncol(df),
        nrow = nrow(df),
        colnames = colnames(df),
        country_id = df$P27[[1]]
      )

      l
    },
    expected = {
      list(
        ncol = 8,
        nrow = 3,
        colnames = c(
          "id",
          "label",
          "P27",
          "P27_label",
          "P19",
          "P19_label",
          "P20",
          "P20_label"
        ),
        country_id = "Q30"
      )
    }
  )


  expect_equal(
    object = {
      df <- tw_get_p_wide(
        id = c("Q180099", "Q228822", "Q191095"),
        p = c("P27", "P19", "P20"),
        both_id_and_label = TRUE,
        label = TRUE,
        property_label_as_column_name = TRUE,
        only_first = FALSE
      )

      l <- list(
        ncol = ncol(df),
        nrow = nrow(df),
        colnames = colnames(df),
        country_id = df$country_of_citizenship[[1]],
        country_label = df$country_of_citizenship_label[[1]]
      )

      l
    },
    expected = {
      list(
        ncol = 8,
        nrow = 3,
        colnames = c(
          "id",
          "label",
          "country_of_citizenship",
          "country_of_citizenship_label",
          "place_of_birth",
          "place_of_birth_label",
          "place_of_death",
          "place_of_death_label"
        ),
        country_id = "Q30",
        country_label = "United States of America"
      )
    }
  )
})
