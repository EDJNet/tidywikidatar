library("testthat")
library("tidywikidatar")

test_that("check if image returned when valid id given", {
  testthat::skip_if_offline()

  test_id_df <- tw_get(
    id = "Q2",
    id_l = tw_test_items
  )

  expect_true(
    object = {
      tw_get_image(
        id = "Q2",
        cache = FALSE,
        id_df = test_id_df
      ) %>%
        tidyr::drop_na() %>%
        nrow() %>%
        as.logical()
    }
  )

  tw_set_cache_folder(path = tempdir())

  expect_true(
    object = {
      tw_get_image(
        id = "Q2",
        cache = TRUE,
        id_df = test_id_df
      ) %>%
        tidyr::drop_na() %>%
        nrow() %>%
        as.logical()
    }
  )
})

test_that("check if image returned when invalid id given", {
  testthat::skip_if_offline()

  test_id_df <- tw_get(
    id = "Q2",
    id_l = tw_test_items
  )


  expect_true(
    object = {
      tw_get_image(id = "non_qid_string") %>%
        is.null()
    }
  )

  expect_true(
    object = {
      tw_get_image_same_length(
        id = "non_qid_string",
        id_df = test_id_df
      ) %>%
        is.na()
    }
  )

  expect_equal(
    object = {
      tw_get_image_same_length(
        id = c("non_qid_string", NA),
        id_df = test_id_df
      ) %>%
        is.na() %>%
        sum()
    }, expected = 2
  )

  expect_equal(
    object = {
      tw_get_image_same_length(
        id = c("non_qid_string", "Q2", "non_qid_string", NA),
        id_df = test_id_df
      ) %>%
        is.na() %>%
        which()
    }, expected = c(1, 3, 4)
  )
})


test_that("check if image metadata returned correctly with or without cache", {
  testthat::skip_if_offline()
  testthat::skip_on_cran() # to prevent error due to calls to Wikimedia Commons from CRAN server

  test_id_df <- tw_get(
    id = "Q2",
    id_l = tw_test_items
  )

  expect_equal(
    object = {
      df <- tw_get_image_metadata_single(
        id = c("Q2"),
        only_first = TRUE,
        cache = FALSE
      )

      list(
        ncol = ncol(df),
        nrow = nrow(df),
        id = df %>% dplyr::pull(id)
      )
    }, expected = list(
      ncol = 19,
      nrow = 1,
      id = "Q2"
    )
  )

  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())

      df <- tw_get_image_metadata(
        id = c("Q2", NA, "not_an_id", "Q5"),
        only_first = TRUE,
        cache = TRUE,
        id_df = test_id_df
      )

      list(
        ncol = ncol(df),
        nrow = nrow(df),
        id = df %>% dplyr::pull(id),
        missing_image = which(is.na(df$image_filename))
      )
    }, expected = list(
      ncol = 19,
      nrow = 4,
      id = c("Q2", NA, "not_an_id", "Q5"),
      missing_image = c(2, 3)
    )
  )

  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())

      df <- tw_get_image_metadata(
        id = c("Q2", NA, "not_an_id", "Q5"),
        only_first = TRUE,
        cache = FALSE,
        id_df = test_id_df
      )

      list(
        ncol = ncol(df),
        nrow = nrow(df),
        id = df %>% dplyr::pull(id)
      )
    }, expected = list(
      ncol = 19,
      nrow = 4,
      id = c("Q2", NA, "not_an_id", "Q5")
    )
  )
})
