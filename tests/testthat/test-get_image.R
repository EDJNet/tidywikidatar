library("testthat")

test_that("check if image returned when valid id given", {
  testthat::skip_if_offline()

  expect_true(
    object = {
      tw_get_image(id = "Q2") %>%
        tidyr::drop_na() %>%
        nrow() %>%
        as.logical()
    }
  )
})

test_that("check if image returned when invalid id given", {
  testthat::skip_if_offline()

  expect_true(
    object = {
      tw_get_image(id = "non_qid_string") %>%
        is.null()
    }
  )

  expect_true(
    object = {
      tw_get_image_same_length(id = "non_qid_string") %>%
        is.na()
    }
  )

  expect_equal(
    object = {
      tw_get_image_same_length(id = c("non_qid_string", NA)) %>%
        is.na() %>%
        sum()
    }, expected = 2
  )

  expect_equal(
    object = {
      tw_get_image_same_length(id = c("Q2", "non_qid_string", NA)) %>%
        is.na() %>%
        sum()
    }, expected = 2
  )
})


test_that("check if image metadata returned correctly with or without cache", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      df <- tw_get_image_metadata_single(
        id = c("Q2"),
        only_first = TRUE
      )

      list(
        ncol = ncol(df),
        nrow = nrow(df),
        id = df[1, 1]
      )
    }, expected = list(
      ncol = ncol(12),
      nrow = nrow(1),
      id = "Q2"
    )
  )

  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      df <- tw_get_image_metadata(
        id = c("Q2", NA, "not_an_id", "Q5"),
        only_first = TRUE
      )

      list(
        ncol = ncol(df),
        nrow = nrow(df),
        id = df[1, 1]
      )
    }, expected = list(
      ncol = ncol(12),
      nrow = nrow(1),
      id = "Q2"
    )
  )
})
