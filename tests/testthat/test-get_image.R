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
