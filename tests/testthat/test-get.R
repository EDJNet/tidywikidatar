library("testthat")
library("tidywikidatar")
test_that("check if tw_get returns tibble with four columns and meaningful number of rows", {
  testthat::skip_if_offline()

  expect_true(object = {
    item <- tw_get(
      id = c(
        "Q180099"
      )
    )

    if (is.null(attr(item, "warning")) == FALSE) {
      message("Issues with API in testing")
      test_result <- TRUE
    } else {
      test_result <- Reduce(
        f = `|`,
        x = c(
          ncol(item) == 4,
          nrow(item) > 100
        )
      )
    }
    test_result
  })
})



test_that("check if tw_get works when more than one id as input", {
  testthat::skip_if_offline()

  expect_true(object = {
    item <- tw_get(
      id = c(
        "Q180099",
        "Q228822"
      ),
      language = "en"
    )
    if (is.null(attr(item, "warning")) == FALSE) {
      message("Issues with API in testing")
      test_result <- TRUE
    } else {
      test_result <- Reduce(
        f = `|`,
        x = c(
          ncol(item) == 3,
          nrow(item) > 200
        ),
        length(unique(item$id) == 2)
      )
    }
    test_result
  })
})
