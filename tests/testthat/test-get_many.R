library("testthat")

test_that("check if labels are returned correctly when more than one id given, including NA", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_get_label(
      id = c(
        "Q180099",
        "Q228822",
        "Not an id",
        "Q180099",
        NA
      ),
      language = "en"
    )
  }, expected = c(
    "Margaret Mead",
    "Ruth Benedict",
    NA,
    "Margaret Mead",
    NA
  ))
})
