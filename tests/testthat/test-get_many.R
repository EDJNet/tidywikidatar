library("testthat")

test_that("check if labels are returned correctly when more than one id given", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_get_label(
      id = c(
        "Q180099",
        "Q228822"
      ),
      language = "en"
    )
  }, expected = c(
    "Margaret Mead",
    "Ruth Benedict"
  ))
})
