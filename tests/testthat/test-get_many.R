test_that("check if labels are returned correctly when more than one id given", {
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
