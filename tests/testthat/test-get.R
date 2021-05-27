library("testthat")
test_that("check if tw_get returns tibble with three columns and meaningful number of rows", {
  expect_equal(object = {
    item <- tw_get(
      id = "Q180099",
      language = "en"
    )

    list(
      ncol = ncol(item),
      nrow = nrow(item) > 100
    )
  }, expected = list(
    ncol = 3,
    nrow = TRUE
  ))
})

test_that("check if tw_get works when more than one id as input", {
  expect_equal(object = {
    item <- tw_get(
      id = c(
        "Q180099",
        "Q228822"
      ),
      language = "en"
    )

    list(
      ncol = ncol(item),
      nrow = nrow(item) > 200,
      unique_id = length(unique(item$id))
    )
  }, expected = list(
    ncol = 3,
    nrow = TRUE,
    unique_id = 2
  ))
})
