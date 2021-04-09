test_that("check if tw_get returns tibble with three columns and meaningful number of rows", {
  expect_equal(object = {
    item <- tw_get(
      id = "Q180099",
      language = "en",
      include_id = TRUE
    )

    list(ncol = ncol(item),
         nrow = nrow(item)>200)
  }, expected = list(ncol = 3,
                     nrow = TRUE))
})
