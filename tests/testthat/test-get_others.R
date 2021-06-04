library("testthat")
test_that("tw_get_description works with more than one id without cache", {
  expect_equal(
    object = {
      tw_disable_cache()
      description <- tw_get_description(
        id = c(
          "Q180099",
          "Q228822"
        ),
        language = "en"
      )

      stringr::str_detect(
        string = description,
        pattern = "American anthropologist"
      )
    },
    c(TRUE, TRUE)
  )
})



test_that("tw_get_description works with one id with cache", {
  expect_true(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      description <- tw_get_description(
        id = c("Q180099"),
        language = "en"
      )

      stringr::str_detect(
        string = description,
        pattern = "American anthropologist"
      )
    }
  )
})

test_that("tw_get_description works with more than one id with cache", {
  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      description <- tw_get_description(
        id = c(
          "Q180099",
          "Q228822"
        ),
        language = "en"
      )

      stringr::str_detect(
        string = description,
        pattern = "American anthropologist"
      )
    },
    c(TRUE, TRUE)
  )
})
