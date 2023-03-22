library("testthat")
test_that("tw_get_description works with more than one id without cache", {
  test_id_df <- tw_get(
    id = c(
      "Q180099",
      "Q228822"
    ),
    language = "en",
    id_l = tw_test_items
  )

  expect_equal(
    object = {
      tw_disable_cache()
      description <- tw_get_description(
        id = c(
          "Q180099",
          "Q228822"
        ),
        language = "en",
        id_df = test_id_df
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
  test_id_df <- tw_get(
    id = c(
      "Q180099",
      "Q228822"
    ),
    language = "en",
    id_l = tw_test_items
  )

  expect_true(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      description <- tw_get_description(
        id = c("Q180099"),
        language = "en",
        id_df = test_id_df
      )

      stringr::str_detect(
        string = description,
        pattern = "American anthropologist"
      )
    }
  )
})

test_that("tw_get_description works with more than one id with cache", {
  test_id_df <- tw_get(
    id = c(
      "Q180099",
      "Q228822"
    ),
    language = "en",
    id_l = tw_test_items
  )

  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      description <- tw_get_description(
        id = c(
          "Q180099",
          "Q228822"
        ),
        language = "en",
        id_df = test_id_df
      )

      stringr::str_detect(
        string = description,
        pattern = "American anthropologist"
      )
    },
    c(TRUE, TRUE)
  )
})
