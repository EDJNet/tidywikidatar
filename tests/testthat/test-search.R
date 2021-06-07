library("testthat")

test_that("check if tw_check_search() returns search if input is not a search", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      tw_disable_cache()
      tw_check_search(search = "Sylvia Pankhurst")
    },
    expected = {
      tw_check_search(search = tw_search("Sylvia Pankhurst"))
    }
  )
})

test_that("check if tw_search() works with input of length one without cache", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      tw_disable_cache()
      search_df <- tw_search("Sylvia Pankhurst")
      list(
        id = search_df$id[1],
        label = search_df$label[1]
      )
    },
    expected = {
      list(
        id = "Q298213",
        label = "Sylvia Pankhurst"
      )
    }
  )
})
