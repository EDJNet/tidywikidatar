library("testthat")
test_that("check if tw_check_search() returns search if input is not a search", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      tw_disable_cache()
      tw_check_search("Sylvia Pankhurst")
    },
    expected = {
      tw_check_search(tw_search("Sylvia Pankhurst"))
    }
  )
})

test_that("check if tw_search() works with input of length one", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      tw_disable_cache()
      search_df <- tw_search("Sylvia Pankhurst")
      list(id = search_df$id[1],
           label = search_df$label[1])
    },
    expected = {
      list(id = "Q298213",
           label = "Sylvia Pankhurst")
    }
  )
})

test_that("check if tw_search() works with input of length more than one", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      tw_disable_cache()
      search_df <- tw_search(search = c("Margaret Mead", "Ruth Benedict"))
      list(more_than_ten_results = nrow(search_df)>10,
           more_than_ten_different_results = nrow(search_df %>% dplyr::distinct())>10,
           three_columns = ncol(search_df)==3)
    },
    expected = {
      list(more_than_ten_results = TRUE,
           more_than_ten_different_results = TRUE,
           three_columns = TRUE)
    }
  )
})

