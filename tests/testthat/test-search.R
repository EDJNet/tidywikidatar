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
