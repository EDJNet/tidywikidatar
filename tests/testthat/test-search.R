test_that("check if tw_check_search() returns search if input is not a search", {
  expect_equal(
    object = {
      tw_check_search("Sylvia Pankhurst")
    },
    expected = {
      tw_check_search(tw_search("Sylvia Pankhurst"))
    }
  )
})
