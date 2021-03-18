test_that("cache defaults to FALSE", {
  expect_equal(
    object = tw_check_cache(cache = NULL),
    expected = FALSE
  )
})

test_that("cache respects param when given", {
  expect_equal(
    object = tw_check_cache(cache = TRUE),
    expected = TRUE
  )
  expect_equal(
    object = tw_check_cache(cache = FALSE),
    expected = FALSE
  )
})
