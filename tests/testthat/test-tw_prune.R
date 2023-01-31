library("testthat")
test_that("invalid properties are not accepted", {

  expect_error(object = tw_prune(p = c(1, 2)))

  expect_error(object = tw_prune(p = c("P5", "aaa")))

 # expect_no_error(object = tw_prune(p = c("P5", NA))) NA are dropped

})
