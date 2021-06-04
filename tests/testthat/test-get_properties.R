test_that("check if property are returned correctly when more than 1 id and one property", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property(
      id = c(
        "Q180099",
        "Q228822"
      ),
      p = "P31"
    ) %>% dplyr::pull(value)
  }, expected = c(
    c("Q5", "Q5")
  ))
})

test_that("check if property are returned correctly when 1 id and more than one property", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property(
      id = "Q180099",
      p = c("P21", "P31")
    ) %>%
      dplyr::pull(value)
  }, expected = c(
    c("Q6581072", "Q5")
  ))
})




test_that("check if property labels are returned correctly", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_get_property_label(property = "P31")
  }, expected = c(
    "instance of"
  ))

  expect_equal(object = {
    tw_get_property_label(property = c("P31", "P21"))
  }, expected = c(
    "instance of",
    "sex or gender"
  ))
})



test_that("check if property labels are returned correctly with cache", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_set_cache_folder(path = tempdir())
    tw_enable_cache()
    tw_get_property_label(property = "P31")
  }, expected = c(
    "instance of"
  ))

  expect_equal(object = {
    tw_get_property_label(property = c("P31", "P21"))
  }, expected = c(
    "instance of",
    "sex or gender"
  ))
})



test_that("check if property descriptions are returned correctly", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property_description(property = "P31")
  }, expected = c(
    "that class of which this subject is a particular example and member"
  ))

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property_description(property = c("P31", "P21"))
  }, expected = c(
    "that class of which this subject is a particular example and member",
    "sex or gender identity of human or animal. For human: male, female, non-binary, intersex, transgender female, transgender male, agender. For animal: male organism, female organism. Groups of same gender use subclass of (P279)"
  ))
})



test_that("check if property descriptions are returned correctly with cache", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_set_cache_folder(path = tempdir())
    tw_enable_cache()
    tw_get_property_description(property = "P31")
  }, expected = c(
    "that class of which this subject is a particular example and member"
  ))

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property_description(property = c("P31", "P21"))
  }, expected = c(
    "that class of which this subject is a particular example and member",
    "sex or gender identity of human or animal. For human: male, female, non-binary, intersex, transgender female, transgender male, agender. For animal: male organism, female organism. Groups of same gender use subclass of (P279)"
  ))
})
