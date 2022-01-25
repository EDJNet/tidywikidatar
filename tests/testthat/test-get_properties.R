library("testthat")
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

test_that("check if property are returned correctly and in the same as order as given when 1 id and more than one property", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property(
      id = "Q180099",
      p = c("P31", "P27")
    ) %>%
      dplyr::pull(value)
  }, expected = c(
    c("Q5", "Q30")
  ))

  expect_equal(object = {
    tw_disable_cache()
    tw_get_property(
      id = "Q180099",
      p = c("P27", "P31")
    ) %>%
      dplyr::pull(value)
  }, expected = c(
    c("Q30", "Q5")
  ))
})

test_that("check if tw_get_property with invalid identifiers", {
  testthat::skip_if_offline()

  expect_true(object = {
    tw_disable_cache()
    tw_get_property(
      id = "non_qid_string",
      p = "P27"
    ) %>%
      is.null()
  })

  expect_true(object = {
    tw_disable_cache()
    tw_get_property_same_length(
      id = "non_qid_string",
      p = "P27"
    ) %>%
      is.na()
  })
})




test_that("check if property labels are returned correctly", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_get_property_label(property = "P31")
  }, expected = c(
    "instance of"
  ))

  expect_equal(object = {
    tw_get_property_label(property = c("P31", "P361"))
  }, expected = c(
    "instance of",
    "part of"
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
    tw_set_cache_folder(path = tempdir())
    tw_get_property_label(property = c("P31", "P361"))
  }, expected = c(
    "instance of",
    "part of"
  ))

  expect_equal(object = {
    tw_set_cache_folder(path = tempdir())
    tw_get_property_label(property = c("P31", "P361", "non_id_string", NA))
  }, expected = c(
    "instance of",
    "part of",
    NA,
    NA
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
    tw_get_property_description(property = c("P31", "P361")) %>%
      stringr::word(end = 3)
  }, expected = c(
    "that class of",
    "object of which"
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
    tw_get_property_description(property = c("P31", "P361")) %>%
      stringr::word(end = 3)
  }, expected = c(
    "that class of",
    "object of which"
  ))
})

test_that("check if property are returned correctly when multiple NA included", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_get_property_same_length(
      id = c("Q180099", NA, "Q228822", NA),
      p = "P31",
      only_first = FALSE
    ) %>%
      unlist() %>%
      is.na() %>%
      sum()
  }, expected = 2)
})

