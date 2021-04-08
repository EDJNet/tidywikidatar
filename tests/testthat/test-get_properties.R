
test_that("check if property labels are returned correctly", {
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



test_that("check if property descriptions are returned correctly", {
  expect_equal(object = {
    tw_get_property_description(property = "P31")
  }, expected = c(
    "that class of which this subject is a particular example and member"
  ))

  expect_equal(object = {
    tw_get_property_description(property = c("P31", "P21"))
  }, expected = c(
    "that class of which this subject is a particular example and member",
    "sex or gender identity of human or animal. For human: male, female, non-binary, intersex, transgender female, transgender male, agender. For animal: male organism, female organism. Groups of same gender use subclass of (P279)"
  ))
})
