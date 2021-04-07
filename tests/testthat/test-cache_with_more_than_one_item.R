test_that("tw_check_cached_items() works as expected", {
  expect_equal(object = {
    tw_set_cache_folder(path = tempdir())
    tw_enable_cache()
    tw_create_cache_folder(ask = FALSE)

    # add three items to local cache
    invisible(tw_get(id = "Q180099", language = "en"))
    invisible(tw_get(id = "Q228822", language = "en"))
    invisible(tw_get(id = "Q184992", language = "en"))

    # check if these other items are in cache
    items_in_cache <- tw_check_cached_items(
      id = c(
        "Q180099",
        "Q228822",
        "Q76857"
      ),
      language = "en"
    )
    # it should return only the two items from the current list of id
    # but not other item already in cache
    items_in_cache
  }, expected = c(
    "Q180099",
    "Q228822"
  ))
})
