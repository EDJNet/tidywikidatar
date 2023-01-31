if (Sys.getenv("tidywikidatar_testing") == "manual") {
  test_that("check if tw_get cached correctly deleted Wikidata items", {
    testthat::skip_if_offline()

    expect_true(object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()

      item <- tw_get(
        id = "Q4007665",
        cache = TRUE
      )
      cached_item_df <- tw_get_cached_item(id = "Q4007665")

      cached_item_df$property == "error"
    })
  })
}
