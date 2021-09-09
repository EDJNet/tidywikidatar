library("testthat")

test_that("cache respects param when given", {
  testthat::skip_if_offline()

  expect_equal(
    object = tw_check_cache(cache = TRUE),
    expected = TRUE
  )
  expect_equal(
    object = tw_check_cache(cache = FALSE),
    expected = FALSE
  )
})


test_that(
  desc = "Cache file location is returned correctly",
  code = {
    testthat::skip_if_offline()

    expect_equal(
      object = {
        tw_set_cache_folder(path = tempdir())
        tw_enable_cache()
        tw_get_cache_file(
          type = "item",
          language = "en"
        )
      },
      expected = {
        fs::path(
          tempdir(),
          "tw_item_db_en.sqlite"
        )
      }
    )
  }
)
