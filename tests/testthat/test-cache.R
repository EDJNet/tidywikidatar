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






test_that(
  desc = "SQLite-based caching works as expected when enabled via parameters",
  code = {
    testthat::skip_if_offline()

    tw_set_cache_folder(path = fs::path(tempdir(), "tw_cache"))

    tw_enable_cache()
    tw_create_cache_folder(ask = FALSE)

    expect_equal(
      object = {
        tw_get_single(id = "Q180099")
      },
      expected = {
        tw_get_cached_item(id = "Q180099")
      }
    )
  }
)
