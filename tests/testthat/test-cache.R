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



test_that("items are stored and retrieved from cache correctly", {
  testthat::skip_if_offline()

  expect_equal(object = {
    tw_set_cache_folder(
      path = fs::path(tempdir(),
                      stringi::stri_rand_strings(n = 1, length = 24))
      )
    tw_enable_cache()
    tw_create_cache_folder(ask = FALSE)

    tw_get(
      id = "Q180099",
      language = "en"
    )
  }, expected = tw_get_cached_item(
    id = "Q180099",
    language = "en"
  ))

  expect_false(object = {
    tw_set_cache_folder(
      path = fs::path(tempdir(),
                      stringi::stri_rand_strings(n = 1, length = 24))
    )
    tw_enable_cache()
    tw_create_cache_folder(ask = FALSE)

    df_from_api <- tw_get(
      id = "Q180099",
      language = "en"
    )

    is.null(df_from_api)
  })
})
