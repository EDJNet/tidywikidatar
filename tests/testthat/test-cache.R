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


test_that(
  desc = "Cache file location is returned correctly",
  code = {
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
          "tw_item_db",
          "tw_item_db_en.sqlite"
        )
      }
    )
  }
)



test_that("items are stored and retrieved from cache correctly", {
  expect_equal(object = {
    tw_set_cache_folder(path = tempdir())
    tw_enable_cache()
    tw_create_cache_folder(ask = FALSE)

    df_from_api <- tw_get(
      id = "Q180099",
      language = "en"
    )

    df_from_cache <- tw_get_cached_item(
      id = "Q180099",
      language = "en"
    )
  }, expected = tw_get(
    id = "Q180099",
    language = "en",
    include_id = FALSE
  ))

  expect_false(object = {
    tw_set_cache_folder(path = tempdir())
    tw_enable_cache()
    tw_create_cache_folder(ask = FALSE)

    df_from_api <- tw_get(
      id = "Q180099",
      language = "en"
    )

    is.null(df_from_api)
  })
})
