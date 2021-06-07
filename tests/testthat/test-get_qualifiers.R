library("testthat")

test_that("check if qualifiers are returned correctly when more than 1 id and one property", {
  testthat::skip_if_offline()

  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      tw_create_cache_folder(ask = FALSE)

      q_df <- tw_get_qualifiers(
        id = c("Q180099", "Q314252"),
        p = "P26",
        language = "en"
      )

      list(
        total_id = length(unique(q_df$id)),
        total_p = length(unique(q_df$property)),
        nrow_check = nrow(q_df) > 7
      )
    }, expected =
      list(
        total_id = 2,
        total_p = 1,
        nrow_check = TRUE
      )
  )
})
