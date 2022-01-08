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


test_that("check if qualifiers are returned correctly when one of qualifiers has string value type (not id, not time)", {
  testthat::skip_if_offline()


  expect_equal(
    object = {
      tw_set_cache_folder(path = tempdir())
      tw_enable_cache()
      tw_create_cache_folder(ask = FALSE)

      q_df <- tw_get_qualifiers(
        id = c("Q4644021"),
        p = "P31",
        language = "en"
      )

      q_df %>%
        dplyr::filter(.data$qualifier_property == "P1545") %>%
        dplyr::pull(.data$qualifier_value)
    }, expected =
      "7"
  )
})
