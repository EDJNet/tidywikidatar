library("testthat")

test_that("Preferred or latest result is chosen when requested", {
  testthat::skip_if_offline()
  tw_set_cache_folder(path = tempdir())
  tw_enable_cache()
  tw_create_cache_folder(ask = FALSE)

  q_df <- tw_get_qualifiers_single(
    id = c("Q220"),
    p = "P17",
    language = "en",
  )


  expect_equal(
    object = {
      preferred_v <- q_df %>%
        dplyr::filter(rank == "preferred") %>%
        dplyr::pull(qualifier_id)
    },
    expected = {
      tw_get_p(id = "Q220", p = "P17", preferred = TRUE, only_first = TRUE) %>%
        unlist()
    }
  )



  expect_equal(
    object = {
      latest_v <- q_df %>%
        dplyr::filter(qualifier_property == "P580") %>%
        dplyr::arrange(qualifier_value) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::pull(qualifier_id)
    },
    expected = {
      tw_get_p(
        id = "Q220",
        p = "P17",
        latest_start_time = TRUE,
        only_first = TRUE
      ) %>%
        unlist()
    }
  )
})
