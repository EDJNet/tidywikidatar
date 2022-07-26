# run only in interactive sessions due to issues with API timeouts
if (interactive()) {
  library("dplyr", warn.conflicts = FALSE)
  library("tidywikidatar")

  tw_set_language(language = "en")

  tw_enable_cache()
  tw_set_cache_folder(path = fs::path(
    fs::path_temp(),
    "tw_data_testing"
  ))
  tw_create_cache_folder(ask = FALSE)



  testthat::test_that("tw_get_wikipedia_category_members() works with sub-categories", {
    testthat::expect_true({
      sub_category_members_df <- tw_get_wikipedia_category_members(
        url = "https://en.wikipedia.org/wiki/Category:American_women_anthropologists",
        category = NULL,
        language = "en",
        cache = TRUE,
        type = "subcat"
      )

      nrow(sub_category_members_df) > 1
    })
  })


  testthat::test_that("tw_get_wikipedia_category_members() works when category, not url is given", {
    testthat::expect_true({
      category_members_df <- tw_get_wikipedia_category_members(
        category = "Category:Puerto Rican women anthropologists",
        language = "en",
        cache = TRUE
      )

      nrow(category_members_df) > 0
    })
  })

  testthat::test_that("tw_get_wikipedia_category_members() works when url, not category is given", {
    testthat::expect_true({
      category_members_df <- tw_get_wikipedia_category_members(
        url = "https://en.wikipedia.org/wiki/Category:Puerto Rican women anthropologists",
        category = NULL,
        type = "page",
        language = "en",
        cache = TRUE
      )

      nrow(category_members_df) > 0
    })
  })
}
