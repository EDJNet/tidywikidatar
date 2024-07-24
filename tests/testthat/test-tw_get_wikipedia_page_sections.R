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


  testthat::test_that("tw_get_wikipedia_page_sections() works with url as input", {
    testthat::expect_true({
      sections_df <- tw_get_wikipedia_page_sections(
        url = "https://en.wikipedia.org/wiki/List_of_2020_Summer_Olympics_medal_winners",
        language = "en",
        cache = TRUE
      )

      sum(
        nrow(sections_df) > 1,
        "Archery" %in% sections_df[["line"]]
      ) == 2
    })
  })


  testthat::test_that("tw_get_wikipedia_page_section_links() works with title as input", {
    testthat::expect_true({
      section_links_df <- tw_get_wikipedia_page_section_links(
        title = "List of 2020 Summer Olympics medal winners",
        section_title = "Archery",
        language = "en",
        cache = TRUE
      )

      nrow(section_links_df) > 1
    })
  })

  testthat::test_that("tw_get_wikipedia_page_section_links() works with url as input", {
    testthat::expect_true({
       section_links_df <- tw_get_wikipedia_page_section_links(
        url = "https://en.wikipedia.org/wiki/List_of_2020_Summer_Olympics_medal_winners",
        section_title = "Archery",
        language = "en",
        cache = TRUE
      )

      nrow(section_links_df) > 1
    })
  })


}
