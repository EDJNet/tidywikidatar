## code to prepare `tw_empty_wikipedia_page_links` dataset goes here

tw_empty_wikipedia_page_links <- c(
  "source_title_url",
  "source_wikipedia_title",
  "source_qid",
  "wikipedia_title",
  "wikipedia_id",
  "qid",
  "description",
  "language"
) %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )

tw_empty_wikipedia_page$wikipedia_id <- as.integer()


usethis::use_data(tw_empty_wikipedia_page_links, overwrite = TRUE)
