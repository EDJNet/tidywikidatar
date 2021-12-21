## code to prepare `tw_empty_wikipedia_page` dataset goes here

tw_empty_wikipedia_page <- c(
  "title",
  "wikipedia_id",
  "qid",
  "description",
  "disambiguation",
  "language"
) %>%
  purrr::map_dfc(setNames,
                 object = list(character())
  )

tw_empty_wikipedia_page$wikipedia_id <- as.integer()
tw_empty_wikipedia_page$disambiguation <- as.logical()


usethis::use_data(tw_empty_wikipedia_page, overwrite = TRUE)
