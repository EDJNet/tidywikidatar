## code to prepare `tw_empty_wikipedia_page_sections` dataset goes here

tw_empty_wikipedia_category_members <- c(
  "category",
  "wikipedia_title",
  "wikipedia_id"
) %>%
  purrr::map_dfc(setNames, object = list(character()))

tw_empty_wikipedia_category_members$wikipedia_id <- as.numeric()


usethis::use_data(tw_empty_wikipedia_category_members, overwrite = TRUE)
