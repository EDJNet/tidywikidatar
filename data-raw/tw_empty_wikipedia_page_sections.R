## code to prepare `tw_empty_wikipedia_page_sections` dataset goes here

tw_empty_wikipedia_page_sections <- c(
  "toclevel",
  "level",
  "line",
  "number",
  "index",
  "fromtitle",
  "byteoffset",
  "anchor"
) %>%
  purrr::map_dfc(setNames, object = list(character()))

tw_empty_wikipedia_page_sections$toclevel <- as.integer()
tw_empty_wikipedia_page_sections$byteoffset <- as.integer()


usethis::use_data(tw_empty_wikipedia_page_sections, overwrite = TRUE)
