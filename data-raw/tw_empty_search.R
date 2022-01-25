## code to prepare `tw_empty_search` dataset goes here
library("dplyr", warn.conflicts = FALSE)

tw_empty_search <- c(
  "search",
  "id",
  "label",
  "description"
) %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )



usethis::use_data(tw_empty_search, overwrite = TRUE)
