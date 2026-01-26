## code to prepare `tw_empty_item` dataset goes here
library("dplyr", warn.conflicts = FALSE)

tw_empty_item <- c(
  "id",
  "property",
  "value",
  "rank"
) %>%
  purrr::map_dfc(setNames, object = list(character()))


usethis::use_data(tw_empty_item, overwrite = TRUE)
