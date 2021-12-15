## code to prepare `tw_empty_qualifiers_df` dataset goes here

tw_empty_qualifiers_df <- c("id",
                            "property",
                            "qualifier_id",
                            "qualifier_property",
                            "qualifier_value",
                            "qualifier_value_type",
                            "rank",
                            "set") %>%
  purrr::map_dfc(setNames,
                 object = list(character()))

tw_empty_qualifiers_df$set <- as.numeric()

usethis::use_data(tw_empty_qualifiers_df, overwrite = TRUE)
