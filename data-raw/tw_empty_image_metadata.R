## code to prepare `tw_empty_image_metadata` dataset goes here


tw_empty_image_metadata <- c(
  "id", "image_filename", "object_name", "image_description",
  "categories", "assessments", "credit", "artist", "permission",
  "license_short_name", "license_url", "license", "usage_terms",
  "attribution_required", "copyrighted", "restrictions", "date_time",
  "date_time_original", "commons_metadata_extension"
) %>%
  purrr::map_dfc(setNames,
    object = list(character())
  )


usethis::use_data(tw_empty_image_metadata, overwrite = TRUE)
