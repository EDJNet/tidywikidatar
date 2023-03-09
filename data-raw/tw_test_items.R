## code to prepare `tw_test_items` dataset goes here

qid <- c("Q2", "Q5", "Q180099", "Q228822", "Q191095", "Q314252")

tw_test_items <- WikidataR::get_item(id = qid)

# names(tw_test_items) <- purrr::map_chr(
#   .x = tw_test_items,
#   .f = function(x) {
#     purrr::pluck(x, "id")
#   })
# tw_test_items[["Q180099"]]

usethis::use_data(tw_test_items, overwrite = TRUE)

id <- c("Q180099", "Q228822")

tw_test_items[purrr::map_chr(
  .x = tw_test_items,
  .f = function(x) {
    purrr::pluck(x, "id")
  }
) %in% id]
