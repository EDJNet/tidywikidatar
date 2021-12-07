## code to prepare `tw_qid_meps` dataset goes here

library("dplyr", warn.conflicts = FALSE)
library("tidywikidatar")

# All those who have "member of the European Parliament" among "position held"

meps_df_query <- tibble::tribble(
  ~p, ~q,
  "P39", "Q27169"
)

meps_df <- tw_query(query = meps_df_query)

tw_qid_meps <- meps_df %>%
  dplyr::select(id) %>%
  dplyr::arrange()

usethis::use_data(tw_qid_meps, overwrite = TRUE)
