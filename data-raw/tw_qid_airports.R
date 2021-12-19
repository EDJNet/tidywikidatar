## code to prepare `tw_qid_airports` dataset goes here
library("dplyr", warn.conflicts = FALSE)

api_url <- "https://www.wikidata.org/w/api.php?action=query&list=backlinks&blnamespace=0&bllimit=500&bltitle=Property:P239&format=json"

base_json <- jsonlite::read_json(api_url)

continue_check <- base_json %>%
  purrr::pluck("continue", "blcontinue")

all_jsons <- list()
page_number <- 1

all_jsons[[page_number]] <- base_json

while (is.null(continue_check) == FALSE & page_number < 10000) {
  Sys.sleep(0.1)
  message(page_number)
  page_number <- page_number + 1

  base_json <- jsonlite::read_json(stringr::str_c(
    api_url,
    "&blcontinue=",
    continue_check
  ))

  all_jsons[[page_number]] <- base_json

  continue_check <- base_json %>%
    purrr::pluck("continue", "blcontinue")
}



all_pages <- purrr::map(
  .x = all_jsons,
  .f = purrr::pluck, "query", "backlinks"
) %>%
  purrr::flatten()

aiport_qid_df <- purrr::map_dfr(
  .x = all_pages,
  .f = function(current_page) {
    tibble::tibble(
      qid = current_page %>%
        purrr::pluck(
          "title"
        )
    )
  }
)

tw_qid_airports <- airport_qid_df %>%
  dplyr::transmute(id = qid) %>%
  dplyr::arrange()

## actually include only airports found in Eurostat's `avia_par_` datasets in 2019

tw_qid_airports <- readr::read_csv("european_airports_with_wikidata_details.csv") %>%
  dplyr::transmute(id = airport_qid) %>%
  dplyr::arrange(id)


usethis::use_data(tw_qid_airports, overwrite = TRUE)
