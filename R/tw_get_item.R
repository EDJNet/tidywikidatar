#' Retrieve item from the Wikidata API and returns it as a list
#'
#' @inheritParams tw_get
#' @inheritParams tw_set_user_agent
#'
#' @returns A list, with as many elements as the unique given id.
#' @export
#'
#' @examples
#' \dontrun{
#'   item_l <- tw_get_item(id = "Q180099")
#'
#'   tidywikidatar:::tw_extract_single(w = item_l)
#' }
tw_get_item <- function(
  id,
  user_agent = tidywikidatar::tw_get_user_agent(),
  retry = 10
) {
  items_l <- tw_check_qid(id = id) %>%
    purrr::set_names() %>%
    purrr::map(.f = function(current_id) {
      api_request <- httr2::request("https://www.wikidata.org/w/api.php") %>%
        httr2::req_url_query(
          action = "wbgetentities",
          ids = current_id,
          format = "json"
        ) %>%
        httr2::req_user_agent(string = "user_agent")

      if (retry) {
        api_request <- api_request %>%
          httr2::req_retry(max_tries = retry)
      }

      resp <- api_request %>%
        httr2::req_perform()

      item_l <- resp %>%
        httr2::resp_body_json() %>%
        purrr::pluck("entities")

      item_l
    })
  items_l
}
