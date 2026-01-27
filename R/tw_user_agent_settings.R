#' Set language to be used by all functions
#'
#' Defaults to current package name (`tidywikidatar`) and version.
#'
#' @param user_agent Defaults to `NULL`. If not given, implicitly defaults to
#'   current package name (`tidywikidatar`) and version.
#'
#' @return The user agent set for the session, implicitly.
#' @export
#' @examples
#' # Default user agent
#' default_user_agent <- tw_get_user_agent()
#' default_user_agent
#' # Custom user agent
#' tw_set_user_agent(user_agent = "custom_project_name/email")
#' new_user_agent <- tw_get_user_agent()
#' new_user_agent
#' # Restore
#' tw_set_user_agent(user_agent = default_user_agent)
tw_set_user_agent <- function(user_agent) {
  if (is.null(user_agent)) {
    user_agent <- Sys.getenv("tw_user_agent")
  } else {
    Sys.setenv(tw_user_agent = user_agent)
  }
  if (user_agent == "") {
    user_agent <- stringr::str_flatten(c(
      "tidywikidatar/",
      as.character(packageVersion("tidywikidatar"))
    ))
  }
  invisible(user_agent)
}

#' @rdname tw_set_user_agent
#' @examples
#' tw_get_user_agent()
#' @export
tw_get_user_agent <- tw_set_user_agent
