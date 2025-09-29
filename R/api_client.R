# Utilities for configuring and scaffolding the Lab 5 API package.

#' Configure the API base URL
#'
#' Stores the base URL in an option and environment variable so later code can
#' retrieve it. Replace the placeholder implementation once you pick the API you
#' want to target.
#'
#' @param base_url Non-empty character scalar holding the base URL.
#' @return Invisibly returns the URL.
#' @examples
#' set_api_base_url("https://api.example.com")
#' @export
set_api_base_url <- function(base_url) {
  if (!is.character(base_url) || length(base_url) != 1L || !nzchar(base_url)) {
    stop("`base_url` must be a non-empty character scalar.", call. = FALSE)
  }

  options(lab5.base_url = base_url)
  Sys.setenv(LAB5_API_BASE_URL = base_url)
  invisible(base_url)
}

#' Retrieve the configured API base URL
#'
#' Reads the base URL from package options or the `LAB5_API_BASE_URL`
#' environment variable. This is a convenience helper you can use in your API
#' client functions.
#'
#' @return Character scalar (possibly empty) with the configured URL.
#' @examples
#' get_api_base_url()
#' @export
get_api_base_url <- function() {
  option_value <- getOption("lab5.base_url", default = "")
  if (is.character(option_value) && length(option_value) == 1L && nzchar(option_value)) {
    return(option_value)
  }

  env_value <- Sys.getenv("LAB5_API_BASE_URL", unset = "")
  if (nzchar(env_value)) {
    return(env_value)
  }

  ""
}

#' Fetch data from the configured API
#'
#' Placeholder that raises an informative error until you implement the actual
#' request logic for your selected API.
#'
#' @param endpoint Character scalar naming the API endpoint.
#' @param ... Reserved for future parameters (query, payload, etc.).
#' @return This function currently throws an error to remind you to implement
#'   it.
#' @examples
#' \dontrun{
#' fetch_api_resource("status")
#' }
#' @export
fetch_api_resource <- function(endpoint, ...) {
  stop(
    "fetch_api_resource() is a placeholder. Replace its body with the code ",
    "that queries your chosen web API.",
    call. = FALSE
  )
}
