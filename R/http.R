#' Get the FBI CDE API base URL
#'
#' Returns the base URL for the FBI Crime Data Explorer API.
#' Can be overridden via `getOption("fbi.cde.base_url")` or
#' `Sys.getenv("FBI_CDE_BASE_URL")` (for tests).
#'
#' @return A character string with the base URL.
#' @export
#'
#' @examples
#' cde_base_url()
cde_base_url <- function() {
  default_url <- "https://cde.ucr.cjis.gov/LATEST/"

  option_url <- getOption("fbi.cde.base_url")
  if (!is.null(option_url) && nzchar(option_url)) {
    return(option_url)
  }

  env_url <- Sys.getenv("FBI_CDE_BASE_URL")
  if (nzchar(env_url)) {
    return(env_url)
  }

  default_url
}

#' Make a request to the FBI CDE API
#'
#' The single network seam for the package. Builds the full URL from
#' base + path + encoded query, performs the GET request, and returns
#' the parsed JSON list.
#'
#' @param path Character string with the API path (e.g. `"summarized/national/V"`).
#' @param query Named list of query parameters (default: `list()`).
#' @param get_fun Function to perform the HTTP GET request. Defaults to
#'   `httr::GET`. Inject this for testing (e.g. with a mock response).
#'
#' @return The parsed JSON response as a list.
#' @keywords internal
cde_request <- function(path, query = list(), get_fun = httr::GET) {
  base <- cde_base_url()
  full_url <- httr::modify_url(paste0(base, path), query = query)

  useragent <- paste0(
    "Mozilla/5.0 (compatible; a bot using the R fbi",
    " package; https://github.com/jacobkap/fbi/)"
  )

  response <- get_fun(full_url, httr::user_agent(useragent))

  if (response$status_code != 200L) {
    body_raw <- httr::content(response, as = "raw", type = "application/json")
    msg <- ""
    if (length(body_raw) > 0) {
      body_text <- rawToChar(body_raw)
      parsed <- tryCatch(
        jsonlite::fromJSON(body_text, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(parsed) && !is.null(parsed$message)) {
        msg <- paste0(" - ", parsed$message)
      }
    }
    stop(
      "HTTP ", response$status_code, " for ", full_url, msg,
      call. = FALSE
    )
  }

  body_raw <- httr::content(response, as = "raw", type = "application/json")
  if (length(body_raw) == 0) {
    stop(
      "Empty response body for ", full_url, " (HTTP 200 but no data)",
      call. = FALSE
    )
  }

  jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE)
}
