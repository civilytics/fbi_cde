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
    body_raw <- response$content
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

  body_raw <- response$content
  if (length(body_raw) == 0) {
    stop(
      "Empty response body for ", full_url, " (HTTP 200 but no data)",
      call. = FALSE
    )
  }

  jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE)
}

#' Build an FBI CDE API path string
#'
#' Constructs the path portion of a CDE API endpoint URL from its components.
#'
#' @param type Character string with the endpoint type
#'   (e.g. `"summarized"`, `"arrest"`, `"nibrs"`, `"shr"`, `"pe"`).
#' @param level Character string with the geographic level. One of
#'   `"national"`, `"state/{ABBR}"`, or `"agency/{ORI}"`.
#' @param offense Optional character string with the offense identifier
#'   (e.g. `"V"` for violent crime, `"LARC"` for larceny). Defaults to `NULL`.
#'
#' @return A character string with the API path, e.g.
#'   `"summarized/national/V"` or `"shr/state/CA"`.
#'
#' @examples
#' cde_path("summarized", "national", "V")
#' cde_path("summarized", "state/CA", "V")
#' cde_path("shr", "agency/CA0010900")
#' @export
#'
cde_path <- function(type, level, offense = NULL) {
  path <- paste0(type, "/", level)
  if (!is.null(offense)) {
    path <- paste0(path, "/", offense)
  }
  path
}

#' Build query parameters for FBI CDE API requests
#'
#' Formats year values and assembles a named list of query parameters
#' for CDE API endpoint calls.
#'
#' @param from Character string or numeric with the start date/year.
#'   When `four_digit_year = FALSE` expects `"MM-YYYY"` format;
#'   when `TRUE` accepts `"YYYY"` or a 4-digit numeric.
#' @param to Character string or numeric with the end date/year.
#'   Same format rules as `from`.
#' @param type Optional character string with the query type
#'   (e.g. `"counts"`, `"rates"`, `"totals"`). Defaults to `NULL`.
#' @param four_digit_year Logical; when `TRUE` formats years as
#'   4-digit `YYYY` (used for police employment `pe` endpoints).
#'   When `FALSE` (default) formats as `MM-YYYY`.
#'
#' @return A named list of query parameters, e.g.
#'   `list(from = "01-2015", to = "12-2020", type = "counts")`.
#'
#' @examples
#' cde_query("01-2015", "12-2020", type = "counts")
#' cde_query(2015, 2020, four_digit_year = TRUE)
#' @export
#'
cde_query <- function(from, to, type = NULL, four_digit_year = FALSE) {
  if (four_digit_year) {
    from <- format(as.numeric(from), "%Y")
    to <- format(as.numeric(to), "%Y")
  } else {
    from <- as.character(from)
    to <- as.character(to)
  }

  params <- list(from = from, to = to)
  if (!is.null(type)) {
    params$type <- type
  }
  params
}
