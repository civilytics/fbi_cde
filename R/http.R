#' Flatten CDE JSON response
#'
#' Turns an object-of-`label -> period -> value` into a long/tidy data.frame
#'
#' @param obj A nested list/object from the CDE API with structure
#'   `label -> period -> value`
#' @return A data.frame with columns: label, period, value
#' @examples
#' obj <- list(
#'   "United States Offenses" = list(
#'     "01-2015" = 27.6,
#'     "01-2016" = 29.62
#'   )
#' )
#' \dontrun{
#' flatten_cde_json(obj)
#' }
flatten_cde_json <- function(obj) {
  if (is.null(obj) || length(obj) == 0) {
    return(data.frame(label = character(), period = character(), value = numeric(), stringsAsFactors = FALSE))
  }

  labels <- c()
  periods <- c()
  values <- c()

  for (label in names(obj)) {
    label_data <- obj[[label]]

    if (is.null(label_data)) {
      next
    }

    for (period in names(label_data)) {
      value <- label_data[[period]]

      if (is.null(value)) {
        next
      }

      labels <- c(labels, label)
      periods <- c(periods, period)
      values <- c(values, value)
    }
  }

  # If every leaf was NULL/empty (e.g. counts suppressed at the national
  # level), return the canonical empty frame so callers always see three
  # columns rather than a degenerate zero-column data.frame.
  if (length(values) == 0) {
    return(data.frame(
      label = character(), period = character(), value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    label = labels,
    period = periods,
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )
}

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
    " package; https://github.com/Civilytics/fbi_cde)"
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
#'   `"national"`, `"state/XX"` (replace XX with state abbreviation), or
#'   `"agency/XX"` (replace XX with ORI code).
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
    from <- as.character(from)
    from <- sub(".*-(\\d{4})$", "\\1", from)
    to <- as.character(to)
    to <- sub(".*-(\\d{4})$", "\\1", to)
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

#' Validate an ORI code
#'
#' Checks if an ORI (Organization Request Identifier) matches the expected
#' 9-character format: 2 letters followed by 7 alphanumerics (letters allowed
#' for state, tribal, campus, and some city ORIs) (e.g. "CA0010900").
#'
#' @param ori Character string or vector of ORI codes.
#' @return Logical vector, TRUE for valid ORIs.
#' @export
#'
#' @examples
#' is_valid_ori("CA0010900")
#' is_valid_ori("CA001300X")
#' is_valid_ori("abc123")
is_valid_ori <- function(ori) {
  grepl("^[A-Z]{2}[A-Z0-9]{7}$", toupper(ori))
}

#' Validate a state abbreviation
#'
#' Checks if a state abbreviation is a valid US state or territory code.
#'
#' @param state_abb Character string or vector of state abbreviations.
#' @return Logical vector, TRUE for valid state abbreviations.
#' @export
#'
#' @examples
#' is_valid_state("CA")
#' is_valid_state("XX")
is_valid_state <- function(state_abb) {
  valid <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
    "DC", "PR", "GU", "VI", "AS", "CZ"
  )
  toupper(state_abb) %in% valid
}

#' Validate a date range
#'
#' Checks that from <= to for date ranges. Supports both MM-YYYY and YYYY formats.
#'
#' @param from Character string for the start date.
#' @param to Character string for the end date.
#' @param year_format Character string, either "mm-yyyy" or "yyyy".
#' @keywords internal
cde_validate_dates <- function(from, to, year_format = "mm-yyyy") {
  if (year_format == "mm-yyyy") {
    from_num <- as.numeric(gsub("([0-9]{2})-([0-9]{4})", "\\2\\1", from))
    to_num <- as.numeric(gsub("([0-9]{2})-([0-9]{4})", "\\2\\1", to))
  } else {
    from_num <- as.numeric(from)
    to_num <- as.numeric(to)
  }
  if (from_num > to_num) {
    stop(
      "Invalid date range: 'from' (", from, ") must be <= 'to' (", to, ")",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
