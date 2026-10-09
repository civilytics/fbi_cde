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
#' Can be overridden via `getOption("fbiCDE.cde.base_url")` or
#' `Sys.getenv("FBI_CDE_BASE_URL")` (for tests).
#'
#' @return A character string with the base URL.
#' @export
#'
#' @examples
#' cde_base_url()
cde_base_url <- function() {
  default_url <- "https://cde.ucr.cjis.gov/LATEST/"

  option_url <- getOption("fbiCDE.cde.base_url")
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
#' Transient failures are retried with exponential backoff (1, 2, 4, ...
#' seconds, capped at 30): an error raised by the request itself (a network
#' failure or timeout) and HTTP 408, 429, 500, 502, 503 or 504. A
#' `Retry-After` header given in seconds is honoured, capped at 60. Any other
#' status fails immediately. Two options tune this:
#' `getOption("fbiCDE.max_retries", 3)`, the number of retries after the first
#' attempt, and `getOption("fbiCDE.timeout", 60)`, the per-attempt timeout in
#' seconds.
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
    "Mozilla/5.0 (compatible; a bot using the R fbiCDE",
    " package; https://github.com/civilytics/fbi_cde)"
  )
  max_retries <- .cde_option_number("fbiCDE.max_retries", 3, min = 0)
  timeout_secs <- .cde_option_number("fbiCDE.timeout", 60, min = 1)

  # A single dropped connection or 503 used to fail the call outright, and in
  # a county or metro fan-out that silently removed the agency from the totals.
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    response <- tryCatch(
      get_fun(full_url, httr::user_agent(useragent),
              httr::timeout(timeout_secs)),
      error = function(e) e
    )
    if (!.cde_is_transient(response) || attempt > max_retries) {
      break
    }
    .cde_sleep(.cde_backoff(attempt, response))
  }
  attempts <- if (attempt > 1L) paste0(" (after ", attempt, " attempts)") else ""

  if (inherits(response, "error")) {
    stop("Request failed for ", full_url, attempts, ": ",
         conditionMessage(response), call. = FALSE)
  }

  if (response$status_code != 200L) {
    stop(
      "HTTP ", response$status_code, " for ", full_url, attempts,
      .cde_error_detail(response$content),
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

# The explanation in an error response body, as " - <text>", or "" if none.
# The CDE answers a bad request with plain text served as application/json
# ("From year and month date is not valid, expected format MM-YYYY."), which
# does not parse; only a JSON `message` field used to be reported, so the
# error said just "HTTP 400". An HTML page (the 404 for an unknown path) adds
# nothing, and a long body is cut short.
.cde_error_detail <- function(body_raw) {
  if (length(body_raw) == 0) {
    return("")
  }
  # Never let a body we cannot read hide the HTTP status itself.
  tryCatch(.cde_error_text(body_raw), error = function(e) "")
}

.cde_error_text <- function(body_raw) {
  text <- rawToChar(body_raw)
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                     error = function(e) NULL)
  detail <- if (is.list(parsed) && is.character(parsed$message) &&
                length(parsed$message) == 1L) {
    parsed$message
  } else if (is.character(parsed) && length(parsed) == 1L) {
    parsed
  } else if (is.null(parsed) && !grepl("^\\s*<", text)) {
    text
  } else {
    ""
  }
  detail <- trimws(gsub("\\s+", " ", detail))
  if (!nzchar(detail)) {
    return("")
  }
  if (nchar(detail) > 300L) {
    detail <- paste0(substr(detail, 1L, 297L), "...")
  }
  paste0(" - ", detail)
}

# Statuses worth retrying: request timeout, rate limiting, and server-side
# failures. Everything else (404, 400, ...) will not change on a retry.
.CDE_TRANSIENT_STATUS <- c(408L, 429L, 500L, 502L, 503L, 504L)

# TRUE when `response` (an httr response, or the error get_fun raised) is worth
# retrying.
.cde_is_transient <- function(response) {
  inherits(response, "error") ||
    response$status_code %in% .CDE_TRANSIENT_STATUS
}

# Seconds to wait before retry number `attempt`: 1, 2, 4, ... capped at 30, or
# the server's Retry-After (in seconds) when it sends one, capped at 60.
.cde_backoff <- function(attempt, response) {
  wait <- min(2^(attempt - 1L), 30)
  retry_after <- if (inherits(response, "error")) NULL else
    response$headers[["retry-after"]]
  if (length(retry_after) == 1L) {
    secs <- suppressWarnings(as.numeric(retry_after))
    if (!is.na(secs) && secs >= 0) {
      wait <- min(secs, 60)
    }
  }
  wait
}

# Isolated so tests can retry without actually sleeping.
.cde_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

# Read a numeric option, validating it rather than failing obscurely later.
.cde_option_number <- function(name, default, min) {
  value <- getOption(name, default)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      value < min) {
    stop("Option '", name, "' must be a single number >= ", min,
         " (got ", deparse(value), ").", call. = FALSE)
  }
  value
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
#'   (e.g. `"V"` for violent crime, `"LAR"` for larceny). Defaults to `NULL`.
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

# Validate one ORI and return it upper-cased, ready for a request path. The
# CDE matches ORIs case-sensitively: `agency/ca0010900` is an unknown agency,
# answered with HTTP 200 and no counts, so a lower-case ORI that passed
# is_valid_ori() used to return zero rows instead of the agency's data.
.check_ori <- function(ori) {
  if (length(ori) != 1L || !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", paste(ori, collapse = ", "),
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
  }
  toupper(ori)
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
