#' Flatten CDE JSON response
#'
#' Turns an object-of-{label -> {period -> value}} into a long/tidy data.frame
#'
#' @param obj A nested list/object from the CDE API with structure {label -> {period -> value}}
#' @return A data.frame with columns: label, period, value
#' @examples
#' obj <- list(
#'   "United States Offenses" = list(
#'     "01-2015" = 27.6,
#'     "01-2016" = 29.62
#'   )
#' )
#' flatten_cde_json(obj)
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

  data.frame(
    label = labels,
    period = periods,
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )
}

#' CDE base URL
#'
#' Returns the base URL for the FBI CDE API. Overridable via the
#' `fbi_cde_base_url` option or the `FBI_CDE_BASE_URL` environment variable.
#'
#' @return A character string with the base URL.
#' @export
#'
#' @examples
#' cde_base_url()
cde_base_url <- function() {
  env <- Sys.getenv("FBI_CDE_BASE_URL")
  if (nzchar(env)) {
    return(env)
  }
  opt <- getOption("fbi_cde_base_url")
  if (!is.null(opt)) {
    return(opt)
  }
  "https://cde.ucr.cjis.gov/LATEST/"
}

#' Build a CDE API path
#'
#' Constructs the path portion of a CDE API URL from the data type,
#' geography level, and optional offense code.
#'
#' @param data_type Character string for the data type (e.g. "summarized", "arrest", "nibrs", "shr", "pe").
#' @param level Character string for geography level: "national", "state/{ABBR}", or "agency/{ORI}".
#' @param offense Optional character string for the offense code (e.g. "V", "all").
#' @return A character string with the API path.
#' @keywords internal
cde_path <- function(data_type, level, offense = NULL) {
  if (!is.null(offense)) {
    paste0(data_type, "/", level, "/", offense)
  } else {
    paste0(data_type, "/", level)
  }
}

#' Build query parameters for CDE API requests
#'
#' Constructs query parameter strings for CDE API requests.
#' Most endpoints use MM-YYYY date format; police employment uses 4-digit years.
#'
#' @param from Character string for the start date (MM-YYYY or YYYY).
#' @param to Character string for the end date (MM-YYYY or YYYY).
#' @param type Optional character string for the type parameter (e.g. "counts", "totals", "rates").
#' @param year_format Character string, either "mm-yyyy" or "yyyy".
#' @return A list of query parameters.
#' @keywords internal
cde_query <- function(from, to, type = NULL, year_format = "mm-yyyy") {
  query <- list()
  if (year_format == "mm-yyyy") {
    query$from <- paste0(from, "-", substr(from, 1, 2))
    query$to <- paste0(to, "-", substr(to, 1, 2))
  } else {
    query$from <- from
    query$to <- to
  }
  if (!is.null(type)) {
    query$type <- type
  }
  query
}

#' Perform a CDE API request
#'
#' The single network seam for the package. Performs the HTTP GET, checks
#' the status code, and returns the parsed JSON body. All other functions
#' should use this instead of calling httr::GET directly.
#'
#' @param path Character string for the API path (e.g. "summarized/national/V").
#' @param query Optional list of query parameters.
#' @return The parsed JSON response (typically a list).
#' @stop On non-200 status codes with a descriptive error message.
#' @export
#'
#' @examples
#' \dontrun{
#' cde_request("summarized/national/V", list(from = "01-2015", to = "12-2020", type = "counts"))
#' }
cde_request <- function(path, query = list()) {
  base <- cde_base_url()
  url <- paste0(base, path)
  if (length(query) > 0) {
    url <- paste0(url, "?", paste(
      names(query),
      vapply(query, as.character, ""),
      sep = "=",
      collapse = "&"
    ))
  }

  useragent <- paste0(
    "Mozilla/5.0 (compatible; a bot using the R fbi",
    " package; https://github.com/Civilytics/fbi_cde/)"
  )

  response <- httr::GET(url, httr::user_agent(useragent))

  if (response$status_code != 200) {
    body <- tryCatch(
      rawToChar(response$content),
      error = function(e) ""
    )
    stop(
      "CDE API request failed: ",
      response$status_code, " ", httr::status_code(response),
      " ", url,
      if (nzchar(body)) paste0("\nResponse: ", substr(body, 1, 500)),
      call. = FALSE
    )
  }

  if (length(response$content) == 0) {
    stop("CDE API returned an empty response for: ", url, call. = FALSE)
  }

  jsonlite::fromJSON(rawToChar(response$content), simplifyVector = TRUE)
}

#' Validate an ORI code
#'
#' Checks if an ORI (Organization Request Identifier) matches the expected
#' 9-character format: 2 letters + 7 digits (e.g. "CA0010900").
#'
#' @param ori Character string or vector of ORI codes.
#' @return Logical vector, TRUE for valid ORIs.
#' @export
#'
#' @examples
#' is_valid_ori("CA0010900")
#' is_valid_ori("abc123")
is_valid_ori <- function(ori) {
  grepl("^[A-Z]{2}[0-9]{7}$", toupper(ori))
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
