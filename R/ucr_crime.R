# Internal helper: parse summarized crime API response into a tidy data.frame
parse_summarized_response <- function(response, geography) {
  offenses <- response$offenses

  # The CDE API renamed the count payload from `counts` to `actuals`; accept
  # either so the parser is resilient to that drift.
  counts_obj <- offenses$actuals %||% offenses$counts

  # Flatten counts if present
  if (!is.null(counts_obj) && length(counts_obj) > 0) {
    counts_df <- flatten_cde_json(counts_obj)
    names(counts_df) <- c("offense", "period", "count")
  } else {
    counts_df <- data.frame(
      offense = character(), period = character(), count = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # Flatten rates if present
  if (!is.null(offenses$rates) && length(offenses$rates) > 0) {
    rates_df <- flatten_cde_json(offenses$rates)
    names(rates_df) <- c("offense", "period", "rate")
  } else {
    rates_df <- data.frame(
      offense = character(), period = character(), rate = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # Full outer join on offense and period
  result <- merge(counts_df, rates_df, by = c("offense", "period"), all = TRUE)
  result$geography <- geography

  # Reorder columns
  result <- result[, c("geography", "offense", "period", "count", "rate")]
  rownames(result) <- NULL

  result
}

#' Get agency-level crime data from the UCR Offenses Known and Clearances
#'
#' @family UCR crime functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default "V" for violent crime). See
#'   `get_offense_codes()` for available codes.
#'
#' @return A data.frame with columns: geography, offense, period, count, rate
#' @export
#'
#' @examples
#' \dontrun{
#' get_agency_crime("AK0010100")
#' }
get_agency_crime <- function(ori,
                             from = "01-2015",
                             to = "12-2020",
                             offense = "V") {
  if (!is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
  }

  cde_validate_dates(from, to, "mm-yyyy")

  path <- cde_path("summarized", paste0("agency/", ori), offense)
  query <- list(from = from, to = to, type = "counts")

  response <- cde_request(path, query)
  parse_summarized_response(response, geography = ori)
}

#' Get state- or national-level estimated crime counts
#'
#' @family UCR crime functions
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default "V" for violent crime). See
#'   `get_offense_codes()` for available codes.
#'
#' @return A data.frame with columns: geography, offense, period, count, rate
#' @export
#'
#' @examples
#' \dontrun{
#' get_estimated_crime("CA")
#' }
get_estimated_crime <- function(state_abb = NULL,
                                from = "01-2015",
                                to = "12-2020",
                                offense = "V") {
  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  cde_validate_dates(from, to, "mm-yyyy")

  if (is.null(state_abb)) {
    level <- "national"
  } else {
    level <- paste0("state/", toupper(state_abb))
  }

  path <- cde_path("summarized", level, offense)
  query <- list(from = from, to = to, type = "counts")

  response <- cde_request(path, query)

  geo <- if (is.null(state_abb)) "US" else toupper(state_abb)
  parse_summarized_response(response, geography = geo)
}

#' Get estimated arson data
#'
#' @family UCR crime functions
#' @inheritParams get_estimated_crime
#'
#' @return A data.frame with columns: geography, offense, period, count, rate
#' @export
#'
#' @examples
#' \dontrun{
#' get_estimated_arson("CA")
#' }
get_estimated_arson <- function(state_abb = NULL,
                                from = "01-2015",
                                to = "12-2020") {
  get_estimated_crime(
    state_abb = state_abb,
    from = from,
    to = to,
    offense = "ARS"
  )
}
