#' Internal helper: parse SHR API response into a tidy data.frame
#'
#' Response shape (CDE API):
#'   A list with `actuals`, `tooltips`, and `cde_properties` elements.
#'
#' @param response Parsed JSON response from the SHR endpoint
#' @param geography Geographic identifier (ORI, state, or "US")
#' @return A data.frame with columns: geography, period, count
#' @keywords internal
parse_shr_response <- function(response, geography) {
  actuals <- response$actuals

  if (is.null(actuals) || length(actuals) == 0) {
    return(data.frame(
      geography = character(),
      period = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  flat <- flatten_cde_json(actuals)
  names(flat) <- c("geography", "period", "count")
  flat$geography <- geography

  flat <- flat[, c("geography", "period", "count")]
  rownames(flat) <- NULL
  flat
}

#' Get Supplemental Homicide Reports (SHR) offense counts
#'
#' Retrieves homicide offense counts from the FBI's Supplemental Homicide Reports
#' via the CDE API endpoint `shr/<level>` with `type=counts`.
#'
#' @family SHR functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2015").
#'
#' @return A data.frame with columns: geography, period, count
#' @export
#'
#' @examples
#' \dontrun{
#' # Gets national-level SHR data
#' get_shr()
#'
#' # Gets California state-level SHR data
#' get_shr(state_abb = "CA")
#'
#' # Gets Oakland PD SHR data
#' get_shr(ori = "CA0010900")
#' }
get_shr <- function(ori = NULL,
                    state_abb = NULL,
                    from = "01-2015",
                    to = "12-2015") {
  if (!is.null(ori) && !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must match format: 2 letters + 7 digits (e.g., CA0010900)",
      call. = FALSE
    )
  }

  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  cde_validate_dates(from, to, "mm-yyyy")

  if (!is.null(ori)) {
    level <- paste0("agency/", ori)
    geography <- ori
  } else if (!is.null(state_abb)) {
    level <- paste0("state/", toupper(state_abb))
    geography <- toupper(state_abb)
  } else {
    level <- "national"
    geography <- "US"
  }

  path <- cde_path("shr", level)
  query <- cde_query(from, to, type = "counts")

  response <- cde_request(path, query)
  parse_shr_response(response, geography)
}
