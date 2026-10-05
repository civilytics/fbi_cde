# Internal helper: parse a summarized/{level}/{offense} response. The CDE
# renamed the count payload from `counts` to `actuals`; accept either.
parse_summarized_response <- function(response, geography, offense, level,
                                      comparison = FALSE) {
  offenses <- response$offenses
  .parse_series(offenses$actuals %||% offenses$counts, offenses$rates,
                geography = geography, offense = offense, level = level,
                comparison = comparison)
}

#' Get agency-level crime data from the UCR Offenses Known and Clearances
#'
#' @family UCR crime functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default "V" for violent crime). See
#'   `get_offense_codes()` for available codes.
#' @param comparison If `TRUE`, also return the comparison series the API
#'   sends alongside the agency's own: its state's and the nation's rates
#'   (these have no `count`). Default `FALSE` returns only the agency's own
#'   series.
#'
#' @return A data.frame with columns `geography`, `offense` (the requested
#'   code), `measure` (`"offenses"` or `"clearances"`), `period`, `count` and
#'   `rate` (per 100,000 population, for that month). With
#'   `comparison = TRUE`, two more columns follow `geography`: `series`
#'   (`"agency"`, `"state"` or `"national"`) and `series_name` (e.g.
#'   `"Oakland Police Department"`, `"California"`, `"United States"`).
#' @export
#'
#' @examples
#' \dontrun{
#' get_agency_crime("AK0010100")
#'
#' # The agency alongside its state and the nation
#' get_agency_crime("CA0010900", from = "01-2019", to = "03-2019",
#'                  comparison = TRUE)
#' }
get_agency_crime <- function(ori,
                             from = "01-2015",
                             to = "12-2020",
                             offense = "V",
                             comparison = FALSE) {
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
  parse_summarized_response(response, geography = ori, offense = offense,
                            level = "agency", comparison = comparison)
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
#' @param comparison If `TRUE`, a state query also returns the national
#'   comparison series (rate only). Default `FALSE` returns only the state's
#'   own series. Has no effect on a national query.
#'
#' @return A data.frame with the columns described in [get_agency_crime()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_estimated_crime("CA")
#' }
get_estimated_crime <- function(state_abb = NULL,
                                from = "01-2015",
                                to = "12-2020",
                                offense = "V",
                                comparison = FALSE) {
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
  parse_summarized_response(response, geography = geo, offense = offense,
                            level = if (is.null(state_abb)) "national" else "state",
                            comparison = comparison)
}

#' Get estimated arson data
#'
#' @family UCR crime functions
#' @inheritParams get_estimated_crime
#'
#' @return A data.frame with the columns described in [get_agency_crime()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_estimated_arson("CA")
#' }
get_estimated_arson <- function(state_abb = NULL,
                                from = "01-2015",
                                to = "12-2020",
                                comparison = FALSE) {
  get_estimated_crime(
    state_abb = state_abb,
    from = from,
    to = to,
    offense = "ARS",
    comparison = comparison
  )
}
