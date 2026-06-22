# Internal helper: parse NIBRS API response into a tidy data.frame
#
# Response shape (CDE API):
#   {"victim": {"age": {"Under 18": 100, "18-24": 200}, "race": {"White": 300}}, ...}
#
# The `section` argument selects the top-level key (victim/offender/offense).
# The `variable` argument selects which sub-object within that section to use.
#
# Returns a long data.frame with columns:
#   geography, offense, period, demographic_type, demographic_value, count
parse_nibrs_response <- function(response, geography, offense, section, variable) {
  section_data <- response[[section]]

  if (is.null(section_data) || length(section_data) == 0) {
    n <- 0
    return(data.frame(
      geography = rep(geography, n),
      offense = rep(offense, n),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  var_data <- section_data[[variable]]

  if (is.null(var_data) || length(var_data) == 0) {
    n <- 0
    return(data.frame(
      geography = rep(geography, n),
      offense = rep(offense, n),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  demo_values <- c()
  counts <- c()

  for (dv in names(var_data)) {
    cnt <- var_data[[dv]]
    if (is.null(cnt)) next
    demo_values <- c(demo_values, dv)
    counts <- c(counts, as.numeric(cnt))
  }

  if (length(counts) == 0) {
    n <- 0
    return(data.frame(
      geography = rep(geography, n),
      offense = rep(offense, n),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    geography = rep(geography, length(counts)),
    offense = rep(offense, length(counts)),
    period = rep("", length(counts)),
    demographic_type = rep(variable, length(counts)),
    demographic_value = demo_values,
    count = counts,
    stringsAsFactors = FALSE
  )
}


#' Gets victim-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' Retrieves victim demographics (age, race, sex, ethnicity, relationship, location)
#' from the CDE API endpoint `nibrs/{level}/{offense}?type=totals`.
#'
#' @family NIBRS functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default "robbery"). See
#'   `list_nibrs_offenses()` for available codes.
#' @param variable A string with the demographic variable to return. Selects
#'   which sub-object within the victim section to use (e.g., "age", "race",
#'   "sex", "ethnicity", "relationship", "location"). By default returns total
#'   count of victims. See `list_nibrs_victim_variables()` for available options.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_victim(ori = "CA0010900")
#' get_nibrs_victim(state_abb = "CA", offense = "murder", variable = "race")
#' }
get_nibrs_victim <- function(ori = NULL,
                              state_abb = NULL,
                              from = "01-2015",
                              to = "12-2020",
                              offense = "robbery",
                              variable = "count") {
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

  path <- cde_path("nibrs", level, offense)
  query <- list(from = from, to = to, type = "totals")

  response <- cde_request(path, query)
  parse_nibrs_response(response, geography, offense, "victim", variable)
}


#' Gets offender-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' Retrieves offender demographics (age, count, ethnicity, race, sex) from the
#' CDE API endpoint `nibrs/{level}/{offense}?type=totals`.
#'
#' @family NIBRS functions
#' @inheritParams get_nibrs_victim
#' @param variable A string with the demographic variable to return. Selects
#'   which sub-object within the offender section to use (e.g., "age", "count",
#'   "ethnicity", "race", "sex"). By default returns total count of offenders.
#'   See `list_nibrs_offender_variables()` for available options.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_offender(ori = "CA0010900")
#' get_nibrs_offender(state_abb = "CA", offense = "murder", variable = "race")
#' }
get_nibrs_offender <- function(ori = NULL,
                                state_abb = NULL,
                                from = "01-2015",
                                to = "12-2020",
                                offense = "all",
                                variable = "count") {
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

  path <- cde_path("nibrs", level, offense)
  query <- list(from = from, to = to, type = "totals")

  response <- cde_request(path, query)
  parse_nibrs_response(response, geography, offense, "offender", variable)
}


#' Gets offense-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' Retrieves offense characteristics (count, weapons, linkedoffense,
#' suspectusing, criminal_activity, property_recovered, property_stolen, bias)
#' from the CDE API endpoint `nibrs/{level}/{offense}?type=totals`.
#'
#' @family NIBRS functions
#' @inheritParams get_nibrs_victim
#' @param variable A string with the variable to return. Selects which
#'   sub-object within the offense section to use (e.g., "count", "weapons",
#'   "linkedoffense", "suspectusing", "criminal_activity",
#'   "property_recovered", "property_stolen", "bias"). By default returns
#'   total count of offenses. See `list_nibrs_offense_variables()` for
#'   available options.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_offense(ori = "CA0010900")
#' get_nibrs_offense(state_abb = "CA", offense = "murder", variable = "weapons")
#' }
get_nibrs_offense <- function(ori = NULL,
                               state_abb = NULL,
                               from = "01-2015",
                               to = "12-2020",
                               offense = "all",
                               variable = "count") {
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

  path <- cde_path("nibrs", level, offense)
  query <- list(from = from, to = to, type = "totals")

  response <- cde_request(path, query)
  parse_nibrs_response(response, geography, offense, "offense", variable)
}


#' Return a vector of all offenses in the NIBRS data.
#'
#' @family NIBRS functions
#'
#' @return
#' A character vector of NIBRS offense codes.
#' @export
#'
#' @examples
#' list_nibrs_offenses()
list_nibrs_offenses <- function() {
  fbi::nibrs_offenses
}


#' Returns a vector of all `variable` parameter options for `get_nibrs_victim()`.
#'
#' @family NIBRS functions
#'
#' @return
#' A character vector of all `variable` parameter options for `get_nibrs_victim()`.
#' @export
#'
#' @examples
#' list_nibrs_victim_variables()
list_nibrs_victim_variables <- function() {
  fbi::nibrs_victim_variables
}


#' Returns a vector of all `variable` parameter options for `get_nibrs_offender()`.
#'
#' @family NIBRS functions
#'
#' @return
#' A character vector of all `variable` parameter options for `get_nibrs_offender()`.
#' @export
#'
#' @examples
#' list_nibrs_offender_variables()
list_nibrs_offender_variables <- function() {
  fbi::nibrs_offender_variables
}


#' Returns a vector of all `variable` parameter options for `get_nibrs_offense()`.
#'
#' @family NIBRS functions
#'
#' @return
#' A character vector of all `variable` parameter options for `get_nibrs_offense()`.
#' @export
#'
#' @examples
#' list_nibrs_offense_variables()
list_nibrs_offense_variables <- function() {
  fbi::nibrs_offense_variables
}


#' Return a vector of all regions choices.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector of all regions choices.
#' @export
#'
#' @examples
#' list_regions()
list_regions <- function() {
  fbi::regions
}
