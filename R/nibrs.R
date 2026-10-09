# Internal helper: parse a nibrs/{level}/{offense}?type=totals response into a
# tidy data.frame.
#
# Response shape (CDE API): victim, offender and offense sections, each mapping
# a variable (age, race, weapons, ...) to {category: count}. The `section`
# argument selects the section and `variable` the map within it.
#
# Returns a long data.frame with columns:
#   geography, offense, period, demographic_type, demographic_value, count
#
# An agency or state with no NIBRS data for the period gets the same shape with
# every count null, which parses to zero rows; nibrs_empty_result() says so.
nibrs_empty_result <- function() {
  message(
    "get_nibrs_*() returned no data for this query: the geography may not ",
    "have reported to NIBRS in this period. Offenses must be codes such as ",
    "'ROB' or '13B' (see list_nibrs_offenses())."
  )
  data.frame(
    geography = character(),
    offense = character(),
    period = character(),
    demographic_type = character(),
    demographic_value = character(),
    count = numeric(),
    stringsAsFactors = FALSE
  )
}

parse_nibrs_response <- function(response, geography, offense, section, variable) {
  section_data <- response[[section]]

  if (is.null(section_data) || length(section_data) == 0) {
    return(nibrs_empty_result())
  }

  var_data <- section_data[[variable]]

  if (is.null(var_data) || length(var_data) == 0) {
    return(nibrs_empty_result())
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
    return(nibrs_empty_result())
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

# Validate and normalise a NIBRS offense code.
#
# The endpoint accepts short codes -- the summary groups (V, P, ROB, BUR, ...)
# and NIBRS offense codes (13B, 35A, 120, ...) -- and returns an all-null
# payload for anything else, including the long names this package used to
# document ("robbery", "burglary-breaking-and-entering") and "all". That
# looked like an upstream outage for months (Gitea #33). Reject the shapes
# that can never work, suggesting the code when the name is recognisable.
.check_nibrs_offense <- function(offense) {
  if (!is.character(offense) || length(offense) != 1L || is.na(offense)) {
    stop("'offense' must be a single NIBRS offense code, e.g. \"ROB\".",
         call. = FALSE)
  }
  code <- toupper(trimws(offense))
  if (identical(code, "ALL")) {
    stop("NIBRS has no all-offenses total. Use \"V\" (violent crime), ",
         "\"P\" (property crime) or an offense code; see ",
         "list_nibrs_offenses().", call. = FALSE)
  }
  if (!grepl("^[A-Z0-9]{1,3}$", code)) {
    stop("'", offense, "' is not a NIBRS offense code; the API returns no ",
         "data for offense names.", .nibrs_code_hint(offense),
         " See list_nibrs_offenses().", call. = FALSE)
  }
  code
}

# " Did you mean 'ROB' (Robbery)?" when a name resembles a known offense label.
.nibrs_code_hint <- function(offense) {
  slug <- function(x) gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(x)))
  tbl <- fbiCDE::nibrs_offenses
  key <- slug(offense)
  labels <- slug(tbl$label)
  hit <- which(labels == key)
  if (length(hit) == 0) {
    hit <- which(startsWith(key, paste0(labels, "-")) |
                   startsWith(labels, paste0(key, "-")))
  }
  if (length(hit) == 0) {
    return("")
  }
  paste0(" Did you mean \"", tbl$code[hit[1]], "\" (", tbl$label[hit[1]],
         ")?")
}

.check_nibrs_variable <- function(variable, valid, lister) {
  if (!is.character(variable) || length(variable) != 1L ||
      !variable %in% valid) {
    stop("'variable' must be one of: ", paste(valid, collapse = ", "),
         " (see ", lister, ").", call. = FALSE)
  }
  invisible(variable)
}

# Shared body of the three get_nibrs_*() functions.
.get_nibrs <- function(section, valid_vars, lister, ori, state_abb, from, to,
                       offense, variable) {
  if (!is.null(ori) && !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
  }
  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }
  cde_validate_dates(from, to, "mm-yyyy")
  offense <- .check_nibrs_offense(offense)
  .check_nibrs_variable(variable, valid_vars, lister)

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

  response <- cde_request(cde_path("nibrs", level, offense),
                          cde_query(from, to, type = "totals"))
  parse_nibrs_response(response, geography, offense, section, variable)
}


#' Gets victim-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' Retrieves victim counts by a demographic or situational variable (age,
#' ethnicity, location, race, relationship, sex) for one offense, summed over
#' the date range, from the CDE API endpoint `nibrs/<level>/<offense>` with
#' `type=totals`.
#'
#' An agency or state that did not report to NIBRS in the period returns zero
#' rows, with a message.
#'
#' @family NIBRS functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default `"V"`, violent crime): a summary group
#'   such as `"V"`, `"P"`, `"ROB"` or `"BUR"`, or a NIBRS offense code such as
#'   `"13B"` or `"35A"`. See [list_nibrs_offenses()]. Offense names such as
#'   `"robbery"` are rejected: the API returns no data for them.
#' @param variable The variable to break victims down by (default `"race"`).
#'   See [list_nibrs_victim_variables()].
#'
#' @return A data.frame with columns: geography, offense (the code), period,
#'   demographic_type (the variable), demographic_value, count.
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_victim(state_abb = "OH", offense = "ROB", variable = "age",
#'                  from = "01-2023", to = "12-2023")
#' }
get_nibrs_victim <- function(ori = NULL,
                             state_abb = NULL,
                             from = "01-2015",
                             to = "12-2020",
                             offense = "V",
                             variable = "race") {
  .get_nibrs("victim", fbiCDE::nibrs_victim_variables,
             "list_nibrs_victim_variables()", ori, state_abb, from, to,
             offense, variable)
}


#' Gets offender-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' Retrieves offender counts by age, ethnicity, race or sex for one offense,
#' summed over the date range, from the CDE API endpoint
#' `nibrs/<level>/<offense>` with `type=totals`. Ages come in ten-year buckets
#' (`"10-19"`, ...).
#'
#' @family NIBRS functions
#' @inheritParams get_nibrs_victim
#' @param variable The variable to break offenders down by (default `"race"`).
#'   See [list_nibrs_offender_variables()].
#'
#' @return A data.frame with the columns described in [get_nibrs_victim()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_offender(state_abb = "OH", offense = "BUR", variable = "age",
#'                    from = "01-2023", to = "12-2023")
#' }
get_nibrs_offender <- function(ori = NULL,
                               state_abb = NULL,
                               from = "01-2015",
                               to = "12-2020",
                               offense = "V",
                               variable = "race") {
  .get_nibrs("offender", fbiCDE::nibrs_offender_variables,
             "list_nibrs_offender_variables()", ori, state_abb, from, to,
             offense, variable)
}


#' Gets offense-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' Retrieves offense characteristics -- the weapons involved, or the other
#' offenses reported in the same incidents -- for one offense, summed over the
#' date range, from the CDE API endpoint `nibrs/<level>/<offense>` with
#' `type=totals`.
#'
#' @family NIBRS functions
#' @inheritParams get_nibrs_victim
#' @param variable `"weapons"` (the default) or `"related_offenses"`. See
#'   [list_nibrs_offense_variables()].
#'
#' @return A data.frame with the columns described in [get_nibrs_victim()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_offense(state_abb = "OH", offense = "ROB", variable = "weapons",
#'                   from = "01-2023", to = "12-2023")
#' }
get_nibrs_offense <- function(ori = NULL,
                              state_abb = NULL,
                              from = "01-2015",
                              to = "12-2020",
                              offense = "V",
                              variable = "weapons") {
  .get_nibrs("offense", fbiCDE::nibrs_offense_variables,
             "list_nibrs_offense_variables()", ori, state_abb, from, to,
             offense, variable)
}


#' List the offense codes the NIBRS functions accept.
#'
#' @family NIBRS functions
#'
#' @return
#' A data.frame with columns `code` and `label`: the summary groups (`"V"`,
#' `"P"`, `"ROB"`, `"BUR"`, ...) and the NIBRS offense codes from the CDE's
#' offense lookup. The API also accepts NIBRS offense codes missing from that
#' lookup, such as `"13A"` (aggravated assault) or `"120"` (robbery).
#' @export
#'
#' @examples
#' list_nibrs_offenses()
list_nibrs_offenses <- function() {
  fbiCDE::nibrs_offenses
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
  fbiCDE::nibrs_victim_variables
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
  fbiCDE::nibrs_offender_variables
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
  fbiCDE::nibrs_offense_variables
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
  fbiCDE::regions
}
