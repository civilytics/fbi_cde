# Internal helper: extract the officers-killed totals summary from
# a CDE `leoka/ytd` or `leoka/monthly` response.
#
# Response shape (CDE API): a length-1 array containing one object keyed
# "leoka_chart_ytd" or "leoka_chart_monthly", nested as
# `<key>$data$chart_data$incidents_victim_officer_totals_ytd`. The outer array
# arrives as a length-1 unnamed list to unwrap. The key is also accepted at the
# top level, in case the API stops wrapping it.
parse_leoka_totals <- function(response, year, month = NA_integer_) {
  empty <- data.frame(
    year = integer(), month = integer(), total_officers = integer(),
    total_incidents = integer(), total_officers_dod = integer(),
    total_officers_doi = integer(), total_incidents_dod = integer(),
    total_incidents_doi = integer(), stringsAsFactors = FALSE
  )

  if (is.null(response) || length(response) == 0) {
    return(empty)
  }

  chart_keys <- c("leoka_chart_ytd", "leoka_chart_monthly")
  container <- response
  chart_key <- intersect(names(container), chart_keys)
  if (length(chart_key) == 0) {
    container <- container[[1]]
    chart_key <- intersect(names(container), chart_keys)
  }
  if (length(chart_key) == 0) {
    return(empty)
  }

  totals <- container[[chart_key[1]]]$data$chart_data$incidents_victim_officer_totals_ytd
  if (is.null(totals)) {
    return(empty)
  }

  data.frame(
    year = as.integer(year),
    month = as.integer(month),
    total_officers = as.integer(totals$total_officers),
    total_incidents = as.integer(totals$total_incidents),
    total_officers_dod = as.integer(totals$total_officers_dod),
    total_officers_doi = as.integer(totals$total_officers_doi),
    total_incidents_dod = as.integer(totals$total_incidents_dod),
    total_incidents_doi = as.integer(totals$total_incidents_doi),
    stringsAsFactors = FALSE
  )
}

#' Get national LEOKA (Law Enforcement Officers Killed and Assaulted) totals
#'
#' @description
#' Counts of law enforcement officers **feloniously killed**, nationally, by
#' year. Despite the program's name, these are not assault counts: the totals
#' match the FBI's published felonious-killing figures (46 officers in 2020,
#' 73 in 2021).
#'
#' National only: the live `leoka/ytd` endpoint ignores state/agency query
#' parameters (verified against the live API -- requests with and without
#' `state`/`ori` params return identical data), so the current CDE API has no
#' state- or agency-level LEOKA breakdown. Data is only available from 2020
#' onward; earlier years return an empty (0-row) result for that year and are
#' dropped, since `leoka/ytd?year=<2015-2019>` returns a `null` payload on
#' the live API.
#'
#' @family UCR crime functions
#' @param from Start year (4-digit), default 2020 (earliest year with data).
#' @param to End year (4-digit), default 2024.
#'
#' @return A data.frame with one row per requested year with data and
#'   columns: year, month (always `NA`; year-to-date totals have no month),
#'   total_officers (officers feloniously killed), total_incidents (incidents
#'   in which they were killed), and the `_dod`/`_doi` splits of each. The API
#'   does not label those splits (they sum to the totals) and the package does
#'   not interpret them.
#' @export
#' @author Jared E. Knowles, Civilytics Consulting
#' @examples
#' \dontrun{
#' get_leoka(2020, 2024)
#' }
get_leoka <- function(from = 2020, to = 2024) {
  cde_validate_dates(from, to, year_format = "yyyy")

  years <- seq.int(as.integer(from), as.integer(to))
  rows <- lapply(years, function(y) {
    response <- cde_request("leoka/ytd", list(year = y))
    parse_leoka_totals(response, year = y)
  })

  result <- do.call(rbind, rows)
  as.data.frame(result)
}

#' Get national LEOKA totals for a single month
#'
#' @description
#' National only; see [get_leoka()].
#'
#' @family UCR crime functions
#' @param year 4-digit year.
#' @param month Month, 1-12.
#'
#' @return A one-row data.frame; see [get_leoka()] for columns.
#' @export
#' @author Jared E. Knowles, Civilytics Consulting
#' @examples
#' \dontrun{
#' get_leoka_monthly(2020, 1)
#' }
get_leoka_monthly <- function(year, month) {
  month <- as.integer(month)
  if (is.na(month) || month < 1 || month > 12) {
    stop("month must be an integer between 1 and 12", call. = FALSE)
  }

  response <- cde_request(
    "leoka/monthly",
    list(year = as.integer(year), month = sprintf("%02d", month))
  )
  parse_leoka_totals(response, year = year, month = month)
}
