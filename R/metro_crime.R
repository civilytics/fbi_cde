# Layer 1 of the metro geography model: itemized, unsummed metro-crime detail.
#
# The fan-out itself lives in R/fanout.R and is shared with the county and
# place levels; this file supplies only the metro column contract, the metro
# metadata columns, and the max_agencies guard.
#
# Unlike its county and place siblings this one is guarded: a metro can be
# hundreds of agencies (New York is 489, each a sequential request), so an
# unbounded call would hang for minutes with no explanation.

.METRO_DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class",
  "county_name", "state_abbr", "county_fips",
  "cbsa_code", "cbsa_title", "cbsa_type", "central_outlying",
  "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

.empty_metro_detail_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    county_name = character(0),
    state_abbr = character(0),
    county_fips = character(0),
    cbsa_code = character(0),
    cbsa_title = character(0),
    cbsa_type = character(0),
    central_outlying = character(0),
    offense = character(0),
    period = character(0),
    count = numeric(0),
    population = numeric(0),
    participated_population = numeric(0),
    rate = numeric(0),
    reported = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Itemized crime detail for every agency in a metropolitan area
#'
#' Fans out one request per member agency across the metro's counties and
#' returns their crime series **unsummed** -- one row per agency-period -- with
#' coverage columns and the CBSA metadata.
#'
#' **This can be an expensive call.** A metro is the union of whole counties, so
#' the largest are very large: New York-Newark-Jersey City resolves to roughly
#' 489 agencies, Chicago 314. Every agency is one sequential request. The median
#' CBSA is only 7 agencies, so the cost is highly skewed -- which is why
#' `max_agencies` refuses the large cases up front rather than letting them run
#' silently for minutes.
#'
#' No metro-level aggregate is provided. Summing across a metro raises the same
#' denominator question the county aggregate deferred, and a metro's is harder
#' (multi-state, mixed coverage).
#'
#' @inheritParams metro_agencies
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class,include_statewide Which agencies to query, as for
#'   [get_county_crime_detail()].
#' @param max_agencies Refuse to run if the filtered agency set is larger than
#'   this (default `150`), erroring **before any request is issued**. Set to
#'   `Inf` to disable, or narrow the set with `agency_class`.
#' @param progress If `TRUE` (the default here, unlike the county and place
#'   equivalents), print a progress line per agency. At metro scale silence is
#'   indistinguishable from a hang.
#' @return A data.frame with one row per agency-period. Agencies whose request
#'   or parse fails are dropped with a warning; their ORIs are recorded in
#'   `attr(x, "dropped")` and the errors in `attr(x, "dropped_reasons")`.
#' @seealso [metro_agencies()], [list_metros()], [get_county_crime_detail()].
#' @export
#' @examples
#' \dontrun{
#' # A small micropolitan area is cheap.
#' get_metro_crime_detail("Aberdeen, WA", from = "01-2019", to = "12-2019")
#' }
get_metro_crime_detail <- function(metro, state = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL,
                                   include_statewide = FALSE,
                                   max_agencies = 150, progress = TRUE) {
  stopifnot(
    is.numeric(max_agencies), length(max_agencies) == 1L, !is.na(max_agencies)
  )
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- metro_agencies(metro, state)
  pre_filter_n <- nrow(agencies)
  agencies <- .filter_agency_members(agencies, agency_class, include_statewide)

  if (nrow(agencies) == 0) {
    if (pre_filter_n > 0) {
      warning("No agencies to query for metro '", metro,
              "' after filtering (",
              .filter_desc(agency_class, include_statewide), ")",
              call. = FALSE)
    }
    return(.empty_metro_detail_frame())
  }

  # Guard BEFORE any request is issued.
  if (nrow(agencies) > max_agencies) {
    stop("Metro '", metro, "' resolves to ", nrow(agencies),
         " agencies, above max_agencies = ", max_agencies,
         ". Each agency is a separate request, so this would take a while. ",
         "Raise the limit (max_agencies = Inf), or narrow the set with ",
         "agency_class.", call. = FALSE)
  }

  .fanout_agency_detail(
    agencies = agencies,
    meta_cols = c("agency_name", "agency_type_name", "agency_class",
                  "county_name", "state_abbr", "county_fips",
                  "cbsa_code", "cbsa_title", "cbsa_type", "central_outlying"),
    cols = .METRO_DETAIL_COLS,
    empty_fn = .empty_metro_detail_frame,
    offense = offense, from = from, to = to, progress = progress
  )
}
