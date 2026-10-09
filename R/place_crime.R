# Layer 1 of the place geography model: itemized, unsummed place-crime detail.
#
# The fan-out itself lives in R/fanout.R and is shared with the county and
# metro levels; this file supplies only the place column contract, the place
# metadata columns (`place_name`/`attribution`), and the `agencies` escape
# hatch. The loops were previously kept in sync by hand and drifted five ways
# (Gitea #54).

# Columns a caller-supplied `agencies` frame (or a place_agencies() result)
# must carry for the fan-out loop below to work.
.PLACE_CRIME_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class",
  "place_name", "county_name", "state_abbr", "attribution"
)

.PLACE_DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class",
  "place_name", "place_type", "place_fips", "cousub_fips",
  "county_name", "state_abbr", "attribution",
  "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

# Built column-by-column rather than from a 0-row matrix() so the columns carry
# their real types. A matrix-derived empty frame types every column `logical`,
# which breaks rbind() against a populated result — the natural way to stack
# several places' detail together.
.empty_place_detail_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    place_name = character(0),
    place_type = character(0),
    place_fips = character(0),
    cousub_fips = character(0),
    county_name = character(0),
    state_abbr = character(0),
    attribution = character(0),
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

#' Itemized crime detail for every agency attributed to a place
#'
#' Fans out one request per member agency and returns their crime series
#' **unsummed** — one row per agency-period — carrying `agency_class`, coverage
#' (`population`, `participated_population`, `reported`), a per-agency `rate`,
#' and the `attribution` that earned each agency its membership.
#'
#' In the common case a place has exactly one member agency, so this is a
#' by-name wrapper over that agency's series. That is the value: ORI discovery
#' is the package's primary friction, and this removes it.
#'
#' No place-level aggregate is provided. Summing an opted-in campus agency into
#' a city total is a modeling choice, not arithmetic; see the county Layer 2
#' (`get_county_crime()`) for the shape that decision takes.
#'
#' @inheritParams place_agencies
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector of place classes to query
#'   instead of the default set (`"place_primary"`, `"campus"`): any of those
#'   plus `"special"` and `"state"`.
#' @param include_statewide If `TRUE`, also query state agencies (class
#'   `"state"`, e.g. a state park's rangers or a capitol police force) found
#'   inside the place by [add_place_spatial_members()]. Default `FALSE`.
#' @param agencies Optional pre-resolved agency membership data.frame, e.g. the
#'   output of [place_agencies()] with [add_place_spatial_members()] applied.
#'   When supplied, it is used directly instead of calling `place_agencies()`
#'   internally — this is the only way to reach `"campus"`, `"special"` or
#'   `"state"` rows, since [place_agencies()] alone never returns them. Campus
#'   rows supplied this way are queried by default. Must be a data.frame
#'   carrying at least `ori`, `agency_name`, `agency_type_name`,
#'   `agency_class`, `place_name`, `county_name`, `state_abbr`, and
#'   `attribution`. Missing `place_type`, `place_fips` and `cousub_fips`
#'   columns are filled from the place crosswalk by ORI.
#'
#'   Column presence is validated, but membership is **not**: a hand-built frame
#'   can contain any ORI, including a sheriff or state police agency. The
#'   supported path ([place_agencies()], optionally through
#'   [add_place_spatial_members()]) can never produce one — county, parish,
#'   state-police, and tribal agencies are excluded structurally. If you build
#'   the frame yourself, that exclusion becomes yours to maintain: attributing a
#'   sheriff to a place double-counts against the place's own agency, because a
#'   sheriff polices the unincorporated remainder and contract cities report
#'   under their own city ORI.
#' @param progress If `TRUE`, print a simple progress line per agency.
#' @return A data.frame with one row per agency-period, carrying: `ori`,
#'   `agency_name`, `agency_type_name`, `agency_class`, `place_name`, the
#'   Census codes `place_type`, `place_fips` and `cousub_fips` (see
#'   [place_agencies()]), `county_name`, `state_abbr`, `attribution`, `offense`,
#'   `period`, `count`, `population`, `participated_population`, `rate`, and
#'   `reported`. Agencies whose request or parse fails are dropped with a
#'   warning and recorded in `attr(x, "dropped")`. If filtering (via
#'   `agency_class`/`include_statewide`) empties a non-empty agency set, a warning
#'   names the place and the filter responsible, and a zero-row frame is
#'   returned.
#' @seealso [place_agencies()], [add_place_spatial_members()],
#'   [impute_reporting_gaps()] for filling reporting gaps in the result.
#' @export
#' @examples
#' \dontrun{
#' get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
#'
#' # Compose with add_place_spatial_members() to reach campus agencies, which
#' # are then queried by default alongside the city's own:
#' x <- add_place_spatial_members(place_agencies("Berkeley", "CA"))
#' get_place_crime_detail("Berkeley", "CA", agencies = x)
#' }
get_place_crime_detail <- function(place, state, county = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL,
                                   include_statewide = FALSE,
                                   agencies = NULL, progress = FALSE) {
  cde_validate_dates(from, to, "mm-yyyy")

  if (!is.null(agencies)) {
    if (!inherits(agencies, "data.frame")) {
      stop("'agencies' must be a data.frame", call. = FALSE)
    }
    missing_cols <- setdiff(.PLACE_CRIME_AGENCY_COLS, names(agencies))
    if (length(missing_cols) > 0) {
      stop("'agencies' is missing required columns: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  } else {
    agencies <- place_agencies(place, state, county = county)
  }
  agencies <- .with_place_codes(agencies)

  pre_filter_n <- nrow(agencies)
  agencies <- .filter_agency_members(agencies, agency_class, include_statewide)

  if (nrow(agencies) == 0) {
    if (pre_filter_n > 0) {
      warning("No agencies to query for place '", place, "', ", state,
              " after filtering (",
              .filter_desc(agency_class, include_statewide), ")",
              call. = FALSE)
    }
    return(.empty_place_detail_frame())
  }

  .fanout_agency_detail(
    agencies = agencies,
    meta_cols = c("agency_name", "agency_type_name", "agency_class",
                  "place_name", "place_type", "place_fips", "cousub_fips",
                  "county_name", "state_abbr", "attribution"),
    cols = .PLACE_DETAIL_COLS,
    empty_fn = .empty_place_detail_frame,
    offense = offense, from = from, to = to, progress = progress
  )
}
