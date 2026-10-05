# Layer 0 of the geography model: the membership resolver. Pure, no network.
# Reads the bundled agency table and returns the classified candidate set for a
# county. All opinion is *declared* here (agency_class, default_member); none is
# applied (no filtering, summation, or fetching).

# `fbi_api_agencies` is a lazy-loaded package dataset; declare it to satisfy
# R CMD check's global-variable analysis. The import keeps `utils` (in Imports)
# visibly used: a top-level `utils::` call does not count as a use.
#' @importFrom utils globalVariables
NULL
utils::globalVariables("fbi_api_agencies")

# Internal accessor so the source of the agency table is swappable in tests.
agencies_table <- function() {
  fbi_api_agencies
}

# TRUE where `county_name` covers `county_key` (already uppercase, trimmed).
#
# The CDE stores a multi-county agency's county_name as a semicolon-separated
# list -- Columbus PD is "DELAWARE; FAIRFIELD; FRANKLIN". Exact string equality
# never matched those 617 agencies, so major city departments were silently
# missing from their own county and metro (#56). Membership against the split
# list fixes that.
#
# Splitting rather than substring-matching is deliberate: `grepl("YORK", ...)`
# would wrongly match "NEW YORK".
.county_name_matches <- function(county_name, county_key) {
  parts <- strsplit(toupper(trimws(county_name)), ";", fixed = TRUE)
  vapply(parts, function(p) county_key %in% trimws(p), logical(1))
}

.COUNTY_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "county_fips", "latitude", "longitude",
  "agency_county_names"
)

# A 0-row, .COUNTY_AGENCY_COLS-shaped frame whose column types match those of a
# populated result. Built column-by-column rather than from `matrix(nrow = 0,
# ...)`, whose default mode is `logical`: that types every column `logical(0)`,
# so `rbind()`ing an empty result onto a populated one — the natural way to
# stack several counties — errors or silently coerces.
#
# These types must track the populated frame: `latitude`/`longitude` are
# numeric because the bundled agency table stores them that way.
.empty_county_agency_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    default_member = logical(0),
    county_name = character(0),
    state_abbr = character(0),
    county_fips = character(0),
    latitude = numeric(0),
    longitude = numeric(0),
    agency_county_names = character(0),
    stringsAsFactors = FALSE
  )
}

#' List the law-enforcement agencies attributed to a county
#'
#' Returns every agency in the bundled agency table whose county and state match,
#' classified by `agency_class` and flagged with a conservative `default_member`
#' indicator. This is an *attribution* model, not a spatial one: it reports the
#' agencies attributed to a county, not crime known to have occurred there.
#'
#' @param county County name (case-insensitive; e.g. `"Alameda"`).
#' @param state Two-letter state abbreviation (e.g. `"CA"`).
#' @return A data.frame with columns `ori`, `agency_name`, `agency_type_name`,
#'   `agency_class`, `default_member`, `county_name`, `state_abbr`, `county_fips`,
#'   `latitude`, `longitude`, `agency_county_names`. `county_name` and
#'   `county_fips` (5-digit, character, preserving leading zeros) identify the
#'   queried county on every row. `agency_county_names` is the CDE's own county
#'   list for the agency: semicolon-separated for an agency that polices several
#'   counties (e.g. Columbus PD, `"DELAWARE; FAIRFIELD; FRANKLIN"`). Such an
#'   agency is attributed in full to each of its counties, so summing results
#'   across counties counts it more than once. `default_member` is `TRUE` for
#'   `county_primary` and `municipal` agencies. Returns a zero-row frame
#'   (with a warning) if no agencies match.
#' @export
#' @examples
#' \dontrun{
#' county_agencies("Alameda", "CA")
#' }
county_agencies <- function(county, state) {
  if (!is_valid_state(state)) {
    stop("Invalid state abbreviation: ", state, call. = FALSE)
  }

  ag <- agencies_table()
  county_key <- toupper(trimws(county))
  state_key <- toupper(trimws(state))

  keep <- .county_name_matches(ag$county_name, county_key) &
    toupper(trimws(ag$state_abbr)) == state_key
  keep[is.na(keep)] <- FALSE
  sel <- ag[keep, , drop = FALSE]

  if (nrow(sel) == 0) {
    warning("No agencies match county '", county, "' in state '", state, "'",
            call. = FALSE)
    return(.empty_county_agency_frame())
  }

  sel$agency_class <- classify_agency(sel$agency_type_name)
  sel$default_member <- sel$agency_class %in% DEFAULT_MEMBER_CLASSES

  # Attribute every row to the county that was asked for. A multi-county
  # agency's raw county_name ("DELAWARE; FAIRFIELD; FRANKLIN") describes the
  # agency, not this row: left in place it split get_county_crime() into one
  # group per distinct string, and deriving county_fips from the first row
  # gave Licking County, OH the FIPS of Fairfield. The raw list is kept in
  # agency_county_names. county_key is already the CDE's own spelling, since
  # it matched one of the split parts exactly.
  sel$agency_county_names <- sel$county_name
  sel$county_name <- county_key
  sel$county_fips <- county_to_fips(state_key, county_key)

  out <- sel[, .COUNTY_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}
