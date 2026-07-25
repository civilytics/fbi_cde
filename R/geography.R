# Layer 0 of the geography model: the membership resolver. Pure, no network.
# Reads the bundled agency table and returns the classified candidate set for a
# county. All opinion is *declared* here (agency_class, default_member); none is
# applied (no filtering, summation, or fetching).

# `fbi_api_agencies` is a lazy-loaded package dataset; declare it to satisfy
# R CMD check's global-variable analysis.
utils::globalVariables("fbi_api_agencies")

# Internal accessor so the source of the agency table is swappable in tests.
agencies_table <- function() {
  fbi_api_agencies
}

.COUNTY_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "county_fips", "latitude", "longitude"
)

# A 0-row, .COUNTY_AGENCY_COLS-shaped frame whose column types match those of a
# populated result. Built column-by-column rather than from `matrix(nrow = 0,
# ...)`, whose default mode is `logical`: that types every column `logical(0)`,
# so `rbind()`ing an empty result onto a populated one — the natural way to
# stack several counties — errors or silently coerces.
#
# `latitude`/`longitude` are character here because that is how the bundled
# agency table stores them (545 rows hold the literal string "NULL"). These
# types must track the populated frame, not what the columns ideally would be.
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
    latitude = character(0),
    longitude = character(0),
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
#'   `latitude`, `longitude`. `county_fips` is the 5-digit county FIPS code
#'   (character, preserving leading zeros). `default_member` is `TRUE` for
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

  keep <- toupper(trimws(ag$county_name)) == county_key &
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

  # Derive county FIPS for the county (all agencies share the same county FIPS)
  sel$county_fips <- county_to_fips(sel$state_abbr[[1]], sel$county_name[[1]])

  out <- sel[, .COUNTY_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}
