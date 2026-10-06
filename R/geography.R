# Layer 0 of the geography model: the membership resolver. Pure, no network.
# Reads the bundled agency table and returns the classified candidate set for a
# county. All opinion is *declared* here (agency_class); none is
# applied (no filtering, summation, or fetching).

# `fbi_api_agencies` is a lazy-loaded package dataset; declare it to satisfy
# R CMD check's global-variable analysis. The import keeps `utils` (in Imports)
# visibly used: a top-level `utils::` call does not count as a use.
#' @importFrom utils globalVariables
NULL
utils::globalVariables("fbi_api_agencies")

# Internal accessor so the source of the agency table is swappable in tests.
# Every geography resolver reads agencies through here, so all of them see the
# county attributions below.
agencies_table <- function() {
  .apply_planning_regions(.apply_county_attributions(fbi_api_agencies))
}

utils::globalVariables("ct_planning_regions")

# Connecticut agencies also belong to their planning region (#52).
#
# Connecticut replaced its counties with nine planning regions as county
# equivalents in 2022, and the 2023 OMB delineation builds the state's CBSAs
# from them, so with only traditional counties every Connecticut metro was
# empty. The CDE's live agency directory now gives each agency's planning
# region as its county; `ct_planning_regions` (data-raw/ct_planning_regions.R)
# records that for the bundled agencies. The region is appended to the
# agency's county list, so it is reachable by its traditional county (as the
# 2019 snapshot has it) and by its planning region, and metros dedupe it.
.apply_planning_regions <- function(ag, regions = ct_planning_regions) {
  i <- match(ag$ori, regions$ori)
  hit <- which(!is.na(i))
  region <- regions$planning_region[i[hit]]
  current <- ag$county_name[hit]
  ag$county_name[hit] <- ifelse(is.na(current) | current == "N/A", region,
                                paste(current, region, sep = "; "))
  ag
}

# Counties the package attributes to agencies the CDE leaves without one.
#
# The CDE gives these agencies no county ("N/A" in the bundled table, "NOT
# SPECIFIED" in the live agency directory), yet each is the police of a whole
# county or county-equivalent. Without an entry here their counties and metros
# silently lost their largest department: county_agencies("District of
# Columbia", "DC") found nothing, and the New York metro had no NYPD.
#
#   - NY0303000, New York City Police Department: all five boroughs, each a
#     county. NYPD reports one citywide series, so like any multi-county agency
#     it is attributed in full to each borough; a borough's NYPD rows are New
#     York City's.
#   - DCMPD0000, the District's Metropolitan Police Department.
#   - MD0040600, Baltimore City Sheriff's Office. Baltimore city is an
#     independent city; its Police Department already carries BALTIMORE CITY.
#
# Keyed by ORI, never by agency name. An entry fills county_name only while it
# is "N/A", so a county the CDE supplies always wins. Each county named here
# must be in the FIPS crosswalk (data-raw/crosswalk_attributed_counties.R).
.AGENCY_COUNTY_ATTRIBUTIONS <- c(
  NY0303000 = "BRONX; KINGS; NEW YORK; QUEENS; RICHMOND",
  DCMPD0000 = "DISTRICT OF COLUMBIA",
  MD0040600 = "BALTIMORE CITY"
)

.apply_county_attributions <- function(ag,
                                       attributions = .AGENCY_COUNTY_ATTRIBUTIONS) {
  i <- match(ag$ori, names(attributions))
  fill <- !is.na(i) & ag$county_name %in% "N/A"
  ag$county_name[fill] <- unname(attributions[i[fill]])
  ag
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
  "ori", "agency_name", "agency_type_name", "agency_class",
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
#' Returns every agency in the bundled agency table whose county and state
#' match, classified by `agency_class`. This is an *attribution* model, not a
#' spatial one: it reports the agencies attributed to a county, not crime known
#' to have occurred there. It returns every class; the crime functions
#' ([get_county_crime_detail()], [get_metro_crime_detail()]) then choose which
#' classes to query.
#'
#' @param county County name (case-insensitive; e.g. `"Alameda"`).
#' @param state Two-letter state abbreviation (e.g. `"CA"`).
#' @return A data.frame with columns `ori`, `agency_name`, `agency_type_name`,
#'   `agency_class`, `county_name`, `state_abbr`, `county_fips`,
#'   `latitude`, `longitude`, `agency_county_names`. `county_name` and
#'   `county_fips` (5-digit, character, preserving leading zeros) identify the
#'   queried county on every row. `agency_county_names` is the CDE's own county
#'   list for the agency: semicolon-separated for an agency that polices several
#'   counties (e.g. Columbus PD, `"DELAWARE; FAIRFIELD; FRANKLIN"`). Such an
#'   agency is attributed in full to each of its counties, so summing results
#'   across counties counts it more than once. `agency_class` is one of
#'   `"county_primary"` (sheriff, parish), `"municipal"` (city police),
#'   `"campus"`, `"state"` (state police and other state agencies),
#'   `"special"` (transit, school, airport, port, park and railroad police,
#'   task forces) or `"tribal"`. Returns a zero-row frame (with a warning) if
#'   no agencies match.
#' @section Agencies the CDE leaves without a county:
#' The CDE assigns no county to three agencies that each police a whole county
#' or county-equivalent, so the package attributes them itself, by ORI:
#'
#' * New York City Police Department (`NY0303000`): the five boroughs, Bronx,
#'   Kings, New York, Queens and Richmond counties.
#' * Metropolitan Police Department of the District of Columbia
#'   (`DCMPD0000`): the District of Columbia.
#' * Baltimore City Sheriff's Office (`MD0040600`): Baltimore city.
#'
#' Connecticut agencies are also attributed to their **planning region**, the
#' county equivalent Connecticut adopted in 2022 and the unit its metro areas
#' are built from, as the CDE's live agency directory reports it. Query either
#' a traditional county (`county_agencies("Hartford", "CT")`) or a region
#' (`county_agencies("Capitol Planning Region", "CT")`), but do not add the
#' two systems together: every agency is in one of each. Six agencies have no
#' region in the directory (the state police, the DMV, two tribal agencies,
#' and Yale and UConn Health, which it lists under other identifiers).
#'
#' The NYPD reports one series for the whole city; the CDE has no borough
#' breakdown. Like any multi-county agency it is attributed in full to each
#' of its counties, so `county_agencies("Kings", "NY")` includes the NYPD, and
#' the crime functions return **citywide** NYPD figures for Brooklyn, with
#' New York City's population. A borough's results therefore describe the
#' city, not the borough. Use the New York metro, or the NYPD itself, rather
#' than summing boroughs. For these agencies `agency_county_names` holds the
#' package's attribution, not a list from the CDE.
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
