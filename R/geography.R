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
  "county_name", "state_abbr", "latitude", "longitude"
)

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
#'   `agency_class`, `default_member`, `county_name`, `state_abbr`, `latitude`,
#'   `longitude`. `default_member` is `TRUE` for `county_primary` and `municipal`
#'   agencies. Returns a zero-row frame (with a warning) if no agencies match.
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
  sel <- ag[keep, , drop = FALSE]

  if (nrow(sel) == 0) {
    warning("No agencies match county '", county, "' in state '", state, "'",
            call. = FALSE)
    empty <- as.data.frame(
      matrix(nrow = 0, ncol = length(.COUNTY_AGENCY_COLS),
             dimnames = list(NULL, .COUNTY_AGENCY_COLS)),
      stringsAsFactors = FALSE
    )
    empty$default_member <- logical(0)
    return(empty)
  }

  sel$agency_class <- classify_agency(sel$agency_type_name)
  sel$default_member <- sel$agency_class %in% DEFAULT_MEMBER_CLASSES

  out <- sel[, .COUNTY_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}
