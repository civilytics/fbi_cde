# Coarse, filterable grouping of the CDE's raw `agency_type_name` values.
# This is where the membership model's opinion is *declared* (see the design
# doc). Unknown or missing types fall through to "special".
#
# Which classes the geography functions query by default is decided by agency
# type alone, never by parsing agency names:
#
# - included: sheriffs/parishes, city police, campus police -- agencies whose
#   jurisdiction is a specific area below the state.
# - "state" (State Police, Other State Agency): added by
#   include_statewide = TRUE. Many of these records are county or troop units,
#   but some are headquarters records covering the whole state, and the type
#   does not tell them apart.
# - "special" (Other) and "tribal": only when requested via agency_class.
#   "Other" mixes single-site agencies (transit, school, airport, port, park,
#   railroad police) with multi-county task forces attributed to one county;
#   how tribal agencies should attribute to counties is unresolved.
.EXCLUDED_BY_DEFAULT_CLASSES <- c("special", "state", "tribal")

# Every class value either classifier can produce; agency_class arguments are
# validated against it.
.ALL_AGENCY_CLASSES <- c("county_primary", "municipal", "campus", "special",
                         "state", "tribal", "place_primary")

.AGENCY_CLASS_MAP <- c(
  "County"                = "county_primary",
  "Parish"                = "county_primary",
  "City"                  = "municipal",
  "Municipality"          = "municipal",
  "Borough"               = "municipal",
  "City and Borough"      = "municipal",
  "University or College" = "campus",
  "State Police"          = "state",
  "Tribal"                = "tribal",
  "Other"                 = "special",
  "Other State Agency"    = "state",
  # The CDE's "Census Area" agencies are Alaska city police departments
  # (Nome, Bethel, Dillingham, ...), not special-purpose agencies.
  "Census Area"           = "municipal"
)

#' Classify an agency type into a membership class
#'
#' Maps the CDE's raw `agency_type_name` to a coarse, filterable `agency_class`
#' used by the geography functions. Unknown or missing values map to `"special"`.
#'
#' @param agency_type_name Character vector of raw `agency_type_name` values.
#' @return Character vector of `agency_class` values: one of `"county_primary"`,
#'   `"municipal"`, `"campus"`, `"state"`, `"tribal"`, `"special"`.
#' @keywords internal
classify_agency <- function(agency_type_name) {
  out <- unname(.AGENCY_CLASS_MAP[as.character(agency_type_name)])
  out[is.na(out)] <- "special"
  out
}

# Place-level classification. Distinct from `classify_agency()` because the
# tiers differ: at place level the municipal agency IS the place's primary
# reporter, and sheriffs, state police and tribal agencies are not place
# members at all. Other state agencies (a state park's rangers, a capitol
# police force) can sit inside a place, so they class as "state" and are only
# queried with include_statewide = TRUE.
#
# Sheriffs police the unincorporated remainder and contract cities report under
# their own city ORI (design §14), so a place's crime is carried entirely by its
# own ORI. Mapping them to NA keeps them structurally unable to become members.
.PLACE_AGENCY_CLASS_MAP <- c(
  "City"                  = "place_primary",
  "Municipality"          = "place_primary",
  "Borough"               = "place_primary",
  "City and Borough"      = "place_primary",
  "Census Area"           = "place_primary",
  "University or College" = "campus",
  "Other"                 = "special",
  "Other State Agency"    = "state"
)

#' Classify an agency type into a place-level membership class
#'
#' Maps the CDE's raw `agency_type_name` to a place-level `agency_class`.
#' Types that can never be place members (`County`, `Parish`, `State Police`,
#' `Tribal`) map to `NA`.
#'
#' @param agency_type_name Character vector of raw `agency_type_name` values.
#' @return Character vector of `"place_primary"`, `"campus"`, `"special"`,
#'   `"state"`, or `NA` for types that are not place members.
#' @keywords internal
classify_place_agency <- function(agency_type_name) {
  unname(.PLACE_AGENCY_CLASS_MAP[as.character(agency_type_name)])
}
