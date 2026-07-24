# Coarse, filterable grouping of the CDE's raw `agency_type_name` values.
# This is where the membership model's opinion is *declared* (see the design
# doc). Unknown or missing types fall through to "special" so they are excluded
# from the conservative default set rather than silently counted.

DEFAULT_MEMBER_CLASSES <- c("county_primary", "municipal")

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
  "Other State Agency"    = "special",
  "Census Area"           = "special"
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
# reporter, and county/state/tribal agencies are not place members at all.
#
# Sheriffs police the unincorporated remainder and contract cities report under
# their own city ORI (design §14), so a place's crime is carried entirely by its
# own ORI. Mapping them to NA keeps them structurally unable to become members.
.PLACE_AGENCY_CLASS_MAP <- c(
  "City"                  = "place_primary",
  "Municipality"          = "place_primary",
  "Borough"               = "place_primary",
  "City and Borough"      = "place_primary",
  "University or College" = "campus",
  "Other"                 = "special",
  "Other State Agency"    = "special",
  "Census Area"           = "special"
)

#' Classify an agency type into a place-level membership class
#'
#' Maps the CDE's raw `agency_type_name` to a place-level `agency_class`.
#' Types that can never be place members (`County`, `Parish`, `State Police`,
#' `Tribal`) map to `NA`.
#'
#' @param agency_type_name Character vector of raw `agency_type_name` values.
#' @return Character vector of `"place_primary"`, `"campus"`, `"special"`, or
#'   `NA` for types that are not place members.
#' @keywords internal
classify_place_agency <- function(agency_type_name) {
  unname(.PLACE_AGENCY_CLASS_MAP[as.character(agency_type_name)])
}
