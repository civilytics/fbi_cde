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
