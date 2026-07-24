# Layer 0 of the place geography model: the municipal membership resolver.
# Pure, no network, no new dependencies.
#
# The municipal tier is a name-identity problem, not a spatial one: CDE city
# agencies are named "<Place> Police Department", so the agency *is* the place.
# See docs/superpowers/specs/2026-07-24-place-membership-v0.4-design.md §1.

# The four agency_type_name values that constitute the municipal tier.
.MUNICIPAL_TYPES <- c("City", "Municipality", "Borough", "City and Borough")

# Trailing agency-name suffixes stripped to recover the bare place name.
# Order matters: longer, more specific alternatives must precede shorter ones
# so "Police Department" is not truncated to "Department" by an earlier match.
.PLACE_SUFFIX_PATTERN <- paste0(
  " (",
  paste(
    c(
      "Police Department",
      "Police Dept\\.?",
      "Department of Public Safety",
      "Public Safety Department",
      "Marshal's Office",
      "Police"
    ),
    collapse = "|"
  ),
  ")$"
)

# Recover the place name from a municipal agency's name by stripping recognized
# trailing suffixes. Names matching no suffix are returned as-is (the CDE stores
# a few hundred agencies under a bare place name).
#
# Stripping repeats to a fixed point because two real records carry a doubled
# suffix — "Las Vegas Metropolitan Police Department Police Department"
# (NV0020100) and "Northeast Police Department Police Department" (PA0081200).
# A single pass would leave a residual suffix in the derived place name.
# The loop is safe: the pattern requires a space before the matched suffix, so a
# name that is only "Police" is a fixed point rather than being stripped empty.
derive_place_name <- function(agency_name) {
  out <- trimws(agency_name)
  repeat {
    stripped <- trimws(sub(.PLACE_SUFFIX_PATTERN, "", out, perl = TRUE))
    if (identical(stripped, out)) {
      break
    }
    out <- stripped
  }
  out
}
