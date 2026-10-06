# Layer 0 of the place geography model: the municipal membership resolver.
# Pure, no network, no new dependencies.
#
# The municipal tier is a name-identity problem, not a spatial one: CDE city
# agencies are named "<Place> Police Department", so the agency *is* the place.
# See specs/2026-07-24-place-membership-v0.4-design.md §1.

# The four agency_type_name values that constitute the municipal tier.
.MUNICIPAL_TYPES <- c("City", "Municipality", "Borough", "City and Borough",
                      "Census Area")

# Trailing agency-name suffixes stripped to recover the bare place name.
# The pattern is end-anchored with `$`, so each alternative must consume the
# entire remaining tail of the string; alternation order does not matter.
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
#
# A ", <Name> County" disambiguator is dropped too, wherever it sits relative
# to the suffix: Pennsylvania and Ohio put it last ("Clay Township Police
# Department, Montgomery County"), New Jersey and Michigan before the suffix
# ("Hamilton Township, Mercer County Police Department"). Left on, it made
# those places unreachable by name. The county itself is already in the
# agency's county_name, which is what place_agencies(county = ) matches.
.COUNTY_DISAMBIGUATOR <- ",\\s*[^,]+ County$"

derive_place_name <- function(agency_name) {
  out <- trimws(sub(.COUNTY_DISAMBIGUATOR, "", agency_name))
  repeat {
    stripped <- trimws(sub(.PLACE_SUFFIX_PATTERN, "", out, perl = TRUE))
    stripped <- trimws(sub(.COUNTY_DISAMBIGUATOR, "", stripped))
    if (identical(stripped, out)) {
      break
    }
    out <- stripped
  }
  out
}

.PLACE_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class",
  "place_name", "place_type", "place_fips", "cousub_fips",
  "county_name", "state_abbr", "attribution",
  "latitude", "longitude"
)

# The columns a membership frame must carry to be extended or queried. The
# Census code columns are not among them: a frame built before they existed,
# or by hand, gets them filled from the crosswalk by ORI.
.PLACE_AGENCY_REQUIRED_COLS <- setdiff(
  .PLACE_AGENCY_COLS, c("place_type", "place_fips", "cousub_fips")
)

utils::globalVariables("place_crosswalk")

#' The Census vintage of the bundled place crosswalk
#'
#' [place_agencies()] gives each municipal agency the Census code of the place
#' or county subdivision it polices, from a crosswalk built from the Census
#' Bureau's 2020 reference code files (see `data-raw/place_fips_crosswalk.R`).
#' Places are occasionally incorporated, merged or dissolved, so codes are
#' only guaranteed against this vintage. Read from the shipped crosswalk's own
#' `"vintage"` attribute when the package loads.
#'
#' @format An integer scalar.
#' @export
PLACE_VINTAGE <- 2020L

# Census codes for each ORI, from the bundled crosswalk: a data.frame with
# place_type, place_fips and cousub_fips aligned to `ori`, NA where the agency
# is not in the crosswalk.
.place_codes <- function(ori) {
  i <- match(ori, place_crosswalk$ori)
  data.frame(
    place_type = place_crosswalk$place_type[i],
    place_fips = place_crosswalk$place_fips[i],
    cousub_fips = place_crosswalk$cousub_fips[i],
    stringsAsFactors = FALSE
  )
}

# Give a membership frame the three Census code columns, filling any that are
# missing from the crosswalk by ORI. Columns already present are kept: rows
# added by add_place_spatial_members() carry their polygon's code instead.
.with_place_codes <- function(x) {
  missing <- setdiff(c("place_type", "place_fips", "cousub_fips"), names(x))
  if (length(missing) > 0) {
    codes <- .place_codes(x$ori)
    for (col in missing) {
      x[[col]] <- codes[[col]]
    }
  }
  x
}

# A 0-row, .PLACE_AGENCY_COLS-shaped frame with the correct column types.
# Built column-by-column rather than via matrix(nrow = 0, ...): a matrix has a
# single element type, so every column would come back "logical" except the
# ones explicitly overridden afterwards. That silently mistyped the character
# columns (ori, agency_name, ...) as logical(0), which breaks or coerces
# unexpectedly under rbind() against a populated result.
.empty_place_agency_frame <- function() {
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
    # Numeric, as the bundled agency table stores coordinates. These types
    # must track the populated frame.
    latitude = numeric(0),
    longitude = numeric(0),
    stringsAsFactors = FALSE
  )
}

# The municipal-tier slice of the agency table, with place names derived.
# Recomputed per call: ~11,600 regex operations, negligible, and it keeps the
# derivation rules in reviewable R source rather than frozen in sysdata.rda.
.municipal_agencies <- function() {
  ag <- agencies_table()
  mun <- ag[ag$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE]
  mun$place_name <- derive_place_name(mun$agency_name)
  mun
}

#' List the law-enforcement agencies attributed to a place
#'
#' Resolves a municipality to the agency that reports crime for it. CDE city
#' agencies are named `"<Place> Police Department"`, so the place name is
#' recovered from the agency name — the agency *is* the place. This is an
#' *attribution* model, not a spatial one.
#'
#' Sheriffs, state police, and tribal agencies are never place members: a
#' sheriff's population is the unincorporated remainder, and contract cities
#' report under their own city ORI. A place's crime is therefore carried
#' entirely by its own agency, with no double-count and no gap.
#'
#' Campus and special-district agencies sit *inside* places but cannot be
#' attributed from names alone (name-matching achieves only ~26% recall). Add
#' them with [add_place_spatial_members()], which is opt-in and requires `sf`
#' and `tigris`.
#'
#' @param place Place name (case-insensitive; e.g. `"Lufkin"`). Supply either
#'   the bare name or the full agency name — both resolve.
#' @param state Two-letter state abbreviation (e.g. `"TX"`).
#' @param county Optional county name, needed only to disambiguate a place name
#'   that occurs in more than one county of the same state.
#' @section Census codes:
#' Each agency carries the Census code of the unit it polices, for joining
#' Census data:
#'
#' * `place_fips`: the 7-digit place code (2-digit state + 5-digit place),
#'   when the agency polices a Census place. `place_type` is then
#'   `"incorporated"`, or `"cdp"` for the few agencies whose only match is a
#'   census-designated place.
#' * `cousub_fips`: the 10-digit county subdivision code (state + county +
#'   subdivision), when the agency polices a township, or a New England or New
#'   York town. Those are governments but not Census places; `place_type` is
#'   then `"county_subdivision"` and `place_fips` is `NA`.
#'
#' Codes are matched by name within the agency's state, and only among Census
#' units lying in one of the agency's own counties; an incorporated place wins
#' over a county subdivision, which wins over a CDP. 98.7% of the 11,646
#' municipal agencies resolve. The rest -- regional and multi-municipality
#' departments, and two names that match more than one unit in their county --
#' get `NA` rather than a guess. Codes follow the Census vintage in
#' [PLACE_VINTAGE].
#'
#' @return A data.frame with columns `ori`, `agency_name`, `agency_type_name`,
#'   `agency_class`, `place_name`, `place_type`, `place_fips`, `cousub_fips`,
#'   `county_name`, `state_abbr`, `attribution`, `latitude`, `longitude`.
#'   `attribution` records how the row earned membership: `"name_identity"`
#'   here, or `"point_in_polygon"` for rows added by
#'   [add_place_spatial_members()]. Returns a zero-row frame (with a warning)
#'   when no municipal agency matches.
#' @seealso [county_agencies()] for the county-level resolver,
#'   [get_place_crime_detail()] for the crime series.
#' @export
#' @examples
#' place_agencies("Lufkin", "TX")
place_agencies <- function(place, state, county = NULL) {
  if (!is_valid_state(state)) {
    stop("Invalid state abbreviation: ", state, call. = FALSE)
  }

  mun <- .municipal_agencies()
  state_key <- toupper(trimws(state))
  # Accept either a bare place name or a full agency name.
  place_key <- toupper(derive_place_name(place))

  keep <- toupper(trimws(mun$state_abbr)) == state_key &
    toupper(mun$place_name) == place_key
  keep[is.na(keep)] <- FALSE
  sel <- mun[keep, , drop = FALSE]

  if (!is.null(county)) {
    county_key <- toupper(trimws(county))
    # Match against the split list, as county_agencies() does: Columbus PD's
    # county_name is "DELAWARE; FAIRFIELD; FRANKLIN", so an exact comparison
    # with county = "Franklin" found nothing.
    in_county <- .county_name_matches(sel$county_name, county_key)
    in_county[is.na(in_county)] <- FALSE
    sel <- sel[in_county, , drop = FALSE]
  }

  if (nrow(sel) == 0) {
    warning("No municipal agency matches place '", place, "' in state '", state,
            "'. Either the place has no reporting agency, or its name differs ",
            "in the CDE agency table.", call. = FALSE)
    return(.empty_place_agency_frame())
  }

  if (nrow(sel) > 1) {
    stop("Place '", place, "' is ambiguous in ", state_key,
         ": it occurs in ", nrow(sel), " counties (",
         paste(sel$county_name, collapse = ", "),
         "). Disambiguate with county = \"",
         trimws(strsplit(sel$county_name[1], ";", fixed = TRUE)[[1]][1]), "\".",
         call. = FALSE)
  }

  sel$agency_class <- classify_place_agency(sel$agency_type_name)
  sel$attribution <- "name_identity"
  sel <- .with_place_codes(sel)

  out <- sel[, .PLACE_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}
