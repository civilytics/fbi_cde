# Layer 0 of the metro geography model: the CBSA membership resolver.
# Pure, no network. A metro is the union of its member counties' agency sets,
# so this delegates to county_agencies() and adds the CBSA columns.

.METRO_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "county_fips", "latitude", "longitude",
  "agency_county_names",
  "cbsa_code", "cbsa_title", "cbsa_type", "central_outlying"
)

# A 0-row frame whose column types match a populated result. Built
# column-by-column, not from matrix(nrow = 0, ...) — see issue #48.
.empty_metro_agency_frame <- function() {
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
    cbsa_code = character(0),
    cbsa_title = character(0),
    cbsa_type = character(0),
    central_outlying = character(0),
    stringsAsFactors = FALSE
  )
}

# Resolve a user-supplied metro name to its crosswalk rows.
#
# Exact title first (unique nationally), then the short name before the first
# comma or dash. An ambiguous short name errors listing candidates rather than
# guessing. Returns 0 rows when nothing matches.
.resolve_cbsa <- function(metro, state = NULL) {
  cw <- .cbsa_table()
  key <- toupper(trimws(metro))

  hit <- cw[toupper(cw$cbsa_title) == key, , drop = FALSE]
  if (nrow(hit) > 0) {
    return(hit)
  }

  # Short-name path: match the portion before the first comma or dash.
  short <- toupper(trimws(sub("[-,].*$", "", cw$cbsa_title)))
  hit <- cw[short == key, , drop = FALSE]

  if (nrow(hit) > 0 && !is.null(state)) {
    # CBSA titles end in a comma-separated list of state abbreviations.
    st <- toupper(trimws(state))
    suffix <- toupper(sub("^.*,\\s*", "", hit$cbsa_title))
    keep <- vapply(strsplit(suffix, "-", fixed = TRUE),
                   function(z) st %in% trimws(z), logical(1))
    hit <- hit[keep, , drop = FALSE]
  }

  titles <- unique(hit$cbsa_title)
  if (length(titles) > 1) {
    stop("Metro '", metro, "' is ambiguous: it matches ", length(titles),
         " CBSAs (", paste(titles, collapse = ", "),
         "). Pass the full title, or disambiguate with state = \"",
         sub("^.*,\\s*", "", titles[1]), "\".", call. = FALSE)
  }

  hit
}

#' List the law-enforcement agencies in a metropolitan area
#'
#' Resolves a Core Based Statistical Area (CBSA) to the union of its member
#' counties' agency sets. A metro is a set of *whole counties*, so this simply
#' stacks [county_agencies()] across them -- `agency_class` and
#' `default_member` keep exactly the meaning they have at county level,
#' including that a county sheriff is a default member.
#'
#' CBSA titles are unique nationally, so `state` is only needed to disambiguate
#' a short name shared by several metros (`"Albany"` matches GA, OR and NY).
#'
#' @param metro A CBSA title (e.g. `"Pittsburgh, PA"`), or a short name
#'   (`"Pittsburgh"`). See [list_metros()] for the available values.
#' @param state Optional two-letter state abbreviation, used only to
#'   disambiguate an ambiguous short name.
#' @return A data.frame with the [county_agencies()] columns plus `cbsa_code`,
#'   `cbsa_title`, `cbsa_type` (`"metro"`/`"micro"`), and `central_outlying`
#'   (whether the agency's county is central or outlying in the CBSA). Each ORI
#'   appears once: an agency that polices several member counties is attributed
#'   to the first of them in delineation order, and its `agency_county_names`
#'   lists all of them. Returns a zero-row frame with a warning when the metro
#'   is unknown.
#' @section Coverage limitations:
#' The bundled crosswalk has 1,915 county-CBSA rows, about 61% of the 3,131
#' counties known to [county_agencies()]. That row count overstates coverage
#' slightly, because some rows are Connecticut planning regions or Puerto
#' Rico municipios that never join a CDE county name (see below). Counted as
#' distinct, CDE-reachable counties instead, coverage is about 58.5% (1,831 of
#' 3,131). Either way, rural counties belong to no CBSA at all; that is a
#' property of the 2023 OMB delineation, not a gap in the data.
#'
#' Connecticut's seven metros are not supported: Bridgeport-Stamford-Danbury,
#' Hartford-West Hartford-East Hartford, New Haven,
#' Norwich-New London-Willimantic, Putnam, Torrington, and Waterbury-Shelton.
#' The 2023 delineation delineates Connecticut by planning regions (FIPS
#' 09110-09190), which replaced its counties in 2022, while the CDE still
#' reports Connecticut agencies by traditional county (09001-09015). The two
#' vocabularies do not join, so all seven Connecticut metros resolve to zero
#' counties. This function warns explicitly in that case rather than
#' returning a silent empty frame, because a quiet zero-row result would read
#' as "no agencies report here", which is false. Tracked as Gitea issue #52.
#'
#' Puerto Rico's 10 CBSAs are unmapped too, but that is academic: the CDE has
#' exactly one Puerto Rico agency.
#'
#' Agencies whose `county_name` is `"N/A"` -- state police, tribal agencies,
#' and the District of Columbia -- cannot be reached from any county-keyed
#' geography, so a metro that includes one is incomplete even when every one
#' of its counties joins cleanly. Washington-Arlington-Alexandria, DC-VA-MD-WV
#' is the visible case: it lists 23 counties but only 22 resolve, because
#' DC's 3 agencies all carry `county_name = "N/A"`.
#' @seealso [list_metros()] to discover metro names,
#'   [county_agencies()].
#' @export
#' @examples
#' \dontrun{
#' metro_agencies("Pittsburgh, PA")
#' }
metro_agencies <- function(metro, state = NULL) {
  hit <- .resolve_cbsa(metro, state)

  if (nrow(hit) == 0) {
    warning("No CBSA matches '", metro,
            "'. See list_metros() for available metros.", call. = FALSE)
    return(.empty_metro_agency_frame())
  }

  fips_to_county <- .cbsa_county_lookup(hit$county_fips)

  parts <- lapply(seq_len(nrow(hit)), function(i) {
    key <- fips_to_county[[hit$county_fips[i]]]
    if (is.null(key)) {
      return(NULL)
    }
    ag <- suppressWarnings(county_agencies(key$county_name, key$state_abbr))
    if (nrow(ag) == 0) {
      return(NULL)
    }
    ag$cbsa_code <- hit$cbsa_code[i]
    ag$cbsa_title <- hit$cbsa_title[i]
    ag$cbsa_type <- hit$cbsa_type[i]
    ag$central_outlying <- hit$central_outlying[i]
    ag
  })

  # Coverage must never be silent. A CBSA whose counties do not join our
  # crosswalk would otherwise return a quiet zero-row frame, which reads as
  # "no agencies report here" -- false, and materially misleading. The live
  # case is Connecticut: the 2023 delineation uses planning regions
  # (09110-09190) while the CDE reports traditional counties (09001-09015),
  # so all seven CT metros resolve to nothing.
  resolved <- sum(hit$county_fips %in% names(fips_to_county))
  if (resolved < nrow(hit)) {
    warning("Metro '", hit$cbsa_title[1], "' lists ", nrow(hit),
            " counties but only ", resolved,
            " could be matched to CDE county names",
            if (any(substr(hit$county_fips, 1L, 2L) == "09")) {
              paste0(". Connecticut is delineated by planning regions, which ",
                     "the CDE does not use, so its metros are not supported")
            } else {
              ""
            },
            ". Results are incomplete.", call. = FALSE)
  }

  out <- rbind_fill(parts)
  if (is.null(out) || nrow(out) == 0) {
    return(.empty_metro_agency_frame())
  }

  # Deduplicate by ORI. A multi-county agency legitimately matches every county
  # it polices, which is correct at county level -- but a metro unions its
  # member counties, and the Columbus CBSA contains all three of Columbus PD's
  # counties. Without this the agency would appear three times and
  # get_metro_crime_detail() would triple-count its crime, breaking the
  # no-double-count invariant the metro layer rests on (#56).
  #
  # The first match is kept, which is the first member county in delineation
  # order: the retained row is attributed to that county (county_name,
  # county_fips and central_outlying all describe it), and its
  # agency_county_names still lists every county the agency covers.
  out <- out[!duplicated(out$ori), , drop = FALSE]

  out <- out[, .METRO_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}

# Map 5-digit county FIPS back to the (county_name, state_abbr) pair
# county_agencies() takes, using the bundled county FIPS crosswalk.
#
# The crosswalk is keyed by the CDE's raw county_name, which includes
# multi-county strings ("FAIRFIELD; LICKING"), so county_fips is NOT unique
# across all rows. Filtering to names without a semicolon gives the canonical
# entry: 3,131 such rows for 3,131 distinct FIPS -- an exact 1:1, with every
# FIPS represented. Skipping that filter would sometimes pick a multi-county
# row, and county_agencies() matches county_name exactly, so it would return a
# subset of the county rather than the county.
.cbsa_county_lookup <- function(fips) {
  cw <- crosswalk
  sel <- cw[!is.na(cw$county_fips) &
              !grepl(";", cw$county_name, fixed = TRUE) &
              cw$county_fips %in% fips, , drop = FALSE]
  sel <- sel[!duplicated(sel$county_fips), , drop = FALSE]
  stats::setNames(
    lapply(seq_len(nrow(sel)), function(i) {
      list(county_name = sel$county_name[i], state_abbr = sel$state_abbr[i])
    }),
    sel$county_fips
  )
}
