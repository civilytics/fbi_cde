# Opt-in spatial attribution of embedded agencies to a place.
#
# Separate from the resolver on purpose: it needs heavy Suggests (sf, tigris)
# AND a network fetch, and it is best-effort inference. Keeping it a distinct
# call makes place_agencies() offline and deterministic, and makes the
# inference structurally opt-in — the impute_reporting_gaps() precedent.

# Agency types eligible for point-in-polygon attribution. Sheriffs and state
# police are deliberately absent: their HQ point says nothing about their
# jurisdiction (design §14), so a PIP hit would be meaningless.
.EMBEDDED_TYPES <- c("University or College", "Other", "Other State Agency")

# Isolated so tests can force the unavailable-dependency branch.
.spatial_deps_available <- function() {
  requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("tigris", quietly = TRUE)
}

# Default polygon source. Only reached when tigris is installed.
# Kept to the two arguments tigris::places() has carried stably (`state`,
# `year`) — optional cosmetic arguments have moved between tigris versions.
.default_places_fun <- function(state, vintage) {
  args <- list(state = state)
  if (!is.null(vintage)) {
    args$year <- vintage
  }
  do.call(tigris::places, args)
}

#' Add spatially-attributed embedded agencies to a place membership frame
#'
#' Attributes campus and other special-district agencies (any agency of type
#' `"University or College"`, `"Other"`, or `"Other State Agency"` — CDE has
#' no literal "transit" or "airport" type) to a place by point-in-polygon of
#' their headquarters coordinates against Census place boundaries. Appends
#' them to a [place_agencies()] result.
#'
#' This is opt-in and best-effort. It requires `sf` and `tigris` (both in
#' \sQuote{Suggests}) and downloads boundary shapefiles. Without them the
#' function emits a message and returns its input unchanged.
#'
#' Sheriffs and state police are never candidates: unlike a university police
#' department — whose headquarters genuinely sits on its campus inside the city
#' — a sheriff's HQ point carries no information about its jurisdiction. That
#' restriction is what makes point-in-polygon defensible for this tier.
#'
#' Agencies with no coordinates in the bundled table cannot be placed: 271 of
#' the 2,324 embedded-tier agencies (11.7%) have none and can never be
#' spatially attributed by this function.
#'
#' @param x A data.frame as returned by [place_agencies()].
#' @param vintage Optional Census boundary year passed to `tigris::places()`.
#'   Boundaries change with annexations and new incorporations, so pin this when
#'   reproducibility matters. `NULL` uses the `tigris` default.
#' @param places_fun Function used to fetch place polygons, called as
#'   `places_fun(state, vintage)` and expected to return an `sf` frame with
#'   `GEOID`, `NAME`, and `CLASSFP` columns. Defaults to `tigris::places()`.
#'   Exposed for testing; you should not need to set it.
#' @return `x` with spatially-attributed rows appended and two columns added:
#'   \itemize{
#'     \item `place_type`: `"incorporated"` or `"cdp"` (Census Designated
#'       Place, i.e. unincorporated). `NA` on `name_identity` rows.
#'     \item `place_fips`: the matched polygon's GEOID. **Best-effort
#'       enrichment, not a promised join key** — it is `NA` on `name_identity`
#'       rows, so it does not cover the municipal tier.
#'   }
#'   Appended rows carry `attribution = "point_in_polygon"`. Their
#'   `agency_class` decides whether [get_place_crime_detail()] queries them by
#'   default: `"campus"` rows yes; `"special"` and `"state"` rows only when
#'   requested.
#' @seealso [place_agencies()]
#' @export
#' @examples
#' \dontrun{
#' x <- place_agencies("Berkeley", "CA")
#' add_place_spatial_members(x)
#' }
add_place_spatial_members <- function(x, vintage = NULL, places_fun = NULL) {
  if (!inherits(x, "data.frame")) {
    stop("'x' must be a data.frame", call. = FALSE)
  }
  missing_cols <- setdiff(.PLACE_AGENCY_COLS, names(x))
  if (length(missing_cols) > 0) {
    stop("'x' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Idempotency: if spatial members are already present, adding them again
  # would duplicate ORIs and reset the first application's place_fips to NA
  # (since the pre-existing point_in_polygon rows don't carry latitude/
  # longitude the second pass expects). Central invariant of this package is
  # "no double-count", so refuse rather than silently corrupt.
  if (any(x$attribution == "point_in_polygon", na.rm = TRUE)) {
    message("'x' already has spatially-attributed members (attribution == ",
            "'point_in_polygon'); returning it unchanged.")
    return(x)
  }

  # Attach unconditionally, before the dependency guard, so every return path
  # — including the sf/tigris-unavailable degradation path — yields the same
  # columns. A caller doing out[!is.na(out$place_type), ] must not see the
  # column vanish just because sf isn't installed.
  x$place_type <- NA_character_
  x$place_fips <- NA_character_

  if (is.null(places_fun)) {
    if (!.spatial_deps_available()) {
      message("Packages 'sf' and 'tigris' are required for ",
              "add_place_spatial_members().\n",
              "Install them with: install.packages(c('sf', 'tigris'))")
      return(x)
    }
    places_fun <- .default_places_fun
  }

  if (nrow(x) == 0) {
    return(x)
  }

  state <- x$state_abbr[1]
  target <- toupper(x$place_name[1])

  polys <- places_fun(state, vintage)
  if (!inherits(polys, "data.frame")) {
    stop("'places_fun' must return a data.frame (an sf frame), got: ",
         paste(class(polys), collapse = "/"), call. = FALSE)
  }
  poly_missing <- setdiff(c("NAME", "GEOID", "CLASSFP"), names(polys))
  if (length(poly_missing) > 0) {
    stop("The polygon frame returned by 'places_fun' is missing required ",
         "columns: ", paste(poly_missing, collapse = ", "),
         ". This usually means the tigris/Census place-polygon schema has ",
         "changed.", call. = FALSE)
  }

  name_match <- !is.na(polys$NAME) & toupper(polys$NAME) == target
  match_poly <- polys[name_match, , drop = FALSE]
  if (nrow(match_poly) == 0) {
    warning("No Census place polygon named '", x$place_name[1], "' in ", state,
            "; returning input unchanged.", call. = FALSE)
    return(x)
  }

  cand <- .embedded_candidates(state)
  if (nrow(cand) == 0) {
    return(x)
  }

  idx <- .locate_in_polygons(cand$longitude, cand$latitude, match_poly)
  keep <- !is.na(idx)
  cand <- cand[keep, , drop = FALSE]
  idx <- idx[keep]
  if (nrow(cand) == 0) {
    return(x)
  }

  # CLASSFP "U*" denotes a Census Designated Place (unincorporated). Indexed
  # per row by `idx` rather than match_poly[1, ]: a name match can return more
  # than one polygon (e.g. a CDP and an incorporated place sharing a name), and
  # each candidate must be tagged with the polygon it actually falls in, not
  # the first name match.
  classfp <- as.character(match_poly$CLASSFP)[idx]

  added <- data.frame(
    ori = cand$ori,
    agency_name = cand$agency_name,
    agency_type_name = cand$agency_type_name,
    agency_class = classify_place_agency(cand$agency_type_name),
    place_name = x$place_name[1],
    county_name = cand$county_name,
    state_abbr = cand$state_abbr,
    attribution = "point_in_polygon",
    latitude = cand$latitude,
    longitude = cand$longitude,
    place_type = ifelse(substr(classfp, 1L, 1L) == "U", "cdp", "incorporated"),
    place_fips = as.character(match_poly$GEOID)[idx],
    stringsAsFactors = FALSE
  )

  out <- rbind(x[, names(added), drop = FALSE], added)
  rownames(out) <- NULL
  out
}

# Embedded-tier agencies in a state, with usable coordinates.
#
# Agencies without coordinates (NA in the bundled table) cannot be placed. This
# costs real coverage: 271 of the 2,324 embedded-tier agencies (11.7%) have
# none and can never be spatially attributed.
.embedded_candidates <- function(state) {
  ag <- agencies_table()
  keep <- ag$agency_type_name %in% .EMBEDDED_TYPES &
    toupper(trimws(ag$state_abbr)) == toupper(trimws(state)) &
    !is.na(ag$latitude) & !is.na(ag$longitude)
  keep[is.na(keep)] <- FALSE
  ag[keep, , drop = FALSE]
}

# For each (lon, lat), the row index of `polys` it falls inside (or NA if
# none). Resolved per point rather than against a union of `polys`, so a point
# is attributed to the specific polygon it falls in when a name match returns
# more than one (e.g. a CDP and an incorporated place sharing a name).
#
# When a caller supplies a custom `places_fun`, the `sf` availability guard in
# add_place_spatial_members() is skipped (it only gates the default,
# tigris-backed branch) — the caller is responsible for `sf` being available
# in that case.
.locate_in_polygons <- function(lon, lat, polys) {
  pts <- sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = 4326
  )
  polys <- sf::st_transform(polys, 4326)
  hits <- sf::st_within(pts, sf::st_geometry(polys))
  vapply(hits, function(h) if (length(h) > 0) h[1] else NA_integer_, integer(1))
}
