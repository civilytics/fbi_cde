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
#' Attributes campus, transit, airport, and other special-district agencies to a
#' place by point-in-polygon of their headquarters coordinates against Census
#' place boundaries. Appends them to a [place_agencies()] result.
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
#'   Appended rows carry `attribution = "point_in_polygon"` and
#'   `default_member = FALSE`.
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
  required <- c("ori", "place_name", "state_abbr", "county_name", "attribution")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("'x' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (is.null(places_fun)) {
    if (!.spatial_deps_available()) {
      message("Packages 'sf' and 'tigris' are required for ",
              "add_place_spatial_members().\n",
              "Install them with: install.packages(c('sf', 'tigris'))")
      return(x)
    }
    places_fun <- .default_places_fun
  }

  x$place_type <- NA_character_
  x$place_fips <- NA_character_

  if (nrow(x) == 0) {
    return(x)
  }

  state <- x$state_abbr[1]
  target <- toupper(x$place_name[1])

  polys <- places_fun(state, vintage)
  match_poly <- polys[toupper(polys$NAME) == target, , drop = FALSE]
  if (nrow(match_poly) == 0) {
    warning("No Census place polygon named '", x$place_name[1], "' in ", state,
            "; returning input unchanged.", call. = FALSE)
    return(x)
  }

  cand <- .embedded_candidates(state)
  if (nrow(cand) == 0) {
    return(x)
  }

  inside <- .points_in_polygon(cand$longitude, cand$latitude, match_poly)
  cand <- cand[inside, , drop = FALSE]
  if (nrow(cand) == 0) {
    return(x)
  }

  # CLASSFP "U*" denotes a Census Designated Place (unincorporated).
  is_cdp <- substr(as.character(match_poly$CLASSFP[1]), 1L, 1L) == "U"

  added <- data.frame(
    ori = cand$ori,
    agency_name = cand$agency_name,
    agency_type_name = cand$agency_type_name,
    agency_class = classify_place_agency(cand$agency_type_name),
    default_member = FALSE,
    place_name = x$place_name[1],
    county_name = cand$county_name,
    state_abbr = cand$state_abbr,
    attribution = "point_in_polygon",
    latitude = cand$latitude,
    longitude = cand$longitude,
    place_type = if (is_cdp) "cdp" else "incorporated",
    place_fips = as.character(match_poly$GEOID[1]),
    stringsAsFactors = FALSE
  )

  out <- rbind(x[, names(added), drop = FALSE], added)
  rownames(out) <- NULL
  out
}

# Embedded-tier agencies in a state, with usable coordinates.
#
# The bundled table stores latitude/longitude as CHARACTER, and 545 rows hold
# the literal string "NULL" rather than a real missing value — so is.na() alone
# does not catch them and sf::st_as_sf() would error on the coercion. Coerce
# explicitly and drop whatever fails to parse. This costs real coverage: 270 of
# the 2,324 embedded-tier agencies (11.6%) have no usable coordinates and can
# never be spatially attributed.
.embedded_candidates <- function(state) {
  ag <- agencies_table()
  lat <- suppressWarnings(as.numeric(as.character(ag$latitude)))
  lon <- suppressWarnings(as.numeric(as.character(ag$longitude)))

  keep <- ag$agency_type_name %in% .EMBEDDED_TYPES &
    toupper(trimws(ag$state_abbr)) == toupper(trimws(state)) &
    !is.na(lat) & !is.na(lon)
  keep[is.na(keep)] <- FALSE

  out <- ag[keep, , drop = FALSE]
  out$latitude <- lat[keep]
  out$longitude <- lon[keep]
  out
}

# TRUE for each (lon, lat) falling inside any polygon of `polys`.
.points_in_polygon <- function(lon, lat, polys) {
  pts <- sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = 4326
  )
  polys <- sf::st_transform(polys, 4326)
  hits <- sf::st_within(pts, sf::st_union(sf::st_geometry(polys)))
  lengths(hits) > 0
}
