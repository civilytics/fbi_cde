# Layer 1 of the place geography model: itemized, unsummed place-crime detail.
# Reuses `parse_agency_detail()` (comparison-row stripping) as-is and mirrors
# the *structure* of the county fan-out (R/county_crime.R): the loop,
# per-agency column projection, empty-frame handling, and drop-record
# bookkeeping are duplicated here rather than shared, because the place and
# county column contracts differ (place carries `place_name`/`attribution`;
# county does not). Keep the two loops in sync by hand if either changes.

# Columns a caller-supplied `agencies` frame (or a place_agencies() result)
# must carry for the fan-out loop below to work.
.PLACE_CRIME_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "place_name", "county_name", "state_abbr", "attribution"
)

.PLACE_DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "place_name", "county_name", "state_abbr", "attribution",
  "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

# Built column-by-column rather than from a 0-row matrix() so the columns carry
# their real types. A matrix-derived empty frame types every column `logical`,
# which breaks rbind() against a populated result — the natural way to stack
# several places' detail together.
.empty_place_detail_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    default_member = logical(0),
    place_name = character(0),
    county_name = character(0),
    state_abbr = character(0),
    attribution = character(0),
    offense = character(0),
    period = character(0),
    count = numeric(0),
    population = numeric(0),
    participated_population = numeric(0),
    rate = numeric(0),
    reported = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Itemized crime detail for every agency attributed to a place
#'
#' Fans out one request per member agency and returns their crime series
#' **unsummed** — one row per agency-period — carrying `agency_class`, coverage
#' (`population`, `participated_population`, `reported`), a per-agency `rate`,
#' and the `attribution` that earned each agency its membership.
#'
#' In the common case a place has exactly one member agency, so this is a
#' by-name wrapper over that agency's series. That is the value: ORI discovery
#' is the package's primary friction, and this removes it.
#'
#' No place-level aggregate is provided. Summing an opted-in campus agency into
#' a city total is a modeling choice, not arithmetic; see the county Layer 2
#' (`get_county_crime()`) for the shape that decision takes.
#'
#' @inheritParams place_agencies
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector; keep only these place classes
#'   (`"place_primary"`, `"campus"`, `"special"`).
#' @param default_only If `TRUE` (the default), keep only default members
#'   (`place_primary`). Ignored if `agency_class` is supplied.
#' @param agencies Optional pre-resolved agency membership data.frame, e.g. the
#'   output of [place_agencies()] with [add_place_spatial_members()] applied.
#'   When supplied, it is used directly instead of calling `place_agencies()`
#'   internally — this is the only way to reach `agency_class = "campus"` or
#'   `"special"` rows, since [place_agencies()] alone never returns them. Must
#'   be a data.frame carrying at least `ori`, `agency_name`,
#'   `agency_type_name`, `agency_class`, `default_member`, `place_name`,
#'   `county_name`, `state_abbr`, and `attribution`.
#'
#'   Column presence is validated, but membership is **not**: a hand-built frame
#'   can contain any ORI, including a sheriff or state police agency. The
#'   supported path ([place_agencies()], optionally through
#'   [add_place_spatial_members()]) can never produce one — county, parish,
#'   state-police, and tribal agencies are excluded structurally. If you build
#'   the frame yourself, that exclusion becomes yours to maintain: attributing a
#'   sheriff to a place double-counts against the place's own agency, because a
#'   sheriff polices the unincorporated remainder and contract cities report
#'   under their own city ORI.
#' @param progress If `TRUE`, print a simple progress line per agency.
#' @return A data.frame with one row per agency-period, carrying: `ori`,
#'   `agency_name`, `agency_type_name`, `agency_class`, `default_member`,
#'   `place_name`, `county_name`, `state_abbr`, `attribution`, `offense`,
#'   `period`, `count`, `population`, `participated_population`, `rate`, and
#'   `reported`. Agencies whose request or parse fails are dropped with a
#'   warning and recorded in `attr(x, "dropped")`. If filtering (via
#'   `agency_class`/`default_only`) empties a non-empty agency set, a warning
#'   names the place and the filter responsible, and a zero-row frame is
#'   returned.
#' @seealso [place_agencies()], [add_place_spatial_members()],
#'   [impute_reporting_gaps()] for filling reporting gaps in the result.
#' @export
#' @examples
#' \dontrun{
#' get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
#'
#' # Compose with add_place_spatial_members() to reach campus/special agencies:
#' x <- add_place_spatial_members(place_agencies("Berkeley", "CA"))
#' get_place_crime_detail("Berkeley", "CA", agencies = x, agency_class = "campus")
#' }
get_place_crime_detail <- function(place, state, county = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL, default_only = TRUE,
                                   agencies = NULL, progress = FALSE) {
  cde_validate_dates(from, to, "mm-yyyy")

  if (!is.null(agencies)) {
    if (!inherits(agencies, "data.frame")) {
      stop("'agencies' must be a data.frame", call. = FALSE)
    }
    missing_cols <- setdiff(.PLACE_CRIME_AGENCY_COLS, names(agencies))
    if (length(missing_cols) > 0) {
      stop("'agencies' is missing required columns: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  } else {
    agencies <- place_agencies(place, state, county = county)
  }

  pre_filter_n <- nrow(agencies)
  if (!is.null(agency_class)) {
    agencies <- agencies[agencies$agency_class %in% agency_class, , drop = FALSE]
  } else if (isTRUE(default_only)) {
    agencies <- agencies[agencies$default_member, , drop = FALSE]
  }

  if (nrow(agencies) == 0) {
    if (pre_filter_n > 0) {
      filter_desc <- if (!is.null(agency_class)) {
        paste0("agency_class = ", paste(agency_class, collapse = ", "))
      } else {
        "default_only = TRUE"
      }
      warning("No agencies to query for place '", place, "', ", state,
              " after filtering (", filter_desc, ")", call. = FALSE)
    }
    return(.empty_place_detail_frame())
  }

  dropped <- character(0)
  parts <- vector("list", nrow(agencies))
  for (i in seq_len(nrow(agencies))) {
    ori <- agencies$ori[i]
    if (isTRUE(progress)) {
      message(sprintf("[%d/%d] %s", i, nrow(agencies), ori))
    }
    path <- cde_path("summarized", paste0("agency/", ori), offense)
    query <- list(from = from, to = to, type = "counts")

    res <- tryCatch(
      parse_agency_detail(cde_request(path, query), ori, offense, from, to),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      dropped <- c(dropped, ori)
      next
    }
    res$agency_name <- agencies$agency_name[i]
    res$agency_type_name <- agencies$agency_type_name[i]
    res$agency_class <- agencies$agency_class[i]
    res$default_member <- agencies$default_member[i]
    res$place_name <- agencies$place_name[i]
    res$county_name <- agencies$county_name[i]
    res$state_abbr <- agencies$state_abbr[i]
    res$attribution <- agencies$attribution[i]
    parts[[i]] <- res
  }

  out <- rbind_fill(parts)
  out <- out[, intersect(.PLACE_DETAIL_COLS, names(out)), drop = FALSE]
  rownames(out) <- NULL

  if (nrow(out) == 0 || ncol(out) == 0) {
    out <- .empty_place_detail_frame()
  }

  if (length(dropped) > 0) {
    warning("Dropped ", length(dropped),
            " agenc", if (length(dropped) == 1) "y" else "ies",
            " that returned no data: ", paste(dropped, collapse = ", "),
            call. = FALSE)
    attr(out, "dropped") <- dropped
  }
  out
}
