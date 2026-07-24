# Layer 1 of the place geography model: itemized, unsummed place-crime detail.
# Reuses the county fan-out machinery (parse_agency_detail, comparison-row
# stripping, partial-failure handling) unchanged.

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
#' @param progress If `TRUE`, print a simple progress line per agency.
#' @return A data.frame with the columns listed in Details. Agencies whose
#'   request or parse fails are dropped with a warning and recorded in
#'   `attr(x, "dropped")`.
#' @seealso [place_agencies()], [add_place_spatial_members()],
#'   [impute_reporting_gaps()] for filling reporting gaps in the result.
#' @export
#' @examples
#' \dontrun{
#' get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
#' }
get_place_crime_detail <- function(place, state, county = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL, default_only = TRUE,
                                   progress = FALSE) {
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- place_agencies(place, state, county = county)
  if (!is.null(agency_class)) {
    agencies <- agencies[agencies$agency_class %in% agency_class, , drop = FALSE]
  } else if (isTRUE(default_only)) {
    agencies <- agencies[agencies$default_member, , drop = FALSE]
  }

  if (nrow(agencies) == 0) {
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
