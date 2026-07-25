# Layer 1 of the metro geography model: itemized, unsummed metro-crime detail.
#
# Reuses parse_agency_detail() and mirrors the county fan-out structure. The
# loop is duplicated rather than shared because the column contracts differ;
# see R/place_crime.R for the same trade-off.
#
# Unlike its county and place siblings this one is guarded: a metro can be
# hundreds of agencies (New York is 459, each a sequential request), so an
# unbounded call would hang for minutes with no explanation.

.METRO_DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "county_fips",
  "cbsa_code", "cbsa_title", "cbsa_type", "central_outlying",
  "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

.empty_metro_detail_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    default_member = logical(0),
    county_name = character(0),
    state_abbr = character(0),
    county_fips = character(0),
    cbsa_code = character(0),
    cbsa_title = character(0),
    cbsa_type = character(0),
    central_outlying = character(0),
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

#' Itemized crime detail for every agency in a metropolitan area
#'
#' Fans out one request per member agency across the metro's counties and
#' returns their crime series **unsummed** -- one row per agency-period -- with
#' coverage columns and the CBSA metadata.
#'
#' **This can be an expensive call.** A metro is the union of whole counties, so
#' the largest are very large: New York-Newark-Jersey City resolves to roughly
#' 459 agencies, Chicago 356. Every agency is one sequential request. The median
#' CBSA is only 7 agencies, so the cost is highly skewed -- which is why
#' `max_agencies` refuses the large cases up front rather than letting them run
#' silently for minutes.
#'
#' No metro-level aggregate is provided. Summing across a metro raises the same
#' denominator question the county aggregate deferred, and a metro's is harder
#' (multi-state, mixed coverage).
#'
#' @inheritParams metro_agencies
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector; keep only these classes.
#' @param default_only If `TRUE` (the default), keep only default members
#'   (`county_primary` + `municipal`). Ignored if `agency_class` is supplied.
#' @param max_agencies Refuse to run if the filtered agency set is larger than
#'   this (default `150`), erroring **before any request is issued**. Set to
#'   `Inf` to disable, or narrow the set with `agency_class`.
#' @param progress If `TRUE` (the default here, unlike the county and place
#'   equivalents), print a progress line per agency. At metro scale silence is
#'   indistinguishable from a hang.
#' @return A data.frame with one row per agency-period. Agencies whose request
#'   or parse fails are dropped with a warning and recorded in
#'   `attr(x, "dropped")`.
#' @seealso [metro_agencies()], [list_metros()], [get_county_crime_detail()].
#' @export
#' @examples
#' \dontrun{
#' # A small micropolitan area is cheap.
#' get_metro_crime_detail("Aberdeen, WA", from = "01-2019", to = "12-2019")
#' }
get_metro_crime_detail <- function(metro, state = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL, default_only = TRUE,
                                   max_agencies = 150, progress = TRUE) {
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- metro_agencies(metro, state)
  if (!is.null(agency_class)) {
    agencies <- agencies[agencies$agency_class %in% agency_class, , drop = FALSE]
  } else if (isTRUE(default_only)) {
    agencies <- agencies[agencies$default_member, , drop = FALSE]
  }

  if (nrow(agencies) == 0) {
    return(.empty_metro_detail_frame())
  }

  # Guard BEFORE any request is issued.
  if (nrow(agencies) > max_agencies) {
    stop("Metro '", metro, "' resolves to ", nrow(agencies),
         " agencies, above max_agencies = ", max_agencies,
         ". Each agency is a separate request, so this would take a while. ",
         "Raise the limit (max_agencies = Inf), or narrow the set with ",
         "agency_class.", call. = FALSE)
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
    res$county_name <- agencies$county_name[i]
    res$state_abbr <- agencies$state_abbr[i]
    res$county_fips <- agencies$county_fips[i]
    res$cbsa_code <- agencies$cbsa_code[i]
    res$cbsa_title <- agencies$cbsa_title[i]
    res$cbsa_type <- agencies$cbsa_type[i]
    res$central_outlying <- agencies$central_outlying[i]
    parts[[i]] <- res
  }

  out <- rbind_fill(parts)
  if (is.null(out) || nrow(out) == 0 || ncol(out) == 0) {
    out <- .empty_metro_detail_frame()
  } else {
    out <- out[, .METRO_DETAIL_COLS, drop = FALSE]
    rownames(out) <- NULL
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
