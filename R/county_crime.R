# Layer 1 of the geography model: itemized, unsummed county-crime detail.
#
# `parse_agency_detail()` below is shared by all three geographic levels, as is
# the fan-out loop in R/fanout.R; this file supplies the county column contract
# and metadata columns.

# Every "MM-YYYY" month from `from` to `to`, inclusive, in chronological order.
# Used to build the reporting grid so a missing month reads as "did not report"
# rather than "reported zero".
enumerate_periods <- function(from, to) {
  f <- as.integer(strsplit(from, "-", fixed = TRUE)[[1]])
  t <- as.integer(strsplit(to, "-", fixed = TRUE)[[1]])
  start <- f[2] * 12L + (f[1] - 1L)
  end <- t[2] * 12L + (t[1] - 1L)
  idx <- seq.int(start, end)
  sprintf("%02d-%d", idx %% 12L + 1L, idx %/% 12L)
}

# Pick the agency's own "<name> Offenses" series from the actuals, defensively
# excluding national/state comparison series that may appear under schema drift.
.agency_offense_key <- function(actual_names) {
  cand <- grep(" Offenses$", actual_names, value = TRUE)
  comparison <- c("United States Offenses",
                  paste(datasets::state.name, "Offenses"))
  cand <- setdiff(cand, comparison)
  if (length(cand) == 0) NA_character_ else cand[1]
}

# Parse one agency's `summarized/agency/{ori}/{offense}` response (nested
# named lists, as `cde_request()` returns with `simplifyVector = FALSE`) into
# a per-period data.frame. A period missing from the agency's own actuals
# series gets `count = NA`, `reported = FALSE`, and `rate = NA`. Comparison
# (state/national) series are never read.
parse_agency_detail <- function(response, ori, offense, from, to) {
  periods <- enumerate_periods(from, to)

  actuals <- response$offenses$actuals %||% response$offenses$counts
  offense_key <- if (is.null(actuals)) NA_character_ else
    .agency_offense_key(names(actuals))
  count_series <- if (is.na(offense_key)) NULL else actuals[[offense_key]]
  label <- if (is.na(offense_key)) NA_character_ else
    sub(" Offenses$", "", offense_key)

  pop_series <- if (is.na(label)) NULL else
    response$populations$population[[label]]
  part_series <- if (is.na(label)) NULL else
    response$populations$participated_population[[label]]

  num <- function(series, period) {
    v <- if (is.null(series)) NULL else series[[period]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }

  count <- vapply(periods, function(p) num(count_series, p), numeric(1))
  population <- vapply(periods, function(p) num(pop_series, p), numeric(1))
  participated <- vapply(periods, function(p) num(part_series, p), numeric(1))
  reported <- !is.na(count)
  rate <- ifelse(!is.na(count) & !is.na(participated) & participated > 0,
                 count / participated * 1e5, NA_real_)

  data.frame(
    ori = ori,
    offense = offense,
    period = periods,
    count = count,
    population = population,
    participated_population = participated,
    rate = rate,
    reported = reported,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

.DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

# A 0-row, .DETAIL_COLS-shaped data.frame, used whenever there is no agency
# data to return (no agencies matched, or every agency's request failed).
#
# Built column-by-column rather than from `matrix(nrow = 0, ...)`, whose default
# mode is `logical`: that types every column `logical(0)`, so `rbind()`ing an
# empty result onto a populated one — the natural way to stack several counties
# — errors or silently coerces. Types here must match a populated result.
.empty_detail_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    default_member = logical(0),
    county_name = character(0),
    state_abbr = character(0),
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

#' Itemized crime detail for every agency attributed to a county
#'
#' Fans out one request per member agency and returns their crime series
#' **unsummed** — one row per agency-period — with each agency's `agency_class`,
#' coverage (`population`, `participated_population`, `reported`), and a
#' per-agency `rate` (`count / participated_population * 1e5`). This is the
#' transparent, power-user primitive; it applies no aggregation and takes no
#' stance on denominators. Filter with `agency_class`/`default_only`; the default
#' returns *every* attributed agency, typed.
#'
#' @param county County name (case-insensitive).
#' @param state Two-letter state abbreviation.
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector; keep only these classes
#'   (`"county_primary"`, `"municipal"`, `"campus"`, `"state"`, `"tribal"`,
#'   `"special"`).
#' @param default_only If `TRUE`, keep only default members (`county_primary` +
#'   `municipal`). Ignored if `agency_class` is supplied.
#' @param progress If `TRUE`, print a simple progress line per agency.
#' @return A data.frame (columns listed in Details). Agencies whose request or
#'   parse fails are dropped with a warning and recorded in `attr(x, "dropped")`.
#'   Agencies classed `state` (e.g. Highway Patrol) or `tribal` are attributed
#'   to a county by HQ location, not jurisdiction; their figures reflect
#'   statewide/jurisdiction-wide totals, not county-specific crime. They are
#'   excluded by default (`default_member` is `FALSE` for both classes) and
#'   should be interpreted with care if opted in via `agency_class`.
#' @export
#' @examples
#' \dontrun{
#' get_county_crime_detail("Alameda", "CA", from = "01-2019", to = "12-2019")
#' }
get_county_crime_detail <- function(county, state, offense = "V",
                                    from = "01-2015", to = "12-2020",
                                    agency_class = NULL, default_only = FALSE,
                                    progress = FALSE) {
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- county_agencies(county, state)
  agencies <- .filter_agency_members(agencies, agency_class, default_only)

  if (nrow(agencies) == 0) {
    warning("No agencies to query for '", county, "', ", state,
            " after filtering", call. = FALSE)
    return(.empty_detail_frame())
  }

  .fanout_agency_detail(
    agencies = agencies,
    meta_cols = c("agency_name", "agency_type_name", "agency_class",
                  "default_member", "county_name", "state_abbr"),
    cols = .DETAIL_COLS,
    empty_fn = .empty_detail_frame,
    offense = offense, from = from, to = to, progress = progress
  )
}

#' Crime reported by a county's own primary agency (sheriff/parish)
#'
#' Resolves the single `county_primary` ORI for a county and returns its own
#' `get_agency_crime()` series. This disambiguates "the county sheriff's own
#' reported crime" from "crime aggregated across the county"
#' (`get_county_crime_detail()`).
#'
#' @inheritParams get_county_crime_detail
#' @return The `get_agency_crime()` data.frame for the county's primary agency.
#' @export
#' @examples
#' \dontrun{
#' get_county_agency_crime("Alameda", "CA")
#' }
get_county_agency_crime <- function(county, state, offense = "V",
                                    from = "01-2015", to = "12-2020") {
  agencies <- county_agencies(county, state)
  prim <- agencies[agencies$agency_class == "county_primary", , drop = FALSE]

  if (nrow(prim) == 0) {
    stop("No county-primary (sheriff/parish) agency found for '", county,
         "', ", state, call. = FALSE)
  }
  if (nrow(prim) > 1) {
    warning("Multiple county-primary agencies for '", county, "', ", state,
            "; using ", prim$ori[1], call. = FALSE)
  }
  get_agency_crime(prim$ori[1], from = from, to = to, offense = offense)
}

# ---- Layer 2: county aggregate (v0.3) ------------------------------------

.AGGREGATE_COLS <- c(
  "county_name", "state_abbr", "offense", "period",
  "count", "population", "participated_population",
  "rate", "denominator_type", "coverage_fraction"
)

# A 0-row, .AGGREGATE_COLS-shaped frame whose column types match a populated
# result. See the note on .empty_detail_frame() for why this is not built from
# `matrix(nrow = 0, ...)`.
.empty_aggregate_frame <- function() {
  data.frame(
    county_name = character(0),
    state_abbr = character(0),
    offense = character(0),
    period = character(0),
    count = numeric(0),
    population = numeric(0),
    participated_population = numeric(0),
    rate = numeric(0),
    denominator_type = character(0),
    coverage_fraction = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Aggregate county crime detail into a coverage-transparent rollup
#'
#' Sums itemized agency-level detail (from [get_county_crime_detail()]) into a
#' single county-wide series. Unlike the FBI's published county rate, this
#' function **never emits a bare rate**: every row carries the chosen denominator
#' value, which denominator was used (`denominator_type`), and a
#' `coverage_fraction` (participated / total population) so the user can see
#' exactly how much of the county is covered by reporting agencies.
#'
#' The default denominator (`"jurisdiction_pop"`) sums each agency's own
#' `population` column. Empirical probes confirm this is coherent: a sheriff's
#' population is the *unincorporated remainder* it polices, and contract cities
#' report under their own ORI — so the sum of `jurisdiction_pop` approximates
#' the full county with no double-count. Use `denominator = "participated_pop"` for a coverage-
#' consistent rate (drops uncovered months), or `denominator = "census_pop"`
#' to supply an external Census population (requires the `county_fips` column
#' from [county_agencies()] and a Census join).
#'
#' @param detail A data.frame as returned by [get_county_crime_detail()], with at
#'   least the columns: `offense`, `period`, `count`, `population`,
#'   `participated_population`, `reported`.
#' @param denominator Denominator strategy. One of:
#'   \itemize{
#'     \item \code{"jurisdiction_pop"} — sum of agency `population` columns
#'       (default; coherent with CDE population semantics).
#'     \item \code{"participated_pop"} — sum of agency
#'       `participated_population` columns (coverage-consistent; the rate
#'       reflects only reporting coverage).
#'     \item \code{"census_pop"} — use a single external population value per
#'       period. Requires `detail` to have a `census_population` column (e.g.
#'       from [join_census_pop()]).
#'   }
#' @return A data.frame with columns: `county_name`, `state_abbr`, `offense`,
#'   `period`, `count` (sum of reported counts), `population` (the denominator
#'   value used), `participated_population` (sum of participated populations),
#'   `rate` (`count / population * 1e5`), `denominator_type` (which strategy was
#'   used), and `coverage_fraction` (`participated_population / population`).
#'   Returns a zero-row frame with correct columns if `detail` is empty.
#' @export
#' @examples
#' \dontrun{
#' detail <- get_county_crime_detail("Alameda", "CA",
#'                                   from = "01-2019", to = "12-2019",
#'                                   default_only = TRUE)
#' get_county_crime(detail)
#'
#' # Coverage-consistent rate instead of the full jurisdiction denominator.
#' get_county_crime(detail, denominator = "participated_pop")
#' }
get_county_crime <- function(detail, denominator = "jurisdiction_pop") {
  if (!inherits(detail, "data.frame")) {
    stop("'detail' must be a data.frame", call. = FALSE)
  }

  required_cols <- c("offense", "period", "count", "population",
                     "participated_population")
  missing_cols <- setdiff(required_cols, names(detail))
  if (length(missing_cols) > 0) {
    stop("'detail' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!denominator %in% c("jurisdiction_pop", "participated_pop",
                          "census_pop")) {
    stop("'denominator' must be one of 'jurisdiction_pop', ",
         "'participated_pop', or 'census_pop'", call. = FALSE)
  }

  if (denominator == "census_pop" &&
      !"census_population" %in% names(detail)) {
    stop("denominator = 'census_pop' requires a 'census_population' column ",
         "in detail (e.g. from join_census_pop())", call. = FALSE)
  }

  # Handle empty input.
  if (nrow(detail) == 0L) {
    return(.empty_aggregate_frame())
  }

  # Group by (county_name, state_abbr, offense, period) and aggregate.
  groups <- split(seq_len(nrow(detail)),
                  interaction(
                    detail$county_name,
                    detail$state_abbr,
                    detail$offense,
                    detail$period,
                    drop = TRUE
                  ),
                  drop = TRUE)

  agg_rows <- lapply(groups, function(idx) {
    sub <- detail[idx[1], , drop = FALSE]  # take first row for metadata
    vals <- detail[idx, , drop = FALSE]

    # Sum counts (NA = did not report; only sum reported values).
    total_count <- sum(vals$count, na.rm = TRUE)

    # Sum participated populations.
    total_participated <- sum(vals$participated_population, na.rm = TRUE)

    # Choose denominator.
    if (denominator == "jurisdiction_pop") {
      denom <- sum(vals$population, na.rm = TRUE)
    } else if (denominator == "participated_pop") {
      denom <- total_participated
    } else {
      # census_pop: take the unique value (all rows in a county share it).
      denom <- vals$census_population[1]
    }

    # Rate and coverage fraction.
    rate <- if (!is.na(denom) && denom > 0)
      total_count / denom * 1e5 else NA_real_
    coverage <- if (!is.na(denom) && denom > 0)
      total_participated / denom else NA_real_

    data.frame(
      county_name = sub$county_name,
      state_abbr = sub$state_abbr,
      offense = sub$offense,
      period = sub$period,
      count = total_count,
      population = denom,
      participated_population = total_participated,
      rate = rate,
      denominator_type = denominator,
      coverage_fraction = coverage,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, agg_rows)
  rownames(out) <- NULL
  out[, .AGGREGATE_COLS, drop = FALSE]
}

