# Layer 1 of the geography model: itemized, unsummed county-crime detail.

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
