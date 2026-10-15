# Total count and mean monthly coverage of a parsed series' offense rows. A
# geography with no rows (nothing reported) totals 0 with coverage NA.
.series_total <- function(x) {
  off <- x[x$measure %in% "offenses", , drop = FALSE]
  if (nrow(off) == 0) {
    return(list(count = 0, coverage = NA_real_))
  }
  coverage <- mean(off$participated_population / off$population,
                   na.rm = TRUE)
  list(count = sum(off$count, na.rm = TRUE),
       coverage = if (is.nan(coverage)) NA_real_ else coverage)
}

# One NIBRS offense code's monthly counts for a year, NIBRS submissions only:
# nibrs/{level}/{code}?type=counts answers in the summarized/ shape.
.nibrs_counts <- function(state_abb, code, year) {
  national <- is.null(state_abb)
  level <- if (national) "national" else paste0("state/", state_abb)
  response <- cde_request(
    cde_path("nibrs", level, code),
    cde_query(sprintf("01-%d", year), sprintf("12-%d", year),
              type = "counts")
  )
  parse_summarized_response(response,
                            geography = if (national) "US" else state_abb,
                            offense = code,
                            level = if (national) "national" else "state")
}

# A ratio of a gap to its published figure; NA where that is not positive.
.gap_share <- function(gap, published) {
  if (is.na(published) || published <= 0) NA_real_ else gap / published
}

.imputation_gap_row <- function(state_abb, year, offense) {
  # The estimate first: an unsupported year fails before the other requests.
  est <- get_nibrs_estimate(state_abb, year = year, offense = offense)
  total <- est[est$variable == "Offense Count - Total", , drop = FALSE]
  if (nrow(total) != 1L) {
    stop("The FBI's estimate for ", offense, " in ", year,
         " has no offense total.", call. = FALSE)
  }

  reported <- .series_total(get_reported_crime(
    state_abb, from = sprintf("01-%d", year), to = sprintf("12-%d", year),
    offense = offense
  ))
  nibrs <- lapply(.ESTIMATE_OFFENSES[[offense]]$nibrs, function(code) {
    .series_total(.nibrs_counts(state_abb, code, year))
  })
  reported_nibrs <- sum(vapply(nibrs, function(n) n$count, numeric(1)))

  published <- total$estimate
  lower <- total$lower_bound
  upper <- total$upper_bound
  data.frame(
    geography = if (is.null(state_abb)) "US" else state_abb,
    year = year,
    offense = offense,
    reported = reported$count,
    reported_nibrs = reported_nibrs,
    reported_other = reported$count - reported_nibrs,
    coverage = reported$coverage,
    coverage_nibrs = nibrs[[1]]$coverage,
    published = published,
    published_lower = lower,
    published_upper = upper,
    gap = published - reported_nibrs,
    gap_lower = lower - reported_nibrs,
    gap_upper = upper - reported_nibrs,
    share_estimated = .gap_share(published - reported_nibrs, published),
    share_lower = .gap_share(lower - reported_nibrs, lower),
    share_upper = .gap_share(upper - reported_nibrs, upper),
    stringsAsFactors = FALSE
  )
}

#' How much of the FBI's published crime figure was estimated
#'
#' Compares the FBI's *estimate* for a state or the nation with the crime
#' agencies *reported* to the FBI, and says how much of the estimate the
#' FBI's model supplied. The FBI builds its estimate from NIBRS reports only
#' and models everything else: agencies that did not report and, in 2022,
#' agencies that reported only in the older summary format. So the gap is
#' measured against `reported_nibrs`, and `reported_other` shows the
#' summary-format reports the model predicted rather than used.
#'
#' The API publishes estimates for 2021 and 2022 only (see
#' [nibrs_estimation_years]), and for states and the nation only. A year it
#' does not cover fails with the API's message listing the years it has.
#'
#' The NIBRS and summary-format counts differ slightly even where every
#' agency reported in NIBRS, because the two count offenses differently
#' (the summary format counts only the most serious offense in an incident).
#' Measured nationally for 2021, that difference is about 1% for violent
#' crime and larger for some offenses (about 5% for burglary, 7% for
#' homicide); `vignette("estimates")` gives the full table. A `gap` or
#' `reported_other` smaller than that difference, including a negative one,
#' means nothing was modeled.
#'
#' Each row takes one estimate request, one `summarized` request and one
#' `nibrs` request per NIBRS offense code that makes up `offense` (six for
#' violent crime).
#'
#' @param state_abb String for state abbreviation. If `NULL` (default)
#'   compares national figures.
#' @param year One or more years, e.g. `2021:2022`.
#' @param offense A summary offense code: `"V"` (violent crime, the default),
#'   `"P"`, `"ROB"`, `"ASS"`, `"BUR"`, `"LAR"`, `"MVT"`, `"ARS"`, `"HOM"` or
#'   `"RPE"`.
#'
#' @return A data.frame with one row per year:
#' \describe{
#'   \item{geography, year, offense}{What was compared}
#'   \item{reported}{Crime reported to the FBI in either format: the year's
#'     sum from [get_reported_crime()]}
#'   \item{reported_nibrs}{Crime reported in NIBRS only}
#'   \item{reported_other}{`reported - reported_nibrs`: summary-format
#'     reports, plus the counting difference. Can be negative}
#'   \item{coverage}{Mean monthly share of the population covered by
#'     agencies reporting in either format}
#'   \item{coverage_nibrs}{The same, for agencies reporting in NIBRS}
#'   \item{published, published_lower, published_upper}{The FBI's estimate
#'     and the bounds of its confidence interval}
#'   \item{gap, gap_lower, gap_upper}{`published - reported_nibrs`: what
#'     the FBI's model supplied, with bounds from the estimate's interval}
#'   \item{share_estimated, share_lower, share_upper}{`gap / published`: the
#'     share of the published figure the model supplied, with bounds}
#' }
#' @family NIBRS functions
#' @export
#'
#' @examples
#' \dontrun{
#' # 2021, when the FBI accepted NIBRS reports only
#' imputation_gap("CA", year = 2021:2022)
#' imputation_gap(year = 2021:2022)
#' }
imputation_gap <- function(state_abb = NULL, year, offense = "V") {
  if (!is.character(offense) || length(offense) != 1L || is.na(offense) ||
      !toupper(offense) %in% names(.ESTIMATE_OFFENSES)) {
    stop("'offense' must be one of: ",
         paste(names(.ESTIMATE_OFFENSES), collapse = ", "), ".",
         call. = FALSE)
  }
  offense <- toupper(offense)
  if (!is.null(state_abb)) {
    .estimate_level(state_abb)  # validates, and checks an estimate exists
    state_abb <- toupper(state_abb)
  }
  if (!is.numeric(year) || length(year) == 0L || anyNA(year) ||
      any(year != round(year))) {
    stop("'year' must be one or more years, e.g. 2021:2022.", call. = FALSE)
  }

  rows <- lapply(unique(as.integer(year)), function(y) {
    .imputation_gap_row(state_abb, y, offense)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
