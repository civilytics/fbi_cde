# Optional reporting-gap imputation (v0.2b)
#
# Standalone, composable transform that fills within-agency temporal holes.
# Operates on rate so a drifting denominator is respected: interpolate rate
# across time (in months), then scale by each period's own participated_population.
# Agencies that never report in-window are left entirely unchanged.

#' Impute reporting gaps in county crime detail data
#'
#' Fills temporal holes within agencies that report some periods but miss others.
#' Operates on \code{rate} so a drifting denominator is respected: the rate is
#' linearly interpolated across time (in months), then scaled by each period's
#' own \code{participated_population} to derive the imputed count. Agencies that
#' never report in the window are left unchanged. Every filled value is flagged
#' with \code{imputed = TRUE} and \code{impute_method}.
#'
#' This is an opt-in transform — the raw detail from [get_county_crime_detail()]
#' stays pristine. Call this function explicitly when you need continuous series
#' for trend analysis. Gaps at the edges (before first or after last reported
#' period) are not filled; only gaps between known values are interpolated.
#'
#' @param detail A data.frame as returned by [get_county_crime_detail()], with at
#'   least the columns: \code{ori}, \code{offense}, \code{period}, \code{count},
#'   \code{participated_population}, \code{rate}, \code{reported}.
#' @param method Imputation method. Currently only `"interpolate"` is supported,
#'   which uses linear interpolation of \code{rate} across time (in months), then
#'   scales by each period's own \code{participated_population}. Gaps at the edges
#'   (before first or after last reported period) are not filled.
#' @return A data.frame with the same rows and columns as \code{detail}, plus two
#'   new columns:
#'   \itemize{
#'     \item \code{imputed}: logical, \code{TRUE} only for rows whose values were
#'       filled by imputation; \code{FALSE} for all original (reported or missing)
#'       rows.
#'     \item \code{impute_method}: character, the method name (e.g. `"interpolate"`)
#'       on imputed rows; \code{NA} elsewhere.
#'   }
#' @importFrom stats approx
#' @export
#' @examples
#' \dontrun{
#' detail <- get_county_crime_detail("Alameda", "CA", from = "01-2019", to = "12-2021")
#' filled <- impute_reporting_gaps(detail, method = "interpolate")
#' table(filled$reported, filled$imputed)
#' }
impute_reporting_gaps <- function(detail, method = "interpolate") {
  if (!inherits(detail, "data.frame")) {
    stop("'detail' must be a data.frame", call. = FALSE)
  }

  required_cols <- c("ori", "offense", "period", "count",
                     "participated_population", "rate", "reported")
  missing_cols <- setdiff(required_cols, names(detail))
  if (length(missing_cols) > 0) {
    stop("'detail' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!identical(method, "interpolate")) {
    stop("Unsupported method: '", method,
         "'. Only 'interpolate' is supported.", call. = FALSE)
  }

  # Convert MM-YYYY to numeric YYYYMM for ordering and interpolation.
  period_to_num <- function(p) {
    parts <- strsplit(p, "-", fixed = TRUE)
    as.numeric(vapply(parts, function(x) paste0(x[2], x[1]), character(1)))
  }

  # Handle empty input: add flag columns and return immediately.
  if (nrow(detail) == 0L) {
    detail$imputed <- logical(0)
    detail$impute_method <- character(0)
    return(detail)
  }

  # Initialize new flag columns.
  detail$imputed <- FALSE
  detail$impute_method <- NA_character_

  # Group by (ori, offense) and process each group independently.
  groups <- split(seq_len(nrow(detail)),
                  interaction(detail$ori, detail$offense, drop = TRUE))

  for (grp in groups) {
    sub <- detail[grp, , drop = FALSE]

    # Only process agencies that have at least one reported period.
    if (!any(sub$reported, na.rm = TRUE)) {
      next  # leave entirely unchanged (imputed stays FALSE)
    }

    # Sort by period within the group. `grp_ord` carries the original row
    # indices in sorted order, so anything computed against `sub` writes back
    # to the right rows even when `detail` was not period-sorted to begin with.
    ord <- order(period_to_num(sub$period))
    grp_ord <- grp[ord]
    sub <- sub[ord, , drop = FALSE]

    periods_num <- period_to_num(sub$period)
    reported_mask <- sub$reported

    # Extract rate values for reported periods.
    x_known <- periods_num[reported_mask]
    y_known <- sub$rate[reported_mask]

    # Defensive: drop any NA rates from known points.
    valid_known <- !is.na(y_known)
    x_known <- x_known[valid_known]
    y_known <- y_known[valid_known]

    # Need at least 2 known points to interpolate between.
    if (length(x_known) < 2L) {
      next
    }

    # Linear interpolation of rate across all periods (no extrapolation).
    interp <- approx(x = x_known, y = y_known, xout = periods_num, rule = 1)

    # Identify which gap periods were successfully interpolated.
    gap_mask <- !reported_mask & !is.na(interp$y)

    if (any(gap_mask)) {
      imputed_rate <- interp$y[gap_mask]
      participated_pop <- sub$participated_population[gap_mask]

      # Only fill if we have positive population data for the gap period.
      can_fill <- !is.na(participated_pop) & participated_pop > 0

      if (any(can_fill)) {
        # Derive count from interpolated rate and period's own population.
        imputed_count <- round(
          imputed_rate[can_fill] * participated_pop[can_fill] / 1e5
        )

        # Map back to the original row indices (sorted order, not input order).
        gap_indices <- grp_ord[gap_mask]
        fillable_indices <- gap_indices[can_fill]

        detail$count[fillable_indices] <- imputed_count
        detail$rate[fillable_indices] <- imputed_rate[can_fill]
        detail$imputed[fillable_indices] <- TRUE
        detail$impute_method[fillable_indices] <- method
      }
    }
  }

  detail
}
