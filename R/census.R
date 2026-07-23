# Optional Census population join (v0.3)
#
# Uses `censusapi` (Suggests) to fetch ACS population estimates by county FIPS.
# Returns a detail frame with an added `census_population` column, ready for
# `get_county_crime(detail, denominator = "census_pop")`.

#' Join Census ACS population estimates to county crime detail
#'
#' Fetches the latest 1-year American Community Survey (ACS) population estimate
#' for each county represented in `detail`, keyed by the `county_fips` column.
#' Returns the detail frame with an added `census_population` column (integer).
#' Counties without a FIPS match or Census data get `NA`.
#'
#' This is an optional feature requiring the \code{censusapi} package (in
#' \sQuote{Suggests}) and a free Census API key (set via the \code{CENSUS_KEY}
#' environment variable or the \code{key} argument). Without \code{censusapi},
#' the function returns `detail` unchanged with a message.
#'
#' @param detail A data.frame as returned by [get_county_crime_detail()], with at
#'   least the columns: `county_name`, `state_abbr`, and `county_fips`.
#' @param key Census API key. Defaults to the \code{CENSUS_KEY} environment
#'   variable. Obtain a free key from
#'   \url{https://api.census.gov/data/key_signup.html}.
#' @param year ACS vintage year (default: current year minus 1, e.g. 2024 data
#'   released in 2025). Must be a year for which ACS 1-year estimates are
#'   available.
#' @return The input `detail` data.frame with an additional `census_population`
#'   column (integer, per county). If \code{censusapi} is not available, returns
#'   `detail` unchanged with a message.
#' @export
#' @examples
#' \dontrun{
#' detail <- get_county_crime_detail("Alameda", "CA",
#'                                   from = "01-2019", to = "12-2019",
#'                                   default_only = TRUE)
#' detail_with_pop <- join_census_pop(detail)
#' agg <- get_county_crime(detail_with_pop, denominator = "census_pop")
#' }
join_census_pop <- function(detail, key = Sys.getenv("CENSUS_KEY"),
                            year = as.integer(format(Sys.Date(), "%Y")) - 1L) {
  if (!inherits(detail, "data.frame")) {
    stop("'detail' must be a data.frame", call. = FALSE)
  }

  required_cols <- c("county_name", "state_abbr", "county_fips")
  missing_cols <- setdiff(required_cols, names(detail))
  if (length(missing_cols) > 0) {
    stop("'detail' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Check for censusapi (Suggests).
  if (!requireNamespace("censusapi", quietly = TRUE)) {
    message("Package 'censusapi' is required for join_census_pop().\n",
            "Install it with: install.packages('censusapi')")
    detail$census_population <- NA_integer_
    return(detail)
  }

  if (!nzchar(key)) {
    stop("Census API key not found. Set the CENSUS_KEY environment variable\n",
         "or supply the 'key' argument. Obtain a free key at:\n",
         "  https://api.census.gov/data/key_signup.html", call. = FALSE)
  }

  # Get unique counties from detail.
  counties <- unique(detail[, c("county_fips", "state_abbr", "county_name"),
                            drop = FALSE])
  counties <- counties[!is.na(counties$county_fips), , drop = FALSE]

  if (nrow(counties) == 0L) {
    message("No resolvable county FIPS codes in detail; adding NA column.")
    detail$census_population <- NA_integer_
    return(detail)
  }

  # ACS B01003: Total population (1-year estimate).
  # The censusapi package uses get_acs() with survey = "acs1".
  pop_lookup <- tryCatch(
    censusapi::get_acs(
      table = "B01003",
      year = year,
      survey = "acs1",
      geometry = FALSE,
      output = "wide",
      output.class = "data.frame",
      variables = "estimate",
      key = key,
      state = unique(counties$state_abbr),
      county = unique(counties$county_fips)
    ),
    error = function(e) {
      warning("Census API request failed: ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )

  if (is.null(pop_lookup)) {
    detail$census_population <- NA_integer_
    return(detail)
  }

  # censusapi returns a data.frame with 'GEO.id' (5-digit FIPS) and
  # 'B01003!001E' (population estimate). Extract and match.
  est_col <- grep("B01003.*E$", names(pop_lookup), value = TRUE)
  if (length(est_col) == 0L) {
    # Fallback: look for any column ending in 'E' (estimate).
    est_col <- grep("E$", names(pop_lookup), value = TRUE)
  }

  if (length(est_col) == 0L || !"GEO.id" %in% names(pop_lookup)) {
    warning("Unexpected Census API response shape; cannot extract population.",
            call. = FALSE)
    detail$census_population <- NA_integer_
    return(detail)
  }

  pop_df <- data.frame(
    county_fips = as.character(pop_lookup$GEO.id),
    census_population = as.integer(pop_lookup[[est_col[1]]]),
    stringsAsFactors = FALSE
  )

  # Merge back to detail (left join by county_fips).
  detail$census_population <- pop_df$census_population[
    match(detail$county_fips, pop_df$county_fips)
  ]

  detail
}
