# Optional Census population join (v0.3)
#
# Uses `censusapi` (Suggests) to fetch ACS population estimates by county FIPS.
# Returns a detail frame with an added `census_population` column, ready for
# `get_county_crime(detail, denominator = "census_pop")`.
#
# The Census call goes through a single injectable seam (`census_fun`), mirroring
# the `cde_request(get_fun = )` pattern in R/http.R, so tests run offline without
# censusapi installed.

# ACS total-population variable (table B01003, estimate 001).
.ACS_POP_VAR <- "B01003_001E"

# Isolated so tests can force the unavailable-dependency branch regardless of
# whether censusapi actually happens to be installed (mirrors
# .spatial_deps_available() in R/place_spatial.R).
.census_deps_available <- function() {
  requireNamespace("censusapi", quietly = TRUE)
}

#' Join Census ACS population estimates to county crime detail
#'
#' Fetches an American Community Survey (ACS) total-population estimate for each
#' county represented in `detail`, keyed by the `county_fips` column. Returns the
#' detail frame with an added `census_population` column (integer). Counties
#' without a FIPS match or without Census data get `NA`.
#'
#' This is an optional feature requiring the \code{censusapi} package (in
#' \sQuote{Suggests}) and a free Census API key (set via the \code{CENSUS_KEY}
#' environment variable or the \code{key} argument). Without \code{censusapi},
#' the function returns `detail` with an all-`NA` `census_population` column and
#' a message.
#'
#' The default dataset is the ACS 5-year release (`"acs/acs5"`). The 1-year
#' release (`"acs/acs1"`) only publishes estimates for geographies of 65,000+
#' people, which excludes roughly two-thirds of US counties — so 5-year is the
#' correct default for a county denominator.
#'
#' @param detail A data.frame as returned by [get_county_crime_detail()], with at
#'   least the columns: `county_name`, `state_abbr`, and `county_fips` (the last
#'   supplied by [county_agencies()]).
#' @param key Census API key. Defaults to the \code{CENSUS_KEY} environment
#'   variable. Obtain a free key from
#'   \url{https://api.census.gov/data/key_signup.html}.
#' @param year ACS vintage year. Defaults to two years before the current year,
#'   because the ACS 5-year release for year `Y` does not publish until December
#'   of `Y + 1`.
#' @param dataset Census dataset name passed to `censusapi::getCensus()`.
#'   Defaults to `"acs/acs5"`; use `"acs/acs1"` for the 1-year release.
#' @param census_fun Function used to call the Census API. Defaults to
#'   `censusapi::getCensus()`. Exposed for testing; you should not need to set it.
#' @return The input `detail` data.frame with an additional `census_population`
#'   column (integer, one value per county). If \code{censusapi} is unavailable
#'   or the request fails, the column is all `NA`.
#' @export
#' @examples
#' \dontrun{
#' detail <- get_county_crime_detail("Alameda", "CA",
#'                                   from = "01-2019", to = "12-2019")
#' detail_with_pop <- join_census_pop(detail)
#' agg <- get_county_crime(detail_with_pop, denominator = "census_pop")
#' }
join_census_pop <- function(detail,
                            key = Sys.getenv("CENSUS_KEY"),
                            year = as.integer(format(Sys.Date(), "%Y")) - 2L,
                            dataset = "acs/acs5",
                            census_fun = NULL) {
  if (!inherits(detail, "data.frame")) {
    stop("'detail' must be a data.frame", call. = FALSE)
  }

  required_cols <- c("county_name", "state_abbr", "county_fips")
  missing_cols <- setdiff(required_cols, names(detail))
  if (length(missing_cols) > 0) {
    stop("'detail' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Resolve the API seam. censusapi lives in Suggests, so only touch its
  # namespace when the caller did not inject a replacement.
  if (is.null(census_fun)) {
    if (!.census_deps_available()) {
      message("Package 'censusapi' is required for join_census_pop().\n",
              "Install it with: install.packages('censusapi')")
      detail$census_population <- NA_integer_
      return(detail)
    }
    census_fun <- censusapi::getCensus
  }

  if (!nzchar(key)) {
    stop("Census API key not found. Set the CENSUS_KEY environment variable ",
         "or supply the 'key' argument. Obtain a free key at: ",
         "https://api.census.gov/data/key_signup.html", call. = FALSE)
  }

  fips <- unique(detail$county_fips[!is.na(detail$county_fips)])
  fips <- fips[nzchar(fips)]

  if (length(fips) == 0L) {
    message("No resolvable county FIPS codes in detail; adding NA column.")
    detail$census_population <- NA_integer_
    return(detail)
  }

  pop_df <- .fetch_census_county_pop(fips, key, year, dataset, census_fun)

  detail$census_population <- if (is.null(pop_df)) {
    NA_integer_
  } else {
    pop_df$census_population[match(detail$county_fips, pop_df$county_fips)]
  }

  detail
}

# Fetch county populations for a set of 5-digit FIPS codes.
#
# `getCensus()` takes one `regionin = "state:SS"` per call, so group the
# requested counties by their state prefix and issue one request per state.
# Returns a data.frame(county_fips, census_population), or NULL if nothing
# could be retrieved.
.fetch_census_county_pop <- function(fips, key, year, dataset, census_fun) {
  state_fips <- substr(fips, 1L, 2L)
  county_fips <- substr(fips, 3L, 5L)

  by_state <- split(county_fips, state_fips)

  results <- lapply(names(by_state), function(st) {
    raw <- tryCatch(
      census_fun(
        name = dataset,
        vintage = year,
        vars = c("NAME", .ACS_POP_VAR),
        region = paste0("county:", paste(by_state[[st]], collapse = ",")),
        regionin = paste0("state:", st),
        key = key
      ),
      error = function(e) {
        warning("Census API request failed for state ", st, ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    .parse_census_pop(raw)
  })

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0L) {
    return(NULL)
  }

  do.call(rbind, results)
}

# Normalize one getCensus() response into data.frame(county_fips,
# census_population). getCensus() returns `state`, `county`, `NAME` and the
# requested variable; geography columns can come back unpadded, so re-pad them
# before assembling the 5-digit key. Returns NULL on an unusable response.
.parse_census_pop <- function(raw) {
  if (is.null(raw) || !inherits(raw, "data.frame") || nrow(raw) == 0L) {
    return(NULL)
  }

  if (!all(c("state", "county") %in% names(raw))) {
    warning("Unexpected Census API response shape: missing state/county ",
            "columns; cannot build county FIPS.", call. = FALSE)
    return(NULL)
  }

  pop_col <- if (.ACS_POP_VAR %in% names(raw)) {
    .ACS_POP_VAR
  } else {
    # Tolerate an unexpected variable-name shape rather than silently dropping
    # the data: fall back to any other B01003 estimate column.
    hits <- grep("^B01003.*E$", names(raw), value = TRUE)
    if (length(hits) == 0L) NULL else hits[1]
  }

  if (is.null(pop_col)) {
    warning("Unexpected Census API response shape; no B01003 population ",
            "column found.", call. = FALSE)
    return(NULL)
  }

  data.frame(
    county_fips = paste0(.pad_fips(raw$state, 2L), .pad_fips(raw$county, 3L)),
    census_population = as.integer(raw[[pop_col]]),
    stringsAsFactors = FALSE
  )
}

# Zero-pad a FIPS component to `width` digits. formatC() pads character input
# with spaces rather than zeros, so coerce through integer first (via character,
# so a factor yields its label and not its level index).
.pad_fips <- function(x, width) {
  formatC(as.integer(as.character(x)), width = width, flag = "0")
}
