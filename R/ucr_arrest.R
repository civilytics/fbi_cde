#' Get arrest offense counts from the UCR Crime Data Explorer
#'
#' Retrieves arrest counts by offense for an agency, state, or nationally.
#' Uses the CDE API endpoint `arrest/<level>/<offense>` with `type=counts`.
#'
#' @family UCR arrest functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense name (default "all" for total arrests). With "all" the
#'   result is a monthly time series. For a specific offense the CDE API only
#'   exposes an aggregate count over the whole `from`-`to` range (the endpoint no
#'   longer accepts a per-offense URL segment, so the offense is selected from
#'   the all-offenses response). See `list_ucr_arrest_offenses()` for valid names.
#'
#' @return A data.frame with columns: geography, offense, period, count, rate.
#'   For a specific offense, `period` and `rate` are `NA` and a single aggregate
#'   `count` row is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_count(ori = "CA0010900")
#' get_arrest_count(state_abb = "CA")
#' get_arrest_count()
#' get_arrest_count(state_abb = "CA", offense = "Robbery")
#' }
get_arrest_count <- function(ori = NULL,
                              state_abb = NULL,
                              from = "01-2015",
                              to = "12-2020",
                              offense = "all") {
  if (!is.null(ori) && !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
  }

  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  cde_validate_dates(from, to, "mm-yyyy")

  if (!is.null(ori)) {
    level <- paste0("agency/", ori)
    geography <- ori
  } else if (!is.null(state_abb)) {
    level <- paste0("state/", toupper(state_abb))
    geography <- toupper(state_abb)
  } else {
    level <- "national"
    geography <- "US"
  }

  # "all" returns the monthly time series directly. A specific offense is no
  # longer addressable in the URL, so fetch the all-offenses totals and pick the
  # requested offense out of the response.
  if (tolower(offense) == "all") {
    path <- cde_path("arrest", level, "all")
    query <- list(from = from, to = to, type = "counts")
    response <- cde_request(path, query)
    return(parse_arrest_counts_response(response, geography))
  }

  valid <- fbiCDE::ucr_arrest_offenses
  if (!tolower(offense) %in% tolower(valid)) {
    stop(
      "Invalid arrest offense: ", offense,
      ". See list_ucr_arrest_offenses() for valid offense names.",
      call. = FALSE
    )
  }

  path <- cde_path("arrest", level, "all")
  query <- list(from = from, to = to, type = "totals")
  response <- cde_request(path, query)
  parse_arrest_offense_total(response, geography, offense)
}

#' Get arrestee demographics from the UCR Crime Data Explorer
#'
#' Retrieves arrest counts broken down by demographic categories (sex, age,
#' race) for a specific offense. Uses the CDE API endpoint
#' `arrest/<level>/<offense>` with `type=totals`.
#'
#' @family UCR arrest functions
#' @inheritParams get_arrest_count
#' @param offense Must be `"all"` (the default). The CDE API only provides
#'   arrest demographics aggregated across all offenses; it does not break
#'   demographics down by offense, so any other value raises an error.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_demographics(ori = "CA0010900")
#' get_arrest_demographics(state_abb = "CA")
#' }
get_arrest_demographics <- function(ori = NULL,
                                     state_abb = NULL,
                                     from = "01-2015",
                                     to = "12-2020",
                                     offense = "all") {
  if (!is.null(ori) && !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
  }

  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  if (tolower(offense) != "all") {
    stop(
      "Only offense = \"all\" is supported for get_arrest_demographics(). ",
      "The CDE API provides arrest demographics aggregated across all ",
      "offenses and does not break them down by offense. Use ",
      "get_arrest_count() for per-offense arrest counts.",
      call. = FALSE
    )
  }

  cde_validate_dates(from, to, "mm-yyyy")

  if (!is.null(ori)) {
    level <- paste0("agency/", ori)
    geography <- ori
  } else if (!is.null(state_abb)) {
    level <- paste0("state/", toupper(state_abb))
    geography <- toupper(state_abb)
  } else {
    level <- "national"
    geography <- "US"
  }

  path <- cde_path("arrest", level, "all")
  query <- list(from = from, to = to, type = "totals")

  response <- cde_request(path, query)
  parse_arrest_demographics_response(response, geography, offense)
}

#' Get arrestee demographics (all offenses)
#'
#' @description
#' **Deprecated.** Per-offense arrest demographics are no
#' longer available from the CDE API, which only provides demographics
#' aggregated across all offenses. This function now returns the same result as
#' `get_arrest_demographics()` and emits a warning. It is retained for backward
#' compatibility and may be removed in a future release.
#'
#' @family UCR arrest functions
#' @param ... Arguments passed to `get_arrest_demographics()` (e.g. `ori`,
#'   `state_abb`, `from`, `to`). Any `offense` argument is ignored.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_demographics_all(state_abb = "CA")
#' }
get_arrest_demographics_all <- function(...) {
  warning(
    "Per-offense arrest demographics are no longer available from the CDE ",
    "API. Returning overall (offense = \"all\") demographics. ",
    "See ?get_arrest_demographics.",
    call. = FALSE
  )
  args <- list(...)
  args$offense <- "all"
  do.call(get_arrest_demographics, args)
}

# Internal: pull the aggregate count for a single offense out of an
# `arrest/{level}/all?type=totals` response. The breakdown lives in three maps
# of increasing granularity; search them in order for a case-insensitive match.
parse_arrest_offense_total <- function(response, geography, offense) {
  empty <- function() {
    data.frame(
      geography = character(), offense = character(), period = character(),
      count = numeric(), rate = numeric(), stringsAsFactors = FALSE
    )
  }

  for (map_name in c("Offense Name", "Offense Category", "Offense Breakdown")) {
    section <- response[[map_name]]
    if (is.null(section) || length(section) == 0) next
    idx <- match(tolower(offense), tolower(names(section)))
    if (!is.na(idx)) {
      cnt <- section[[idx]]
      if (is.null(cnt) || length(cnt) != 1) return(empty())
      return(data.frame(
        geography = geography,
        offense = names(section)[idx],
        period = NA_character_,
        count = as.numeric(cnt),
        rate = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }

  empty()
}

# Internal: parse arrest counts response into a tidy data.frame
#
# Response shape:
#   A list with `offenses` containing `counts` and `rates` sub-objects.
parse_arrest_counts_response <- function(response, geography) {
  empty <- function() {
    data.frame(
      geography = character(),
      offense = character(),
      period = character(),
      count = numeric(),
      rate = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # The CDE arrest endpoint dropped the `offenses` wrapper and renamed
  # `counts` to `actuals`. Accept both: prefer the top-level payload, fall
  # back to the legacy `offenses` container.
  container <- if (!is.null(response$offenses) && length(response$offenses) > 0) {
    response$offenses
  } else {
    response
  }

  if (is.null(container) || length(container) == 0) {
    return(empty())
  }

  counts_obj <- container$actuals %||% container$counts
  rates_obj <- container$rates

  if ((is.null(counts_obj) || length(counts_obj) == 0) &&
      (is.null(rates_obj) || length(rates_obj) == 0)) {
    return(empty())
  }

  counts_df <- data.frame(
    offense = character(), period = character(), count = numeric(),
    stringsAsFactors = FALSE
  )
  rates_df <- data.frame(
    offense = character(), period = character(), rate = numeric(),
    stringsAsFactors = FALSE
  )

  if (!is.null(counts_obj) && length(counts_obj) > 0) {
    counts_df <- flatten_cde_json(counts_obj)
    names(counts_df) <- c("offense", "period", "count")
  }

  if (!is.null(rates_obj) && length(rates_obj) > 0) {
    rates_df <- flatten_cde_json(rates_obj)
    names(rates_df) <- c("offense", "period", "rate")
  }

  result <- merge(counts_df, rates_df, by = c("offense", "period"), all = TRUE)
  result$geography <- geography
  result <- result[, c("geography", "offense", "period", "count", "rate")]
  rownames(result) <- NULL
  result
}

# Internal: parse arrest demographics response into a tidy data.frame
#
# Response shape:
#   A list with `offenses` containing a `totals` sub-object mapping
#   demographic categories to period-value pairs.
parse_arrest_demographics_response <- function(response, geography, offense) {
  empty <- function() {
    data.frame(
      geography = character(),
      offense = character(),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(response) || length(response) == 0) {
    return(empty())
  }

  # The CDE arrest demographics payload moved from a nested `offenses$totals`
  # object to top-level keys (one per demographic category). Accept the legacy
  # location if present, otherwise treat the response itself as the container.
  totals <- response$offenses$totals %||% response
  if (is.null(totals) || length(totals) == 0) {
    return(empty())
  }

  # Top-level keys that are metadata, not demographic breakdowns.
  skip_keys <- c(
    "cde_properties", "Offense Name", "Offense Category", "Offense Breakdown",
    "populations", "tooltips", "rates", "actuals"
  )

  demo_types <- c()
  demo_values <- c()
  counts <- c()

  for (demo_type in names(totals)) {
    if (demo_type %in% skip_keys) next
    demo_data <- totals[[demo_type]]
    # Accept both nested lists (live, simplifyVector = FALSE) and named atomic
    # vectors (fixtures parsed with simplifyVector = TRUE).
    if (length(demo_data) == 0 || is.null(names(demo_data))) next
    for (demo_value in names(demo_data)) {
      count <- demo_data[[demo_value]]
      # Only keep scalar, numeric-coercible leaves; this naturally skips
      # nested / string-valued metadata categories.
      if (is.null(count) || length(count) != 1) next
      num <- suppressWarnings(as.numeric(count))
      if (is.na(num)) next
      demo_types <- c(demo_types, demo_type)
      demo_values <- c(demo_values, demo_value)
      counts <- c(counts, num)
    }
  }

  if (length(counts) == 0) {
    return(empty())
  }

  periods <- rep(NA_character_, length(counts))

  data.frame(
    geography = rep(geography, length(counts)),
    offense = rep(offense, length(counts)),
    period = periods,
    demographic_type = demo_types,
    demographic_value = demo_values,
    count = counts,
    stringsAsFactors = FALSE
  )
}

#' Return a vector of all offenses in the UCR Arrest data.
#'
#' @family UCR arrest functions
#'
#' @return
#' A character vector of UCR arrest offense codes.
#' @export
#'
#' @examples
#' list_ucr_arrest_offenses()
list_ucr_arrest_offenses <- function() {
  fbiCDE::ucr_arrest_offenses
}
