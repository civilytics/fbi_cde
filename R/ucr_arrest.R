#' Get arrest offense counts from the UCR Crime Data Explorer
#'
#' Retrieves arrest counts by offense for an agency, state, or nationally.
#' Uses the CDE API endpoint `arrest/{level}/{offense}?type=counts`.
#'
#' @family UCR arrest functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default "all" for total arrests). See
#'   `list_ucr_arrest_offenses()` for available codes.
#'
#' @return A data.frame with columns: geography, offense, period, count, rate
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_count(ori = "CA0010900")
#' get_arrest_count(state_abb = "CA")
#' get_arrest_count()
#' }
get_arrest_count <- function(ori = NULL,
                              state_abb = NULL,
                              from = "01-2015",
                              to = "12-2020",
                              offense = "all") {
  if (!is.null(ori) && !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must match format: 2 letters + 7 digits (e.g., CA0010900)",
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

  path <- cde_path("arrest", level, offense)
  query <- list(from = from, to = to, type = "counts")

  response <- cde_request(path, query)
  parse_arrest_counts_response(response, geography)
}

#' Get arrestee demographics from the UCR Crime Data Explorer
#'
#' Retrieves arrest counts broken down by demographic categories (sex, age,
#' race) for a specific offense. Uses the CDE API endpoint
#' `arrest/{level}/{offense}?type=totals`.
#'
#' @family UCR arrest functions
#' @inheritParams get_arrest_count
#' @param offense Offense code (required for demographics). See
#'   `list_ucr_arrest_offenses()` for available codes.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_demographics(ori = "CA0010900", offense = "robbery")
#' get_arrest_demographics(state_abb = "CA", offense = "murder")
#' }
get_arrest_demographics <- function(ori = NULL,
                                     state_abb = NULL,
                                     from = "01-2015",
                                     to = "12-2020",
                                     offense) {
  if (!is.null(ori) && !is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must match format: 2 letters + 7 digits (e.g., CA0010900)",
      call. = FALSE
    )
  }

  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  if (missing(offense)) {
    stop("offense is required for get_arrest_demographics()", call. = FALSE)
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

  path <- cde_path("arrest", level, offense)
  query <- list(from = from, to = to, type = "totals")

  response <- cde_request(path, query)
  parse_arrest_demographics_response(response, geography, offense)
}

#' Get arrestee demographics for all UCR arrest offenses
#'
#' Calls `get_arrest_demographics()` for each offense in
#' `list_ucr_arrest_offenses()` and combines the results.
#'
#' @family UCR arrest functions
#' @inheritParams get_arrest_demographics
#' @param ... Additional arguments passed to `get_arrest_demographics()`.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_demographics_all(state_abb = "CA", offense = "murder")
#' }
get_arrest_demographics_all <- function(...) {
  offenses <- list_ucr_arrest_offenses()
  results <- list()

  for (o in offenses) {
    results[[o]] <- tryCatch(
      get_arrest_demographics(offense = o, ...),
      error = function(e) data.frame()
    )
  }

  valid <- vapply(results, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))
  if (sum(valid) == 0) {
    return(data.frame(
      geography = character(),
      offense = character(),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  data.table::rbindlist(results[valid], fill = TRUE)
}

# Internal: parse arrest counts response into a tidy data.frame
#
# Response shape:
#   {"offenses": {"counts": {"Label": {"01-2015": 100}}, "rates": {"Label": {"01-2015": 0.5}}}}
parse_arrest_counts_response <- function(response, geography) {
  offenses <- response$offenses

  if (is.null(offenses) || length(offenses) == 0) {
    return(data.frame(
      geography = geography,
      offense = character(),
      period = character(),
      count = numeric(),
      rate = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  counts_df <- data.frame(
    offense = character(), period = character(), count = numeric(),
    stringsAsFactors = FALSE
  )
  rates_df <- data.frame(
    offense = character(), period = character(), rate = numeric(),
    stringsAsFactors = FALSE
  )

  if (!is.null(offenses$counts) && length(offenses$counts) > 0) {
    counts_df <- flatten_cde_json(offenses$counts)
    names(counts_df) <- c("offense", "period", "count")
  }

  if (!is.null(offenses$rates) && length(offenses$rates) > 0) {
    rates_df <- flatten_cde_json(offenses$rates)
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
#   {"offenses": {"totals": {"Arrestee Sex": {"Male": 500}, "Age": {"Under 18": 100}}}}
parse_arrest_demographics_response <- function(response, geography, offense) {
  offenses <- response$offenses

  if (is.null(offenses) || length(offenses) == 0) {
    return(data.frame(
      geography = character(),
      offense = character(),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  totals <- offenses$totals
  if (is.null(totals) || length(totals) == 0) {
    return(data.frame(
      geography = character(),
      offense = character(),
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  labels <- c()
  periods <- c()
  demo_types <- c()
  demo_values <- c()
  counts <- c()

  for (demo_type in names(totals)) {
    demo_data <- totals[[demo_type]]
    if (is.null(demo_data) || length(demo_data) == 0) next
    for (demo_value in names(demo_data)) {
      count <- demo_data[[demo_value]]
      if (is.null(count)) next
      labels <- c(labels, demo_type)
      periods <- c(periods, demo_value)
      demo_types <- c(demo_types, demo_type)
      demo_values <- c(demo_values, demo_value)
      counts <- c(counts, as.numeric(count))
    }
  }

  if (length(counts) == 0) {
    return(data.frame(
      geography = geography,
      offense = offense,
      period = character(),
      demographic_type = character(),
      demographic_value = character(),
      count = numeric(),
      stringsAsFactors = FALSE
    ))
  }

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
  fbi::ucr_arrest_offenses
}
