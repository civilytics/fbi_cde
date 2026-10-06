#' Get police staffing data
#'
#' Returns an agency's annual count of sworn officers and civilian employees,
#' by sex, from the CDE's police employment (`pe`) endpoint.
#'
#' The endpoint also takes a state, a region or the nation, but the CDE
#' currently answers those with every value missing -- counts, rates and
#' populations alike -- so they return no rows, with a message. Only
#' agency-level staffing is available.
#'
#' @inheritParams get_estimated_arson
#' @inheritParams get_agency_crime
#' @param region Character string for the census region (e.g. "Northeast",
#'   "Midwest", "South", "West").
#' @param from,to First and last year, as four-digit `YYYY` strings (defaults
#'   `"2015"` and `"2020"`). Unlike the other data functions, police
#'   employment is annual.
#'
#' @return A data.frame with one row per year: `year`, `male_officers`,
#'   `female_officers`, `male_civilians`, `female_civilians`, `ori` (the
#'   requested geography), and the totals `male_total`, `female_total`,
#'   `civilians_total`, `officers_total` and `employees_total`. Zero rows,
#'   with a message, when the CDE returns no data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Oakland Police Department, California
#' get_police_employment("CA0010900", from = "2018", to = "2020")
#' }
get_police_employment <- function(ori = NULL,
                                    state_abb = NULL,
                                    region = NULL,
                                    from = "2015",
                                    to = "2020") {

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

  if (!is.null(ori)) {
    level <- paste0("agency/", ori)
    geography <- ori
  } else if (!is.null(state_abb)) {
    level <- paste0("state/", toupper(state_abb))
    geography <- toupper(state_abb)
  } else if (!is.null(region)) {
    level <- paste0("region/", region)
    geography <- region
  } else {
    level <- "national"
    geography <- "US"
  }

  path <- cde_path("pe", level)
  query <- cde_query(from, to, four_digit_year = TRUE)

  response <- cde_request(path, query)
  out <- parse_police_employment_response(response, geography)

  if (nrow(out) == 0) {
    message(
      "get_police_employment() returned no data for ", geography, ", ",
      from, "-", to, ".",
      if (is.null(ori)) {
        paste0(" The CDE currently publishes police staffing for individual ",
               "agencies only; state, region and national requests come back ",
               "empty.")
      } else {
        ""
      }
    )
  }
  out
}

#' Internal helper: parse police employment API response into a wide data.frame
#'
#' Response shape (CDE API):
#'   A named list where each top-level key maps to a list of period-value pairs.
#'
#' @param response Parsed JSON response from the pe endpoint
#' @param geography Geographic identifier (ORI, state, region, or "US")
#' @return A wide data.frame with columns: year, male_officers, female_officers,
#'   male_civilians, female_civilians, ori, male_total, female_total,
#'   civilians_total, officers_total, employees_total
#' @keywords internal
parse_police_employment_response <- function(response, geography) {
  if (is.null(response) || length(response) == 0) {
    return(data.frame(
      year = integer(),
      male_officers = integer(),
      female_officers = integer(),
      male_civilians = integer(),
      female_civilians = integer(),
      ori = character(),
      male_total = integer(),
      female_total = integer(),
      civilians_total = integer(),
      officers_total = integer(),
      employees_total = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # The CDE pe endpoint nests the staffing categories under `actuals`
  # ({category -> {year -> value}}); older responses placed them at the top
  # level. Flatten whichever container holds the category breakdown.
  flat_src <- response$actuals %||% response

  # Flatten the response: {label -> {period -> value}} -> long data.frame
  flat <- flatten_cde_json(flat_src)
  names(flat) <- c("category", "period", "value")

  # Get all unique years from periods
  all_years <- sort(unique(as.integer(flat$period)))

  if (length(all_years) == 0) {
    return(data.frame(
      year = integer(),
      male_officers = integer(),
      female_officers = integer(),
      male_civilians = integer(),
      female_civilians = integer(),
      ori = character(),
      male_total = integer(),
      female_total = integer(),
      civilians_total = integer(),
      officers_total = integer(),
      employees_total = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Build wide data.frame
  result <- data.frame(
    year = all_years,
    male_officers = integer(length(all_years)),
    female_officers = integer(length(all_years)),
    male_civilians = integer(length(all_years)),
    female_civilians = integer(length(all_years)),
    ori = rep(geography, length(all_years)),
    male_total = integer(length(all_years)),
    female_total = integer(length(all_years)),
    civilians_total = integer(length(all_years)),
    officers_total = integer(length(all_years)),
    employees_total = integer(length(all_years)),
    stringsAsFactors = FALSE
  )

  # Map category names to column indices
  col_map <- c(
    "Male Officers" = "male_officers",
    "Female Officers" = "female_officers",
    "Male Civilians" = "male_civilians",
    "Female Civilians" = "female_civilians",
    "Male Total" = "male_total",
    "Female Total" = "female_total",
    "Civilians Total" = "civilians_total",
    "Officers Total" = "officers_total",
    "Employees Total" = "employees_total"
  )

  # Fill in values from the flat data
  for (i in seq_len(nrow(flat))) {
    cat <- flat$category[i]
    yr <- as.integer(flat$period[i])
    val <- flat$value[i]
    col <- col_map[cat]
    if (!is.na(col)) {
      row_idx <- match(yr, result$year)
      if (!is.na(row_idx)) {
        result[[col]][row_idx] <- val
      }
    }
  }

  # Compute derived columns if not present in API response
  if ("male_total" %in% names(result)) {
    result$male_total <- result$male_officers + result$male_civilians
  }
  if ("female_total" %in% names(result)) {
    result$female_total <- result$female_officers + result$female_civilians
  }
  if ("civilians_total" %in% names(result)) {
    result$civilians_total <- result$male_civilians + result$female_civilians
  }
  if ("officers_total" %in% names(result)) {
    result$officers_total <- result$male_officers + result$female_officers
  }
  if ("employees_total" %in% names(result)) {
    result$employees_total <- result$officers_total + result$civilians_total
  }

  # Reorder columns to match expected output
  result <- result[, c("year", "male_officers", "female_officers",
                        "male_civilians", "female_civilians", "ori",
                        "male_total", "female_total", "civilians_total",
                        "officers_total", "employees_total")]

  rownames(result) <- NULL
  result
}
