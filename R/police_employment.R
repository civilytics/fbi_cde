#' Get agency-, state-, region-, or national-level police staffing data.
#'
#' @inheritParams get_estimated_arson
#' @inheritParams get_agency_crime
#' @param region Character string for the census region (e.g. "Northeast",
#'   "Midwest", "South", "West").
#'
#' @return A data.frame with columns for annual number of employees and officers
#'   (also broken up by gender).
#' @export
#'
#' @examples
#' \dontrun{
#' # Gets only Oakland Police Department in California
#' get_police_employment("CA0010900")
#'
#' # Gets California state-level estimates
#' get_police_employment(state_abb = "CA")
#'
#' # Gets national-level estimates
#' get_police_employment()
#' }
get_police_employment <- function(ori = NULL,
                                    state_abb = NULL,
                                    region = NULL,
                                    from = "2015",
                                    to = "2020") {

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
  parse_police_employment_response(response, geography)
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
