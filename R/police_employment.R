#' Get police staffing data
#'
#' Returns annual counts of sworn officers and civilian employees, by sex, for
#' an agency, a state, or the nation, from the CDE's police employment (`pe`)
#' endpoint.
#'
#' @inheritParams get_estimated_arson
#' @inheritParams get_agency_crime
#' @param region Not supported: the CDE publishes no region-level police
#'   employment, and passing a region is an error. Sum the states instead.
#' @param from,to First and last year, as four-digit `YYYY` strings (defaults
#'   `"2015"` and `"2020"`). Unlike the other data functions, police
#'   employment is annual.
#'
#' @return A data.frame with one row per year: `year`, `male_officers`,
#'   `female_officers`, `male_civilians`, `female_civilians`, `ori` (the
#'   requested geography), and the totals `male_total`, `female_total`,
#'   `civilians_total`, `officers_total` and `employees_total`, then
#'   `participated_population` (the population of the agencies that
#'   reported) and `employees_per_1000` (employees per 1,000 of that
#'   population). For a state or the nation the counts are sums over the
#'   agencies that reported, so they move with coverage: compare years by the
#'   rate, not the count. Zero rows, with a message, when the CDE returns no
#'   data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Oakland Police Department, California
#' get_police_employment("CA0010900", from = "2018", to = "2020")
#'
#' # California, and the nation
#' get_police_employment(state_abb = "CA", from = "2018", to = "2020")
#' get_police_employment(from = "2018", to = "2020")
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

  # The pe endpoint is not shaped like the others: the nation is plain `pe`,
  # a state `pe/{ST}`, an agency `pe/{ST}/{ORI}`. The package used to send
  # `pe/state/{ST}` and `pe/national`, which the CDE answers with every value
  # null; that read as "staffing is published for agencies only". It is not.
  # No region form returns data.
  if (!is.null(ori)) {
    ori <- toupper(ori)
    path <- paste("pe", substr(ori, 1L, 2L), ori, sep = "/")
    geography <- ori
  } else if (!is.null(state_abb)) {
    geography <- toupper(state_abb)
    path <- paste("pe", geography, sep = "/")
  } else if (!is.null(region)) {
    stop("The CDE publishes no region-level police employment. ",
         "Request the region's states with state_abb and sum them.",
         call. = FALSE)
  } else {
    path <- "pe"
    geography <- "US"
  }

  query <- cde_query(from, to, four_digit_year = TRUE)

  response <- cde_request(path, query)
  out <- parse_police_employment_response(response, geography)

  if (nrow(out) == 0) {
    message("get_police_employment() returned no data for ", geography, ", ",
            from, "-", to, ".")
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
#'   civilians_total, officers_total, employees_total,
#'   participated_population, employees_per_1000
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
      participated_population = numeric(),
      employees_per_1000 = numeric(),
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
      participated_population = numeric(),
      employees_per_1000 = numeric(),
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
  # Coverage. A state's or the nation's counts are sums over the agencies
  # that reported that year, and how many report changes from year to year:
  # Texas's 2018-2020 employee count rose 39% while its participated
  # population rose by about as much and the rate held near 3.4 per 1,000.
  # Without these two columns that reads as a staffing boom.
  # Each map is {label -> {year -> value}} with a single label; take the
  # first, so a renamed label does not silently drop the column.
  by_year <- function(map) {
    years <- if (length(map) > 0) map[[1]] else list()
    unname(vapply(as.character(result$year), function(y) {
      v <- years[[y]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1)))
  }
  result$participated_population <- by_year(response$populations)
  result$employees_per_1000 <- by_year(response$rates)

  result <- result[, c("year", "male_officers", "female_officers",
                        "male_civilians", "female_civilians", "ori",
                        "male_total", "female_total", "civilians_total",
                        "officers_total", "employees_total",
                        "participated_population", "employees_per_1000")]

  rownames(result) <- NULL
  result
}
