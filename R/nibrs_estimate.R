# The CDE's NIBRS estimates: nibrs-estimation/{national,state/{id}}/{indicator}
# with a required ?year=YYYY. The path is used by the CDE web app's "NIBRS
# Estimates" page and is absent from the CDE's public API documentation. State
# and indicator IDs are the lookup's own numbers, not FIPS (Alaska is 1,
# California 6). See specs/2026-10-09-capstone-imputation-gap-design.md.

# Names the estimates lookup uses that datasets::state.name does not.
.ESTIMATE_STATE_EXTRAS <- c(
  "District of Columbia" = "DC", "Puerto Rico" = "PR", "Guam" = "GU",
  "U.S. Virgin Islands" = "VI", "American Samoa" = "AS",
  "Canal Zone" = "CZ", "Mariana Islands" = "MP"
)

# Parse nibrs-estimation/lookup/all into the three bundled vocabularies.
# "Federal" and "Other" have no state abbreviation and are left out.
.parse_estimation_lookup <- function(response) {
  data <- response[[1]]$data

  abbr <- datasets::state.abb
  names(abbr) <- datasets::state.name
  abbr <- c(abbr, .ESTIMATE_STATE_EXTRAS)

  st <- data$States
  states <- data.frame(
    state_abbr = unname(abbr[names(st)]),
    state_name = names(st),
    id = as.integer(unlist(st, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  states <- states[!is.na(states$state_abbr), , drop = FALSE]
  states <- states[order(states$id), , drop = FALSE]
  rownames(states) <- NULL

  ind <- data$Indicators
  indicators <- do.call(rbind, lapply(names(ind), function(group) {
    data.frame(
      group = group,
      indicator = names(ind[[group]]),
      id = as.integer(unlist(ind[[group]], use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  }))
  indicators <- indicators[order(indicators$group, indicators$id,
                                 method = "radix"), , drop = FALSE]
  rownames(indicators) <- NULL

  years <- sort(as.integer(unlist(data$Years, use.names = FALSE)))

  list(states = states, indicators = indicators, years = years)
}

# Summary offense codes the estimates and imputation_gap() support: the
# estimate indicator each maps to (by its name in nibrs_estimation_indicators)
# and the NIBRS offense codes whose counts make up the same offense. Summary
# property crime is burglary, larceny and motor vehicle theft (arson is
# separate); summary rape is the revised definition (11A, 11B, 11C), the
# estimates' "Revised Rape" (their "Rape" is 11A alone). Each code was
# checked live against summarized/ on 2026-10-15 (spec 3.3).
.ESTIMATE_OFFENSES <- list(
  V   = list(indicator = "Violent Crime",
             nibrs = c("09A", "11A", "11B", "11C", "120", "13A")),
  P   = list(indicator = "Property Crime",
             nibrs = c("220", "23A", "23B", "23C", "23D", "23E", "23F",
                       "23G", "23H", "240")),
  ROB = list(indicator = "Robbery", nibrs = "120"),
  ASS = list(indicator = "Aggravated Assault", nibrs = "13A"),
  BUR = list(indicator = "Burglary/B&E", nibrs = "220"),
  LAR = list(indicator = "Larceny/Theft Offenses",
             nibrs = c("23A", "23B", "23C", "23D", "23E", "23F", "23G",
                       "23H")),
  MVT = list(indicator = "Motor Vehicle Theft", nibrs = "240"),
  ARS = list(indicator = "Arson", nibrs = "200"),
  HOM = list(indicator = "Murder and Non-negligent Manslaughter",
             nibrs = "09A"),
  RPE = list(indicator = "Revised Rape", nibrs = c("11A", "11B", "11C"))
)

.ESTIMATE_COLS <- c("geography", "year", "offense", "indicator", "table",
                    "variable", "category", "estimate", "lower_bound",
                    "upper_bound")

# "national" or "state/<the estimates' own state id>".
.estimate_level <- function(state_abb) {
  if (is.null(state_abb)) {
    return("national")
  }
  if (length(state_abb) != 1L || !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", paste(state_abb, collapse = ", "),
         call. = FALSE)
  }
  st <- fbiCDE::nibrs_estimation_states
  id <- st$id[st$state_abbr == toupper(state_abb)]
  if (length(id) != 1L) {
    stop("The FBI publishes no NIBRS estimate for ", toupper(state_abb), ".",
         call. = FALSE)
  }
  paste0("state/", id)
}

# One whole-number year. Membership in nibrs_estimation_years is not checked
# here: the API answers an unsupported year with HTTP 400 naming the years it
# has, which stays right when the FBI adds one and the bundled list is stale.
.check_estimate_year <- function(year) {
  if (!is.numeric(year) || length(year) != 1L || is.na(year) ||
      year != round(year)) {
    stop("'year' must be a single year, e.g. 2022.", call. = FALSE)
  }
  as.integer(year)
}

# A summary code from .ESTIMATE_OFFENSES, or an id in
# nibrs_estimation_indicators -> the indicator id.
.estimate_indicator <- function(offense) {
  ind <- fbiCDE::nibrs_estimation_indicators
  if (length(offense) == 1L && !is.na(offense)) {
    code <- toupper(as.character(offense))
    if (code %in% names(.ESTIMATE_OFFENSES)) {
      return(ind$id[ind$indicator == .ESTIMATE_OFFENSES[[code]]$indicator])
    }
    if (grepl("^[0-9]+$", code) && as.integer(code) %in% ind$id) {
      return(as.integer(code))
    }
  }
  stop("'offense' must be one of ",
       paste(names(.ESTIMATE_OFFENSES), collapse = ", "),
       ", or an id from nibrs_estimation_indicators.", call. = FALSE)
}

# Parse one nibrs-estimation response: [{"data": {<table>: {<variable>:
# [{<category>: n, "lower_bound": x, "upper_bound": y}, ...]}}}]. Bounds can
# be null (a 0 cell) or negative (small cells); both are kept as published.
parse_nibrs_estimate_response <- function(response, geography, year, offense,
                                          indicator, table) {
  tbl <- if (length(response) > 0) response[[1]]$data[[table]] else NULL
  rows <- lapply(names(tbl), function(variable) {
    do.call(rbind, lapply(tbl[[variable]], function(entry) {
      category <- setdiff(names(entry), c("lower_bound", "upper_bound"))
      if (length(category) != 1L) {
        stop("Unexpected NIBRS estimate entry under '", variable, "'.",
             call. = FALSE)
      }
      data.frame(
        variable = variable,
        category = category,
        estimate = as.numeric(entry[[category]] %||% NA),
        lower_bound = as.numeric(entry$lower_bound %||% NA),
        upper_bound = as.numeric(entry$upper_bound %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(variable = character(0), category = character(0),
                      estimate = numeric(0), lower_bound = numeric(0),
                      upper_bound = numeric(0), stringsAsFactors = FALSE)
  }
  n <- nrow(out)
  out <- cbind(
    data.frame(geography = rep(geography, n), year = rep(year, n),
               offense = rep(offense, n), indicator = rep(indicator, n),
               table = rep(table, n), stringsAsFactors = FALSE),
    out
  )
  out[, .ESTIMATE_COLS, drop = FALSE]
}

#' Get the FBI's published NIBRS estimates for a state or the nation
#'
#' Returns the FBI's *estimate*: its published figure for a state or the
#' nation, which adds a statistical model's prediction for everything outside
#' NIBRS to what agencies reported in NIBRS. Each value comes with its 95%
#' confidence interval. Compare it with crime *reported* to the FBI using
#' [imputation_gap()].
#'
#' The API publishes these estimates for the years in
#' [nibrs_estimation_years] (2021 and 2022), and only for states and the
#' nation: nothing for counties, cities or metro areas. A year it does not
#' cover fails with the API's own message listing the years it has. The
#' estimates come from `nibrs-estimation/`, the endpoint behind the CDE's
#' "NIBRS Estimates" page, which the CDE's public API documentation does not
#' list.
#'
#' @family NIBRS functions
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param year A single year, e.g. `2022`.
#' @param offense A summary offense code (`"V"`, the default, for violent
#'   crime; `"P"`, `"ROB"`, `"ASS"`, `"BUR"`, `"LAR"`, `"MVT"`, `"ARS"`,
#'   `"HOM"` or `"RPE"`) or an `id` from [nibrs_estimation_indicators].
#' @param table Which of the estimate's tables to return: `"Offense"` (the
#'   default), `"Victim"`, `"Incident"` or `"Arrest"`.
#'
#' @return A data.frame with one row per category: `geography`, `year`,
#'   `offense`, `indicator` (the estimates API's offense id), `table`,
#'   `variable` (e.g. `"Victim age"`), `category` (e.g. `"25-34"`),
#'   `estimate`, `lower_bound` and `upper_bound`. The offense total is the
#'   `"Offense"` table's row with `variable` `"Offense Count - Total"`.
#'   Bounds are as published: `NA` where the FBI gives none, and below zero
#'   for some small counts.
#' @export
#'
#' @examples
#' \dontrun{
#' ca <- get_nibrs_estimate("CA", year = 2021)
#' ca[ca$variable == "Offense Count - Total", ]
#' }
get_nibrs_estimate <- function(state_abb = NULL, year, offense = "V",
                               table = "Offense") {
  level <- .estimate_level(state_abb)
  year <- .check_estimate_year(year)
  indicator <- .estimate_indicator(offense)
  table <- match.arg(table, c("Offense", "Victim", "Incident", "Arrest"))

  response <- cde_request(cde_path("nibrs-estimation", level, indicator),
                          list(year = year))
  geography <- if (is.null(state_abb)) "US" else toupper(state_abb)
  parse_nibrs_estimate_response(response, geography, year,
                                toupper(as.character(offense)), indicator,
                                table)
}
