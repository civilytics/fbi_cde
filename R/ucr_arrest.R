#' Get arrest offense counts from the UCR Crime Data Explorer
#'
#' Retrieves monthly arrest counts for an agency, a state, or the nation, for
#' all offenses or one, from the CDE API endpoint `arrest/<level>/<code>`
#' with `type=counts`.
#'
#' @family UCR arrest functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense `"all"` (the default) for total arrests, or one offense.
#'   An offense is a name from any of the API's three levels, matched
#'   case-insensitively: offense names (`"Robbery"`, `"Drug Possession"`),
#'   categories (`"Drug/Narcotic Offenses"`) and breakdowns
#'   (`"Drug Possession - Marijuana"`); see [list_ucr_arrest_offenses()]. The
#'   API addresses offenses by numeric code ([ucr_arrest_offense_codes]), one
#'   per breakdown, so a name covering several (a category such as
#'   `"Drug/Narcotic Offenses"` has 11) costs one request per code, and the
#'   series is their sum. A string that is a name at more than one level
#'   resolves as an offense name first: `"Sex Offenses"` is code 240, not the
#'   category of that name, which holds only `"Rape (Legacy)"`. Note that
#'   `"Drug Abuse Violations"` is only the drug arrests not classed as
#'   possession or sale; total drug arrests are `"Drug/Narcotic Offenses"`.
#' @param comparison If `TRUE`, also return the comparison series the API
#'   sends alongside an agency's or state's own: its state's and the nation's
#'   arrest rates (these have no `count`). Default `FALSE` returns only the
#'   queried geography's own series.
#'
#' @return A data.frame with one row per month: `geography`, `offense`
#'   (`"all"`, or the API's spelling of the offense name), `measure`
#'   (`"arrests"`), `period`, `count`, `rate` (per 100,000 population, for
#'   that month), and the `population` and `participated_population`
#'   described in [get_agency_crime()] (their ratio is the reporting
#'   coverage). With `comparison = TRUE`, `series` and `series_name` columns
#'   follow `geography`, as in [get_agency_crime()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_count(ori = "CA0010900")
#' get_arrest_count(state_abb = "CA")
#' get_arrest_count()
#' get_arrest_count(state_abb = "CA", offense = "Robbery")
#' get_arrest_count(state_abb = "CA", offense = "Drug/Narcotic Offenses")
#' }
get_arrest_count <- function(ori = NULL,
                              state_abb = NULL,
                              from = "01-2015",
                              to = "12-2020",
                              offense = "all",
                              comparison = FALSE) {
  if (!is.null(ori)) {
    ori <- .check_ori(ori)
  }

  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  cde_validate_dates(from, to, "mm-yyyy")

  if (!is.null(ori)) {
    level <- paste0("agency/", ori)
    geography <- ori
    series_level <- "agency"
  } else if (!is.null(state_abb)) {
    level <- paste0("state/", toupper(state_abb))
    geography <- toupper(state_abb)
    series_level <- "state"
  } else {
    level <- "national"
    geography <- "US"
    series_level <- "national"
  }

  # One offense is addressed by numeric code, never by name (a name is an
  # HTTP 400). This used to be read as "a specific offense is no longer
  # addressable", and the package returned a single total picked out of the
  # all-offense response; the code paths give the full monthly series.
  sel <- .arrest_offense_selection(offense)
  query <- list(from = from, to = to, type = "counts")
  parts <- lapply(sel$codes, function(code) {
    response <- cde_request(cde_path("arrest", level, code), query)
    parse_arrest_counts_response(response, geography, level = series_level,
                                 comparison = comparison,
                                 offense = sel$offense)
  })
  .sum_arrest_parts(parts, c("count", "rate"))
}

# Resolve `offense` to the arrest codes it covers: "all", an offense name at
# any of the three levels (name, then category, then breakdown), or a code.
# Returns list(codes, offense), `offense` being the API's spelling.
.arrest_offense_selection <- function(offense) {
  if (!is.character(offense) || length(offense) != 1 || is.na(offense)) {
    stop("'offense' must be a single string.", call. = FALSE)
  }
  key <- tolower(trimws(offense))
  if (key == "all") {
    return(list(codes = "all", offense = "all"))
  }

  tbl <- fbiCDE::ucr_arrest_offense_codes
  if (key %in% tbl$code) {
    return(list(codes = key, offense = tbl$breakdown[tbl$code == key]))
  }
  for (col in c("name", "category", "breakdown")) {
    hit <- tolower(tbl[[col]]) == key
    if (any(hit)) {
      return(list(codes = tbl$code[hit], offense = tbl[[col]][hit][1]))
    }
  }

  # A name the totals report but no code covers: the API has no arrests
  # under it (Rape, Rape - Not Specified and Runaway, as of the bundled
  # vintage).
  known <- fbiCDE::ucr_arrest_offenses$offense
  idx <- match(key, tolower(known))
  if (!is.na(idx)) {
    stop("The API reports no arrests under '", known[idx], "' and has no ",
         "code for it.",
         if (key == "rape") " Rape arrests are reported as \"Rape (Legacy)\"." else "",
         call. = FALSE)
  }
  stop("Invalid arrest offense: '", offense, "' is not a name the API ",
       "reports. See list_ucr_arrest_offenses().", call. = FALSE)
}

# Combine the per-code results for one offense. A single code passes through;
# several are summed over `sum_cols`, keyed by every other column except the
# population ones, which are the geography's and identical across codes.
.sum_arrest_parts <- function(parts, sum_cols) {
  if (length(parts) == 1) {
    return(parts[[1]])
  }
  all_rows <- do.call(rbind, parts)
  if (nrow(all_rows) == 0) {
    return(all_rows)
  }
  key_cols <- setdiff(names(all_rows),
                      c(sum_cols, "population", "participated_population"))
  key <- do.call(paste, c(lapply(all_rows[key_cols], as.character), sep = "\r"))
  first <- !duplicated(key)
  grp <- factor(key, levels = key[first])
  out <- all_rows[first, , drop = FALSE]
  for (col in sum_cols) {
    out[[col]] <- unname(vapply(split(all_rows[[col]], grp), function(v) {
      if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
    }, numeric(1)))
  }
  rownames(out) <- NULL
  out
}

#' Get arrestee demographics from the UCR Crime Data Explorer
#'
#' Retrieves arrest counts broken down by demographic categories (sex, age by
#' sex, race) over the date range, for all offenses or one. Uses the CDE API
#' endpoint `arrest/<level>/<code>` with `type=totals`. With an offense this is
#' the offense-by-age cross-tabulation: juvenile arrests for larceny, say.
#'
#' @family UCR arrest functions
#' @inheritParams get_arrest_count
#' @param offense `"all"` (the default), or one offense, as in
#'   [get_arrest_count()]. A name covering several codes is the sum of their
#'   demographics.
#' @section All-offense demographics are incomplete:
#' The API's all-offense demographics leave out arrests filed under the five
#' "(Unspecified)" offense codes (140, 150, 151, 156 and 170, e.g. "Drug
#' Abuse Violations (Unspecified)"), although its all-offense counts include
#' them: in Ohio in 2023, 883 of 188,836 arrests, 57 of them juvenile.
#' Summing `get_arrest_demographics()` over the offense names includes them.
#'
#' @return A data.frame with columns: geography, offense, period,
#'   demographic_type, demographic_value, count
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_demographics(ori = "CA0010900")
#' get_arrest_demographics(state_abb = "CA")
#' get_arrest_demographics(state_abb = "OH", offense = "Larceny")
#' }
get_arrest_demographics <- function(ori = NULL,
                                     state_abb = NULL,
                                     from = "01-2015",
                                     to = "12-2020",
                                     offense = "all") {
  if (!is.null(ori)) {
    ori <- .check_ori(ori)
  }

  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  cde_validate_dates(from, to, "mm-yyyy")
  sel <- .arrest_offense_selection(offense)

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

  query <- list(from = from, to = to, type = "totals")
  parts <- lapply(sel$codes, function(code) {
    response <- cde_request(cde_path("arrest", level, code), query)
    parse_arrest_demographics_response(response, geography, sel$offense)
  })
  .sum_arrest_parts(parts, "count")
}

#' Get arrestee demographics (all offenses)
#'
#' @description
#' **Deprecated.** Use [get_arrest_demographics()], which takes an
#' `offense`. This alias returns all-offense demographics (`offense = "all"`)
#' and emits a warning. It is retained for backward compatibility and may be
#' removed in a future release.
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
    "get_arrest_demographics_all() is deprecated; use ",
    "get_arrest_demographics(), which takes an offense. Returning ",
    "all-offense (offense = \"all\") demographics.",
    call. = FALSE
  )
  args <- list(...)
  args$offense <- "all"
  do.call(get_arrest_demographics, args)
}

# Internal: parse an arrest/{level}/{code}?type=counts response.
#
# The CDE arrest endpoint dropped the `offenses` wrapper and renamed `counts`
# to `actuals`. Accept both: prefer the top-level payload, fall back to the
# legacy `offenses` container.
parse_arrest_counts_response <- function(response, geography, level,
                                         comparison = FALSE, offense = "all") {
  container <- if (!is.null(response$offenses) && length(response$offenses) > 0) {
    response$offenses
  } else {
    response
  }
  .parse_series(container$actuals %||% container$counts, container$rates,
                geography = geography, offense = offense, level = level,
                comparison = comparison, populations = response$populations)
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

#' List the offense names `get_arrest_count()` accepts
#'
#' The CDE reports arrests at three levels of detail, and `get_arrest_count()`
#' and `get_arrest_demographics()` accept a name from any of them. Three names
#' it lists, `"Rape"`, `"Rape - Not Specified"` and `"Runaway"`, have no
#' arrests in the API; see
#' [ucr_arrest_offense_codes] for the codes behind the rest.
#'
#' @family UCR arrest functions
#' @param level `"all"` (the default) for every name, or one level:
#'   `"name"` (the 34 offense names, e.g. `"Drug Possession"`), `"category"`
#'   (29, e.g. `"Drug/Narcotic Offenses"`) or `"breakdown"` (49, e.g.
#'   `"Drug Possession - Marijuana"`). Within a level the counts do not
#'   overlap, so ranking offenses means ranking one level.
#'
#' @return A sorted character vector of offense names.
#' @export
#'
#' @examples
#' list_ucr_arrest_offenses()
#' list_ucr_arrest_offenses("category")
list_ucr_arrest_offenses <- function(level = c("all", "name", "category",
                                               "breakdown")) {
  level <- match.arg(level)
  tbl <- fbiCDE::ucr_arrest_offenses
  if (level != "all") {
    tbl <- tbl[tbl$level == level, , drop = FALSE]
  }
  sort(unique(tbl$offense))
}
