# Internal helper: flatten a CDE `agency/byStateAbbr/{state}` response (a
# list keyed by county name -> agency records) into one row per agency.
#
# Response shape (CDE API): a named list where each key is a county name and
# each value is either a list of per-agency lists (live `cde_request()`,
# `simplifyVector = FALSE`) or a data.frame of agencies (offline fixtures
# read with `simplifyVector = TRUE`); both shapes are handled.
parse_agency_participation_response <- function(response) {
  empty <- data.frame(
    ori = character(), agency_name = character(), agency_type_name = character(),
    state_abbr = character(), state_name = character(), county = character(),
    is_nibrs = logical(), nibrs_start_date = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(response) || length(response) == 0) {
    return(empty)
  }

  rows <- lapply(names(response), function(county) {
    county_data <- response[[county]]

    if (is.data.frame(county_data)) {
      return(data.frame(
        ori = as.character(county_data$ori),
        agency_name = as.character(county_data$agency_name),
        agency_type_name = as.character(county_data$agency_type_name),
        state_abbr = as.character(county_data$state_abbr),
        state_name = as.character(county_data$state_name),
        county = if (!is.null(county_data$counties)) as.character(county_data$counties) else county,
        is_nibrs = as.logical(county_data$is_nibrs),
        nibrs_start_date = as.character(county_data$nibrs_start_date),
        stringsAsFactors = FALSE
      ))
    }

    agency_rows <- lapply(county_data, function(agency) {
      data.frame(
        ori = agency$ori %||% NA_character_,
        agency_name = agency$agency_name %||% NA_character_,
        agency_type_name = agency$agency_type_name %||% NA_character_,
        state_abbr = agency$state_abbr %||% NA_character_,
        state_name = agency$state_name %||% NA_character_,
        county = agency$counties %||% county,
        is_nibrs = isTRUE(agency$is_nibrs),
        nibrs_start_date = agency$nibrs_start_date %||% NA_character_,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, agency_rows)
  })

  result <- rbind_fill(rows)
  as.data.frame(result)
}

#' Get agency-level NIBRS participation status
#'
#' @description
#' The current CDE API has no dedicated participation-*rate* endpoint: the
#' legacy `participation/...` paths this function used to call return 404,
#' and the `participation/*` namespace still present in the current API is
#' scoped to Use-of-Force agency reporting, not general UCR/NIBRS
#' participation (confirmed by inspecting the CDE web app's client bundle and
#' probing the live API -- see Issue #1). This is a modernized
#' reimplementation: it reports whether `ori` currently reports to NIBRS,
#' sourced live from `agency/byStateAbbr/` (state derived from the first two
#' characters of `ori`), in place of the retired year-by-year
#' SRS/NIBRS participation series.
#'
#' @family UCR crime functions
#' @param ori A string of the 9-character ORI code for the desired agency.
#' @param key API key for the legacy FBI API (not needed for CDE API).
#'
#' @return A one-row data.frame with columns: ori, agency_name,
#'   agency_type_name, state_abbr, state_name, county, is_nibrs,
#'   nibrs_start_date.
#' @export
#' @author Jared E. Knowles, Civilytics Consulting
#' @examples
#' \dontrun{
#' get_agency_participation("CA0010900")
#' }
get_agency_participation <- function(ori, key = get_api_key()) {
  if (!is_valid_ori(ori)) {
    stop(
      "Invalid ORI code: ", ori,
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
  }

  ori <- toupper(ori)
  state_abb <- substr(ori, 1, 2)
  path <- paste0("agency/byStateAbbr/", state_abb)

  response <- cde_request(path)
  agencies <- parse_agency_participation_response(response)

  result <- agencies[agencies$ori == ori, ]
  rownames(result) <- NULL
  result
}

#' Get state-level NIBRS participation rate
#'
#' @description
#' See [get_agency_participation()] for why this is a modernized
#' reimplementation (share of agencies currently reporting NIBRS) rather than
#' the retired year-by-year SRS/NIBRS participation series.
#'
#' @family UCR crime functions
#' @param state_abb String input for the state abbreviation to get data for.
#' @param key API key for the legacy FBI API (not needed for CDE API).
#'
#' @return A one-row data.frame with columns: state_abbr, total_agencies,
#'   nibrs_agencies, participation_rate.
#' @export
#' @author Jared E. Knowles, Civilytics Consulting
#' @examples
#' \dontrun{
#' get_state_participation("MT")
#' }
get_state_participation <- function(state_abb, key = get_api_key()) {
  if (!is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  state_abb <- toupper(state_abb)
  path <- paste0("agency/byStateAbbr/", state_abb)

  response <- cde_request(path)
  agencies <- parse_agency_participation_response(response)

  total <- nrow(agencies)
  nibrs <- sum(agencies$is_nibrs)

  data.frame(
    state_abbr = state_abb,
    total_agencies = total,
    nibrs_agencies = nibrs,
    participation_rate = if (total == 0) NA_real_ else nibrs / total,
    stringsAsFactors = FALSE
  )
}

#' Get region-level NIBRS participation rate
#'
#' @description
#' Aggregates [get_state_participation()] across every state in
#' `region_name`. Census region membership is taken from the bundled
#' [fbi_api_agencies] dataset -- a fixed geography, not live crime data --
#' while each state's participation counts are fetched live. See
#' [get_agency_participation()] for why this is a modernized reimplementation
#' rather than the retired year-by-year SRS/NIBRS participation series.
#'
#' @family UCR crime functions
#' @param region_name String for the census region to get data for. See
#'   [regions] for valid values.
#' @param key API key for the legacy FBI API (not needed for CDE API).
#'
#' @return A one-row data.frame with columns: region, total_agencies,
#'   nibrs_agencies, participation_rate.
#' @author Jared E. Knowles, Civilytics Consulting
#' @export
#' @examples
#' \dontrun{
#' get_region_participation("South")
#' }
get_region_participation <- function(region_name, key = get_api_key()) {
  if (!region_name %in% fbiCDE::regions) {
    stop(
      "Invalid region_name: ", region_name,
      ". Must be one of: ", paste(fbiCDE::regions, collapse = ", "),
      call. = FALSE
    )
  }

  states_in_region <- unique(
    fbiCDE::fbi_api_agencies$state_abbr[fbiCDE::fbi_api_agencies$region_name == region_name]
  )
  states_in_region <- states_in_region[!is.na(states_in_region)]

  if (length(states_in_region) == 0) {
    return(data.frame(
      region = region_name, total_agencies = 0L, nibrs_agencies = 0L,
      participation_rate = NA_real_, stringsAsFactors = FALSE
    ))
  }

  state_results <- lapply(states_in_region, function(s) {
    get_state_participation(s, key = key)
  })
  combined <- do.call(rbind, state_results)

  total <- sum(combined$total_agencies)
  nibrs <- sum(combined$nibrs_agencies)

  data.frame(
    region = region_name,
    total_agencies = total,
    nibrs_agencies = nibrs,
    participation_rate = if (total == 0) NA_real_ else nibrs / total,
    stringsAsFactors = FALSE
  )
}
