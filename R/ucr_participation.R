# A state the CDE's directory does not know (AS, CZ, or an ORI prefix such as
# NB) is answered with a query echo rather than an empty list:
# `{"cde_agencies_query": {"counties": null, "parameters": {...}, ...}}`.
# Read as a county, that crashed the parser on its timestamp string.
.is_empty_agency_directory <- function(response) {
  is.null(response) || length(response) == 0 ||
    identical(names(response), "cde_agencies_query")
}

# The CDE files an agency in its directory under the state's postal code,
# but two states' ORIs start with an older code: Nebraska's with NB (all 269
# bundled agencies) and Guam's with GM. Map an ORI prefix, or a bundled
# `state_abbr` (Guam is "GM" there too), to the directory's key.
.ORI_PREFIX_STATES <- c(NB = "NE", GM = "GU")

.directory_state <- function(code) {
  code <- toupper(code)
  mapped <- unname(.ORI_PREFIX_STATES[code])
  ifelse(is.na(mapped), code, mapped)
}

# Internal helper: flatten a CDE `agency/byStateAbbr/{state}` response (a
# list keyed by county name -> agency records) into one row per agency.
#
# Response shape (CDE API): a named list where each key is a county name and
# each value is a list of per-agency records.
parse_agency_participation_response <- function(response) {
  empty <- data.frame(
    ori = character(), agency_name = character(), agency_type_name = character(),
    state_abbr = character(), state_name = character(), county = character(),
    is_nibrs = logical(), nibrs_start_date = character(),
    stringsAsFactors = FALSE
  )

  if (.is_empty_agency_directory(response)) {
    return(empty)
  }

  rows <- lapply(names(response), function(county) {
    county_data <- response[[county]]

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
#' characters of `ori`; Nebraska's `NB` and Guam's `GM` are looked up as `NE`
#' and `GU`), in place of the retired year-by-year SRS/NIBRS participation
#' series.
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
  ori <- .check_ori(ori)
  state_abb <- .directory_state(substr(ori, 1, 2))
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
  states_in_region <- unique(.directory_state(
    states_in_region[!is.na(states_in_region)]
  ))

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
