#' Get all agencies from the CDE API
#'
#' Iterates over all state lookup endpoints to rebuild the full agency table.
#'
#' @return A data.frame with agency information (ORI, name, state, county, etc.).
#' @export
#'
#' @examples
#' \dontrun{
#' agencies <- get_agencies()
#' head(agencies)
#' }
get_agencies <- function() {
  states <- get_states()
  all_agencies <- list()

  for (state_abbr in states$stateAbbreviation) {
    path <- cde_path("agency", state_abbr)
    response <- cde_request(path)

    if (!is.null(response) && length(response) > 0) {
      all_agencies[[state_abbr]] <- response
    }
  }

  if (length(all_agencies) == 0) {
    return(data.frame())
  }

  # Combine all agency lists into a single data.frame
  combined <- data.table::rbindlist(lapply(all_agencies, function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      as.data.frame(x, stringsAsFactors = FALSE)
    } else {
      as.data.frame(x, stringsAsFactors = FALSE)
    }
  }), fill = TRUE)

  combined <- data.table::setorder(combined, "ori")
  combined[] <- lapply(combined, as.character)
  combined
}

#' Get offense codes from the CDE API
#'
#' Retrieves valid offense codes from the CDE lookup endpoint.
#'
#' @param type Character string for the offense type (e.g. "crime-trend", "nibrs").
#' @return A data.frame with offense codes and labels.
#' @export
#'
#' @examples
#' \dontrun{
#' offenses <- get_offense_codes()
#' head(offenses)
#' }
get_offense_codes <- function(type = "crime-trend") {
  path <- "lookup/offenses"
  query <- list(type = type)
  response <- cde_request(path, query)

  if (is.null(response) || length(response) == 0) {
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }

  codes <- names(response)
  labels <- unlist(response, use.names = FALSE)

  data.frame(
    code = codes,
    label = as.character(labels),
    stringsAsFactors = FALSE
  )
}

#' Get state list from the CDE API
#'
#' Retrieves the list of states/territories from the CDE lookup endpoint.
#'
#' @return A data.frame with state abbreviations and names.
#' @export
#'
#' @examples
#' \dontrun{
#' states <- get_states()
#' head(states)
#' }
get_states <- function() {
  path <- "lookup/states"
  response <- cde_request(path)

  if (is.null(response) || length(response) == 0) {
    return(data.frame(
      stateAbbreviation = character(),
      stateName = character(),
      stringsAsFactors = FALSE
    ))
  }

  # The response is a named list where names are abbreviations and values are names
  state_abbr <- names(response)
  state_name <- unlist(response, use.names = FALSE)

  data.frame(
    stateAbbreviation = state_abbr,
    stateName = as.character(state_name),
    stringsAsFactors = FALSE
  )
}

#' Get information about the selected agency
#'
#' Get information about the selected agency including the 9-digit ORI, geographic information, type of agency, and whether they report to NIBRS.
#'
#' @param agency
#' A string or vector of strings with the name of the agency you want to lookup (capitalized is ignored).
#' @param state
#' A string or vector of strings of state names. If used, returns only agencies in that state. This is useful in cases where multiple agencies have the same name in different states and you only want specific states.
#' @param ori_only
#' If TRUE (not default), returns only the ORI and the agency_name columns.
#' @param exact_match
#' If TRUE (default), finds matches based on exact match of agency name. Else,
#' uses `grep()` to find agencies with similar names to inputted agency.
#'
#' @return
#' A data.frame with information about the agency - including ORI code and geographic information. The agency will have as many rows as agencies matched from the `agency` input.
#' @export
#'
#' @examples
#' get_agency_info("Oakland Police Department")
#' get_agency_info("Oakland Police Department", state = "california")
get_agency_info <- function(agency,
                            state = NULL,
                            ori_only = FALSE,
                            exact_match = TRUE) {
  if (exact_match) {
    data <- fbi::fbi_api_agencies[tolower(fbi::fbi_api_agencies$agency_name) %in%
                                    tolower(agency), ]
  } else {
    data <- fbi::fbi_api_agencies[grep(tolower(agency),
                                       tolower(fbi::fbi_api_agencies$agency_name)), ]
  }
  if (!is.null(state)) {
    data <- data[tolower(data$state_name) %in% tolower(state), ]
  }

  if (nrow(data) == 0) {
    message("No matching agencies found. Please revise your `agency` input.")
  }
  if (ori_only) {
    data <- data[, c("agency_name", "ori")]
  }

  return(data)
}
