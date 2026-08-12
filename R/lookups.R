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
  combined <- rbind_fill(lapply(all_agencies, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  combined <- combined[order(combined$ori), , drop = FALSE]
  combined[] <- lapply(combined, as.character)
  rownames(combined) <- NULL
  combined
}

#' Get offense codes from the CDE API
#'
#' Retrieves valid offense codes from the CDE lookup endpoint.
#'
#' @param type Character string for the offense type (e.g. "crime-trend",
#'   "arrest", "hate-crime", "nibrs"). "nibrs" currently returns no codes
#'   live (`crimeGroups: null`).
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

  empty <- data.frame(code = character(), label = character(), stringsAsFactors = FALSE)

  groups <- response$crimeGroups
  if (is.null(groups) || length(groups) == 0) {
    return(empty)
  }

  # `crimeGroups` is a list of `{label, crimes}` groups, where `crimes` is a
  # list of `{label, value}` offense entries. With `simplifyVector = TRUE`
  # (offline fixture reads) `crimeGroups` simplifies to a data.frame whose
  # `crimes` column holds one nested data.frame per group; with
  # `simplifyVector = FALSE` (live `cde_request()`) it stays a list of lists.
  # Both shapes are handled below.
  crimes_list <- if (is.data.frame(groups)) groups$crimes else lapply(groups, function(g) g$crimes)

  rows <- lapply(crimes_list, function(crimes) {
    if (is.null(crimes) || length(crimes) == 0) {
      return(NULL)
    }
    if (is.data.frame(crimes)) {
      data.frame(
        code = as.character(crimes$value),
        label = as.character(crimes$label),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        code = vapply(crimes, function(c) as.character(c$value %||% NA), character(1)),
        label = vapply(crimes, function(c) as.character(c$label %||% NA), character(1)),
        stringsAsFactors = FALSE
      )
    }
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(empty)
  }

  result <- do.call(rbind, rows)
  as.data.frame(result)
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

  empty <- data.frame(stateAbbreviation = character(), stateName = character(), stringsAsFactors = FALSE)

  states <- response$get_states$cde_states_query$states
  if (is.null(states) || length(states) == 0) {
    return(empty)
  }

  # `states` is an array of `{abbr, name}` objects. With
  # `simplifyVector = TRUE` (offline fixture reads) it simplifies to a
  # data.frame directly; with `simplifyVector = FALSE` (live
  # `cde_request()`) it stays a list of per-state lists.
  if (is.data.frame(states)) {
    return(data.frame(
      stateAbbreviation = as.character(states$abbr),
      stateName = as.character(states$name),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    stateAbbreviation = vapply(states, function(s) as.character(s$abbr %||% NA), character(1)),
    stateName = vapply(states, function(s) as.character(s$name %||% NA), character(1)),
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
    data <- fbiCDE::fbi_api_agencies[tolower(fbiCDE::fbi_api_agencies$agency_name) %in%
                                    tolower(agency), ]
  } else {
    data <- fbiCDE::fbi_api_agencies[grep(tolower(agency),
                                       tolower(fbiCDE::fbi_api_agencies$agency_name)), ]
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
