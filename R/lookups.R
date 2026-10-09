#' Get all agencies from the CDE API
#'
#' Iterates over every state in [get_states()] and fetches its agency directory
#' from the CDE endpoint `agency/byStateAbbr/{state}` (the same endpoint the
#' participation functions use) to rebuild the full agency table. This issues
#' one request per state.
#'
#' @return A data.frame with one row per agency and one column per field the
#'   API returns (currently `ori`, `counties`, `is_nibrs`, `latitude`,
#'   `longitude`, `state_abbr`, `state_name`, `agency_name`, `agency_type_name`,
#'   `nibrs_start_date`), all stored as character and sorted by `ori`. These
#'   field names follow the live API and differ from the bundled
#'   [fbi_api_agencies] snapshot (e.g. `counties` rather than `county_name`).
#'   States whose request fails are skipped with a warning and listed in
#'   `attr(x, "failed_states")`.
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
  failed <- character(0)

  for (state_abbr in states$stateAbbreviation) {
    # This used to request `agency/{state}` and coerce the response with
    # as.data.frame(), which on the live county-keyed shape produced one junk
    # row per state rather than one row per agency.
    path <- cde_path("agency/byStateAbbr", state_abbr)
    response <- tryCatch(cde_request(path), error = function(e) e)
    if (inherits(response, "error")) {
      failed <- c(failed, state_abbr)
      next
    }
    all_agencies[[state_abbr]] <- .flatten_agency_directory(response)
  }

  if (length(failed) > 0) {
    warning("Could not fetch the agency directory for ", length(failed),
            " state", if (length(failed) == 1) "" else "s", ": ",
            paste(failed, collapse = ", "), call. = FALSE)
  }

  combined <- rbind_fill(all_agencies)
  if (nrow(combined) == 0 || !"ori" %in% names(combined)) {
    out <- data.frame()
  } else {
    combined[] <- lapply(combined, as.character)
    # An agency filed under more than one county key would otherwise repeat;
    # a record without an ORI is not an agency.
    combined <- combined[!is.na(combined$ori) & !duplicated(combined$ori), ,
                         drop = FALSE]
    out <- combined[order(combined$ori), , drop = FALSE]
    rownames(out) <- NULL
  }
  if (length(failed) > 0) {
    attr(out, "failed_states") <- failed
  }
  out
}

# Flatten one `agency/byStateAbbr/{state}` response -- a named list of county
# -> array of agency records -- into one row per agency, keeping every scalar
# field.
.flatten_agency_directory <- function(response) {
  if (.is_empty_agency_directory(response)) {
    return(NULL)
  }
  rows <- lapply(response, function(county_data) {
    rbind_fill(lapply(county_data, function(agency) {
      if (!is.list(agency)) {
        return(NULL)
      }
      agency <- lapply(agency, function(v) {
        if (is.null(v) || length(v) != 1) NA else v
      })
      as.data.frame(agency, stringsAsFactors = FALSE)
    }))
  })
  rbind_fill(rows)
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
  # list of `{label, value}` offense entries.
  crimes_list <- lapply(groups, function(g) g$crimes)

  rows <- lapply(crimes_list, function(crimes) {
    if (is.null(crimes) || length(crimes) == 0) {
      return(NULL)
    }
    data.frame(
      code = vapply(crimes, function(c) as.character(c$value %||% NA), character(1)),
      label = vapply(crimes, function(c) as.character(c$label %||% NA), character(1)),
      stringsAsFactors = FALSE
    )
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

  # `states` is an array of `{abbr, name}` objects.
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
#' treats each element of `agency` as a regular expression (case ignored) and
#' returns the agencies whose names match any of them.
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
    # grep() takes one pattern; it used only agency[1], with a warning.
    names_lc <- tolower(fbiCDE::fbi_api_agencies$agency_name)
    hits <- unique(unlist(lapply(tolower(agency), grep, x = names_lc)))
    data <- fbiCDE::fbi_api_agencies[sort(hits), ]
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
