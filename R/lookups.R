get_agencies <- function(key = get_api_key()) {
  url <- make_url("agencies",
                  start_year = NULL,
                  key = key)
  data <- parse_lookup_response(url)
  data[] <- sapply(data, as.character)
  data <- data.table::setorder(data, "ori")
  data <- as.data.frame(data)
  return(data)
}

parse_lookup_response <- function(url) {
  # Returns a stupid list of lists of lists
  response <- httr::GET(url = paste0(url))

  if (response$status_code %in% 200) {
    response <- jsonlite::fromJSON(rawToChar(response$content))
    response <- lapply(response, function(x)data.frame(t(sapply(x, c))))
    response <- data.table::rbindlist(response)
  }
  return(response)
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

#' Checks if the ORI is valid
#'
#' Checks if ORI (a 9-digit unique identifier code assigned to each agency)
#' is valid for the API.
#'
#' @inheritParams get_agency_crime
#'
#' @return
#' TRUE if ori is valid, else FALSE. If ori is a vector, returns TRUE or FALSE
#' for each ori.
#' @export
#'
#' @examples
#' # ORI for Oakland Police Deparment in California
#' is_valid_ori("CA0010900")
#' # Incorrect ori
#' is_valid_ori("abc123")
is_valid_ori <- function(ori) {
  return(ori %in% fbi::fbi_api_agencies$ori)
}



