#' Get state level UCR and NIBRS participation data
#'
#' @family UCR crime functions
#' @inheritParams get_agency_crime
#'
#' @param state_abb
#' String or vector of strings input for state abbreviation(s) to get data for.
#' If NULL (default) returns national data.
#'
#' @return
#' A data.frame with state-level NIBRS and UCR participation rates for selected state
#' @export
#' @author Jared E. Knowles, Civilytics Consulting
#' @examples
#'\dontrun{
#' get_participation_state("MT")
#' }
get_participation_state <- function(state_abb = NULL,
                                key = get_api_key()) {

  url_section <- combine_url_section("participation",
                                     ori = NULL,
                                     state_abb = state_abb,
                                     region_name = NULL)

  url <- fbi:::make_url(url_section, start_year = NULL, end_year = NULL, key = key)
  data <- fbi:::url_to_dataframe(url)
  data <- clean_column_names(data)
  data <- data.table::setorder(data, -"year")
  data$state <- make_state(data$state_abbr)
  rownames(data) <- 1:nrow(data)
  # data <- data[, c(2, 16, 1, 3, 4:15)]
  return(data)
}


#' Get regional level UCR and NIBRS participation data
#'
#' @family UCR crime functions
#' @inheritParams get_agency_crime
#'
#' @param state_abb
#' String or vector of strings input for state abbreviation(s) to get data for.
#' If NULL (default) returns national data.
#'
#' @return
#' A data.frame with regional NIBRS and UCR participation rates for selected state#'
#' @author Jared E. Knowles, Civilytics Consulting
#' @export
#'
#' @examples
#'\dontrun{
#' get_participation_region("South")
#' }
get_participation_region <- function(region_name = NULL,
                                     key = get_api_key()) {

  url_section <- combine_url_section("participation",
                                     ori = NULL,
                                     state_abb = NULL,
                                     region_name = region_name)

  url <- make_url(url_section, start_year = NULL, end_year = NULL, key = key)
  data <-url_to_dataframe(url)
  data <- clean_column_names(data)
  data <- data.table::setorder(data, -"year")
  #
  data$region <- region_name
  rownames(data) <- 1:nrow(data)
  # confirm?
  # data <- data[, c(2, 16, 1, 3, 4:15)]
  return(data)
}


