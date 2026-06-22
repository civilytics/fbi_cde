#' Get state level UCR and NIBRS participation data
#'
#' @family UCR crime functions
#' @inheritParams get_agency_crime
#'
#' @param state_abb String or vector of strings input for state abbreviation(s)
#'   to get data for. If NULL (default) returns national data.
#' @param key API key for the legacy FBI API (not needed for CDE API).
#'
#' @return A data.frame with state-level NIBRS and UCR participation rates for
#'   selected state
#' @export
#' @author Jared E. Knowles, Civilytics Consulting
#' @examples
#'\dontrun{
#' get_state_participation("MT")
#' }
get_state_participation <- function(state_abb = NULL,
                                key = get_api_key()) {

  url_section <- combine_url_section("participation",
                                     ori = NULL,
                                     state_abb = state_abb,
                                     region_name = NULL)

  url <- make_url(url_section, start_year = NULL, end_year = NULL, key = key)
  data <- url_to_dataframe(url)
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
#' @param region_name String or vector of strings for region to get data for
#' @param key API key for the legacy FBI API (not needed for CDE API).
#'
#' @return A data.frame with regional NIBRS and UCR participation rates for
#'   selected state
#' @author Jared E. Knowles, Civilytics Consulting
#' @export
#'
#' @examples
#'\dontrun{
#' get_region_participation("South")
#' }
get_region_participation <- function(region_name = NULL,
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


#' Get agency level UCR and NIBRS participation data
#'
#' @family UCR crime functions
#' @inheritParams get_agency_crime
#' @param key API key for the legacy FBI API (not needed for CDE API).
#'
#' @return A data.frame with regional NIBRS and UCR participation rates for
#'   selected agency
#' @author Jared E. Knowles, Civilytics Consulting
#' @export
#'
#' @examples
#' \dontrun{
#' get_agency_participation("AK0010100")
#' }
get_agency_participation <- function(ori,
                              key = get_api_key()) {


  url_section <- combine_url_section("participation",
                                     ori = ori,
                                     state_abb = NULL,
                                     region_name = NULL)

  url <- make_url(url_section, start_year = NULL, end_year = NULL, key = key)

  response <- url_to_dataframe(url)
  response <- clean_column_names(response)
  response <- data.table::setorder(response, -"year")
  response <- as.data.frame(response)
  return(response)
}
