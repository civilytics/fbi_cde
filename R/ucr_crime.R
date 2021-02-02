#' Get agency-level crime data from the UCR's Offenses Known and Clearances by Arrest data set.
#'
#' @family UCR crime functions
#' @param ori
#' A string or vector of strings of the 9-character ORI code (unique agency ID) for the desired agency.
#' @param key
#' A string containing your FBI's Crime Data Explorer API key
#'
#' @return
#' A data.frame with agency-level UCR crime data for selected agency.
#' @export
#'
#' @examples
#' \dontrun{
#' get_agency_crime("AK0010100")
#' }
get_agency_crime <- function(ori,
                             key = get_api_key()) {

  url <- paste0("https://api.usa.gov/crime/fbi/sapi/",
                "api/summarized/agencies/",
                ori, "/",
                "offenses/",
                1985, "/",
                2018,
                "?api_key=",
                key)

  response <- url_to_dataframe(url)
  response <- data.table::as.data.table(response)
  response <- srs_long_to_wide(response)
  response <- clean_column_names(response)
  response <- as.data.frame(response)
  return(response)
}

#' Get state- or national-level estimated crime counts from the UCR's Offenses Known
#' and Clearances by Arrest data set.
#'
#' @family UCR crime functions
#' @inheritParams get_agency_crime
#'
#' @param state_abb
#' String or vector of strings input for state abbreviation(s) to get data for.
#' If NULL (default) returns national data.
#'
#' @return
#' A data.frame with state-level estimated UCR crime data for selected state
#' @export
#'
#' @examples
#'\dontrun{
#' get_estimated_crime("CA")
#' }
get_estimated_crime <- function(state_abb = NULL,
                                key = get_api_key()) {

  url_section <- combine_url_section("estimates",
                                     ori = NULL,
                                     state_abb = state_abb,
                                     region_name = NULL)
  url <- make_url(url_section, 1979, key)


  data <- url_to_dataframe(url)
  data <- clean_column_names(data)
  data <- data.table::setorder(data, -"year")
  data$state <- make_state(data$state_abbr)
  rownames(data) <- 1:nrow(data)
  data <- data[, c(2, 16, 1, 3, 4:15)]

  return(data)
}

#' Get state-, region- or national-level estimated arson data from the UCR's Offenses Known and Clearances by Arrest data set.
#'
#' @family UCR crime functions
#' @inheritParams get_agency_crime
#'
#' @param state_abb
#' String or vector of strings input for state abbreviation(s) to get data for.
#' If NULL (default) returns national data. If `state_abb` and `region` both have values, will use return `region` input data.
#' @param region
#' String or vector of strings input for region name(s) to get data for. Please run `list_regions()`  to see all possible variables.
#'
#' @return
#' A data.frame with state-level estimated UCR arson data for selected state
#' @export
#'
#' @examples
#' \dontrun{
#' get_estimated_arson("CA")
#' }
get_estimated_arson <- function(state_abb = NULL,
                                region = NULL,
                                key = get_api_key()) {

  url_section <- combine_url_section("arson",
                                     ori = NULL,
                                     state_abb = state_abb,
                                     region_name = region)
  url <- make_url(url_section, 1975, key)


  data <- url_to_dataframe(url)
  data <- clean_column_names(data)
  return(data)
}


