get_nibrs <- function(key = get_api_key(),
                      offense,
                      variable,
                      ori,
                      region_name,
                      state_abb,
                      type) {

  data_type <- paste0("data/nibrs/", offense, "/", type)

  url_section <- combine_url_section(data_type, ori, region_name, state_abb)
  api_url <- paste0("https://api.usa.gov/crime/fbi/sapi/api/",
                    url_section, "/", variable, "/?api_key=", key)


  data <- url_to_dataframe(api_url)
  data <- clean_column_names(data)
  data$offense <- offense
  return(data)
}

#' Gets victim-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' @inheritParams get_agency_crime
#' @inheritParams get_estimated_arson
#' @family NIBRS functions
#'
#' @param offense
#' A string or vector of strings with the offenses you want to scrape. If
#' input is 'all' (default), returns data for all offenses. Please run `list_nibrs_offenses()` to see all possible offenses.
#' @param variable
#' A string with the variable you want the data to be in. This is essentially the units you want returned (number of victims by age, race, sex, etc.). By default returns total count of victims. Please run `list_nibrs_victim_variables()`  to see all possible variables.
#'
#' @return
#' A data.frame of NIBRS victim data.
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_victim(ori = "DE0010100")
#' }
get_nibrs_victim <- function(key = get_api_key(),
                             offense     = "robbery",
                             variable    = "race",
                             ori         = NULL,
                             region      = NULL,
                             state_abb   = NULL) {

  data <- get_nibrs(key         = key,
                    offense     = offense,
                    variable    = variable,
                    ori         = ori,
                    region_name = region,
                    state_abb   = state_abb,
                    type        = "victim")

  return(data)
}

#' Gets offender-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' @inheritParams get_nibrs_victim
#' @family NIBRS functions
#'
#' @param variable
#' A string with the variable you want the data to be in. This is essentially the units you want returned (number of offenders by age, race, sex, etc.). By default returns total count of offenders Please run `list_nibrs_offender_variables()`  to see all possible variables.
#'
#' @return
#' A data.frame of NIBRS offender data.
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_offender(ori = "DE0010100")
#' }
get_nibrs_offender <- function(key = get_api_key(),
                               offense     = "all",
                               variable    = "count",
                               ori         = NULL,
                               region      = NULL,
                               state_abb   = NULL) {

  data <- get_nibrs(key         = key,
                    offense     = offense,
                    variable    = variable,
                    ori         = ori,
                    region_name = region,
                    state_abb   = state_abb,
                    type        = "offender")

  return(data)
}

#' Gets offense-level data from the FBI's National Incident-Based Reporting System (NIBRS)
#'
#' @inheritParams get_nibrs_victim
#' @family NIBRS functions
#'
#' @param variable
#' A string with the variable you want the data to be in. This is essentially the units you want returned (number of offenses by weapon used, race, bias motivation, etc.). By default returns total count of offenders Please run `list_nibrs_offense_variables()`  to see all possible variables.
#'
#' @return
#' A data.frame of NIBRS offense data.
#' @export
#'
#' @examples
#' \dontrun{
#' get_nibrs_offense(ori = "DE0010100")
#' }
get_nibrs_offense <- function(key = get_api_key(),
                              offense     = "all",
                              variable    = "count",
                              ori         = NULL,
                              region      = NULL,
                              state_abb   = NULL) {

  data <- get_nibrs(key         = key,
                    offense     = offense,
                    variable    = variable,
                    ori         = ori,
                    region_name = region,
                    state_abb   = state_abb,
                    type        = "offense")

  return(data)
}

#' Return a vector of all offenses in the NIBRS data.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector containing all permissible offenses to input to `get_nibrs_offenses()`,
#' `get_nibrs_victim()`, and `get_nibrs_offender()`.`
#' @export
#'
#' @examples
#' list_nibrs_offenses()
list_nibrs_offenses <- function() {
  return(fbi::nibrs_offenses)
}


#' Returns a vector of all `variable` parameter options for `get_nibrs_victim()`.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector of all `variable` parameter options for `get_nibrs_victim()`.
#' @export
#'
#' @examples
#' list_nibrs_victim_variables()
list_nibrs_victim_variables <- function() {
  return(fbi::nibrs_victim_variables)
}

#' Returns a vector of all `variable` parameter options for `get_nibrs_offender()`.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector of all `variable` parameter options for `get_nibrs_offender()`.
#' @export
#'
#' @examples
#' list_nibrs_offender_variables()
list_nibrs_offender_variables <- function() {
  return(fbi::nibrs_offender_variables)
}

#' Returns a vector of all `variable` parameter options for `get_nibrs_offense()`.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector of all `variable` parameter options for `get_nibrs_offense()`.
#' @export
#'
#' @examples
#' list_nibrs_offender_variables()
list_nibrs_offense_variables <- function() {
  return(fbi::nibrs_offense_variables)
}


#' Return a vector of all offenses in the UCR Arrest data.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector containing all permissible offenses to input to `get_agency_arrest()`.
#' @export
#'
#' @examples
#' list_ucr_arrest_offenses()
list_ucr_arrest_offenses <- function() {
  return(fbi::ucr_arrest_offenses)
}

#' Return a vector of all regions choices.
#'
#' @family NIBRS functions
#'
#' @return
#' A vector of all regions choices.
#' @export
#'
#' @examples
#' list_regions()
list_regions <- function() {
  return(fbi::regions)
}
