#' Get agency-, state-, region-, or national-level police staffing data.
#'
#' @inheritParams get_estimated_arson
#' @inheritParams get_agency_crime
#'
#' @return
#' A data.frame with columns for annual number of employees and officers (also broken up by gender).
#' @export
#'
#' @examples
#' \dontrun{
#' # Gets only Oakland Police Department in California
#' get_police_employment("CA0010900")
#'
#' # Gets California state-level estimates
#' get_police_employment(state_abb = "CA")
#'
#' # Gets national-level estimates
#' get_police_employment()
#' }
get_police_employment <- function(ori = NULL,
                                  state_abb = NULL,
                                  region = NULL,
                                  key = get_api_key()) {

  data_type <- "police-employment"
  url_section <- combine_url_section(data_type, ori, region, state_abb)
  url <- make_url(url_section, 1985, key)

  data <- url_to_dataframe(url)
  data <- clean_column_names(data)

  names(data) <- gsub("_ct$", "", names(data))
  names(data) <- gsub("^civilian$", "civilians_total", names(data))
  names(data) <- gsub("_civilian$", "_civilians", names(data))
  names(data) <- gsub("_officer$", "_officers", names(data))
  names(data) <- gsub("total_pe$", "total_police_employees", names(data))
  names(data) <- gsub("pe_ct_per_1000$", "police_employees_per_1000_pop", names(data))
  data$officers_total  <- data$female_officers + data$male_officers
  data$employees_total <- data$officers_total + data$civilians_total


  return(data)
}
