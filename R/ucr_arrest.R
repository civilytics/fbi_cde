#' Get arrest data from the UCR's Arrests by Age, Sex, and Race data set.
#'
#' @inheritParams get_agency_crime
#' @inheritParams get_estimated_arson
#'
#' @param monthly
#' If TRUE (not default), returns data as monthly units. Otherwise returns annual data.
#'
#' @return
#' A data.frame with the number of arrests for each crime-year in the jurisdiction.
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_count(ori = "CA0010900")
#' get_arrest_count()
#' }
get_arrest_count <- function(ori = NULL,
                             state_abb = NULL,
                             region = NULL,
                             monthly = FALSE,
                             key = get_api_key()) {

  url_part <- "data/arrest/agencies/offense"
  if (is.null(ori) & is.null(state_abb) & is.null(region)) {
    url_part <- "data/arrest"
  }
  url_section <- combine_url_section(url_part,
                                     ori = ori,
                                     state_abb = state_abb,
                                     region_name = region)
  if (monthly) {
    start_year <- "monthly/1985"
  } else {
    start_year <- "all/1985"
  }

  url <- make_url(url_section, start_year, key)
  url <- gsub("offense/agencies", "offense", url)
  url <- gsub("national", "national/offense", url)

  data <- url_to_dataframe(url)
  data <- clean_column_names(data)

  if (!is.null(ori)) {
    data$ori <- ori
    data <- data[, c("ori", "year",
                     names(data)[which(!names(data) %in% c("year", "ori"))])]
  }
  data <- data[order(data$year, decreasing = TRUE), ]
  rownames(data) <- 1:nrow(data)
  return(data)
}


#' Get arrest data by arrestee demographic from the UCR's Arrests by Age, Sex, and Race data set.
#'
#' @inheritParams get_agency_crime
#' @inheritParams get_estimated_arson
#'
#' @param offense
#' A string or vector of strings with the offenses you want to scrape. If
#' input is 'all' (default), returns data for all offenses. Please run `list_ucr_arrest_offenses()` to see all possible offenses.
#'
#' @return
#' A data.frame with the number of arrests by arrestee demographic for each crime-year in the jurisdiction.
#' @export
#'
#' @examples
#' \dontrun{
#' get_arrest_demographics(ori = "CA0010900", offense = "robbery")
#' get_arrest_demographics(offense = "robbery")
#' }
get_arrest_demographics <- function(ori = NULL,
                                    state_abb = NULL,
                                    region = NULL,
                                    offense = "all",
                                    key = get_api_key()) {

  url_section <- combine_url_section("data/arrest",
                                     ori = ori,
                                     state_abb = state_abb,
                                     region_name = region)

  data <- data.frame()
  for (arrest_variable in c("male", "female", "race")) {
    url_section_temp <- paste0(url_section, "/", offense, "/", arrest_variable)
    url <- make_url(url_section_temp, 1985, key)
    url <- gsub("offense/agencies", "offense", url)

    temp <- url_to_dataframe(url)
    temp <- clean_column_names(temp)
    names(temp) <- gsub("range", "", names(temp))
    names(temp) <- paste0(offense, "_", arrest_variable, "_", names(temp))
    names(temp) <- gsub(".*year", "year", names(temp))
    names(temp) <- gsub("_race_", "_", names(temp))

    if (nrow(data) == 0) {
      data <- temp
    } else {
      data <- merge(data, temp, by = "year")
    }
  }
  if (!is.null(ori)) {
    data$ori <- ori
    data <- data[, c("ori", "year",
                     names(data)[which(!names(data) %in% c("year", "ori"))])]
  }
  data <- data[order(data$year, decreasing = TRUE), ]
  rownames(data) <- 1:nrow(data)
  return(data)
}

