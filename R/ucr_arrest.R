#' Get arrest data from the UCR's Arrests by Age, Sex, and Race data set.
#'
#' @inheritParams get_agency_crime
#' @inheritParams get_estimated_arson
#'
#' @param monthly
#' @param end_year = last year of the collection to get, defaults to the prior calendar year from today
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
                             end_year = make_year(),
                             key = get_api_key()) {

  url_part <- "data/arrest"
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

  if (end_year == make_year()) {
    end_year <- make_year() - 2
  }

  url <- make_url(url_section, start_year = start_year,
                  end_year = end_year,  key)
  url <- gsub("offense/agencies", "offense", url)
  url <- gsub("national", "national/offense", url)

  if(is.null(ori) & !is.null(state_abb)) {
    url <- gsub("/states", "states/offense/", url)
  } else if (is.null(ori) & !is.null(region)) {
    url <- gsub("/regions", "regions/offense/", url)
  }


  data <- url_to_dataframe(url)
  data <- clean_column_names(data)

  if (!is.null(ori)) {
    data$ori <- ori
    data <- data[, c("ori", "year",
                     names(data)[which(!names(data) %in% c("year", "ori"))])]
  } else if (!is.null(state_abb)) {
    data$state_abb <- state_abb
    data <- data[, c("state_abb", "year",
                     names(data)[which(!names(data) %in% c("year", "state_abb"))])]
  } else if (!is.null(region)) {
    data <- data[, c("region_name", "year", "region_code",
                     names(data)[which(!names(data) %in% c("region_name", "year", "region_code"))])]
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
                                    end_year = make_year(),
                                    key = get_api_key()) {

  # TODO: Fix API calls to get state and region accurately

  url_section <- combine_url_section("data/arrest",
                                     ori = ori,
                                     state_abb = state_abb,
                                     region_name = region)

  data <- data.frame()
  for (arrest_variable in c("male", "female", "race")) {
    url_section_temp <- paste0(url_section, "/", offense, "/", arrest_variable)
    url <- make_url(url_section_temp, start_year = 1985, end_year = end_year, key)
    if (!is.null(ori)) {
      url <- gsub("offense/agencies", "offense", url)
    }


    # https://api.usa.gov/crime/fbi/sapi/api/arrest/states/MT//race/1988/1995?API_KEY=iiHnOKfno2Mgkt5AynpvPpUQTEyxE77jo1RU8PIv

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

