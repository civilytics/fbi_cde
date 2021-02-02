make_state <- function(state_abb) {
  state <- datasets::state.name[match(tolower(state_abb),
                                      tolower(datasets::state.abb))]
  state[tolower(state_abb) == "CZ"] <- "canal zone"
  state[tolower(state_abb) == "DC"] <- "district of columbia"
  state[tolower(state_abb) == "GU"] <- "guam"
  state[tolower(state_abb) == "PR"] <- "puerto rico"
  return(state)
}

read.csv_system_file <- function(file) {
  data <- utils::read.csv(system.file("testdata",
                                      file,
                                      package = "fbi"))
  data$ori <- as.character(data$ori)
  rownames(data) <- 1:nrow(data)
  return(data)
}

clean_column_names <- function(.data) {
  names(.data) <- tolower(names(.data))
  names(.data) <- gsub("-", "_", names(.data))
  names(.data) <- gsub("^data_year$", "year", names(.data))


  # Fix arrest column names
  names(.data) <- gsub("^disorderly$", "disorderly_conduct", names(.data))
  names(.data) <- gsub("^driving$", "dui", names(.data))
  names(.data) <- gsub("^drug_abuse_gt$", "drug_grand_total", names(.data))
  names(.data) <- gsub("^drug_poss_m$", "drug_poss_marijuana", names(.data))
  names(.data) <- gsub("^drug_sales_m$", "drug_sales_marijuana", names(.data))
  names(.data) <- gsub("^g_all$", "gambling_all_others", names(.data))
  names(.data) <- gsub("^g_b$", "gambling_bookmaking", names(.data))
  names(.data) <- gsub("^g_n$", "gambling_numbers", names(.data))
  names(.data) <- gsub("^g_t$", "gambling_total", names(.data))
  names(.data) <- gsub("^ht_c_s_a$", "human_trafficking_commercial_sex_traffic", names(.data))
  names(.data) <- gsub("^ht_i_s$", "human_trafficking_servitude", names(.data))
  names(.data) <- gsub("^liquor$", "liquor_laws", names(.data))
  names(.data) <- gsub("^mvt$", "motor_vehicle_theft", names(.data))
  names(.data) <- gsub("^offense_family$", "offense_against_family", names(.data))
  names(.data) <- gsub("^prostitution$", "prostitution_total", names(.data))
  names(.data) <- gsub("^prostitution_a_p_p$", "prostitution_assisting", names(.data))
  names(.data) <- gsub("^prostitution_p$", "prostitution_performing", names(.data))
  names(.data) <- gsub("^prostitution_p_p$", "prostitution_purchasing", names(.data))
  names(.data) <- gsub("^sex_offense$", "other_sex_offenses", names(.data))



  .data$csv_header <- NULL
  return(.data)
}


url_to_dataframe <- function(url) {
  useragent <- paste0(
    "Mozilla/5.0 (compatible; a bot using the R fbi",
    " package; https://github.com/jacobkap/fbi/)")

  response <- httr::GET(url = url,
                        httr::user_agent(useragent))


  if (response$status_code %in% 200) {
    response <- jsonlite::fromJSON(rawToChar(response$content))
    response <- response$results
  }
  return(response)
}



prep_ucr_crime_test <- function(file_name, type = "crime") {
  data <- utils::read.csv(system.file("testdata",
                                      file_name,
                                      package = "fbi"))
  ori <- strsplit(gsub(".csv", "", file_name), "-")[[1]]
  ori <- gsub("_police", "", ori)
  crime <- paste0(ori[2:length(ori)], collapse = "_")
  crime <- paste0(crime, "_")
  ori <- ori[1]


  if (type == "police") {
    names(data) <- c("Year", "Label", "Value")
    crime <- ""
  }
  data <- stats::reshape(data,
                         idvar = "Year",
                         timevar = "Label",
                         direction = "wide")
  names(data) <- gsub("^Value\\.", "", names(data))
  names(data)[2:ncol(data)] <- paste0(crime, names(data)[2:ncol(data)])
  data$ori <- ori
  names(data) <- tolower(names(data))
  names(data) <- gsub("_reported$", "_actual", names(data))
  names(data) <- gsub(" ", "_", names(data))
  data <- data.frame(data)
  rownames(data) <- 1:nrow(data)

  if (type == "police") {
    data$male_total      <- data$male_civilians   + data$male_officers
    data$female_total    <- data$female_civilians + data$female_officers
    data$civilians_total <- data$female_civilian  + data$male_civilian
    data$officers_total  <- data$female_officer   + data$male_officer
    data$employees_total <- data$civilians_total  + data$officers_total

  }

  return(data)
}

combine_url_section <- function(data_type, ori, region_name, state_abb) {
  url_section <- paste0(data_type, "/national")
  if (!is.null(ori)) {
    url_section <- paste0(data_type, "/agencies/")
    url_section <- paste0(url_section, ori)
  } else if (!is.null(region_name)) {
    url_section <- paste0(data_type, "/regions/")
    url_section <- paste0(url_section, region_name)
  } else if (!is.null(state_abb)) {
    url_section <- paste0(data_type, "/states/")
    url_section <- paste0(url_section, state_abb)
  }
  return(url_section)
}


make_url <- function(url_section,
                     start_year,
                     key) {

  url <- paste0("https://api.usa.gov/crime/fbi/sapi/api/",
                url_section,
                "/",
                start_year,
                "/2018?API_KEY=",
                key)
  return(url)
}

srs_long_to_wide <- function(.data) {
  .data <- data.table::melt(.data, id = c("ori",
                                          "state_abbr",
                                          "data_year",
                                          "offense"))
  .data <- data.table::dcast(.data,
                             formula = ori + state_abbr + data_year ~ offense + variable,
                             measure.var = c("value"),
                             fun.aggregate = mean,
                             fill = NA)
  return(.data)
}
