nibrs_offenses <- c("aggravated-assault",
                    "all-offenses",
                    "all-other-larceny",
                    "all-other-offenses",
                    "animal-cruelty",
                    "arson",
                    "assault-offenses",
                    "assisting-or-promoting-prostitution",
                    "bad-checks",
                    "betting",
                    "bribery",
                    "burglary-breaking-and-entering",
                    "counterfeiting-forgery",
                    "credit-card-automated-teller-machine-fraud",
                    "crime-against-person",
                    "crime-against-property",
                    "crime-against-society",
                    "curfew-loitering-vagrancy-violations",
                    "destruction-damage-vandalism-of-property",
                    "driving-under-the-influence",
                    "drug-equipment-violations", # GIVE SAME RESULTS AS NARCOTIC OFFENSES
                    "drug-violations", # GIVES EQUIPMENT VIOLATIONS RESULT
                    "drugs-narcotic-offenses", # GIVE SAME RESULTS AS EQUIPMENT VIOLATIONS
                    "drunkenness",
                    "embezzlement",
                    "extortion-blackmail",
                    "false-pretenses-swindle-confidence-game",
                    "fondling",
                    "fraud-offenses",
                    "gambling-equipment-violation",
                    "gambling-offenses",
                    "hacking-computer-invasion",
                    "homicide-offenses",
                    "human-trafficking-commerical-involuntary-servitude",
                    "human-trafficking-commerical-sex-acts",
                    "human-trafficking-offenses",
                    "identity-theft",
                    "impersonation",
                    "incest",
                    "intimidation",
                    "justifiable-homicide",
                    "kidnapping-abduction",
                    "larceny-theft-offenses",
                    "liquor-law-violations",
                    "motor-vehicle-theft",
                    "murder-and-nonnegligent-manslaughter",
                    "negligent-manslaughter",
                    #  "not-specified",
                    "operating-promoting-assiting-gambling",
                    "peeping-tom,pocket-picking",
                    "pornography-obscence-material",
                    "prostitution",
                    "prostitution-offenses",
                    "purchasing-prostitution",
                    "purse-snatching",
                    "rape",
                    "robbery",
                    "sex-offenses",
                    "sex-offenses-non-forcible",
                    "sexual-assult-with-an-object",
                    "shoplifting",
                    "simple-assault",
                    "sodomy",
                    "sports-tampering",
                    "statutory-rape",
                    "stolen-property-offenses",
                    "theft-from-building",
                    "theft-from-coin-operated-machine-or-device",
                    "theft-from-motor-vehicle",
                    "theft-of-motor-vehicle-parts-or-accessories",
                    "weapon-law-violation",
                    "welfare-fraud",
                    "wire-fraud")


nibrs_victim_variables <- sort(c("age",
                                 "count",
                                 "ethnicity",
                                 "race",
                                 "sex",
                                 "relationship",
                                 "location"))

nibrs_offender_variables <- c("age",
                              "count",
                              "ethnicity",
                              "race",
                              "sex")

nibrs_offense_variables <- sort(c("count",
                                  "weapons",
                                  "linkedoffense",
                                  "suspectusing",
                                  "criminal_activity",
                                  "property_recovered",
                                  "property_stolen",
                                  "bias"))


ucr_arrest_offenses <- c("aggravated-assault",
                         "all-other-offenses",
                         "arson",
                         "burglary",
                         "curfew",
                         "disorderly-conduct",
                         "dui",
                         "drug-grand-total",
                         "drug-possession-marijuana",
                         "drug-possession-opium",
                         "drug-possession-other",
                         "drug-possession-subtotal",
                         "drug-possession-synthetic",
                         "drug-sales-marijuana",
                         "drug-sales-opium",
                         "drug-sales-other",
                         "drug-sales-subtotal",
                         "drug-sales-synthetic",
                         "drunkenness",
                         "embezzlement",
                         "forgery",
                         "fraud",
                         "gambling-all-other",
                         "gambling-bookmaking",
                         "gambling-numbers",
                         "gambling-total",
                         "human-trafficking-commercial",
                         "human-trafficking-servitude",
                         "larceny",
                         "liqour-laws", # MISPELLED
                         "motor-vehcile-theft", # MISPELLED
                         "murder",
                         "offense-against-family",
                         "prostitution",
                         "prostitution-assisting",
                         "prostitution-prostitution",
                         "prostitution-purchasing",
                         "rape",
                         "robbery",
                         "runaway",
                         "sex-offenses",
                         "simple-assault",
                         "stolen-property",
                         "suspicion",
                         "vagrancy",
                         "vandalism",
                         "weapons")

regions <- c("U.S. Territories",
             "Northeast",
             "Midwest",
             "South",
             "West",
             "Other")

# usethis::use_data(nibrs_offender_variables,
#                   nibrs_offense_variables,
#                   nibrs_offenses,
#                   nibrs_victim_variables,
#                   regions,
#                   ucr_arrest_offenses)


#' All available variables for NIBRS offender data.
#'
#' The `get_nibrs_offender()` function requires a string input for
#' which variable you want data on. This is a vector of strings
#' with all possible variables.
#'
#'
#' @format A vector with 5 elements:
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"nibrs_offender_variables"

#' All available variables for NIBRS offense data.
#'
#' The `get_nibrs_offense()` function requires a string input for
#' which variable you want data on. This is a vector of strings
#' with all possible variables.
#'
#' @format A vector with 8 elements:
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"nibrs_offense_variables"

#' All offenses available to get NIBRS data for.
#'
#' This is a vector of strings with the names of every offense available
#' for the NIBRS data functions.
#'
#' @format A vector with 72 elements:
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"nibrs_offenses"

#' All available variables for NIBRS victim data.
#'
#' The `get_nibrs_victim()` function requires a string input for
#' which variable you want data on. This is a vector of strings
#' with all possible variables.
#'
#' @format A vector with 7 elements:
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"nibrs_victim_variables"

#' Regions available to get estimated UCR and NIBRS data for.
#'
#' Some functions let you get regional estimated data and require a
#' string input for which region you want data from. This is a vector
#' of strings for the six regions available.
#'
#' @format A vector with 6 elements:
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"regions"

#' All offenses available to get NIBRS data for.
#'
#' This is a vector of strings with the names of every offense available
#' to get arrest data for from the `get_arrest_demographics()` function.
#'
#' @format A vector with 47 elements:
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"ucr_arrest_offenses"


#' All agencies included in the FBI's Crime Data Explorer API.
#'
#' A dataset containing information about the 18,459 agencies included in the
#' FBI's API.
#'
#' @format A data frame with 18459 rows and 13 variables:
#' \describe{
#'   \item{ori}{9-digit unique ID number for the agency}
#'   \item{agency_name}{Agency name}
#'   \item{agency_type_name}{Type of department - e.g city, county, school}
#'   \item{state_name}{State name}
#'   \item{state_abbr}{State abbreviation}
#'   \item{division_name}{Census division name}
#'   \item{region_name}{Census region name}
#'   \item{region_desc}{Census region code}
#'   \item{county_name}{County name}
#'   \item{nibrs}{Boolean on whether the agency reports to NBIRS}
#'   \item{latitude}{Agency latitude}
#'   \item{longitude}{Agency longitude}
#'   \item{nibrs_start_date}{Date agency started reporting to NIBRS. NULL if
#'   the agency does not report to NIBRS}
#'
#' }
#' @source \url{https://crime-data-explorer.fr.cloud.gov/api}
"fbi_api_agencies"
