load(system.file("testdata",
                 "state_level_estimated_crime.rda",
                 package = "fbi"))


police_matching_columns <- c("year",
                             "male_officers",
                             "female_officers",
                             "male_civilians",
                             "female_civilians",
                             "ori",
                             "male_total",
                             "female_total",
                             "civilians_total",
                             "officers_total",
                             "employees_total")

arrest_matching_columns <- c("ori",
                             "year",
                             "aggravated_assault",
                             "all_other_offenses",
                             "arson",
                             "burglary",
                             "curfew",
                             "disorderly_conduct",
                             "dui",
                             "drug_grand_total",
                             "drunkness",
                             "embezzlement",
                             "forgery",
                             "fraud",
                             "gambling_total",
                             "human_trafficking_commercial_sex_traffic",
                             "human_trafficking_servitude",
                             "larceny",
                             "liquor_laws",
                             "manslaughter",
                             "motor_vehicle_theft",
                             "murder",
                             "offense_against_family",
                             "prostitution_total",
                             "rape",
                             "robbery",
                             "other_sex_offenses",
                             "simple_assault",
                             "stolen_property",
                             "suspicion",
                             "vagrancy",
                             "vandalism",
                             "weapons")

crime_matching_columns <- c("year",
                            "ori",
                            "violent_crime_actual",
                            "violent_crime_cleared",
                            "homicide_actual",
                            "homicide_cleared",
                            # "rape_actual",
                            # "rape_cleared",
                            "robbery_actual",
                            "robbery_cleared",
                            "aggravated_assault_actual",
                            "aggravated_assault_cleared",
                            "property_crime_actual",
                            "property_crime_cleared",
                            "arson_actual",
                            "arson_cleared",
                            "burglary_actual",
                            "burglary_cleared",
                            "larceny_actual",
                            "larceny_cleared",
                            "motor_vehicle_theft_actual",
                            "motor_vehicle_theft_cleared")

fix_arrest_test <- function(ori) {
  real <- get_arrest_count(ori)[, arrest_matching_columns]
  test <- read.csv_system_file(paste0(ori, '_arrest_count.csv'))
  test <- test[test$year %in% real$year, ]
  rownames(test) <- 1:nrow(test)
  return(test)
}

#
#
