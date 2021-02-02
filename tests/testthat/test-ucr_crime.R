

test_that("State-level UCR crime estimates works", {
  skip_on_cran()
  us_estimated_crime <- get_estimated_crime()
  us_estimated_crime$state_id <- NULL
  us_estimated_crime$arson <- NULL
  us_estimated_crime$state <- NULL
  us_estimated_crime_test <- state_level_estimated_crime[state_level_estimated_crime$state %in%
                                                           "United States", ]
  us_estimated_crime_test$state <- NULL
  rownames(us_estimated_crime_test) <- 1:nrow(us_estimated_crime_test)
  us_estimated_crime$state_abbr <- ""


  ca_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "CA", ]
  rownames(ca_estimate_crime) <- 1:nrow(ca_estimate_crime)
  pa_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "PA", ]
  rownames(pa_estimate_crime) <- 1:nrow(pa_estimate_crime)
  oh_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "OH", ]
  rownames(oh_estimate_crime) <- 1:nrow(oh_estimate_crime)
  me_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "ME", ]
  rownames(me_estimate_crime) <- 1:nrow(me_estimate_crime)
  ny_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "NY", ]
  rownames(ny_estimate_crime) <- 1:nrow(ny_estimate_crime)
  wy_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "WY", ]
  rownames(wy_estimate_crime) <- 1:nrow(wy_estimate_crime)
  dc_estimate_crime <- state_level_estimated_crime[state_level_estimated_crime$state_abbr %in%
                                                     "DC", ]
  rownames(dc_estimate_crime) <- 1:nrow(dc_estimate_crime)
  dc_estimate_crime$state <- NA
  dc_estimate_crime$state <- as.character(dc_estimate_crime$state)





  # Removes arson column and state number column
  expect_equal(get_estimated_crime(state_abb = "CA")[, -c(3, 16)],
               ca_estimate_crime)
  expect_equal(get_estimated_crime(state_abb = "PA")[, -c(3, 16)],
               pa_estimate_crime)
  expect_equal(get_estimated_crime(state_abb = "OH")[, -c(3, 16)],
               oh_estimate_crime)
  expect_equal(get_estimated_crime(state_abb = "ME")[, -c(3, 16)],
               me_estimate_crime)
  expect_equal(get_estimated_crime(state_abb = "NY")[, -c(3, 16)],
               ny_estimate_crime)
  expect_equal(get_estimated_crime(state_abb = "WY")[, -c(3, 16)],
               wy_estimate_crime)
  expect_equal(get_estimated_crime(state_abb = "DC")[, -c(3, 16)],
               dc_estimate_crime)
})


test_that("Agency-level UCR data works", {
  skip_on_cran()
  expect_equal(get_agency_crime('AK0010100')[, crime_matching_columns],
               read.csv_system_file('AK0010100_combined.csv'))
  expect_equal(get_agency_crime('AL0210200')[, crime_matching_columns],
               read.csv_system_file('AL0210200_combined.csv'))
  expect_equal(get_agency_crime('AR0600500')[, crime_matching_columns],
               read.csv_system_file('AR0600500_combined.csv'))
  expect_equal(get_agency_crime('AZ0072300')[, crime_matching_columns],
               read.csv_system_file('AZ0072300_combined.csv'))
  expect_equal(get_agency_crime('CA0190000')[, crime_matching_columns],
               read.csv_system_file('CA0190000_combined.csv'))
  expect_equal(get_agency_crime('CA0194200')[, crime_matching_columns],
               read.csv_system_file('CA0194200_combined.csv'))
  expect_equal(get_agency_crime('CA0250000')[, crime_matching_columns],
               read.csv_system_file('CA0250000_combined.csv'))
  expect_equal(get_agency_crime('CA0371100')[, crime_matching_columns],
               read.csv_system_file('CA0371100_combined.csv'))
  expect_equal(get_agency_crime('CA0380100')[, crime_matching_columns],
               read.csv_system_file('CA0380100_combined.csv'))
  expect_equal(get_agency_crime('CA0401300')[, crime_matching_columns],
               read.csv_system_file('CA0401300_combined.csv'))
  expect_equal(get_agency_crime('CA0421200')[, crime_matching_columns],
               read.csv_system_file('CA0421200_combined.csv'))
  expect_equal(get_agency_crime('CA0431300')[, crime_matching_columns],
               read.csv_system_file('CA0431300_combined.csv'))
  expect_equal(get_agency_crime('CA0470500')[, crime_matching_columns],
               read.csv_system_file('CA0470500_combined.csv'))
  expect_equal(get_agency_crime('GA1440000')[, crime_matching_columns],
               read.csv_system_file('GA1440000_combined.csv'))
  expect_equal(get_agency_crime('GA1560100')[, crime_matching_columns],
               read.csv_system_file('GA1560100_combined.csv'))
  expect_equal(get_agency_crime('IL0460000')[, crime_matching_columns],
               read.csv_system_file('IL0460000_combined.csv'))
  expect_equal(get_agency_crime('IL0630400')[, crime_matching_columns],
               read.csv_system_file('IL0630400_combined.csv'))
  expect_equal(get_agency_crime('IL0901900')[, crime_matching_columns],
               read.csv_system_file('IL0901900_combined.csv'))
  expect_equal(get_agency_crime('ILCPD0000')[, crime_matching_columns],
               read.csv_system_file('ILCPD0000_combined.csv'))
  expect_equal(get_agency_crime('IN0200200')[, crime_matching_columns],
               read.csv_system_file('IN0200200_combined.csv'))
  expect_equal(get_agency_crime('IN0410800')[, crime_matching_columns],
               read.csv_system_file('IN0410800_combined.csv'))
  expect_equal(get_agency_crime('IN0420100')[, crime_matching_columns],
               read.csv_system_file('IN0420100_combined.csv'))
  expect_equal(get_agency_crime('IN0490400')[, crime_matching_columns],
               read.csv_system_file('IN0490400_combined.csv'))
  expect_equal(get_agency_crime('INIPD0000')[, crime_matching_columns],
               read.csv_system_file('INIPD0000_combined.csv'))
  expect_equal(get_agency_crime('KY0420700')[, crime_matching_columns],
               read.csv_system_file('KY0420700_combined.csv'))
  expect_equal(get_agency_crime('LA0190100')[, crime_matching_columns],
               read.csv_system_file('LA0190100_combined.csv'))
  expect_equal(get_agency_crime('MA0090700')[, crime_matching_columns],
               read.csv_system_file('MA0090700_combined.csv'))
  expect_equal(get_agency_crime('MN0730600')[, crime_matching_columns],
               read.csv_system_file('MN0730600_combined.csv'))
  expect_equal(get_agency_crime('MO0956200')[, crime_matching_columns],
               read.csv_system_file('MO0956200_combined.csv'))
  expect_equal(get_agency_crime('MS0020000')[, crime_matching_columns],
               read.csv_system_file('MS0020000_combined.csv'))
  expect_equal(get_agency_crime('MS0250000')[, crime_matching_columns],
               read.csv_system_file('MS0250000_combined.csv'))
  expect_equal(get_agency_crime('NB0710000')[, crime_matching_columns],
               read.csv_system_file('NB0710000_combined.csv'))
  expect_equal(get_agency_crime('NC0030000')[, crime_matching_columns],
               read.csv_system_file('NC0030000_combined.csv'))
  expect_equal(get_agency_crime('NC0460200')[, crime_matching_columns],
               read.csv_system_file('NC0460200_combined.csv'))
  expect_equal(get_agency_crime('NC0600100')[, crime_matching_columns],
               read.csv_system_file('NC0600100_combined.csv'))
  expect_equal(get_agency_crime('NC0670300')[, crime_matching_columns],
               read.csv_system_file('NC0670300_combined.csv'))
  expect_equal(get_agency_crime('NJ0024600')[, crime_matching_columns],
               read.csv_system_file('NJ0024600_combined.csv'))
  expect_equal(get_agency_crime('NJ0121300')[, crime_matching_columns],
               read.csv_system_file('NJ0121300_combined.csv'))
  expect_equal(get_agency_crime('NJ0151400')[, crime_matching_columns],
               read.csv_system_file('NJ0151400_combined.csv'))
  expect_equal(get_agency_crime('NV0020100')[, crime_matching_columns],
               read.csv_system_file('NV0020100_combined.csv'))
  expect_equal(get_agency_crime('NY0290000')[, crime_matching_columns],
               read.csv_system_file('NY0290000_combined.csv'))
  expect_equal(get_agency_crime('NY0303000')[, crime_matching_columns],
               read.csv_system_file('NY0303000_combined.csv'))
  expect_equal(get_agency_crime('NY0350200')[, crime_matching_columns],
               read.csv_system_file('NY0350200_combined.csv'))
  expect_equal(get_agency_crime('NY0510100')[, crime_matching_columns],
               read.csv_system_file('NY0510100_combined.csv'))
  expect_equal(get_agency_crime('NY0513600')[, crime_matching_columns],
               read.csv_system_file('NY0513600_combined.csv'))
  expect_equal(get_agency_crime('NY0560100')[, crime_matching_columns],
               read.csv_system_file('NY0560100_combined.csv'))
  expect_equal(get_agency_crime('OH0350400')[, crime_matching_columns],
               read.csv_system_file('OH0350400_combined.csv'))
  expect_equal(get_agency_crime('OH0451300')[, crime_matching_columns],
               read.csv_system_file('OH0451300_combined.csv'))
  expect_equal(get_agency_crime('OH0691100')[, crime_matching_columns],
               read.csv_system_file('OH0691100_combined.csv'))
  expect_equal(get_agency_crime('OK0170100')[, crime_matching_columns],
               read.csv_system_file('OK0170100_combined.csv'))
  expect_equal(get_agency_crime('OR0030900')[, crime_matching_columns],
               read.csv_system_file('OR0030900_combined.csv'))
  expect_equal(get_agency_crime('PA0060500')[, crime_matching_columns],
               read.csv_system_file('PA0060500_combined.csv'))
  expect_equal(get_agency_crime('PA0115800')[, crime_matching_columns],
               read.csv_system_file('PA0115800_combined.csv'))
  expect_equal(get_agency_crime('PA0360700')[, crime_matching_columns],
               read.csv_system_file('PA0360700_combined.csv'))
  expect_equal(get_agency_crime('PA0406500')[, crime_matching_columns],
               read.csv_system_file('PA0406500_combined.csv'))
  expect_equal(get_agency_crime('PAPEP0000')[, crime_matching_columns],
               read.csv_system_file('PAPEP0000_combined.csv'))
  expect_equal(get_agency_crime('SC014FC00')[, crime_matching_columns],
               read.csv_system_file('SC014FC00_combined.csv'))
  expect_equal(get_agency_crime('SC0231000')[, crime_matching_columns],
               read.csv_system_file('SC0231000_combined.csv'))
  expect_equal(get_agency_crime('SC041SP00')[, crime_matching_columns],
               read.csv_system_file('SC041SP00_combined.csv'))
  expect_equal(get_agency_crime('SD0620200')[, crime_matching_columns],
               read.csv_system_file('SD0620200_combined.csv'))
  expect_equal(get_agency_crime('TX0151100')[, crime_matching_columns],
               read.csv_system_file('TX0151100_combined.csv'))
  expect_equal(get_agency_crime('TX0770100')[, crime_matching_columns],
               read.csv_system_file('TX0770100_combined.csv'))
  expect_equal(get_agency_crime('TX1010000')[, crime_matching_columns],
               read.csv_system_file('TX1010000_combined.csv'))
  expect_equal(get_agency_crime('TX2270100')[, crime_matching_columns],
               read.csv_system_file('TX2270100_combined.csv'))
  expect_equal(get_agency_crime('TX2300300')[, crime_matching_columns],
               read.csv_system_file('TX2300300_combined.csv'))
  expect_equal(get_agency_crime('TXSPD0000')[, crime_matching_columns],
               read.csv_system_file('TXSPD0000_combined.csv'))
  expect_equal(get_agency_crime('VA0510100')[, crime_matching_columns],
               read.csv_system_file('VA0510100_combined.csv'))
  expect_equal(get_agency_crime('VA0820500')[, crime_matching_columns],
               read.csv_system_file('VA0820500_combined.csv'))
  expect_equal(get_agency_crime('WI0540800')[, crime_matching_columns],
               read.csv_system_file('WI0540800_combined.csv'))
  expect_equal(get_agency_crime('WV0140500')[, crime_matching_columns],
               read.csv_system_file('WV0140500_combined.csv'))

})
