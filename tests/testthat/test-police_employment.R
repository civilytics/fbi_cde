
test_that("Agency-level UCR police employee data works", {
  skip_on_cran()
  expect_equal(get_police_employment('AL0100200')[, police_matching_columns],
               prep_ucr_crime_test('AL0100200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('AL0360000')[, police_matching_columns],
               prep_ucr_crime_test('AL0360000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('AL0610300')[, police_matching_columns],
               prep_ucr_crime_test('AL0610300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('AZ0072300')[, police_matching_columns],
               prep_ucr_crime_test('AZ0072300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0190000')[, police_matching_columns],
               prep_ucr_crime_test('CA0190000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0194200')[, police_matching_columns],
               prep_ucr_crime_test('CA0194200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0197500')[, police_matching_columns],
               prep_ucr_crime_test('CA0197500_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0331400')[, police_matching_columns],
               prep_ucr_crime_test('CA0331400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0371100')[, police_matching_columns],
               prep_ucr_crime_test('CA0371100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0380100')[, police_matching_columns],
               prep_ucr_crime_test('CA0380100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CA0431300')[, police_matching_columns],
               prep_ucr_crime_test('CA0431300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CO0030400')[, police_matching_columns],
               prep_ucr_crime_test('CO0030400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('CT0007600')[, police_matching_columns],
               prep_ucr_crime_test('CT0007600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('FL0010100')[, police_matching_columns],
               prep_ucr_crime_test('FL0010100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('FL0130000')[, police_matching_columns],
               prep_ucr_crime_test('FL0130000_police-employment-breakout.csv',
                                   type = 'police'))

  expect_equal(get_police_employment('FL0290000')[, police_matching_columns],
               prep_ucr_crime_test('FL0290000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('GA0600800')[, police_matching_columns],
               prep_ucr_crime_test('GA0600800_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('GA0601000')[, police_matching_columns],
               prep_ucr_crime_test('GA0601000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('GA0670800')[, police_matching_columns],
               prep_ucr_crime_test('GA0670800_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('GA1250100')[, police_matching_columns],
               prep_ucr_crime_test('GA1250100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('HI0020000')[, police_matching_columns],
               prep_ucr_crime_test('HI0020000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('ID0260100')[, police_matching_columns],
               prep_ucr_crime_test('ID0260100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('IL0165300')[, police_matching_columns],
               prep_ucr_crime_test('IL0165300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('IL0166M00')[, police_matching_columns],
               prep_ucr_crime_test('IL0166M00_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('IL0320100')[, police_matching_columns],
               prep_ucr_crime_test('IL0320100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('ILCPD0000')[, police_matching_columns],
               prep_ucr_crime_test('ILCPD0000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('IN0730000')[, police_matching_columns],
               prep_ucr_crime_test('IN0730000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('IN0920000')[, police_matching_columns],
               prep_ucr_crime_test('IN0920000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('IN376SP00')[, police_matching_columns],
               prep_ucr_crime_test('IN376SP00_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('INIPD0000')[, police_matching_columns],
               prep_ucr_crime_test('INIPD0000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('KS0040200')[, police_matching_columns],
               prep_ucr_crime_test('KS0040200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('KS0310200')[, police_matching_columns],
               prep_ucr_crime_test('KS0310200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('KS0460100')[, police_matching_columns],
               prep_ucr_crime_test('KS0460100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('KS0630300')[, police_matching_columns],
               prep_ucr_crime_test('KS0630300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('KS0950000')[, police_matching_columns],
               prep_ucr_crime_test('KS0950000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('LA0310000')[, police_matching_columns],
               prep_ucr_crime_test('LA0310000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MA0092900')[, police_matching_columns],
               prep_ucr_crime_test('MA0092900_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MA0130500')[, police_matching_columns],
               prep_ucr_crime_test('MA0130500_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MA314SP00')[, police_matching_columns],
               prep_ucr_crime_test('MA314SP00_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MD0160400')[, police_matching_columns],
               prep_ucr_crime_test('MD0160400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MI0400400')[, police_matching_columns],
               prep_ucr_crime_test('MI0400400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MI4183400')[, police_matching_columns],
               prep_ucr_crime_test('MI4183400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MI5284100')[, police_matching_columns],
               prep_ucr_crime_test('MI5284100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MI5828700')[, police_matching_columns],
               prep_ucr_crime_test('MI5828700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MN0191000')[, police_matching_columns],
               prep_ucr_crime_test('MN0191000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MN0310700')[, police_matching_columns],
               prep_ucr_crime_test('MN0310700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MNMHP0400')[, police_matching_columns],
               prep_ucr_crime_test('MNMHP0400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0100600')[, police_matching_columns],
               prep_ucr_crime_test('MO0100600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0170000')[, police_matching_columns],
               prep_ucr_crime_test('MO0170000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0640100')[, police_matching_columns],
               prep_ucr_crime_test('MO0640100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0720900')[, police_matching_columns],
               prep_ucr_crime_test('MO0720900_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0850200')[, police_matching_columns],
               prep_ucr_crime_test('MO0850200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0955200')[, police_matching_columns],
               prep_ucr_crime_test('MO0955200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO0970000')[, police_matching_columns],
               prep_ucr_crime_test('MO0970000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MO1050100')[, police_matching_columns],
               prep_ucr_crime_test('MO1050100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MS0110100')[, police_matching_columns],
               prep_ucr_crime_test('MS0110100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('MT0250100')[, police_matching_columns],
               prep_ucr_crime_test('MT0250100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NB0130000')[, police_matching_columns],
               prep_ucr_crime_test('NB0130000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NC0320100')[, police_matching_columns],
               prep_ucr_crime_test('NC0320100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NC0600100')[, police_matching_columns],
               prep_ucr_crime_test('NC0600100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NH009010X')[, police_matching_columns],
               prep_ucr_crime_test('NH009010X_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NJ0020100')[, police_matching_columns],
               prep_ucr_crime_test('NJ0020100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NJ0043000')[, police_matching_columns],
               prep_ucr_crime_test('NJ0043000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NJ0043400')[, police_matching_columns],
               prep_ucr_crime_test('NJ0043400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NJ0132400')[, police_matching_columns],
               prep_ucr_crime_test('NJ0132400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NJ0134700')[, police_matching_columns],
               prep_ucr_crime_test('NJ0134700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NV0020100')[, police_matching_columns],
               prep_ucr_crime_test('NV0020100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NVDI05500')[, police_matching_columns],
               prep_ucr_crime_test('NVDI05500_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0112000')[, police_matching_columns],
               prep_ucr_crime_test('NY0112000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0290000')[, police_matching_columns],
               prep_ucr_crime_test('NY0290000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0303000')[, police_matching_columns],
               prep_ucr_crime_test('NY0303000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0323100')[, police_matching_columns],
               prep_ucr_crime_test('NY0323100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0370000')[, police_matching_columns],
               prep_ucr_crime_test('NY0370000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0502400')[, police_matching_columns],
               prep_ucr_crime_test('NY0502400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0510100')[, police_matching_columns],
               prep_ucr_crime_test('NY0510100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('NY0542200')[, police_matching_columns],
               prep_ucr_crime_test('NY0542200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('OH0191800')[, police_matching_columns],
               prep_ucr_crime_test('OH0191800_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('OH0350400')[, police_matching_columns],
               prep_ucr_crime_test('OH0350400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('OH0490000')[, police_matching_columns],
               prep_ucr_crime_test('OH0490000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('OK0241500')[, police_matching_columns],
               prep_ucr_crime_test('OK0241500_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('OR0050600')[, police_matching_columns],
               prep_ucr_crime_test('OR0050600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('OR0350000')[, police_matching_columns],
               prep_ucr_crime_test('OR0350000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('PA0020P00')[, police_matching_columns],
               prep_ucr_crime_test('PA0020P00_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('PA0192600')[, police_matching_columns],
               prep_ucr_crime_test('PA0192600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('PA0390500')[, police_matching_columns],
               prep_ucr_crime_test('PA0390500_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('PA0630700')[, police_matching_columns],
               prep_ucr_crime_test('PA0630700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('PA0674700')[, police_matching_columns],
               prep_ucr_crime_test('PA0674700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('PAPEP0000')[, police_matching_columns],
               prep_ucr_crime_test('PAPEP0000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('RI0010200')[, police_matching_columns],
               prep_ucr_crime_test('RI0010200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('SC0230600')[, police_matching_columns],
               prep_ucr_crime_test('SC0230600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TN0500200')[, police_matching_columns],
               prep_ucr_crime_test('TN0500200_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX0050100')[, police_matching_columns],
               prep_ucr_crime_test('TX0050100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX0151700')[, police_matching_columns],
               prep_ucr_crime_test('TX0151700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX0574700')[, police_matching_columns],
               prep_ucr_crime_test('TX0574700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX0700600')[, police_matching_columns],
               prep_ucr_crime_test('TX0700600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX1010000')[, police_matching_columns],
               prep_ucr_crime_test('TX1010000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX1710000')[, police_matching_columns],
               prep_ucr_crime_test('TX1710000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX1740000')[, police_matching_columns],
               prep_ucr_crime_test('TX1740000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX1900000')[, police_matching_columns],
               prep_ucr_crime_test('TX1900000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX2260000')[, police_matching_columns],
               prep_ucr_crime_test('TX2260000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX2270100')[, police_matching_columns],
               prep_ucr_crime_test('TX2270100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TX2460000')[, police_matching_columns],
               prep_ucr_crime_test('TX2460000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TXDPD0000')[, police_matching_columns],
               prep_ucr_crime_test('TXDPD0000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TXHPD0000')[, police_matching_columns],
               prep_ucr_crime_test('TXHPD0000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('TXSPD0000')[, police_matching_columns],
               prep_ucr_crime_test('TXSPD0000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('UT0240300')[, police_matching_columns],
               prep_ucr_crime_test('UT0240300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('VA0290100')[, police_matching_columns],
               prep_ucr_crime_test('VA0290100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('VA041SP00')[, police_matching_columns],
               prep_ucr_crime_test('VA041SP00_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('VA0430700')[, police_matching_columns],
               prep_ucr_crime_test('VA0430700_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('VA0490000')[, police_matching_columns],
               prep_ucr_crime_test('VA0490000_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('VA0750500')[, police_matching_columns],
               prep_ucr_crime_test('VA0750500_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('VA090SP00')[, police_matching_columns],
               prep_ucr_crime_test('VA090SP00_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WA0180100')[, police_matching_columns],
               prep_ucr_crime_test('WA0180100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WA0271300')[, police_matching_columns],
               prep_ucr_crime_test('WA0271300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WA0390400')[, police_matching_columns],
               prep_ucr_crime_test('WA0390400_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WI0050300')[, police_matching_columns],
               prep_ucr_crime_test('WI0050300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WI0050800')[, police_matching_columns],
               prep_ucr_crime_test('WI0050800_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WI0200300')[, police_matching_columns],
               prep_ucr_crime_test('WI0200300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WI0460600')[, police_matching_columns],
               prep_ucr_crime_test('WI0460600_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WI0680100')[, police_matching_columns],
               prep_ucr_crime_test('WI0680100_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WV0100300')[, police_matching_columns],
               prep_ucr_crime_test('WV0100300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WVWSP9300')[, police_matching_columns],
               prep_ucr_crime_test('WVWSP9300_police-employment-breakout.csv',
                                   type = 'police'))
  expect_equal(get_police_employment('WY0050200')[, police_matching_columns],
               prep_ucr_crime_test('WY0050200_police-employment-breakout.csv',
                                   type = 'police'))

})
