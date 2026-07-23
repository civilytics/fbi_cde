library(testthat)
test_that("county_to_fips resolves common counties", {
  expect_equal(county_to_fips("CA", "LOS ANGELES"), "06037")
  expect_equal(county_to_fips("TX", "HARRIS"), "48201")
  expect_equal(county_to_fips("NY", "NEW YORK"), "36061")
  expect_equal(county_to_fips("IL", "COOK"), "17031")
})

test_that("county_to_fips handles TX name variants", {
  # DeWitt (capital W, no space) in TX
  expect_equal(county_to_fips("TX", "DEWITT"), "48123")
  # La Salle (space, period) in TX
  expect_equal(county_to_fips("TX", "LA SALLE"), "48283")
  # Multi-county agency (takes first county)
  expect_equal(county_to_fips("TX", "DEWITT; LAVACA"), "48123")
})

test_that("county_to_fips handles independent cities", {
  # MD Baltimore City vs County
  expect_equal(county_to_fips("MD", "BALTIMORE CITY"), "24510")
  expect_equal(county_to_fips("MD", "BALTIMORE"), "24005")
  
  # MO St. Louis City vs County
  expect_equal(county_to_fips("MO", "ST LOUIS CITY"), "29510")
  expect_equal(county_to_fips("MO", "ST LOUIS"), "29189")
  expect_equal(county_to_fips("MO", "ST LOUIS; FRANKLIN"), "29189")
  
  # MN St. Louis County (not an independent city state)
  expect_equal(county_to_fips("MN", "ST LOUIS"), "27137")
})

test_that("county_to_fips handles Virginia independent cities", {
  expect_equal(county_to_fips("VA", "ALEXANDRIA CITY"), "51510")
  expect_equal(county_to_fips("VA", "NORFOLK CITY"), "51710")
  expect_equal(county_to_fips("VA", "VIRGINIA BEACH CITY"), "51810")
  # VA county (not a city)
  expect_equal(county_to_fips("VA", "ACCOMACK"), "51001")
})

test_that("county_to_fips handles Connecticut planning regions", {
  # CT uses planning regions instead of traditional counties
  expect_equal(county_to_fips("CT", "HARTFORD"), "09003")
  expect_equal(county_to_fips("CT", "FAIRFIELD"), "09001")
  expect_equal(county_to_fips("CT", "NEW HAVEN"), "09009")
})

test_that("county_to_fips handles Alaska boroughs", {
  expect_equal(county_to_fips("AK", "ANCHORAGE"), "02020")
  expect_equal(county_to_fips("AK", "FAIRBANKS NORTH STAR"), "02090")
  expect_equal(county_to_fips("AK", "ALEUTIANS WEST"), "02016")
})

test_that("county_to_fips handles ST./ST. name variants", {
  expect_equal(county_to_fips("IL", "ST CLAIR"), "17163")
  expect_equal(county_to_fips("LA", "ST CHARLES"), "22089")
  expect_equal(county_to_fips("MO", "STE GENEVIEVE"), "29186")
})

test_that("county_to_fips returns NA for unresolvable cases", {
  # N/A county (state police, tribal, etc.)
  expect_true(is.na(county_to_fips("AK", "N/A")))
  expect_true(is.na(county_to_fips("TX", "N/A")))
  
  # VALDEZ-CORDOVA (dissolved AK borough)
  expect_true(is.na(county_to_fips("AK", "VALDEZ-CORDOVA")))
})

test_that("county_to_fips handles state FIPS codes", {
  expect_equal(county_to_fips("06", "LOS ANGELES"), "06037")
  expect_equal(county_to_fips("48", "HARRIS"), "48201")
  expect_equal(county_to_fips("24", "BALTIMORE CITY"), "24510")
})

test_that("county_to_fips is case-insensitive", {
  expect_equal(county_to_fips("ca", "los angeles"), "06037")
  expect_equal(county_to_fips("Ca", "Los Angeles"), "06037")
  expect_equal(county_to_fips("CA", "Los Angeles"), "06037")
})
