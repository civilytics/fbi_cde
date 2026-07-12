# Offline unit tests for pure helper functions. These run everywhere (incl. CI)
# and require no network or API key. They exist partly to prove the test harness
# executes on CI, not just skips live tests.

test_that("make_state maps abbreviations to full names", {
  expect_equal(make_state("CA"), "California")
  expect_equal(make_state("NY"), "New York")
})

test_that("combine_url_section builds geography paths", {
  expect_equal(
    combine_url_section("summarized", ori = NULL, region_name = NULL, state_abb = NULL),
    "summarized/national"
  )
  expect_equal(
    combine_url_section("summarized", ori = "CA0010900", region_name = NULL, state_abb = NULL),
    "summarized/agencies/CA0010900"
  )
  expect_equal(
    combine_url_section("summarized", ori = NULL, region_name = NULL, state_abb = "CA"),
    "summarized/states/CA"
  )
})

test_that("is_valid_ori checks ORI format (2 letters + 7 digits)", {
  expect_true(is_valid_ori("CA0010900"))
  expect_true(is_valid_ori("NY1234567"))
  expect_false(is_valid_ori("not-an-ori"))
  expect_false(is_valid_ori("ABC123"))
  expect_false(is_valid_ori("A0010900"))
  # case-insensitive
  expect_true(is_valid_ori("ca0010900"))
  # vectorised
  expect_equal(is_valid_ori(c("CA0010900", "not-an-ori")), c(TRUE, FALSE))
})

test_that("is_valid_state checks state abbreviation format", {
  expect_true(is_valid_state("CA"))
  expect_true(is_valid_state("NY"))
  expect_true(is_valid_state("DC"))
  expect_true(is_valid_state("PR"))
  expect_false(is_valid_state("XX"))
  expect_false(is_valid_state("ABC"))
  expect_false(is_valid_state(""))
  # case-insensitive
  expect_true(is_valid_state("ca"))
  expect_true(is_valid_state("Ca"))
})

test_that("cde_validate_dates rejects inverted ranges", {
  expect_error(cde_validate_dates("12-2020", "01-2015", "mm-yyyy"), "Invalid date range")
  expect_error(cde_validate_dates("2020", "2015", "yyyy"), "Invalid date range")
  expect_true(cde_validate_dates("01-2015", "12-2020", "mm-yyyy"))
  expect_true(cde_validate_dates("2015", "2020", "yyyy"))
})

test_that("clean_column_names lowercases, renames, and drops csv_header", {
  cleaned <- clean_column_names(
    data.frame(DATA_YEAR = 1, MVT = 2, csv_header = 3)
  )
  expect_named(cleaned, c("year", "motor_vehicle_theft"))
})

test_that("flatten_cde_json produces a tidy frame from fixture data", {
  fixture <- jsonlite::fromJSON(readLines(testthat::test_path("fixtures/summarized-national-V.json"), warn = FALSE), simplifyVector = TRUE)
  rates <- fixture$offenses$rates
  result <- flatten_cde_json(rates)
  
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("label", "period", "value"))
  expect_equal(nrow(result), 6)
  expect_equal(result$label[1], "United States Offenses")
  expect_equal(result$period[1], "01-2019")
  expect_equal(result$value[1], 27.84)
  expect_equal(result$value[4], 12.20)
})

test_that("flatten_cde_json handles empty input", {
  result <- flatten_cde_json(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("label", "period", "value"))
})

test_that("flatten_cde_json handles NULL input", {
  result <- flatten_cde_json(NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
