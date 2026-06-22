# Offline tests for arrest functions using fixtures.

test_that("get_arrest_count parses agency-level counts response", {
  local_fbi_fixture("arrest-agency-CA0010900-counts.json")
  result <- get_arrest_count("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_true("rate" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_true(nrow(result) > 0)
})

test_that("get_arrest_count parses national-level counts response", {
  local_fbi_fixture("arrest-national-all-counts.json")
  result <- get_arrest_count()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
})

test_that("get_arrest_demographics parses demographics response", {
  local_fbi_fixture("arrest-agency-CA0010900-robbery-totals.json")
  result <- get_arrest_demographics("CA0010900", offense = "robbery")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_equal(result$offense[1], "robbery")
  expect_true(nrow(result) > 0)
})

test_that("get_arrest_demographics returns empty for no totals data", {
  local_mocked_bindings(
    cde_request = function(...) list(offenses = list()),
    .package = "fbi"
  )
  result <- get_arrest_demographics("CA0010900", offense = "robbery")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# Validation tests

test_that("get_arrest_count validates ORI", {
  expect_error(get_arrest_count("bad-ori"), "Invalid ORI code")
})

test_that("get_arrest_count validates state abbreviation", {
  expect_error(get_arrest_count(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_arrest_count rejects inverted date ranges", {
  expect_error(
    get_arrest_count(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_arrest_demographics validates ORI", {
  expect_error(
    get_arrest_demographics("bad-ori", offense = "robbery"),
    "Invalid ORI code"
  )
})

test_that("get_arrest_demographics validates state abbreviation", {
  expect_error(
    get_arrest_demographics(state_abb = "XX", offense = "robbery"),
    "Invalid state abbreviation"
  )
})

test_that("get_arrest_demographics requires offense", {
  expect_error(
    get_arrest_demographics("CA0010900"),
    "offense is required"
  )
})

# Live tests

test_that("get_arrest_count returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_arrest_count("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
})

test_that("get_arrest_demographics returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_arrest_demographics("CA0010900", offense = "robbery")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
})

test_that("list_ucr_arrest_offenses returns a character vector", {
  result <- list_ucr_arrest_offenses()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})
