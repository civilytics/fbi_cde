# Offline tests for arrest functions using fixtures.
#
# Fixtures recorded live from arrest/{level}/all?type=counts for
# 01-2019..03-2019 (see tests/testthat/fixtures/README.md). At agency level
# the response includes state/national comparison series alongside the
# agency's own counts; they are dropped unless comparison = TRUE -- see
# test-ucr_crime.R for the same pattern on summarized crime.

test_that("get_arrest_count returns only the agency's own series by default", {
  local_fbi_fixture("arrest-agency-CA0010900-counts.json")
  result <- get_arrest_count("CA0010900", from = "01-2019", to = "03-2019")

  expect_equal(names(result),
               c("geography", "offense", "measure", "period", "count", "rate"))
  expect_equal(nrow(result), 3)
  expect_equal(unique(result$geography), "CA0010900")
  expect_equal(unique(result$offense), "all")
  expect_equal(unique(result$measure), "arrests")
  expect_equal(result$period, c("01-2019", "02-2019", "03-2019"))
  expect_equal(result$count, c(823, 774, 762))
  expect_false(anyNA(result$count))
})

test_that("get_arrest_count(comparison = TRUE) labels state and national series", {
  local_fbi_fixture("arrest-agency-CA0010900-counts.json")
  result <- get_arrest_count("CA0010900", from = "01-2019", to = "03-2019",
                             comparison = TRUE)

  expect_equal(nrow(result), 9)
  expect_equal(unique(result$series), c("agency", "state", "national"))
  expect_equal(unique(result$series_name),
               c("Oakland Police Department", "California", "United States"))
  expect_true(all(is.na(result$count[result$series != "agency"])))
  expect_false(anyNA(result$rate))
})

test_that("get_arrest_count parses national-level counts response", {
  local_fbi_fixture("arrest-national-all-counts.json")
  result <- get_arrest_count(from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(unique(result$geography), "US")
  expect_equal(nrow(result), 3)
  expect_equal(result$count, c(807594, 758199, 866055))
  expect_equal(unique(result$measure), "arrests")
})

test_that("get_arrest_demographics parses demographics response", {
  local_fbi_fixture("arrest-national-all-totals.json")
  result <- get_arrest_demographics()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
  # demographics are aggregated across all offenses
  expect_true(all(result$demographic_type %in%
    c("Arrestee Sex", "Arrestee Race",
      "Male Arrests By Age", "Female Arrests By Age")))
})

test_that("get_arrest_demographics rejects a specific offense", {
  expect_error(
    get_arrest_demographics("CA0010900", offense = "Robbery"),
    "Only offense .* is supported"
  )
})

test_that("get_arrest_demographics returns empty for no totals data", {
  local_mocked_bindings(
    cde_request = function(...) list(cde_properties = list()),
    .package = "fbiCDE"
  )
  result <- get_arrest_demographics("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_arrest_count recovers a specific offense from the all response", {
  local_fbi_fixture("arrest-national-all-totals.json")
  result <- get_arrest_count(offense = "Robbery")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$offense[1], "Robbery")
  expect_equal(result$geography[1], "US")
  expect_true(is.na(result$period[1]))
  expect_true(is.na(result$rate[1]))
  expect_true(result$count[1] > 0)
})

test_that("get_arrest_count is case-insensitive for offense names", {
  local_fbi_fixture("arrest-national-all-totals.json")
  result <- get_arrest_count(offense = "robbery")

  expect_equal(nrow(result), 1)
  expect_equal(result$offense[1], "Robbery")
})

test_that("get_arrest_count rejects an unknown offense", {
  expect_error(
    get_arrest_count(offense = "jaywalking"),
    "Invalid arrest offense"
  )
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

test_that("get_arrest_demographics defaults to offense = all", {
  local_fbi_fixture("arrest-national-all-totals.json")
  expect_silent(result <- get_arrest_demographics("CA0010900"))
  expect_s3_class(result, "data.frame")
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
  result <- get_arrest_demographics("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
})

test_that("get_arrest_count recovers a specific offense from live API", {
  skip_if_no_fbi_api()
  result <- get_arrest_count(state_abb = "CA", offense = "Robbery")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$offense[1], "Robbery")
  expect_true(result$count[1] > 0)
})

test_that("list_ucr_arrest_offenses returns a character vector", {
  result <- list_ucr_arrest_offenses()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})
