# Offline tests for summarized crime functions using fixtures.
#
# Fixtures were recorded live from summarized/{level}/{offense} for
# 01-2019..03-2019 (see tests/testthat/fixtures/README.md). At agency/state
# level the API includes comparison rows (state/national rates) alongside the
# geography's own actuals, so a full outer join on (offense, period) yields
# more rows than just offense x period for the requested geography alone --
# the comparison rows have a real rate but NA count.

test_that("get_agency_crime parses agency-level response", {
  local_fbi_fixture("summarized-agency-CA0010900-V.json")
  result <- get_agency_crime("CA0010900", from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_true("rate" %in% names(result))
  expect_equal(unique(result$geography), "CA0010900")
  expect_equal(nrow(result), 18)

  agency_rows <- result[result$offense == "Oakland Police Department Offenses", ]
  expect_equal(agency_rows$count, c(472, 368, 438))
})

test_that("get_estimated_crime parses national-level response", {
  local_fbi_fixture("summarized-national-V.json")
  result <- get_estimated_crime(from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$geography), "US")
  expect_equal(nrow(result), 6)
  expect_true("rate" %in% names(result))
  # National-level actuals are populated (unlike state-level comparison rows).
  expect_true(all(!is.na(result$count)))
})

test_that("get_estimated_crime parses state-level response", {
  local_fbi_fixture("summarized-state-CA-V.json")
  result <- get_estimated_crime("CA", from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$geography), "CA")
  expect_equal(nrow(result), 12)

  ca_rows <- result[result$offense == "California Offenses", ]
  expect_equal(ca_rows$count, c(13582, 12113, 14201))

  # Comparison rows (national rates alongside the state's own) have no count.
  us_rows <- result[result$offense == "United States Offenses", ]
  expect_true(all(is.na(us_rows$count)))
})

test_that("get_estimated_arson parses arson response", {
  local_fbi_fixture("summarized-national-ARS.json")
  result <- get_estimated_arson(from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$geography), "US")
  expect_equal(nrow(result), 6)
  expect_true(all(!is.na(result$count)))
})

test_that("get_estimated_crime validates state abbreviation", {
  expect_error(get_estimated_crime(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_agency_crime validates ORI", {
  expect_error(get_agency_crime("bad-ori"), "Invalid ORI code")
})

test_that("get_estimated_crime rejects inverted date ranges", {
  expect_error(
    get_estimated_crime(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

# Live tests

test_that("get_agency_crime returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_agency_crime("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
})

test_that("get_estimated_crime returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_estimated_crime()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("get_estimated_arson returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_estimated_arson()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})
