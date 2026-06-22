# Offline tests for summarized crime functions using fixtures.

test_that("get_agency_crime parses agency-level response", {
  local_fbi_fixture("summarized-agency-CA0010900-V.json")
  result <- get_agency_crime("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_true("rate" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 3)
})

test_that("get_estimated_crime parses national-level response (rates only)", {
  local_fbi_fixture("summarized-national-V.json")
  result <- get_estimated_crime()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
  expect_true("rate" %in% names(result))
  # count column should be NA since fixture has no counts
  expect_true(all(is.na(result$count)))
})

test_that("get_estimated_crime parses state-level response", {
  local_fbi_fixture("summarized-state-CA-V.json")
  result <- get_estimated_crime("CA")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "CA")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 3)
})

test_that("get_estimated_arson parses arson response", {
  local_fbi_fixture("summarized-national-AR.json")
  result <- get_estimated_arson()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
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
