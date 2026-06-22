# Offline tests for SHR functions using fixtures.

test_that("get_shr parses national-level response", {
  local_fbi_fixture("shr-national.json")
  result <- get_shr()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 12)
})

test_that("get_shr parses agency-level response", {
  local_fbi_fixture("shr-agency-CA0010900.json")
  result <- get_shr(ori = "CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 12)
})

test_that("get_shr parses state-level response", {
  local_fbi_fixture("shr-state-CA.json")
  result <- get_shr(state_abb = "CA")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "CA")
  expect_true(nrow(result) > 0)
  expect_equal(nrow(result), 12)
})

test_that("get_shr validates ORI", {
  expect_error(get_shr(ori = "bad-ori"), "Invalid ORI code")
})

test_that("get_shr validates state abbreviation", {
  expect_error(get_shr(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_shr rejects inverted date ranges", {
  expect_error(
    get_shr(from = "12-2015", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_shr returns correct count values from national fixture", {
  local_fbi_fixture("shr-national.json")
  result <- get_shr()

  jan_count <- result$count[result$period == "01-2015"]
  expect_equal(jan_count, 1088)

  jun_count <- result$count[result$period == "06-2015"]
  expect_equal(jun_count, 1120)
})

test_that("get_shr returns correct count values from agency fixture", {
  local_fbi_fixture("shr-agency-CA0010900.json")
  result <- get_shr(ori = "CA0010900")

  jan_count <- result$count[result$period == "01-2015"]
  expect_equal(jan_count, 10)

  jun_count <- result$count[result$period == "06-2015"]
  expect_equal(jun_count, 18)
})

# Live tests

test_that("get_shr returns expected shape from live API (national)", {
  skip_if_no_fbi_api()
  result <- get_shr()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "US")
})

test_that("get_shr returns expected shape from live API (state)", {
  skip_if_no_fbi_api()
  result <- get_shr(state_abb = "CA")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "CA")
})

test_that("get_shr returns expected shape from live API (agency)", {
  skip_if_no_fbi_api()
  result <- get_shr(ori = "CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
})
