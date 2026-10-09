# Offline tests for LEOKA using fixtures.

test_that("get_leoka parses year-to-date totals", {
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      expect_equal(path, "leoka/ytd")
      expect_equal(query$year, 2020)
      read_fixture("leoka-ytd-2020.json")
    },
    .package = "fbiCDE"
  )
  result <- get_leoka(2020, 2020)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 2020)
  expect_true(is.na(result$month))
  expect_equal(result$total_officers, 46)
  expect_equal(result$total_incidents, 44)
  expect_equal(result$total_officers_dod, 2)
  expect_equal(result$total_officers_doi, 44)
})

test_that("get_leoka loops over a year range, dropping a year with no data", {
  # Both responses are recorded: the CDE answers 2019 with a null chart.
  years <- integer(0)
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      years <<- c(years, query$year)
      read_fixture(sprintf("leoka-ytd-%d.json", query$year))
    },
    .package = "fbiCDE"
  )
  result <- get_leoka(2019, 2020)

  expect_equal(years, c(2019, 2020))
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 2020)
})

test_that("get_leoka rejects inverted year ranges", {
  expect_error(get_leoka(2020, 2015), "Invalid date range")
})

test_that("get_leoka_monthly parses a single month", {
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      expect_equal(path, "leoka/monthly")
      expect_equal(query$year, 2020)
      expect_equal(query$month, "01")
      read_fixture("leoka-monthly-2020-01.json")
    },
    .package = "fbiCDE"
  )
  result <- get_leoka_monthly(2020, 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$year, 2020)
  expect_equal(result$month, 1)
  expect_equal(result$total_officers, 4)
  expect_equal(result$total_incidents, 3)
})

test_that("get_leoka_monthly validates month", {
  expect_error(get_leoka_monthly(2020, 13), "month must be")
  expect_error(get_leoka_monthly(2020, 0), "month must be")
})

# Live tests

test_that("get_leoka returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_leoka(2020, 2021)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(result$total_officers >= 0))
})

test_that("get_leoka_monthly returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_leoka_monthly(2020, 1)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$month, 1)
  expect_true(result$total_officers >= 0)
})
