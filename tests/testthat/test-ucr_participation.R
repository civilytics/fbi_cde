# Offline tests for the modernized participation functions.
#
# The current CDE API has no dedicated participation-rate endpoint (see
# roxygen docs on get_agency_participation()); these functions instead
# report NIBRS reporting status/rate sourced from `agency/byStateAbbr/{state}`.
# The Rhode Island fixture (participation-agency-byStateAbbr-RI.json) has 49
# agencies, 48 reporting NIBRS and 1 (RI0050900) not.

test_that("get_agency_participation returns the matching agency", {
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_agency_participation("RI0020100")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$ori, "RI0020100")
  expect_equal(result$agency_name, "Coventry Police Department")
  expect_true(result$is_nibrs)
})

test_that("get_agency_participation reports non-participating agencies", {
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_agency_participation("RI0050900")

  expect_equal(nrow(result), 1)
  expect_false(result$is_nibrs)
  expect_true(is.na(result$nibrs_start_date))
})

test_that("get_agency_participation validates ORI", {
  expect_error(get_agency_participation("bad-ori"), "Invalid ORI code")
})

test_that("get_state_participation returns an aggregate rate", {
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_state_participation("RI")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$state_abbr, "RI")
  expect_equal(result$total_agencies, 49)
  expect_equal(result$nibrs_agencies, 48)
  expect_equal(result$participation_rate, 48 / 49)
})

test_that("get_state_participation validates state abbreviation", {
  expect_error(get_state_participation("ZZ"), "Invalid state abbreviation")
})

test_that("get_region_participation aggregates across states in the region", {
  # Every state in the region is queried with the same mocked cde_request(),
  # so this exercises the aggregation logic (sum of per-state totals), not a
  # realistic region-wide count.
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_region_participation("Northeast")

  n_states <- length(unique(
    fbi_api_agencies$state_abbr[fbi_api_agencies$region_name == "Northeast"]
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$region, "Northeast")
  expect_equal(result$total_agencies, 49 * n_states)
  expect_equal(result$nibrs_agencies, 48 * n_states)
})

test_that("get_region_participation validates region_name", {
  expect_error(get_region_participation("Nowhere"), "Invalid region_name")
})

# Live tests

test_that("get_agency_participation returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_agency_participation("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$ori, "CA0010900")
  expect_true(is.logical(result$is_nibrs))
})

test_that("get_state_participation returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_state_participation("RI")

  expect_s3_class(result, "data.frame")
  expect_true(result$total_agencies > 0)
  expect_true(result$participation_rate >= 0 && result$participation_rate <= 1)
})

test_that("get_region_participation returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_region_participation("West")

  expect_s3_class(result, "data.frame")
  expect_true(result$total_agencies > 0)
  expect_true(result$participation_rate >= 0 && result$participation_rate <= 1)
})
