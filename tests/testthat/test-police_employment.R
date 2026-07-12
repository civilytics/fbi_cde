
# Offline tests for police employment using fixtures.
#
# pe-agency-CA0010900.json and pe-national.json were recorded live from
# pe/{level}?from=2018&to=2020 (see tests/testthat/fixtures/README.md).

test_that("get_police_employment parses agency-level response", {
  local_fbi_fixture("pe-agency-CA0010900.json")
  result <- get_police_employment("CA0010900", from = "2018", to = "2020")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(result$ori[1], "CA0010900")
  expect_equal(nrow(result), 3)
  expect_equal(result$year[1], 2018)
  expect_equal(result$male_officers[1], 633)
  expect_equal(result$female_officers[1], 98)
  expect_equal(result$male_civilians[1], 60)
  expect_equal(result$female_civilians[1], 219)
  expect_equal(result$male_total[1], 693)
  expect_equal(result$female_total[1], 317)
  expect_equal(result$civilians_total[1], 279)
  expect_equal(result$officers_total[1], 731)
  expect_equal(result$employees_total[1], 1010)
})

test_that("get_police_employment parses national-level response with suppressed counts", {
  # National-level `actuals` (employee counts) are suppressed upstream --
  # only `rates` are populated. See the live tests below.
  local_fbi_fixture("pe-national.json")
  result <- get_police_employment(from = "2018", to = "2020")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 0)
})

test_that("get_police_employment parses state-level response with suppressed counts", {
  # State-level `actuals` (employee counts) are suppressed upstream just like
  # national -- only `rates` are populated. See the live tests below.
  local_fbi_fixture("pe-state-CA.json")
  result <- get_police_employment(state_abb = "CA")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 0)
})

test_that("get_police_employment validates ORI", {
  expect_error(get_police_employment("bad-ori"), "Invalid ORI code")
})

test_that("get_police_employment validates state abbreviation", {
  expect_error(get_police_employment(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_police_employment returns empty data.frame for empty response", {
  local_fbi_fixture("pe-agency-CA0010900.json")
  # Mock an empty response
  testthat::local_mocked_bindings(
    cde_request = function(...) list(),
    .package = "fbi"
  )
  result <- get_police_employment("CA0010900")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), police_matching_columns)
})

test_that("get_police_employment returns expected shape from live API (agency)", {
  skip_if_no_fbi_api()

  for (ori in c("CA0010900", "TX0050100", "NY0112000")) {
    result <- get_police_employment(ori, from = "2018", to = "2020")

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), police_matching_columns)
    expect_true(nrow(result) > 0)
    expect_equal(unique(result$ori), ori)
    expect_true(all(result$employees_total >= 0))
  }
})

test_that("get_police_employment returns expected shape from live API (state)", {
  skip_if_no_fbi_api()
  result <- get_police_employment(state_abb = "CA", from = "2018", to = "2020")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  # State-level employee counts are suppressed upstream (only rates); see
  # the national case below.
  expect_equal(nrow(result), 0)
})

test_that("get_police_employment returns expected shape from live API (national)", {
  skip_if_no_fbi_api()
  result <- get_police_employment(from = "2018", to = "2020")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  # National employee counts are suppressed upstream (only rates); the
  # function correctly returns 0 rows here.
  expect_equal(nrow(result), 0)
})

