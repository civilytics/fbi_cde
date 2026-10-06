
# Offline tests for police employment using fixtures.
#
# pe-agency-CA0010900.json and pe-national.json were recorded live from
# pe/{level}?from=2018&to=2020 (see tests/testthat/fixtures/README.md).

police_matching_columns <- c(
  "year", "male_officers", "female_officers", "male_civilians",
  "female_civilians", "ori", "male_total", "female_total", "civilians_total",
  "officers_total", "employees_total"
)

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

test_that("get_police_employment says why a national request is empty", {
  # The CDE answers national requests with every value null -- counts, rates
  # and populations. See the live tests below.
  local_fbi_fixture("pe-national.json")
  expect_message(
    result <- get_police_employment(from = "2018", to = "2020"),
    "individual agencies only"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 0)
})

test_that("get_police_employment says why a state request is empty", {
  # Same as national: every value in the state response is null.
  local_fbi_fixture("pe-state-CA.json")
  expect_message(
    result <- get_police_employment(state_abb = "CA"),
    "returned no data for CA"
  )

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
    .package = "fbiCDE"
  )
  expect_message(
    result <- get_police_employment("CA0010900"),
    "returned no data for CA0010900"
  )
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
  # The CDE answers state requests with every value null; if this starts
  # returning rows, the documented agency-only limitation has lifted.
  expect_message(
    result <- get_police_employment(state_abb = "CA", from = "2018", to = "2020"),
    "individual agencies only"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 0)
})

test_that("get_police_employment returns expected shape from live API (national)", {
  skip_if_no_fbi_api()
  expect_message(
    result <- get_police_employment(from = "2018", to = "2020"),
    "individual agencies only"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 0)
})

