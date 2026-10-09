
# Offline tests for police employment using fixtures.
#
# The pe-*.json fixtures were recorded live, with from=2018&to=2020, from
# pe/CA/CA0010900, pe/CA and pe (see tests/testthat/fixtures/README.md).

police_matching_columns <- c(
  "year", "male_officers", "female_officers", "male_civilians",
  "female_civilians", "ori", "male_total", "female_total", "civilians_total",
  "officers_total", "employees_total", "participated_population",
  "employees_per_1000"
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

test_that("get_police_employment parses a national response", {
  local_fbi_fixture("pe-national.json")
  result <- get_police_employment(from = "2018", to = "2020")

  expect_equal(names(result), police_matching_columns)
  expect_equal(result$year, c(2018, 2019, 2020))
  expect_equal(unique(result$ori), "US")
  expect_equal(result$male_officers[1], 614322)
})

test_that("get_police_employment parses a state response", {
  local_fbi_fixture("pe-state-CA.json")
  result <- get_police_employment(state_abb = "CA", from = "2018", to = "2020")

  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 3)
  expect_equal(unique(result$ori), "CA")
  expect_equal(result$male_officers[1], 68663)
  # Coverage rides along: a state's counts are sums over reporting agencies.
  expect_equal(result$participated_population, c(33867039, 33821670, 33781093))
  expect_equal(result$employees_per_1000, c(3.54, 3.59, 3.57))
})

test_that("get_police_employment requests the paths the pe endpoint answers", {
  # pe/state/{ST} and pe/national come back all-null; the endpoint wants
  # pe/{ST}, plain pe, and pe/{ST}/{ORI}.
  paths <- character(0)
  testthat::local_mocked_bindings(
    cde_request = function(path, ...) {
      paths <<- c(paths, path)
      read_fixture("pe-agency-CA0010900.json")
    },
    .package = "fbiCDE"
  )
  get_police_employment("ca0010900", from = "2018", to = "2020")
  get_police_employment(state_abb = "ca", from = "2018", to = "2020")
  get_police_employment(from = "2018", to = "2020")
  expect_equal(paths, c("pe/CA/CA0010900", "pe/CA", "pe"))
})

test_that("get_police_employment rejects a region", {
  expect_error(get_police_employment(region = "South"), "no region-level")
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
  result <- get_police_employment(state_abb = "CA", from = "2018", to = "2020")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 3)
  expect_true(all(result$officers_total > 0))
})

test_that("get_police_employment returns expected shape from live API (national)", {
  skip_if_no_fbi_api()
  result <- get_police_employment(from = "2018", to = "2020")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), police_matching_columns)
  expect_equal(nrow(result), 3)
  expect_true(all(result$officers_total > 500000))
})
