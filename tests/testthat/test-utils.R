# Offline unit tests for pure helper functions. These run everywhere (incl. CI)
# and require no network or API key. They exist partly to prove the test harness
# executes on CI, not just skips live tests.

test_that("make_state maps abbreviations to full names", {
  expect_equal(make_state("CA"), "California")
  expect_equal(make_state("NY"), "New York")
})

test_that("combine_url_section builds geography paths", {
  expect_equal(
    combine_url_section("summarized", ori = NULL, region_name = NULL, state_abb = NULL),
    "summarized/national"
  )
  expect_equal(
    combine_url_section("summarized", ori = "CA0010900", region_name = NULL, state_abb = NULL),
    "summarized/agencies/CA0010900"
  )
  expect_equal(
    combine_url_section("summarized", ori = NULL, region_name = NULL, state_abb = "CA"),
    "summarized/states/CA"
  )
})

test_that("is_valid_ori checks ORI format (2 letters + 7 alphanumerics)", {
  expect_true(is_valid_ori("CA0010900"))
  expect_true(is_valid_ori("NY1234567"))
  # Letter-bearing ORIs are valid: contract cities, state, tribal, campus.
  expect_true(is_valid_ori("CA001300X"))   # Dublin PD (Alameda)
  expect_true(is_valid_ori("CA0191H0X"))   # West Hollywood PD (LASD contract)
  expect_true(is_valid_ori("ARASP0000"))   # Arkansas State Police
  expect_false(is_valid_ori("not-an-ori"))
  expect_false(is_valid_ori("ABC123"))     # too short
  expect_false(is_valid_ori("A0010900"))   # only 1 leading letter
  expect_false(is_valid_ori("CA010900"))   # 8 chars
  expect_false(is_valid_ori("CA00109!0"))  # illegal char
  # Case-insensitive.
  expect_true(is_valid_ori("ca0010900"))
  expect_true(is_valid_ori("ca001300x"))
  # Vectorized.
  expect_equal(is_valid_ori(c("CA0010900", "not-an-ori")), c(TRUE, FALSE))
})

test_that("is_valid_state checks state abbreviation format", {
  expect_true(is_valid_state("CA"))
  expect_true(is_valid_state("NY"))
  expect_true(is_valid_state("DC"))
  expect_true(is_valid_state("PR"))
  expect_false(is_valid_state("XX"))
  expect_false(is_valid_state("ABC"))
  expect_false(is_valid_state(""))
  # case-insensitive
  expect_true(is_valid_state("ca"))
  expect_true(is_valid_state("Ca"))
})

test_that("cde_validate_dates rejects inverted ranges", {
  expect_error(cde_validate_dates("12-2020", "01-2015", "mm-yyyy"), "Invalid date range")
  expect_error(cde_validate_dates("2020", "2015", "yyyy"), "Invalid date range")
  expect_true(cde_validate_dates("01-2015", "12-2020", "mm-yyyy"))
  expect_true(cde_validate_dates("2015", "2020", "yyyy"))
})

test_that("clean_column_names lowercases, renames, and drops csv_header", {
  cleaned <- clean_column_names(
    data.frame(DATA_YEAR = 1, MVT = 2, csv_header = 3)
  )
  expect_named(cleaned, c("year", "motor_vehicle_theft"))
})

test_that("flatten_cde_json produces a tidy frame from fixture data", {
  fixture <- jsonlite::fromJSON(readLines(testthat::test_path("fixtures/summarized-national-V.json"), warn = FALSE), simplifyVector = TRUE)
  rates <- fixture$offenses$rates
  result <- flatten_cde_json(rates)
  
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("label", "period", "value"))
  expect_equal(nrow(result), 6)
  expect_equal(result$label[1], "United States Offenses")
  expect_equal(result$period[1], "01-2019")
  expect_equal(result$value[1], 27.84)
  expect_equal(result$value[4], 12.20)
})

test_that("flatten_cde_json handles empty input", {
  result <- flatten_cde_json(list())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("label", "period", "value"))
})

test_that("flatten_cde_json handles NULL input", {
  result <- flatten_cde_json(NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("rbind_fill unions columns and fills missing with NA", {
  a <- data.frame(x = 1L, y = "a", stringsAsFactors = FALSE)
  b <- data.frame(x = 2L, z = TRUE, stringsAsFactors = FALSE)
  out <- rbind_fill(list(a, NULL, b))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_setequal(names(out), c("x", "y", "z"))
  expect_equal(out$x, c(1L, 2L))
  expect_true(is.na(out$z[1]))   # a had no z
  expect_true(is.na(out$y[2]))   # b had no y
})

test_that("rbind_fill returns empty frame for empty or all-NULL input", {
  expect_equal(nrow(rbind_fill(list())), 0L)
  expect_equal(nrow(rbind_fill(list(NULL, NULL))), 0L)
})

test_that("rbind_fill handles a zero-row input missing a column present elsewhere", {
  e1 <- data.frame(x = integer(0), y = character(0))
  e2 <- data.frame(x = 1L, z = 2L)

  out <- rbind_fill(list(e1, e2))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_setequal(names(out), c("x", "y", "z"))
  expect_equal(out$x, 1L)
  expect_true(is.na(out$y))
  expect_equal(out$z, 2L)

  # Reverse order should give the same unioned result.
  out_rev <- rbind_fill(list(e2, e1))
  expect_s3_class(out_rev, "data.frame")
  expect_equal(nrow(out_rev), 1L)
  expect_setequal(names(out_rev), c("x", "y", "z"))
  expect_equal(out_rev$x, 1L)
  expect_true(is.na(out_rev$y))
  expect_equal(out_rev$z, 2L)
})
