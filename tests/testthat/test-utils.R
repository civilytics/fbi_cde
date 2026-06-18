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

test_that("is_valid_ori checks membership in the bundled agency list", {
  expect_true(is_valid_ori("CA0010900"))
  expect_false(is_valid_ori("not-an-ori"))
  # vectorised
  expect_equal(is_valid_ori(c("CA0010900", "not-an-ori")), c(TRUE, FALSE))
})

test_that("clean_column_names lowercases, renames, and drops csv_header", {
  cleaned <- clean_column_names(
    data.frame(DATA_YEAR = 1, MVT = 2, csv_header = 3)
  )
  expect_named(cleaned, c("year", "motor_vehicle_theft"))
})
