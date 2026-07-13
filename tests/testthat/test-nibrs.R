# Offline tests for NIBRS functions using fixtures.

# ---- get_nibrs_victim ----

test_that("get_nibrs_victim parses agency-level victim response", {
  local_fbi_fixture("nibrs-victim-robbery.json")
  result <- get_nibrs_victim("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_equal(result$offense[1], "robbery")
  expect_true(nrow(result) > 0)
})

test_that("get_nibrs_victim parses national-level victim response", {
  local_fbi_fixture("nibrs-victim-national-all.json")
  result <- get_nibrs_victim()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
})

test_that("get_nibrs_victim returns correct demographic_type for variable", {
  local_fbi_fixture("nibrs-victim-robbery.json")
  result <- get_nibrs_victim("CA0010900", variable = "race")

  expect_s3_class(result, "data.frame")
  expect_true(all(result$demographic_type == "race"))
  expect_true(all(result$demographic_value %in%
    c("White", "Black", "American Indian/Alaskan Native", "Asian/Pacific Islander", "Other")))
})

test_that("get_nibrs_victim returns empty for missing section", {
  # Issue #33: an empty result now emits a message so users don't mistake
  # the live API's current all-null NIBRS payload for a genuine zero count.
  local_mocked_bindings(
    cde_request = function(...) list(),
    .package = "fbi"
  )
  expect_message(result <- get_nibrs_victim("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_nibrs_victim returns empty for missing variable", {
  local_mocked_bindings(
    cde_request = function(...) list(victim = list()),
    .package = "fbi"
  )
  expect_message(result <- get_nibrs_victim("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# ---- get_nibrs_offender ----

test_that("get_nibrs_offender parses offender response", {
  local_fbi_fixture("nibrs-offender-all.json")
  result <- get_nibrs_offender("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_equal(result$offense[1], "all")
  expect_true(nrow(result) > 0)
})

test_that("get_nibrs_offender returns correct demographic_type for variable", {
  local_fbi_fixture("nibrs-offender-all.json")
  result <- get_nibrs_offender("CA0010900", variable = "ethnicity")

  expect_s3_class(result, "data.frame")
  expect_true(all(result$demographic_type == "ethnicity"))
  expect_true(all(result$demographic_value %in%
    c("Hispanic/Latino", "Not Hispanic/Latino", "Unknown")))
})

test_that("get_nibrs_offender returns empty for missing section", {
  local_mocked_bindings(
    cde_request = function(...) list(),
    .package = "fbi"
  )
  expect_message(result <- get_nibrs_offender("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# ---- get_nibrs_offense ----

test_that("get_nibrs_offense parses offense response", {
  local_fbi_fixture("nibrs-offense-all.json")
  result <- get_nibrs_offense("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "CA0010900")
  expect_equal(result$offense[1], "all")
  expect_true(nrow(result) > 0)
})

test_that("get_nibrs_offense returns correct demographic_type for variable", {
  local_fbi_fixture("nibrs-offense-all.json")
  result <- get_nibrs_offense("CA0010900", variable = "weapons")

  expect_s3_class(result, "data.frame")
  expect_true(all(result$demographic_type == "weapons"))
  expect_true(all(result$demographic_value %in%
    c("Firearm", "Knife/Cutting Instrument", "Hands/Feet/Legs", "Other", "Weapon Unknown")))
})

test_that("get_nibrs_offense returns empty for missing section", {
  local_mocked_bindings(
    cde_request = function(...) list(),
    .package = "fbi"
  )
  expect_message(result <- get_nibrs_offense("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# ---- Validation tests ----

test_that("get_nibrs_victim validates ORI", {
  expect_error(get_nibrs_victim("bad-ori"), "Invalid ORI code")
})

test_that("get_nibrs_victim validates state abbreviation", {
  expect_error(get_nibrs_victim(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_nibrs_victim rejects inverted date ranges", {
  expect_error(
    get_nibrs_victim(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_nibrs_offender validates ORI", {
  expect_error(get_nibrs_offender("bad-ori"), "Invalid ORI code")
})

test_that("get_nibrs_offender validates state abbreviation", {
  expect_error(get_nibrs_offender(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_nibrs_offender rejects inverted date ranges", {
  expect_error(
    get_nibrs_offender(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_nibrs_offense validates ORI", {
  expect_error(get_nibrs_offense("bad-ori"), "Invalid ORI code")
})

test_that("get_nibrs_offense validates state abbreviation", {
  expect_error(get_nibrs_offense(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_nibrs_offense rejects inverted date ranges", {
  expect_error(
    get_nibrs_offense(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

# ---- list_* functions ----

test_that("list_nibrs_offenses returns a character vector", {
  result <- list_nibrs_offenses()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true("robbery" %in% result)
})

test_that("list_nibrs_victim_variables returns a character vector", {
  result <- list_nibrs_victim_variables()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true("age" %in% result)
  expect_true("race" %in% result)
  expect_true("sex" %in% result)
})

test_that("list_nibrs_offender_variables returns a character vector", {
  result <- list_nibrs_offender_variables()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true("age" %in% result)
  expect_true("race" %in% result)
  expect_true("sex" %in% result)
})

test_that("list_nibrs_offense_variables returns a character vector", {
  result <- list_nibrs_offense_variables()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true("count" %in% result)
  expect_true("weapons" %in% result)
  expect_true("bias" %in% result)
})

# ---- Live tests ----
#
# As of 2026-07 the live NIBRS totals endpoint returns no data for any input
# (Issue #33) -- these assert today's actual (degraded) behavior, i.e. an
# empty frame plus the Issue #33 message, so a real regression (an error, or
# the message disappearing without rows resuming) still fails the suite. When
# #33 is resolved, flip these back to asserting nrow(result) > 0 with no
# message.

test_that("get_nibrs_victim returns expected shape from live API", {
  skip_if_no_fbi_api()
  expect_message(result <- get_nibrs_victim("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("geography" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
})

test_that("get_nibrs_offender returns expected shape from live API", {
  skip_if_no_fbi_api()
  expect_message(result <- get_nibrs_offender("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("geography" %in% names(result))
  expect_true("demographic_type" %in% names(result))
})

test_that("get_nibrs_offense returns expected shape from live API", {
  skip_if_no_fbi_api()
  expect_message(result <- get_nibrs_offense("CA0010900"), "returned no data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("geography" %in% names(result))
  expect_true("demographic_type" %in% names(result))
})
