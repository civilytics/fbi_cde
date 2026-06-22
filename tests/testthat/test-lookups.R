# Offline tests for lookup functions using fixtures.

test_that("get_offense_codes parses offense lookup response", {
  local_fbi_fixture("lookup-offenses.json")
  result <- get_offense_codes("crime-trend")

  expect_s3_class(result, "data.frame")
  expect_true("code" %in% names(result))
  expect_true("label" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("get_states parses state lookup response", {
  local_fbi_fixture("lookup-states.json")
  result <- get_states()

  expect_s3_class(result, "data.frame")
  expect_true("stateAbbreviation" %in% names(result))
  expect_true("stateName" %in% names(result))
  expect_true(nrow(result) > 0)
  expect_true("CA" %in% result$stateAbbreviation)
})

test_that("get_agencies parses agency lookup response by state", {
  local_fbi_fixture("agency-byStateAbbr-CA.json")
  result <- get_agencies()

  expect_s3_class(result, "data.frame")
  expect_true("ori" %in% names(result))
  expect_true(nrow(result) > 0)
})
