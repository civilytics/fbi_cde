# Offline tests for lookup functions using fixtures.

test_that("get_offense_codes parses offense lookup response", {
  # lookup-offenses.json recorded live from lookup/offenses?type=crime-trend
  # -- a nested {crimeGroups: [{label, crimes: [{label, value}]}]} shape
  # (Issue #32).
  local_fbi_fixture("lookup-offenses.json")
  result <- get_offense_codes("crime-trend")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("code", "label"))
  expect_equal(nrow(result), 72)
  expect_equal(result$code[1], "ASS")
  expect_equal(result$label[1], "Aggravated Assault")
})

test_that("get_offense_codes returns empty data.frame when crimeGroups is null", {
  testthat::local_mocked_bindings(
    cde_request = function(...) list(crimeGroups = NULL),
    .package = "fbi"
  )
  result <- get_offense_codes("nibrs")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("code", "label"))
  expect_equal(nrow(result), 0)
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

# Live tests

test_that("get_offense_codes returns expected shape from live API", {
  skip_if_no_fbi_api()

  for (type in c("crime-trend", "arrest", "hate-crime")) {
    result <- get_offense_codes(type)

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), c("code", "label"))
    expect_true(nrow(result) > 0)
    expect_false(any(result$code == "crimeGroups"))
  }
})
