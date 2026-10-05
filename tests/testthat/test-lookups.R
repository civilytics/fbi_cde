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
    .package = "fbiCDE"
  )
  result <- get_offense_codes("nibrs")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("code", "label"))
  expect_equal(nrow(result), 0)
})

test_that("get_states parses state lookup response", {
  # lookup-states.json recorded live from lookup/states -- a nested
  # {get_states: {cde_states_query: {states: [{abbr, name}]}}} shape.
  local_fbi_fixture("lookup-states.json")
  result <- get_states()

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("stateAbbreviation", "stateName"))
  expect_equal(nrow(result), 51)
  expect_true("CA" %in% result$stateAbbreviation)
  expect_equal(result$stateName[result$stateAbbreviation == "CA"], "California")
})

# get_agencies() calls get_states() first, then the agency directory once per
# state. The RI fixture is a recorded `agency/byStateAbbr/RI` response (a
# county-keyed object, 49 agencies); every other state answers with an empty
# object. Parsed both ways because the live seam uses simplifyVector = FALSE
# while read_fixture() simplifies -- the old test only exercised the latter,
# which is how a parser that produced one junk row per state went unnoticed.
mock_agency_directory <- function(simplify, fail_state = NULL) {
  paths <- character(0)
  states <- read_fixture("lookup-states.json")
  ri <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "participation-agency-byStateAbbr-RI.json"),
    simplifyVector = simplify
  )
  fun <- function(path, ...) {
    paths <<- c(paths, path)
    if (identical(path, "lookup/states")) return(states)
    if (!is.null(fail_state) &&
        identical(path, paste0("agency/byStateAbbr/", fail_state))) {
      stop("HTTP 500 for ", path, call. = FALSE)
    }
    if (identical(path, "agency/byStateAbbr/RI")) ri else list()
  }
  list(fun = fun, paths = function() paths)
}

for (simplify in c(FALSE, TRUE)) {
  test_that(paste("get_agencies flattens the county-keyed directory, simplify =",
                  simplify), {
    m <- mock_agency_directory(simplify)
    testthat::local_mocked_bindings(cde_request = m$fun, .package = "fbiCDE")
    result <- get_agencies()

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 49L)
    expect_false(any(duplicated(result$ori)))
    expect_true(all(grepl("^RI", result$ori)))
    expect_true(all(c("ori", "agency_name", "agency_type_name", "counties",
                      "is_nibrs") %in% names(result)))
    expect_true("Coventry Police Department" %in% result$agency_name)
    expect_equal(result$ori, sort(result$ori))
    # One directory request per state, on the endpoint that exists.
    dir_paths <- setdiff(m$paths(), "lookup/states")
    expect_true(all(grepl("^agency/byStateAbbr/[A-Z]{2}$", dir_paths)))
    expect_true("agency/byStateAbbr/RI" %in% dir_paths)
  })
}

test_that("get_agencies skips a failing state with a warning", {
  m <- mock_agency_directory(FALSE, fail_state = "CA")
  testthat::local_mocked_bindings(cde_request = m$fun, .package = "fbiCDE")
  expect_warning(result <- get_agencies(), "1 state: CA")
  expect_equal(nrow(result), 49L)
  expect_equal(attr(result, "failed_states"), "CA")
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

test_that("get_agencies returns one row per agency from live API", {
  skip_if_no_fbi_api()
  result <- get_agencies()

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 15000L)
  expect_false(any(duplicated(result$ori)))
  expect_true("CA0010900" %in% result$ori)
})

test_that("get_states returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_states()

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("stateAbbreviation", "stateName"))
  expect_true(nrow(result) > 0)
  expect_true("CA" %in% result$stateAbbreviation)
})
