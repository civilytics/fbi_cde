# Zero-row return frames must agree with their populated counterparts.
#
# A frame built from `matrix(nrow = 0, ...)` types every column `logical`,
# because that is `matrix()`'s default mode. The columns still have the right
# names, so a names-only test passes while `rbind(empty, populated)` errors or
# silently coerces a character ORI column to logical. Stacking results across
# geographies is the natural workflow, so this bites in ordinary use.
#
# The assertion that catches it is "empty agrees with populated" — not "empty
# has the types I expect". The latter passed while `.empty_place_agency_frame()`
# declared numeric lat/long that the populated frame returns as character.

# A minimal `summarized/agency/{ori}/{offense}` response in the nested-list
# shape cde_request() returns. Defined locally so this file stands alone when
# tests are run with a filter.
make_response <- function(name, counts, pop = 20000) {
  offense_key <- paste(name, "Offenses")
  actuals <- stats::setNames(list(as.list(counts)), offense_key)
  pop_series <- as.list(stats::setNames(rep(pop, length(counts)), names(counts)))
  pop_list <- stats::setNames(list(pop_series), name)
  list(
    offenses = list(actuals = actuals, rates = list()),
    populations = list(population = pop_list,
                       participated_population = pop_list)
  )
}

# Assert a zero-row frame agrees with a populated one on names and column types,
# and that stacking the two is lossless.
expect_frames_agree <- function(empty, populated) {
  col_types <- function(x) vapply(x, function(col) class(col)[1], character(1))

  testthat::expect_equal(nrow(empty), 0L)
  # Guard against the guard: a zero-row "populated" frame would make every
  # comparison below trivially true.
  testthat::expect_gt(nrow(populated), 0L)
  testthat::expect_equal(names(empty), names(populated))
  testthat::expect_equal(col_types(empty), col_types(populated))

  combined <- rbind(empty, populated)
  testthat::expect_equal(nrow(combined), nrow(populated))
  testthat::expect_equal(col_types(combined), col_types(populated))
}

# ---- county_agencies() -----------------------------------------------------

test_that("county_agencies empty frame agrees with its populated frame", {
  populated <- county_agencies("Alameda", "CA")
  empty <- suppressWarnings(county_agencies("Nowhere County", "CA"))

  expect_frames_agree(empty, populated)
  # The specific regression: ORI must survive the stack as character.
  expect_equal(rbind(empty, populated)$ori, populated$ori)
})

# ---- get_county_crime_detail() ---------------------------------------------

test_that("get_county_crime_detail empty frame agrees with its populated frame", {
  agencies <- data.frame(
    ori = "CA0000001",
    agency_name = "Alpha PD",
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = "TESTONIA",
    state_abbr = "CA",
    latitude = 0, longitude = 0,
    stringsAsFactors = FALSE
  )
  response <- make_response("Alpha PD", c("01-2021" = 10, "02-2021" = 12))

  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) response,
    .package = "fbiCDE"
  )

  populated <- get_county_crime_detail("Testonia", "CA", offense = "V",
                                       from = "01-2021", to = "02-2021")
  expect_frames_agree(.empty_detail_frame(), populated)
})

# ---- get_county_crime() ----------------------------------------------------

test_that("get_county_crime empty frame agrees with its populated frame", {
  detail <- data.frame(
    county_name = "TESTONIA", state_abbr = "CA", offense = "V",
    period = "01-2021", count = 10, population = 1000,
    participated_population = 1000, rate = 1, reported = TRUE,
    stringsAsFactors = FALSE
  )

  populated <- get_county_crime(detail)
  empty <- get_county_crime(detail[0, , drop = FALSE])

  expect_frames_agree(empty, populated)
})

# ---- place_agencies() ------------------------------------------------------

test_that("place_agencies empty frame agrees with its populated frame", {
  populated <- place_agencies("Lufkin", "TX")
  empty <- suppressWarnings(place_agencies("Nowheresville", "TX"))

  expect_frames_agree(empty, populated)
})

# ---- get_place_crime_detail() ----------------------------------------------

test_that("get_place_crime_detail empty frame agrees with its populated frame", {
  response <- make_response("Lufkin Police Department",
                                   c("01-2021" = 10, "02-2021" = 12))

  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) response,
    .package = "fbiCDE"
  )

  populated <- get_place_crime_detail("Lufkin", "TX",
                                      from = "01-2021", to = "02-2021")
  expect_frames_agree(.empty_place_detail_frame(), populated)
})

# ---- metro_agencies() ------------------------------------------------------

test_that("metro_agencies empty frame agrees with its populated frame", {
  populated <- metro_agencies("Pittsburgh, PA")
  empty <- suppressWarnings(metro_agencies("Nowhere Metro"))

  expect_frames_agree(empty, populated)
})

# ---- get_metro_crime_detail() ----------------------------------------------

test_that("get_metro_crime_detail empty frame agrees with its populated frame", {
  agencies <- data.frame(
    ori = "PA0000001",
    agency_name = "Alpha PD",
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = "ALLEGHENY",
    state_abbr = "PA",
    county_fips = "42003",
    latitude = "0", longitude = "0",
    cbsa_code = "38300",
    cbsa_title = "Pittsburgh, PA",
    cbsa_type = "metro",
    central_outlying = "Central",
    stringsAsFactors = FALSE
  )
  response <- make_response("Alpha PD", c("01-2021" = 10, "02-2021" = 12))

  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(path, query = list(), ...) response,
    .package = "fbiCDE"
  )

  populated <- get_metro_crime_detail("Pittsburgh, PA", from = "01-2021",
                                      to = "02-2021", progress = FALSE)
  expect_frames_agree(.empty_metro_detail_frame(), populated)
})
