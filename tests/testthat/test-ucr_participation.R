# Offline tests for the modernized participation functions.
#
# The current CDE API has no dedicated participation-rate endpoint (see
# roxygen docs on get_agency_participation()); these functions instead
# report NIBRS reporting status/rate sourced from `agency/byStateAbbr/{state}`.
# The Rhode Island fixture (participation-agency-byStateAbbr-RI.json) has 49
# agencies, 48 reporting NIBRS and 1 (RI0050900) not.

test_that("get_agency_participation returns the matching agency", {
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_agency_participation("RI0020100")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$ori, "RI0020100")
  expect_equal(result$agency_name, "Coventry Police Department")
  expect_true(result$is_nibrs)
})

test_that("get_agency_participation reports non-participating agencies", {
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_agency_participation("RI0050900")

  expect_equal(nrow(result), 1)
  expect_false(result$is_nibrs)
  expect_true(is.na(result$nibrs_start_date))
})

test_that("get_agency_participation validates ORI", {
  expect_error(get_agency_participation("bad-ori"), "Invalid ORI code")
})

test_that("get_agency_participation looks NB and GM ORIs up as NE and GU", {
  # Nebraska's ORIs start NB and Guam's GM, but the CDE files them under NE
  # and GU; byStateAbbr/NB answers with an empty directory.
  seen <- character(0)
  fixture <- read_fixture("participation-agency-byStateAbbr-RI.json")
  testthat::local_mocked_bindings(
    cde_request = function(path, ...) {
      seen <<- c(seen, path)
      fixture
    },
    .package = "fbiCDE"
  )
  get_agency_participation("NB0010100")
  get_agency_participation("gm0010000")
  get_agency_participation("RI0020100")
  expect_equal(seen, c("agency/byStateAbbr/NE", "agency/byStateAbbr/GU",
                       "agency/byStateAbbr/RI"))
})

test_that("an empty agency directory is zero agencies, not an error", {
  # The CDE answers a state it does not know (AS, CZ, NB) with a query echo.
  local_fbi_fixture("participation-agency-byStateAbbr-AS.json")

  state <- get_state_participation("AS")
  expect_equal(state$total_agencies, 0L)
  expect_true(is.na(state$participation_rate))

  agency <- get_agency_participation("AS0010000")
  expect_equal(nrow(agency), 0L)
  expect_named(agency, names(parse_agency_participation_response(NULL)))

  # get_agencies() reads the same endpoint.
  expect_null(.flatten_agency_directory(
    read_fixture("participation-agency-byStateAbbr-AS.json")
  ))
})

test_that("get_region_participation queries the territories' directory keys", {
  # Guam is "GM" in the bundled table, which is_valid_state() rejected.
  seen <- character(0)
  fixture <- read_fixture("participation-agency-byStateAbbr-AS.json")
  testthat::local_mocked_bindings(
    cde_request = function(path, ...) {
      seen <<- c(seen, path)
      fixture
    },
    .package = "fbiCDE"
  )
  out <- get_region_participation("U.S. Territories")
  expect_equal(out$total_agencies, 0L)
  expect_setequal(seen, paste0("agency/byStateAbbr/", c("GU", "PR", "VI")))
})

test_that("get_state_participation returns an aggregate rate", {
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_state_participation("RI")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$state_abbr, "RI")
  expect_equal(result$total_agencies, 49)
  expect_equal(result$nibrs_agencies, 48)
  expect_equal(result$participation_rate, 48 / 49)
})

test_that("get_state_participation validates state abbreviation", {
  expect_error(get_state_participation("ZZ"), "Invalid state abbreviation")
})

test_that("get_region_participation aggregates across states in the region", {
  # Every state in the region is queried with the same mocked cde_request(),
  # so this exercises the aggregation logic (sum of per-state totals), not a
  # realistic region-wide count.
  local_fbi_fixture("participation-agency-byStateAbbr-RI.json")
  result <- get_region_participation("Northeast")

  n_states <- length(unique(
    fbi_api_agencies$state_abbr[fbi_api_agencies$region_name == "Northeast"]
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$region, "Northeast")
  expect_equal(result$total_agencies, 49 * n_states)
  expect_equal(result$nibrs_agencies, 48 * n_states)
})

test_that("get_region_participation validates region_name", {
  expect_error(get_region_participation("Nowhere"), "Invalid region_name")
})

# Live tests

test_that("get_agency_participation returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_agency_participation("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$ori, "CA0010900")
  expect_true(is.logical(result$is_nibrs))
})

test_that("get_state_participation returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_state_participation("RI")

  expect_s3_class(result, "data.frame")
  expect_true(result$total_agencies > 0)
  expect_true(result$participation_rate >= 0 && result$participation_rate <= 1)
})

test_that("get_region_participation returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_region_participation("West")

  expect_s3_class(result, "data.frame")
  expect_true(result$total_agencies > 0)
  expect_true(result$participation_rate >= 0 && result$participation_rate <= 1)
})

test_that("Nebraska and the territories resolve live", {
  skip_if_no_fbi_api()
  ne <- get_agency_participation("NB0010100")
  expect_equal(nrow(ne), 1L)
  expect_equal(ne$state_abbr, "NE")

  terr <- get_region_participation("U.S. Territories")
  expect_true(terr$total_agencies > 0)

  expect_equal(get_state_participation("AS")$total_agencies, 0L)
})
