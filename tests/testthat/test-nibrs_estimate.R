test_that("the estimates lookup parses into the bundled vocabularies", {
  parsed <- .parse_estimation_lookup(
    read_fixture("nibrs-estimation-lookup-all.json")
  )
  expect_equal(parsed$states, nibrs_estimation_states)
  expect_equal(parsed$indicators, nibrs_estimation_indicators)
  expect_equal(parsed$years, nibrs_estimation_years)
})

test_that("estimate state ids are the lookup's own, not FIPS", {
  st <- nibrs_estimation_states
  ids <- st$id
  names(ids) <- st$state_abbr
  expect_equal(unname(ids[c("AK", "AL", "CA", "OH", "TX", "DC")]),
               c(1L, 2L, 6L, 39L, 48L, 10L))
  # "Federal" (98) and "Other" (99) have no state abbreviation.
  expect_false(any(c("Federal", "Other") %in% st$state_name))
  expect_equal(nrow(st), 57L)
  expect_false(anyNA(st$state_abbr))
})

test_that("every supported offense maps to exactly one estimate indicator", {
  ind <- nibrs_estimation_indicators
  for (code in names(.ESTIMATE_OFFENSES)) {
    hits <- ind$id[ind$indicator == .ESTIMATE_OFFENSES[[code]]$indicator]
    expect_length(hits, 1L)
  }
  expect_equal(
    ind$id[ind$indicator == .ESTIMATE_OFFENSES$V$indicator], 133L
  )
  expect_equal(.ESTIMATE_OFFENSES$V$nibrs,
               c("09A", "11A", "11B", "11C", "120", "13A"))
})

test_that("the bundled estimate vocabularies match the live lookup", {
  skip_if_no_fbi_api()
  # Fails when the FBI adds a year: rebuild with data-raw/api_vocabularies.R.
  live <- .parse_estimation_lookup(cde_request("nibrs-estimation/lookup/all"))
  expect_equal(live$years, nibrs_estimation_years)
  expect_equal(live$states, nibrs_estimation_states)
  expect_equal(live$indicators, nibrs_estimation_indicators)
})

# Capture the request a call makes, without needing a response.
capture_request <- function(env = parent.frame()) {
  seen <- new.env()
  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$path <- path
      seen$query <- query
      stop("captured")
    },
    .package = "fbiCDE", .env = env
  )
  seen
}

test_that("get_nibrs_estimate parses a state's offense table", {
  seen <- new.env()
  fixture <- read_fixture("nibrs-estimation-state-6-133-2022.json")
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$path <- path
      seen$query <- query
      fixture
    },
    .package = "fbiCDE"
  )
  out <- get_nibrs_estimate("CA", year = 2022)
  expect_equal(seen$path, "nibrs-estimation/state/6/133")
  expect_equal(seen$query, list(year = 2022L))
  expect_equal(names(out), c("geography", "year", "offense", "indicator",
                             "table", "variable", "category", "estimate",
                             "lower_bound", "upper_bound"))
  total <- out[out$variable == "Offense Count - Total", ]
  expect_equal(nrow(total), 1L)
  expect_equal(total$category, "Count - Total")
  expect_equal(total$estimate, 207381)
  expect_equal(total$lower_bound, 183100.636344443)
  expect_equal(total$upper_bound, 231662.98194076)
  expect_equal(unique(out$geography), "CA")
  expect_equal(unique(out$year), 2022L)
  expect_equal(unique(out$indicator), 133L)
  expect_equal(unique(out$table), "Offense")
})

test_that("get_nibrs_estimate parses the national estimate", {
  seen <- new.env()
  fixture <- read_fixture("nibrs-estimation-national-133-2021.json")
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$path <- path
      fixture
    },
    .package = "fbiCDE"
  )
  out <- get_nibrs_estimate(year = 2021)
  expect_equal(seen$path, "nibrs-estimation/national/133")
  expect_equal(unique(out$geography), "US")
  expect_equal(out$estimate[out$variable == "Offense Count - Total"],
               1325991)
})

test_that("null bounds parse to NA and negative bounds are kept", {
  local_fbi_fixture("nibrs-estimation-state-39-133-2022.json")
  out <- get_nibrs_estimate("OH", year = 2022, table = "Arrest")
  under5 <- out[out$variable == "Arrestee age" & out$category == "Under 5", ]
  expect_equal(under5$estimate, 0)
  expect_true(is.na(under5$lower_bound))
  expect_true(is.na(under5$upper_bound))
  aian <- out[out$variable == "Arrestee race" &
                out$category == "American Indian or Alaska Native", ]
  expect_equal(aian$estimate, 30)
  expect_equal(aian$lower_bound, -4.22302049047399)
})

test_that("offense takes a summary code or an indicator id", {
  seen <- capture_request()
  expect_error(get_nibrs_estimate("TX", 2022, offense = "rob"), "captured")
  expect_equal(seen$path, "nibrs-estimation/state/48/121")
  expect_error(get_nibrs_estimate(year = 2022, offense = 58), "captured")
  expect_equal(seen$path, "nibrs-estimation/national/58")
  expect_error(get_nibrs_estimate(year = 2022, offense = "58"), "captured")
  expect_equal(seen$path, "nibrs-estimation/national/58")
})

test_that("bad arguments are refused before any request", {
  local_mocked_bindings(
    cde_request = function(...) stop("no request expected"),
    .package = "fbiCDE"
  )
  expect_error(get_nibrs_estimate("XX", 2022), "Invalid state")
  expect_error(get_nibrs_estimate("CA", 2022, offense = "13A"),
               "must be one of")
  expect_error(get_nibrs_estimate("CA", 2022, offense = 99999),
               "must be one of")
  expect_error(get_nibrs_estimate("CA", "2022"), "single year")
  expect_error(get_nibrs_estimate("CA", 2021:2022), "single year")
  expect_error(get_nibrs_estimate("CA", 2022, table = "Bogus"))
})

test_that("get_nibrs_estimate returns a total for every supported offense live", {
  skip_if_no_fbi_api()
  for (code in names(.ESTIMATE_OFFENSES)) {
    out <- get_nibrs_estimate("TX", year = 2022, offense = code)
    total <- out$estimate[out$variable == "Offense Count - Total"]
    expect_length(total, 1L)
    expect_gt(total, 0)
  }
})

test_that("a year without estimates fails with the API's list of years live", {
  skip_if_no_fbi_api()
  expect_error(get_nibrs_estimate("CA", year = 2020), "2021")
})
