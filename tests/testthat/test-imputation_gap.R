# Answer each request imputation_gap() makes for California with the matching
# recorded fixture, and record the order of the requests.
local_gap_fixtures <- function(env = parent.frame()) {
  seen <- new.env()
  seen$paths <- character(0)
  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$paths <- c(seen$paths, path)
      year <- if (!is.null(query$year)) query$year else
        sub("^\\d{2}-", "", query$from)
      name <- if (path == "nibrs-estimation/state/6/133") {
        sprintf("nibrs-estimation-state-6-133-%s.json", year)
      } else if (path == "summarized/state/CA/V") {
        sprintf("summarized-state-CA-V-%s.json", year)
      } else if (startsWith(path, "nibrs/state/CA/")) {
        stopifnot(identical(query$type, "counts"))
        sprintf("nibrs-counts-state-CA-%s-%s.json", basename(path), year)
      } else {
        stop("unexpected request: ", path)
      }
      read_fixture(name)
    },
    .package = "fbiCDE", .env = env
  )
  seen
}

test_that("imputation_gap splits California 2022 by reporting format", {
  local_gap_fixtures()
  out <- imputation_gap("CA", year = 2022)
  expect_equal(names(out), c(
    "geography", "year", "offense", "reported", "reported_nibrs",
    "reported_other", "coverage", "coverage_nibrs", "published",
    "published_lower", "published_upper", "gap", "gap_lower", "gap_upper",
    "share_estimated", "share_lower", "share_upper"
  ))
  expect_equal(nrow(out), 1L)
  expect_equal(out$geography, "CA")
  expect_equal(out$year, 2022L)
  expect_equal(out$offense, "V")
  expect_equal(out$reported, 193824)
  expect_equal(out$reported_nibrs, 71625)
  expect_equal(out$reported_other, 122199)
  expect_equal(out$coverage, 0.987835, tolerance = 1e-6)
  expect_equal(out$coverage_nibrs, 0.465859, tolerance = 1e-6)
  expect_equal(out$published, 207381)
  expect_equal(out$gap, 135756)
  expect_equal(out$gap_lower, 183100.636344443 - 71625)
  expect_equal(out$gap_upper, 231662.98194076 - 71625)
  expect_equal(out$share_estimated, 0.6546212, tolerance = 1e-6)
  expect_equal(out$share_lower, 0.6088217, tolerance = 1e-6)
  expect_equal(out$share_upper, 0.6908224, tolerance = 1e-6)
})

test_that("a NIBRS-only year gives a negative reported_other, not zero", {
  local_gap_fixtures()
  out <- imputation_gap("CA", year = 2021)
  # In 2021 the FBI took NIBRS only; the two reported series differ by
  # counting rules alone.
  expect_equal(out$reported, 36439)
  expect_equal(out$reported_nibrs, 36943)
  expect_equal(out$reported_other, -504)
  expect_equal(out$coverage, out$coverage_nibrs, tolerance = 1e-6)
  expect_equal(out$coverage, 0.2632175, tolerance = 1e-6)
  expect_equal(out$gap, 180596)
  expect_equal(out$share_estimated, 0.8301776, tolerance = 1e-6)
})

test_that("several years give one row each, estimate request first", {
  seen <- local_gap_fixtures()
  out <- imputation_gap("CA", year = c(2021, 2022))
  expect_equal(out$year, c(2021L, 2022L))
  expect_equal(out$published, c(217539, 207381))
  expect_equal(seen$paths[1], "nibrs-estimation/state/6/133")
  expect_equal(seen$paths[2], "summarized/state/CA/V")
  expect_equal(seen$paths[3:8],
               paste0("nibrs/state/CA/",
                      c("09A", "11A", "11B", "11C", "120", "13A")))
  expect_length(seen$paths, 16L)
})

test_that("a lower-case state gives the same result", {
  local_gap_fixtures()
  expect_equal(imputation_gap("ca", year = 2022),
               imputation_gap("CA", year = 2022))
})

test_that("a year without estimates fails before any other request", {
  seen <- new.env()
  seen$paths <- character(0)
  local_mocked_bindings(
    cde_request = function(path, ...) {
      seen$paths <- c(seen$paths, path)
      stop("HTTP 400 - Bad request, an invalid year was sent. NIBRS ",
           "Estimation currently only supports the years: 2021,2022")
    },
    .package = "fbiCDE"
  )
  expect_error(imputation_gap("CA", year = 2020), "2021,2022")
  expect_equal(seen$paths, "nibrs-estimation/state/6/133")
})

test_that("a state without a published estimate says so", {
  seen <- new.env()
  seen$paths <- character(0)
  fixture <- read_fixture("nibrs-estimation-state-5-133-2022.json")
  local_mocked_bindings(
    cde_request = function(path, ...) {
      seen$paths <- c(seen$paths, path)
      fixture
    },
    .package = "fbiCDE"
  )
  expect_equal(nrow(get_nibrs_estimate("AZ", year = 2022)), 0L)
  seen$paths <- character(0)
  expect_error(imputation_gap("AZ", year = 2022),
               "The FBI publishes no V total for AZ in 2022.", fixed = TRUE)
  expect_equal(seen$paths, "nibrs-estimation/state/5/133")
})

test_that("unsupported arguments are refused before any request", {
  local_mocked_bindings(
    cde_request = function(...) stop("no request expected"),
    .package = "fbiCDE"
  )
  expect_error(imputation_gap("CA", 2022, offense = "13A"), "must be one of")
  expect_error(imputation_gap("CA", 2022, offense = "SHR"), "must be one of")
  expect_error(imputation_gap("CA", 2022, offense = c("V", "P")),
               "must be one of")
  expect_error(imputation_gap("XX", 2022), "Invalid state")
  expect_error(imputation_gap("CA", "2022"), "years")
  expect_error(imputation_gap("CA", 2022.5), "years")
})

test_that("Texas 2022 is almost entirely reported in NIBRS live", {
  skip_if_no_fbi_api()
  out <- imputation_gap("TX", year = 2022)
  expect_gt(out$coverage_nibrs, 0.95)
  expect_lt(abs(out$reported_other) / out$reported, 0.02)
  expect_lt(abs(out$share_estimated), 0.02)
})

test_that("every supported offense's estimate matches Texas's NIBRS counts live", {
  skip_if_no_fbi_api()
  # Texas 2022 reported almost entirely in NIBRS, so the estimate and its
  # NIBRS counts agree closely. An offense mapped to the wrong indicator
  # (RPE to "Rape", NIBRS 11A only) was 47% off.
  for (code in names(.ESTIMATE_OFFENSES)) {
    out <- imputation_gap("TX", year = 2022, offense = code)
    expect_lt(abs(out$share_estimated), 0.1, label = code)
  }
})

test_that("Arizona has no published estimate live", {
  skip_if_no_fbi_api()
  # Fails if the FBI starts publishing Arizona's estimate: the offline test
  # then needs another state's recorded empty response.
  expect_error(imputation_gap("AZ", year = 2022), "publishes no V total")
})

test_that("the national gap covers both estimate years live", {
  skip_if_no_fbi_api()
  out <- imputation_gap(year = 2021:2022)
  expect_equal(out$geography, c("US", "US"))
  expect_true(all(out$reported_nibrs > 9e5))
  expect_true(all(out$published > out$reported_nibrs))
})
