# A minimal agency response in the nested-list shape cde_request() returns.
make_metro_response <- function(name, counts, pop = 20000) {
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

# A two-agency metro membership frame, so the fan-out is exercised with N >= 2.
fake_metro_agencies <- function() {
  data.frame(
    ori = c("PA0000001", "PA0000002"),
    agency_name = c("Alpha PD", "Beta PD"),
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = c("ALLEGHENY", "BUTLER"),
    state_abbr = "PA",
    county_fips = c("42003", "42019"),
    latitude = "0", longitude = "0",
    cbsa_code = "38300",
    cbsa_title = "Pittsburgh, PA",
    cbsa_type = "metro",
    central_outlying = c("Central", "Outlying"),
    stringsAsFactors = FALSE
  )
}

# ---- The max_agencies guard ------------------------------------------------

test_that("the guard errors before issuing ANY request", {
  called <- 0L
  big <- fake_metro_agencies()[rep(1:2, 60), , drop = FALSE]   # 120 agencies
  big$ori <- sprintf("PA%07d", seq_len(nrow(big)))

  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) big,
    cde_request = function(...) {
      called <<- called + 1L
      stop("must not be reached")
    },
    .package = "fbiCDE"
  )

  expect_error(
    get_metro_crime_detail("Big Metro", max_agencies = 10,
                           from = "01-2021", to = "01-2021"),
    "max_agencies"
  )
  # The whole point of the guard: no network work happened.
  expect_equal(called, 0L)
})

test_that("the guard error names the metro and the agency count", {
  big <- fake_metro_agencies()[rep(1:2, 60), , drop = FALSE]
  big$ori <- sprintf("PA%07d", seq_len(nrow(big)))

  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) big,
    .package = "fbiCDE"
  )

  err <- tryCatch(
    get_metro_crime_detail("Big Metro", max_agencies = 10,
                           from = "01-2021", to = "01-2021"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "Big Metro")
  expect_match(conditionMessage(err), "120")
})

test_that("max_agencies = Inf disables the guard", {
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(...) make_metro_response("Alpha PD",
                                                    c("01-2021" = 5)),
    .package = "fbiCDE"
  )

  out <- get_metro_crime_detail("Pittsburgh, PA", max_agencies = Inf,
                                from = "01-2021", to = "01-2021",
                                progress = FALSE)
  expect_gt(nrow(out), 0L)
})

# ---- Fan-out ---------------------------------------------------------------

test_that("get_metro_crime_detail returns per-agency-period rows", {
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(path, query = list(), ...) {
      make_metro_response("Alpha PD", c("01-2021" = 10, "02-2021" = 12))
    },
    .package = "fbiCDE"
  )

  out <- get_metro_crime_detail("Pittsburgh, PA", from = "01-2021",
                                to = "02-2021", progress = FALSE)

  expect_equal(nrow(out), 4L)                       # 2 agencies x 2 periods
  expect_setequal(unique(out$ori), c("PA0000001", "PA0000002"))
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
  expect_setequal(unique(out$central_outlying), c("Central", "Outlying"))
  expect_equal(names(out), .METRO_DETAIL_COLS)
})

test_that("a failing ORI is dropped with a warning and recorded", {
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(path, query = list(), ...) {
      if (grepl("PA0000002", path)) stop("503")
      make_metro_response("Alpha PD", c("01-2021" = 10))
    },
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_metro_crime_detail("Pittsburgh, PA", from = "01-2021",
                                  to = "01-2021", progress = FALSE),
    "Dropped"
  )
  expect_equal(nrow(out), 1L)                       # the surviving agency
  expect_equal(attr(out, "dropped"), "PA0000002")
})

test_that("an unknown metro returns a typed empty frame", {
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) .empty_metro_agency_frame(),
    .package = "fbiCDE"
  )
  out <- get_metro_crime_detail("Nowhere", from = "01-2021", to = "01-2021",
                                progress = FALSE)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_DETAIL_COLS)
})

test_that("a filter that empties a non-empty agency set warns (I2)", {
  # metro_agencies() itself resolves fine (2 municipal agencies), but the
  # agency_class filter matches none of them. Unlike the case where
  # metro_agencies() returns nothing to begin with, this must warn: both
  # siblings (get_county_crime_detail(), get_place_crime_detail()) already do,
  # and there is no reason for the metro path to be silent here.
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_metro_crime_detail("Pittsburgh, PA", agency_class = "tribal",
                                  from = "01-2021", to = "01-2021",
                                  progress = FALSE),
    "Pittsburgh, PA"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_DETAIL_COLS)
})

test_that("no filter warning when metro_agencies() already returned nothing", {
  # metro_agencies() warns for its own reasons (unknown metro, Connecticut,
  # etc.); get_metro_crime_detail() must not pile a second, redundant warning
  # on top when the frame was already empty before any filtering happened.
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) .empty_metro_agency_frame(),
    .package = "fbiCDE"
  )
  expect_no_warning(
    out <- get_metro_crime_detail("Nowhere", from = "01-2021", to = "01-2021",
                                  progress = FALSE)
  )
  expect_equal(nrow(out), 0L)
})

test_that("get_metro_crime_detail validates the date range", {
  # cde_validate_dates() coerces a malformed "yyyy-mm" string with as.numeric(),
  # which produces NA with a coercion warning, and the subsequent NA > NA
  # comparison is what actually raises the error -- not a deliberate
  # validation message. Match that real message (rather than a bare
  # expect_error()) so this test fails if the failure mode ever changes, and
  # suppress the one coercion warning the suite emits, as
  # test-place_crime.R does for the same case.
  suppressWarnings(expect_error(
    get_metro_crime_detail("Pittsburgh, PA", from = "2021-01", to = "01-2021"),
    "missing value where TRUE/FALSE needed"
  ))
})

# ---- max_agencies type safety (M5) -----------------------------------------

test_that("a non-numeric max_agencies errors instead of silently disabling the guard", {
  # "9" > "281" compares lexicographically and is FALSE, so a string
  # max_agencies would silently disable the guard -- the function's headline
  # safety feature -- rather than raising an error.
  expect_error(
    get_metro_crime_detail("Pittsburgh, PA", max_agencies = "9",
                           from = "01-2021", to = "01-2021")
  )
})

# ---- Live API --------------------------------------------------------------

test_that("get_metro_crime_detail works against the live API", {
  skip_if_no_fbi_api()
  # A small micro area keeps the live fan-out cheap.
  out <- get_metro_crime_detail("Aberdeen, WA", from = "01-2019",
                                to = "03-2019", progress = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), .METRO_DETAIL_COLS)
})
