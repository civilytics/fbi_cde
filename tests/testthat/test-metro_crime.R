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
    .package = "fbi"
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
    .package = "fbi"
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
    .package = "fbi"
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
    .package = "fbi"
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
    .package = "fbi"
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
    .package = "fbi"
  )
  out <- get_metro_crime_detail("Nowhere", from = "01-2021", to = "01-2021",
                                progress = FALSE)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_DETAIL_COLS)
})

test_that("get_metro_crime_detail validates the date range", {
  expect_error(
    get_metro_crime_detail("Pittsburgh, PA", from = "2021-01", to = "01-2021")
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
