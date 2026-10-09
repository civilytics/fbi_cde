test_that("enumerate_periods lists inclusive MM-YYYY months", {
  expect_equal(enumerate_periods("01-2019", "03-2019"),
               c("01-2019", "02-2019", "03-2019"))
  expect_equal(enumerate_periods("11-2020", "02-2021"),
               c("11-2020", "12-2020", "01-2021", "02-2021"))
  expect_equal(enumerate_periods("05-2022", "05-2022"), "05-2022")
})

test_that("parse_agency_detail builds per-period rows with reported flag", {
  # simplifyVector = FALSE shape, as cde_request() returns. 02-2021 is a hole.
  response <- list(
    offenses = list(
      actuals = list(
        "Testville PD Offenses"   = list("01-2021" = 10, "03-2021" = 14),
        "Testville PD Clearances" = list("01-2021" = 3,  "03-2021" = 5)
      ),
      rates = list(
        "Testville PD Offenses" = list("01-2021" = 50, "03-2021" = 70),
        "California Offenses"    = list("01-2021" = 40)  # comparison — ignored
      )
    ),
    populations = list(
      population = list(
        "Testville PD" = list("01-2021" = 20000, "02-2021" = 20000,
                              "03-2021" = 20000)
      ),
      participated_population = list(
        "Testville PD" = list("01-2021" = 20000, "02-2021" = 20000,
                              "03-2021" = 20000)
      )
    )
  )

  out <- parse_agency_detail(response, ori = "CA9999999", offense = "V",
                             from = "01-2021", to = "03-2021")

  expect_equal(out$period, c("01-2021", "02-2021", "03-2021"))
  expect_equal(out$count, c(10, NA, 14))
  expect_equal(out$reported, c(TRUE, FALSE, TRUE))
  expect_equal(out$population, c(20000, 20000, 20000))
  expect_equal(out$offense, rep("V", 3))
  expect_equal(out$ori, rep("CA9999999", 3))
  # rate = count / participated_population * 1e5, per period.
  expect_equal(out$rate[1], 10 / 20000 * 1e5)
  expect_true(is.na(out$rate[2]))
  # Comparison series never leak in.
  expect_false(any(grepl("California", as.character(unlist(out)))))
})

test_that("parse_agency_detail distinguishes explicit-0 from missing periods", {
  # 02-2021 is present with an actual value of 0 (agency reported and had zero
  # offenses); 03-2021 is absent entirely (agency did not report). These must
  # not collapse to the same thing: reported-0 is `count = 0, reported = TRUE`;
  # missing is `count = NA, reported = FALSE`. This is the flag that lets
  # callers detect e.g. the 2021 California reporting collapse.
  response <- list(
    offenses = list(
      actuals = list(
        "Testville PD Offenses" = list("01-2021" = 10, "02-2021" = 0)
      )
    ),
    populations = list(
      population = list(
        "Testville PD" = list("01-2021" = 20000, "02-2021" = 20000,
                              "03-2021" = 20000)
      ),
      participated_population = list(
        "Testville PD" = list("01-2021" = 20000, "02-2021" = 20000,
                              "03-2021" = 20000)
      )
    )
  )

  out <- parse_agency_detail(response, ori = "CA9999999", offense = "V",
                             from = "01-2021", to = "03-2021")

  expect_equal(out$period, c("01-2021", "02-2021", "03-2021"))
  expect_equal(out$count, c(10, 0, NA))
  expect_equal(out$reported, c(TRUE, TRUE, FALSE))
  # Explicit 0 with a positive participated_population yields rate = 0, not NA.
  expect_equal(out$rate[2], 0)
  expect_true(is.na(out$rate[3]))
})

# Helper: a minimal one-agency response for the inline fan-out mock. `counts` is
# a named numeric vector, e.g. c("01-2021" = 10, "02-2021" = 12).
make_agency_response <- function(name, counts, pop = 20000) {
  offense_key <- paste(name, "Offenses")
  actuals <- setNames(list(as.list(counts)), offense_key)
  pop_series <- as.list(setNames(rep(pop, length(counts)), names(counts)))
  pop_list <- setNames(list(pop_series), name)
  list(
    offenses = list(actuals = actuals, rates = list()),
    populations = list(population = pop_list,
                       participated_population = pop_list)
  )
}

test_that("get_county_crime_detail fans out, filters, and reports drops", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002", "CA0000003"),
    agency_name = c("Alpha PD", "Beta University", "Gamma PD"),
    agency_type_name = c("City", "University or College", "City"),
    agency_class = c("municipal", "campus", "municipal"),
    county_name = "TESTONIA",
    state_abbr = "CA",
    county_fips = "06999",
    latitude = 0, longitude = 0,
    stringsAsFactors = FALSE
  )
  responses <- list(
    CA0000001 = make_agency_response(
      "Alpha PD", c("01-2021" = 10, "02-2021" = 12)),
    CA0000003 = make_agency_response(
      "Gamma PD", c("01-2021" = 4, "02-2021" = 6))
    # CA0000002 intentionally absent -> its request errors -> dropped.
  )

  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) {
      ori <- sub("^summarized/agency/([A-Za-z0-9]{9})/.*$", "\\1", path)
      r <- responses[[ori]]
      if (is.null(r)) stop("no data for ", ori)
      r
    },
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_county_crime_detail("Testonia", "CA", offense = "V",
                                   from = "01-2021", to = "02-2021"),
    "Dropped 1 agenc"
  )

  expect_s3_class(out, "data.frame")
  # 2 successful agencies x 2 periods = 4 rows; campus errored and was dropped.
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$ori), c("CA0000001", "CA0000003"))
  expect_equal(attr(out, "dropped"), "CA0000002")
  expect_true(all(c("agency_class", "count", "population", "rate",
                    "reported") %in% names(out)))
})

test_that("get_county_crime_detail returns .DETAIL_COLS-shaped empty frame when all agencies fail", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002"),
    agency_name = c("Alpha PD", "Beta University"),
    agency_type_name = c("City", "University or College"),
    agency_class = c("municipal", "campus"),
    county_name = "TESTONIA", state_abbr = "CA", county_fips = "06999",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) {
      stop("simulated total outage")
    },
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_county_crime_detail("Testonia", "CA", offense = "V",
                                   from = "01-2021", to = "02-2021"),
    "Dropped 2 agenc"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), .DETAIL_COLS)
  expect_equal(nrow(out), 0L)
  expect_setequal(attr(out, "dropped"), c("CA0000001", "CA0000002"))
})

test_that("get_county_crime_detail queries sub-state agencies by default; others on request", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002", "CA0000003", "CA0000004", "CA0000005"),
    agency_name = c("Alpha PD", "Beta University", "Gamma Transit",
                    "Highway Patrol: Testonia County", "Testonia Tribal PD"),
    agency_type_name = c("City", "University or College", "Other",
                         "State Police", "Tribal"),
    agency_class = c("municipal", "campus", "special", "state", "tribal"),
    county_name = "TESTONIA", state_abbr = "CA", county_fips = "06999",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) {
      make_agency_response("Any", c("01-2021" = 1))
    },
    .package = "fbiCDE"
  )
  queried <- function(...) {
    sort(unique(get_county_crime_detail("Testonia", "CA", from = "01-2021",
                                        to = "01-2021", ...)$ori))
  }

  # Default: city and campus police; not special, state or tribal.
  expect_equal(queried(), c("CA0000001", "CA0000002"))
  expect_equal(queried(include_statewide = TRUE),
               c("CA0000001", "CA0000002", "CA0000004"))
  expect_equal(queried(agency_class = "special"), "CA0000003")
  expect_equal(queried(agency_class = "tribal"), "CA0000005")
})

test_that("the fan-out parses a recorded agency response like get_agency_crime", {
  # Every other fan-out test uses a hand-built response; this one reads the
  # recorded Oakland fixture.
  oakland <- county_agencies("Alameda", "CA")
  oakland <- oakland[oakland$ori == "CA0010900", ]
  fixture <- read_fixture("summarized-agency-CA0010900-V.json")
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) oakland,
    cde_request = function(path, ...) fixture,
    .package = "fbiCDE"
  )
  detail <- get_county_crime_detail("Alameda", "CA", from = "01-2019",
                                    to = "03-2019")
  series <- get_agency_crime("CA0010900", from = "01-2019", to = "03-2019")
  offenses <- series[series$measure == "offenses", ]

  expect_equal(detail$period, offenses$period)
  expect_equal(detail$count, offenses$count)
  expect_equal(detail$population, offenses$population)
  expect_true(all(detail$reported))
  expect_equal(unique(detail$agency_county_names), "ALAMEDA")
})

test_that("an unknown county warns once, from the resolver", {
  warnings <- character(0)
  out <- withCallingHandlers(
    get_county_crime_detail("Nowhere", "CA", from = "01-2021", to = "01-2021"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(nrow(out), 0L)
  expect_length(warnings, 1L)
  expect_match(warnings, "No agencies match county")
})

test_that("get_county_crime_detail carries each agency's own county list", {
  agencies <- data.frame(
    ori = "OHCOP0000", agency_name = "Columbus Police Department",
    agency_type_name = "City", agency_class = "municipal",
    county_name = "DELAWARE", state_abbr = "OH", county_fips = "39041",
    latitude = 0, longitude = 0,
    agency_county_names = "DELAWARE; FAIRFIELD; FRANKLIN",
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) {
      make_agency_response("Columbus Police Department", c("01-2021" = 1))
    },
    .package = "fbiCDE"
  )
  out <- get_county_crime_detail("Delaware", "OH", from = "01-2021",
                                 to = "01-2021")
  expect_equal(names(out), .DETAIL_COLS)
  expect_equal(out$agency_county_names, "DELAWARE; FAIRFIELD; FRANKLIN")
})

test_that("get_county_crime_detail works live for a small county", {
  skip_if_no_fbi_api()
  out <- get_county_crime_detail("Alameda", "CA", offense = "V",
                                 from = "01-2019", to = "03-2019")
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)
  expect_true(all(c("ori", "count", "population", "reported") %in% names(out)))
  expect_true(all(out$agency_class %in%
                    c("county_primary", "municipal", "campus")))
  # Oakland PD is queried by default for Alameda County.
  expect_true("CA0010900" %in% out$ori)
})

test_that("get_county_agency_crime resolves the county_primary ORI", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000009"),
    agency_name = c("Alpha PD", "Testonia County Sheriff"),
    agency_type_name = c("City", "County"),
    agency_class = c("municipal", "county_primary"),
    county_name = "TESTONIA", state_abbr = "CA", county_fips = "06999",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  called <- new.env()
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    get_agency_crime = function(ori, ...) {
      called$ori <- ori
      data.frame(geography = ori, offense = "x", period = "01-2021",
                 count = 1, rate = 1, stringsAsFactors = FALSE)
    },
    .package = "fbiCDE"
  )
  out <- get_county_agency_crime("Testonia", "CA")
  expect_equal(called$ori, "CA0000009")   # the sheriff, not the city
  expect_s3_class(out, "data.frame")
})

test_that("get_county_agency_crime warns and uses the first ORI when multiple county_primary agencies exist", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000009", "CA0000008"),
    agency_name = c("Alpha PD", "Testonia County Sheriff",
                    "Testonia County Police Department"),
    agency_type_name = c("City", "County", "County"),
    agency_class = c("municipal", "county_primary", "county_primary"),
    county_name = "TESTONIA", state_abbr = "CA", county_fips = "06999",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  called <- new.env()
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    get_agency_crime = function(ori, ...) {
      called$ori <- ori
      data.frame(geography = ori, offense = "x", period = "01-2021",
                 count = 1, rate = 1, stringsAsFactors = FALSE)
    },
    .package = "fbiCDE"
  )
  expect_warning(out <- get_county_agency_crime("Testonia", "CA"),
                 "Multiple county-primary")
  expect_equal(called$ori, "CA0000009")   # prim$ori[1]: the first county_primary row
  expect_s3_class(out, "data.frame")
})

test_that("get_county_agency_crime errors when no county_primary exists", {
  agencies <- data.frame(
    ori = "CA0000001", agency_name = "Alpha PD",
    agency_type_name = "City", agency_class = "municipal",
    county_name = "TESTONIA", state_abbr = "CA", county_fips = "06999",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies, .package = "fbiCDE")
  expect_error(get_county_agency_crime("Testonia", "CA"),
               "No county-primary")
})
