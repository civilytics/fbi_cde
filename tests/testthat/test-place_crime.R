# A minimal `summarized/agency/{ori}/{offense}` response in the nested-list
# shape cde_request() returns (simplifyVector = FALSE). Includes a state
# comparison series that must be stripped.
fake_agency_response <- function(label = "Lufkin Police Department",
                                 counts = list(`01-2021` = 10, `02-2021` = 12),
                                 pops = list(`01-2021` = 20000, `02-2021` = 20000)) {
  offense_key <- paste(label, "Offenses")
  actuals <- list()
  actuals[[offense_key]] <- counts
  actuals[["Texas Offenses"]] <- list(`01-2021` = 999, `02-2021` = 999)

  populations <- list(
    population = stats::setNames(list(pops), label),
    participated_population = stats::setNames(list(pops), label)
  )
  list(offenses = list(actuals = actuals), populations = populations)
}

test_that("get_place_crime_detail returns per-period rows for the place's agency", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbiCDE"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_equal(out$period, c("01-2021", "02-2021"))
  expect_equal(out$count, c(10, 12))
  expect_equal(unique(out$place_name), "Lufkin")
  expect_equal(unique(out$agency_class), "place_primary")
  expect_equal(unique(out$attribution), "name_identity")
  expect_true(all(out$reported))
})

test_that("get_place_crime_detail returns the documented columns in order", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbiCDE"
  )
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  expect_equal(names(out), .PLACE_DETAIL_COLS)
})

test_that("get_place_crime_detail strips state comparison rows", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbiCDE"
  )
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  # The Texas comparison series carries 999; it must never reach the output.
  expect_false(any(out$count == 999))
})

test_that("get_place_crime_detail flags a non-reporting period rather than zero", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(
      counts = list(`01-2021` = 10)  # 02-2021 absent entirely
    ),
    .package = "fbiCDE"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_true(is.na(feb$count))
  expect_false(feb$reported)
})

test_that("get_place_crime_detail drops a failing ORI with a warning and records it", {
  testthat::local_mocked_bindings(
    cde_request = function(...) stop("503"),
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021"),
    "Dropped"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_DETAIL_COLS)
  expect_true(length(attr(out, "dropped")) == 1L)
})

test_that("get_place_crime_detail warns and returns an empty frame for an unknown place", {
  # The warning comes from place_agencies(); assert it, then assert the shape.
  expect_warning(
    out <- get_place_crime_detail("Nowheresville", "TX",
                                  from = "01-2021", to = "02-2021"),
    "No municipal agency"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_DETAIL_COLS)
})

test_that("get_place_crime_detail validates the date range", {
  suppressWarnings(expect_error(
    get_place_crime_detail("Lufkin", "TX", from = "2021-01", to = "02-2021")
  ))
})

# ---- agencies = (composition with add_place_spatial_members()) -----------

fake_place_agencies <- function(n = 1) {
  data.frame(
    ori = if (n == 1) "TX1234567" else c("TX1234567", "TX7654321"),
    agency_name = if (n == 1) "Lufkin Police Department" else
      c("Lufkin Police Department", "Angelina College"),
    agency_type_name = if (n == 1) "City" else c("City", "University or College"),
    agency_class = if (n == 1) "place_primary" else c("place_primary", "campus"),
    place_name = "Lufkin",
    county_name = "ANGELINA",
    state_abbr = "TX",
    attribution = if (n == 1) "name_identity" else
      c("name_identity", "point_in_polygon"),
    stringsAsFactors = FALSE
  )
}

test_that("get_place_crime_detail uses a supplied agencies frame instead of re-resolving", {
  called <- FALSE
  testthat::local_mocked_bindings(
    place_agencies = function(...) {
      called <<- TRUE
      stop("place_agencies() should not be called when agencies is supplied")
    },
    .package = "fbiCDE"
  )
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbiCDE"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021",
                                agencies = fake_place_agencies(1))

  expect_false(called)
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out$agency_class), "place_primary")
})

test_that("get_place_crime_detail can reach campus agencies via a supplied agencies frame", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(label = "Angelina College"),
    .package = "fbiCDE"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021",
                                agencies = fake_place_agencies(2),
                                agency_class = "campus")

  expect_equal(nrow(out), 2L)
  expect_equal(unique(out$agency_class), "campus")
  expect_equal(unique(out$attribution), "point_in_polygon")
})

test_that("get_place_crime_detail queries supplied campus members by default", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbiCDE"
  )
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021",
                                agencies = fake_place_agencies(2))
  expect_setequal(unique(out$agency_class), c("place_primary", "campus"))
})

test_that("get_place_crime_detail warns rather than silently returning empty when a filter is unsatisfiable", {
  expect_warning(
    out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021",
                                  agencies = fake_place_agencies(1),
                                  agency_class = "special"),
    "No agencies to query"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_DETAIL_COLS)
})

test_that("get_place_crime_detail errors clearly on an invalid agencies argument", {
  expect_error(
    get_place_crime_detail("Lufkin", "TX", agencies = "not a data.frame"),
    "must be a data.frame"
  )
  expect_error(
    get_place_crime_detail("Lufkin", "TX", agencies = data.frame(ori = "TX1234567")),
    "missing required columns"
  )
})

test_that("get_place_crime_detail: with 2 supplied agencies, one failing ORI is dropped and the other survives", {
  agencies <- fake_place_agencies(2)
  testthat::local_mocked_bindings(
    cde_request = function(path, ...) {
      if (grepl("TX7654321", path, fixed = TRUE)) {
        stop("503")
      }
      fake_agency_response()
    },
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021",
                                  agencies = agencies, agency_class = c("place_primary", "campus")),
    "Dropped 1 agenc"
  )

  expect_equal(nrow(out), 2L)
  expect_equal(unique(out$ori), "TX1234567")
  expect_equal(attr(out, "dropped"), "TX7654321")
})

# ---- Live API -------------------------------------------------------------

test_that("get_place_crime_detail works against the live API", {
  skip_if_no_fbi_api()
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), .PLACE_DETAIL_COLS)
  expect_gt(nrow(out), 0L)
})
