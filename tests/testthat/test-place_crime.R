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
    .package = "fbi"
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
    .package = "fbi"
  )
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  expect_equal(names(out), .PLACE_DETAIL_COLS)
})

test_that("get_place_crime_detail strips state comparison rows", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbi"
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
    .package = "fbi"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_true(is.na(feb$count))
  expect_false(feb$reported)
})

test_that("get_place_crime_detail drops a failing ORI with a warning and records it", {
  testthat::local_mocked_bindings(
    cde_request = function(...) stop("503"),
    .package = "fbi"
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
  expect_error(
    get_place_crime_detail("Lufkin", "TX", from = "2021-01", to = "02-2021")
  )
})

# ---- Live API -------------------------------------------------------------

test_that("get_place_crime_detail works against the live API", {
  skip_if_no_fbi_api()
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), .PLACE_DETAIL_COLS)
  expect_gt(nrow(out), 0L)
})
