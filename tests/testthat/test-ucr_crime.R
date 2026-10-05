# Offline tests for summarized crime functions using fixtures.
#
# Fixtures were recorded live from summarized/{level}/{offense} for
# 01-2019..03-2019 (see tests/testthat/fixtures/README.md). At agency/state
# level the API sends comparison series (state/national rates, no counts)
# alongside the geography's own. They are dropped by default and returned,
# labelled, with comparison = TRUE.

test_that("get_agency_crime returns only the agency's own series by default", {
  local_fbi_fixture("summarized-agency-CA0010900-V.json")
  result <- get_agency_crime("CA0010900", from = "01-2019", to = "03-2019")

  expect_equal(names(result),
               c("geography", "offense", "measure", "period", "count", "rate",
                 "population", "participated_population"))
  expect_equal(nrow(result), 6)
  expect_equal(unique(result$geography), "CA0010900")
  # The requested code, not a series label.
  expect_equal(unique(result$offense), "V")
  expect_equal(result$measure, rep(c("offenses", "clearances"), each = 3))
  expect_equal(result$period, rep(c("01-2019", "02-2019", "03-2019"), 2))
  expect_equal(result$count, c(472, 368, 438, 106, 40, 37))
  expect_equal(result$rate[1], 108.75)
})

test_that("get_agency_crime(comparison = TRUE) labels state and national series", {
  local_fbi_fixture("summarized-agency-CA0010900-V.json")
  result <- get_agency_crime("CA0010900", from = "01-2019", to = "03-2019",
                             comparison = TRUE)

  expect_equal(names(result),
               c("geography", "series", "series_name", "offense", "measure",
                 "period", "count", "rate", "population",
                 "participated_population"))
  expect_equal(nrow(result), 18)
  expect_equal(table(result$series)[c("agency", "state", "national")],
               c(agency = 6L, state = 6L, national = 6L),
               ignore_attr = TRUE)
  expect_equal(unique(result$series_name[result$series == "state"]),
               "California")
  # Comparison series carry a rate but never a count.
  expect_true(all(is.na(result$count[result$series != "agency"])))
  expect_false(anyNA(result$rate))
  expect_equal(
    result$rate[result$series == "national" & result$measure == "offenses"],
    c(27.84, 24.11, 27.66)
  )
})

test_that("get_estimated_crime parses national-level response", {
  local_fbi_fixture("summarized-national-V.json")
  result <- get_estimated_crime(from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$geography), "US")
  expect_equal(nrow(result), 6)
  expect_true("rate" %in% names(result))
  # National-level actuals are populated (unlike state-level comparison rows).
  expect_true(all(!is.na(result$count)))
})

test_that("get_estimated_crime parses state-level response", {
  local_fbi_fixture("summarized-state-CA-V.json")
  result <- get_estimated_crime("CA", from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$geography), "CA")
  expect_equal(nrow(result), 6)
  expect_equal(result$count[result$measure == "offenses"],
               c(13582, 12113, 14201))

  # The national comparison series comes back only on request.
  with_us <- get_estimated_crime("CA", from = "01-2019", to = "03-2019",
                                 comparison = TRUE)
  expect_equal(nrow(with_us), 12)
  expect_equal(unique(with_us$series), c("state", "national"))
  expect_true(all(is.na(with_us$count[with_us$series == "national"])))
})

test_that("get_estimated_arson parses arson response", {
  local_fbi_fixture("summarized-national-ARS.json")
  result <- get_estimated_arson(from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$geography), "US")
  expect_equal(nrow(result), 6)
  expect_true(all(!is.na(result$count)))
})

test_that("get_estimated_crime validates state abbreviation", {
  expect_error(get_estimated_crime(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_agency_crime validates ORI", {
  expect_error(get_agency_crime("bad-ori"), "Invalid ORI code")
})

test_that("get_estimated_crime rejects inverted date ranges", {
  expect_error(
    get_estimated_crime(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

# Live tests

test_that("get_agency_crime returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_agency_crime("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
})

test_that("get_estimated_crime returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_estimated_crime()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("get_estimated_arson returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_estimated_arson()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# ---- Series parsing ---------------------------------------------------------

test_that("periods sort chronologically, not as strings", {
  # As strings, "01-2020" sorts before "12-2019".
  counts <- list("Some PD Offenses" = list("01-2020" = 2, "12-2019" = 1))
  out <- .parse_series(counts, NULL, geography = "XX0000000", offense = "V",
                       level = "agency")
  expect_equal(out$period, c("12-2019", "01-2020"))
})

test_that("a series without counts falls back on its name to find its level", {
  # Counts suppressed: only rates. The agency series must not be mistaken for
  # a comparison series, nor the state for the agency.
  rates <- list(
    "Some PD Offenses" = list("01-2019" = 5),
    "Ohio Offenses" = list("01-2019" = 3),
    "United States Offenses" = list("01-2019" = 2)
  )
  out <- .parse_series(NULL, rates, geography = "OH0000000", offense = "V",
                       level = "agency", comparison = TRUE)
  expect_equal(out$series, c("agency", "state", "national"))
  expect_equal(out$series_name, c("Some PD", "Ohio", "United States"))
})

test_that("an unrecognised label is kept with measure NA", {
  counts <- list("Something Else" = list("01-2019" = 4))
  out <- .parse_series(counts, NULL, geography = "US", offense = "V",
                       level = "national")
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$measure))
})

test_that("each row carries its series' population and reporting coverage", {
  local_fbi_fixture("summarized-agency-CA0010900-V.json")
  fx <- read_fixture("summarized-agency-CA0010900-V.json")
  result <- get_agency_crime("CA0010900", from = "01-2019", to = "03-2019",
                             comparison = TRUE)

  pop_of <- function(map, name, period) as.numeric(map[[name]][[period]])
  for (i in seq_len(nrow(result))) {
    expect_equal(result$population[i],
                 pop_of(fx$populations$population, result$series_name[i],
                        result$period[i]))
    expect_equal(result$participated_population[i],
                 pop_of(fx$populations$participated_population,
                        result$series_name[i], result$period[i]))
  }
  expect_false(anyNA(result$population))
})

test_that("a series missing from the populations map gets NA, not an error", {
  counts <- list("Some PD Offenses" = list("01-2019" = 5))
  out <- .parse_series(counts, NULL, geography = "XX0000000", offense = "V",
                       level = "agency",
                       populations = list(population = list()))
  expect_true(is.na(out$population))
  expect_true(is.na(out$participated_population))
})
