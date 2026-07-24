# ---- Helpers: build in-memory detail frames matching .DETAIL_COLS shape ----

make_detail <- function(ori, offense = "V", periods, counts,
                        pops = NULL, part_pops = NULL, rates = NULL) {
  n <- length(periods)
  if (is.null(pops)) pops <- rep(20000, n)
  if (is.null(part_pops)) part_pops <- rep(20000, n)
  if (is.null(rates)) {
    rates <- ifelse(!is.na(counts) & part_pops > 0,
                    counts / part_pops * 1e5, NA_real_)
  }
  data.frame(
    ori = rep(ori, n),
    agency_name = paste0(substr(ori, 1, 2), " Agency"),
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = "TESTONIA",
    state_abbr = "CA",
    offense = rep(offense, n),
    period = periods,
    count = counts,
    population = pops,
    participated_population = part_pops,
    rate = rates,
    reported = !is.na(counts),
    stringsAsFactors = FALSE
  )
}

# ---- Layer 2 aggregate tests ----------------------------------------------

test_that("get_county_crime sums counts and uses jurisdiction_pop denominator", {
  # Two agencies in the same county, same period.
  a <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021"),
    counts = c(10, 12),
    pops = c(30000, 30000),
    part_pops = c(30000, 30000)
  )
  b <- make_detail(
    ori = "CA9990002",
    periods = c("01-2021", "02-2021"),
    counts = c(5, 8),
    pops = c(20000, 20000),
    part_pops = c(20000, 20000)
  )

  detail <- rbind(a, b)
  out <- get_county_crime(detail)

  expect_equal(nrow(out), 2L)
  expect_setequal(unique(out$period), c("01-2021", "02-2021"))

  jan <- out[out$period == "01-2021", , drop = FALSE]
  expect_equal(jan$count, 15L)  # 10 + 5
  expect_equal(jan$population, 50000L)  # 30000 + 20000 (jurisdiction_pop)
  expect_equal(jan$participated_population, 50000L)
  expect_equal(jan$rate, 15 / 50000 * 1e5)
  expect_equal(jan$denominator_type, "jurisdiction_pop")
  expect_equal(jan$coverage_fraction, 1.0)

  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_equal(feb$count, 20L)  # 12 + 8
})

test_that("get_county_crime handles partial coverage (missing periods)", {
  # Agency A reports all months; Agency B misses Feb.
  a <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021"),
    counts = c(10, 12),
    pops = c(30000, 30000),
    part_pops = c(30000, 30000)
  )
  b <- make_detail(
    ori = "CA9990002",
    periods = c("01-2021", "02-2021"),
    counts = c(5, NA),  # Feb is a gap (did not report)
    pops = c(20000, 20000),
    part_pops = c(20000, 10000)  # participated pop drops in Feb
  )

  detail <- rbind(a, b)
  out <- get_county_crime(detail)

  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_equal(feb$count, 12L)  # only A's count (B's is NA, na.rm=TRUE)
  expect_equal(feb$population, 50000L)  # jurisdiction_pop sums all pops
  expect_equal(feb$participated_population, 40000L)  # 30000 + 10000
  expect_equal(feb$coverage_fraction, 40000 / 50000)
})

test_that("get_county_crime uses participated_pop denominator when requested", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10,
    pops = 30000,
    part_pops = 25000
  )

  out <- get_county_crime(a, denominator = "participated_pop")

  expect_equal(out$population, 25000L)
  expect_equal(out$denominator_type, "participated_pop")
  expect_equal(out$coverage_fraction, 1.0)
})

test_that("get_county_crime uses census_pop denominator when provided", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10,
    pops = 30000,
    part_pops = 25000
  )
  a$census_population <- 100000L

  out <- get_county_crime(a, denominator = "census_pop")

  expect_equal(out$population, 100000L)
  expect_equal(out$denominator_type, "census_pop")
  expect_equal(out$coverage_fraction, 25000 / 100000)
})

test_that("get_county_crime errors without census_population column", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10
  )

  expect_error(get_county_crime(a, denominator = "census_pop"),
               "requires a")
})

test_that("get_county_crime rejects invalid denominator", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10
  )

  expect_error(get_county_crime(a, denominator = "invalid"),
               "must be one of")
})

test_that("get_county_crime rejects non-data.frame input", {
  expect_error(get_county_crime("not a data.frame"),
               "must be a data.frame")
})

test_that("get_county_crime rejects detail missing required columns", {
  expect_error(get_county_crime(data.frame(x = 1)),
               "missing required columns")
})

test_that("get_county_crime returns empty frame with correct columns for empty input", {
  empty <- .empty_detail_frame()
  out <- get_county_crime(empty)

  expect_equal(nrow(out), 0L)
  expect_setequal(names(out), .AGGREGATE_COLS)
})

test_that("get_county_crime aggregates across multiple offenses", {
  a <- make_detail(
    ori = "CA9990001",
    offense = "V",
    periods = "01-2021",
    counts = 10,
    pops = 30000,
    part_pops = 30000
  )
  b <- make_detail(
    ori = "CA9990001",
    offense = "P",
    periods = "01-2021",
    counts = 5,
    pops = 30000,
    part_pops = 30000
  )

  detail <- rbind(a, b)
  out <- get_county_crime(detail)

  expect_equal(nrow(out), 2L)
  expect_setequal(unique(out$offense), c("V", "P"))

  violent <- out[out$offense == "V", , drop = FALSE]
  expect_equal(violent$count, 10L)

  property <- out[out$offense == "P", , drop = FALSE]
  expect_equal(property$count, 5L)
})

test_that("get_county_crime aggregates across multiple counties", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10,
    pops = 30000,
    part_pops = 30000
  )
  a$county_name <- "ALAMEDA"

  b <- make_detail(
    ori = "CA9990002",
    periods = "01-2021",
    counts = 5,
    pops = 20000,
    part_pops = 20000
  )
  b$county_name <- "CONTRA COSTA"

  detail <- rbind(a, b)
  out <- get_county_crime(detail)

  expect_equal(nrow(out), 2L)
  expect_setequal(unique(out$county_name), c("ALAMEDA", "CONTRA COSTA"))

  alameda <- out[out$county_name == "ALAMEDA", , drop = FALSE]
  expect_equal(alameda$count, 10L)

  contra <- out[out$county_name == "CONTRA COSTA", , drop = FALSE]
  expect_equal(contra$count, 5L)
})

test_that("get_county_crime output has all .AGGREGATE_COLS", {
  a <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021"),
    counts = c(10, 12)
  )

  out <- get_county_crime(a)
  expect_setequal(names(out), .AGGREGATE_COLS)
})

# ---- Census join tests (offline, no network) ------------------------------

test_that("join_census_pop adds NA column when censusapi is not available", {
  skip_on_ci()  # CI may have censusapi installed; test is environment-dependent.

  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10
  )
  a$county_fips <- "06001"

  # censusapi is in Suggests and likely not installed; expect NA column + message.
  out <- suppressMessages(join_census_pop(a))
  expect_true("census_population" %in% names(out))
  expect_true(is.na(out$census_population))
})

test_that("join_census_pop rejects non-data.frame input", {
  expect_error(join_census_pop("not a data.frame"),
               "must be a data.frame")
})

test_that("join_census_pop rejects detail missing required columns", {
  expect_error(join_census_pop(data.frame(x = 1)),
               "missing required columns")
})

test_that("join_census_pop requires Census API key", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10
  )
  a$county_fips <- "06001"

  # Only test if censusapi is available (it's in Suggests).
  if (requireNamespace("censusapi", quietly = TRUE)) {
    expect_error(join_census_pop(a, key = ""),
                 "Census API key not found")
  }
})
