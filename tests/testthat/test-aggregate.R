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
    county_name = "TESTONIA",
    state_abbr = "CA",
    county_fips = "06999",
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

# This test verifies that when censusapi is NOT installed (or not available),
# join_census_pop returns the detail frame with an all-NA census_population
# column and a message — rather than erroring. We inject census_fun = NULL
# and simulate the "not available" condition by mocking .census_deps_available().
#
# This runs on CI too: it does NOT depend on whether censusapi happens to be
# installed in the CI environment, because we control that via local_mocked_bindings.
# NOTE: mocking base::requireNamespace() directly (.package = "base") does NOT
# work here — it doesn't intercept the unqualified call inside join_census_pop(),
# so the check silently falls through to the real requireNamespace() result. The
# dependency check is isolated into .census_deps_available() (see R/census.R)
# specifically so it can be mocked within the package's own namespace, mirroring
# .spatial_deps_available() in R/place_spatial.R.
test_that("join_census_pop adds NA column when censusapi is not available", {
  a <- make_detail(
    ori = "CA9990001",
    periods = "01-2021",
    counts = 10
  )
  a$county_fips <- "06001"

  local_mocked_bindings(
    .census_deps_available = function() FALSE,
    .package = "fbiCDE"
  )

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

  # census_fun is injected so this exercises the key check without needing
  # censusapi (Suggests) installed.
  expect_error(
    join_census_pop(a, key = "", census_fun = function(...) NULL),
    "Census API key not found"
  )
})

# ---- join_census_pop: Census API seam --------------------------------------

# A stand-in for censusapi::getCensus(). Records the arguments it was called
# with and returns a response in getCensus()'s documented shape.
fake_get_census <- function(calls = NULL, pop = c(`001` = 1682353)) {
  function(name, vintage, vars, region, regionin, key, ...) {
    if (!is.null(calls)) {
      calls$args <- c(calls$args, list(list(
        name = name, vintage = vintage, vars = vars,
        region = region, regionin = regionin, key = key
      )))
    }
    counties <- strsplit(sub("^county:", "", region), ",", fixed = TRUE)[[1]]
    data.frame(
      state = sub("^state:", "", regionin),
      county = counties,
      NAME = paste("County", counties),
      B01003_001E = as.integer(pop[counties]),
      stringsAsFactors = FALSE
    )
  }
}

test_that("join_census_pop attaches Census population keyed by county FIPS", {
  a <- make_detail(ori = "CA9990001", periods = c("01-2021", "02-2021"),
                   counts = c(10, 12))
  a$county_fips <- "06001"

  out <- join_census_pop(a, key = "fake-key",
                         census_fun = fake_get_census())

  expect_true("census_population" %in% names(out))
  expect_equal(out$census_population, rep(1682353L, 2L))
  # Original rows are otherwise untouched.
  expect_equal(out$count, a$count)
})

test_that("join_census_pop calls getCensus with the documented argument shape", {
  calls <- new.env(parent = emptyenv())
  calls$args <- list()

  a <- make_detail(ori = "CA9990001", periods = "01-2021", counts = 10)
  a$county_fips <- "06001"

  join_census_pop(a, key = "fake-key", year = 2023,
                  census_fun = fake_get_census(calls))

  expect_length(calls$args, 1L)
  got <- calls$args[[1]]
  expect_equal(got$name, "acs/acs5")
  expect_equal(got$vintage, 2023)
  expect_equal(got$vars, c("NAME", "B01003_001E"))
  expect_equal(got$region, "county:001")
  expect_equal(got$regionin, "state:06")
  expect_equal(got$key, "fake-key")
})

test_that("join_census_pop issues one request per state and joins each back", {
  calls <- new.env(parent = emptyenv())
  calls$args <- list()

  a <- make_detail(ori = "CA9990001", periods = "01-2021", counts = 10)
  a$county_fips <- "06001"
  b <- make_detail(ori = "TX9990001", periods = "01-2021", counts = 20)
  b$state_abbr <- "TX"
  b$county_fips <- "48453"

  detail <- rbind(a, b)
  out <- join_census_pop(
    detail, key = "fake-key",
    census_fun = fake_get_census(calls, pop = c(`001` = 1682353,
                                                `453` = 1290188))
  )

  # regionin takes a single state, so one call per state prefix.
  expect_length(calls$args, 2L)
  expect_setequal(vapply(calls$args, function(x) x$regionin, character(1)),
                  c("state:06", "state:48"))

  expect_equal(out$census_population[out$county_fips == "06001"], 1682353L)
  expect_equal(out$census_population[out$county_fips == "48453"], 1290188L)
})

test_that("join_census_pop zero-pads unpadded state/county codes from the API", {
  a <- make_detail(ori = "CA9990001", periods = "01-2021", counts = 10)
  a$county_fips <- "06001"

  # Some Census responses return geography codes without leading zeros.
  unpadded <- function(...) {
    data.frame(state = 6L, county = 1L, NAME = "Alameda County",
               B01003_001E = 1682353L, stringsAsFactors = FALSE)
  }

  out <- join_census_pop(a, key = "fake-key", census_fun = unpadded)
  expect_equal(out$census_population, 1682353L)
})

test_that("join_census_pop warns and returns NA when the Census request fails", {
  a <- make_detail(ori = "CA9990001", periods = "01-2021", counts = 10)
  a$county_fips <- "06001"

  boom <- function(...) stop("503 service unavailable")

  expect_warning(
    out <- join_census_pop(a, key = "fake-key", census_fun = boom),
    "Census API request failed"
  )
  expect_true(all(is.na(out$census_population)))
})

test_that("join_census_pop warns on an unrecognized response shape", {
  a <- make_detail(ori = "CA9990001", periods = "01-2021", counts = 10)
  a$county_fips <- "06001"

  wrong_shape <- function(...) {
    data.frame(GEO_ID = "0500000US06001", total = 1682353L,
               stringsAsFactors = FALSE)
  }

  expect_warning(
    out <- join_census_pop(a, key = "fake-key", census_fun = wrong_shape),
    "missing state/county"
  )
  expect_true(all(is.na(out$census_population)))
})

test_that("join_census_pop output feeds get_county_crime(census_pop)", {
  a <- make_detail(ori = "CA9990001", periods = "01-2021", counts = 10,
                   pops = 30000, part_pops = 30000)
  a$county_fips <- "06001"

  joined <- join_census_pop(a, key = "fake-key",
                            census_fun = fake_get_census())
  agg <- get_county_crime(joined, denominator = "census_pop")

  expect_equal(agg$population, 1682353L)
  expect_equal(agg$denominator_type, "census_pop")
  expect_equal(agg$rate, 10 / 1682353 * 1e5)
})

# ---- End to end through the real resolver ---------------------------------
#
# The tests above build detail frames by hand and inject county_fips, which is
# how two bugs went unnoticed: get_county_crime_detail() never emitted
# county_fips (so join_census_pop() errored on real output), and a
# multi-county agency's raw county_name split the aggregate into one group per
# distinct string. These run the bundled agency table through the actual
# resolver and fan-out, mocking only the HTTP seam.

one_month_response <- function(path, ...) {
  pop <- list("01-2021" = 1000)
  list(
    offenses = list(actuals = list("Some PD Offenses" = list("01-2021" = 10)),
                    rates = list()),
    populations = list(population = list("Some PD" = pop),
                       participated_population = list("Some PD" = pop))
  )
}

test_that("a county containing multi-county agencies aggregates to one row per period", {
  local_mocked_bindings(cde_request = one_month_response, .package = "fbiCDE")

  # Franklin County, OH: Columbus PD is "DELAWARE; FAIRFIELD; FRANKLIN", and
  # several suburbs are also listed under more than one county.
  detail <- get_county_crime_detail("Franklin", "OH", from = "01-2021",
                                    to = "01-2021")
  expect_true("OHCOP0000" %in% detail$ori)
  expect_equal(unique(detail$county_name), "FRANKLIN")

  agg <- get_county_crime(detail)
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$county_name, "FRANKLIN")
  expect_equal(agg$count, 10 * length(unique(detail$ori)))
})

test_that("county detail feeds join_census_pop and the census_pop denominator", {
  local_mocked_bindings(cde_request = one_month_response, .package = "fbiCDE")

  detail <- get_county_crime_detail("Licking", "OH", from = "01-2021",
                                    to = "01-2021")
  # Licking's first matching agency is multi-county ("FAIRFIELD; LICKING;
  # FRANKLIN"); its FIPS was once taken from that row and came out as
  # Fairfield's (39045).
  expect_equal(unique(detail$county_fips), "39089")

  joined <- join_census_pop(detail, key = "fake-key",
                            census_fun = fake_get_census(pop = c(`089` = 178519)))
  expect_true(all(joined$census_population == 178519L))

  agg <- get_county_crime(joined, denominator = "census_pop")
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$population, 178519)
})

test_that("get_county_crime warns about and carries agencies the fan-out dropped", {
  local_mocked_bindings(
    cde_request = function(path, ...) {
      if (grepl("OHCOP0000", path)) stop("HTTP 500 for ", path, call. = FALSE)
      one_month_response(path)
    },
    .package = "fbiCDE"
  )

  expect_warning(
    detail <- get_county_crime_detail("Franklin", "OH", from = "01-2021",
                                      to = "01-2021"),
    "request or parse failed: OHCOP0000"
  )
  expect_match(attr(detail, "dropped_reasons")[["OHCOP0000"]], "HTTP 500")

  expect_warning(agg <- get_county_crime(detail), "absent from these totals")
  expect_equal(attr(agg, "dropped"), "OHCOP0000")
})
