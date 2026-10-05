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

# ---- Core interpolation tests ---------------------------------------------

test_that("impute_reporting_gaps fills a single gap between two reported periods", {
  # Agency reports in 01-2021 and 03-2021, but 02-2021 is a hole.
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(10, NA, 14)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  # The gap row (02-2021) should be filled.
  gap <- out[out$period == "02-2021", , drop = FALSE]
  expect_false(is.na(gap$count))
  expect_true(gap$imputed)
  expect_equal(gap$impute_method, "interpolate")

  # The interpolated count should be between the two known values.
  expect_true(gap$count >= min(10, 14))
  expect_true(gap$count <= max(10, 14))

  # With constant population (20000), rate interpolation yields midpoint:
  # rate_01 = 10/20000*1e5 = 50, rate_03 = 14/20000*1e5 = 70
  # rate_02 (midpoint) = 60, count_02 = round(60 * 20000 / 1e5) = 12
  expect_equal(gap$count, 12L)
})

test_that("impute_reporting_gaps leaves genuinely-reported rows untouched", {
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(10, NA, 14)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  # Reported rows should have imputed == FALSE and original count preserved.
  reported <- out[out$reported, , drop = FALSE]
  expect_false(any(reported$imputed))
  expect_true(all(is.na(reported$impute_method)))

  jan <- out[out$period == "01-2021", , drop = FALSE]
  expect_equal(jan$count, 10L)
  expect_true(jan$reported)
  expect_false(jan$imputed)

  mar <- out[out$period == "03-2021", , drop = FALSE]
  expect_equal(mar$count, 14L)
  expect_true(mar$reported)
  expect_false(mar$imputed)
})

test_that("agency with no reported periods is returned entirely unchanged", {
  # All counts are NA, all reported = FALSE.
  detail <- make_detail(
    ori = "CA9990002",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(NA, NA, NA)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  expect_false(any(out$imputed))
  expect_true(all(is.na(out$count)))
  expect_true(all(!out$reported))
  expect_true(all(is.na(out$impute_method)))
})

test_that("output has all original columns plus imputed and impute_method", {
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021"),
    counts = c(10, NA)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  expect_true(all(.DETAIL_COLS %in% names(out)))
  expect_true("imputed" %in% names(out))
  expect_true("impute_method" %in% names(out))
})

# ---- Multi-agency and edge-case tests -------------------------------------

test_that("multiple agencies are processed independently", {
  # Agency A: reports all three periods (no gaps).
  a <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(10, 12, 14)
  )
  # Agency B: gap in middle.
  b <- make_detail(
    ori = "CA9990002",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(5, NA, 9)
  )
  # Agency C: no reports at all.
  c <- make_detail(
    ori = "CA9990003",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(NA, NA, NA)
  )

  detail <- rbind(a, b, c)
  out <- impute_reporting_gaps(detail, method = "interpolate")

  # Agency A: no gaps, nothing imputed.
  a_out <- out[out$ori == "CA9990001", , drop = FALSE]
  expect_false(any(a_out$imputed))

  # Agency B: gap filled.
  b_out <- out[out$ori == "CA9990002", , drop = FALSE]
  b_gap <- b_out[b_out$period == "02-2021", , drop = FALSE]
  expect_true(b_gap$imputed)
  expect_false(is.na(b_gap$count))

  # Agency C: entirely unchanged.
  c_out <- out[out$ori == "CA9990003", , drop = FALSE]
  expect_false(any(c_out$imputed))
  expect_true(all(is.na(c_out$count)))
})

test_that("gaps at the edges (before first / after last reported) are not filled", {
  # Reports only in the middle period — gaps at both edges.
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(NA, 12, NA)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  # Only one reported period — need at least 2 to interpolate.
  expect_false(any(out$imputed))
})

test_that("drifting denominator is respected via rate interpolation", {
  # Population halves in the gap period. Interpolating count directly would give
  # midpoint of (10, 20) = 15. Interpolating rate and scaling by the gap's
  # population gives a different (correct) answer.
  periods <- c("01-2021", "02-2021", "03-2021")
  counts <- c(10, NA, 20)
  part_pops <- c(20000, 10000, 20000)  # gap period has half the population

  rates <- c(10 / 20000 * 1e5, NA, 20 / 20000 * 1e5)
  # rate_01 = 50, rate_03 = 100 → rate_02 (midpoint) = 75
  # count_02 = round(75 * 10000 / 1e5) = round(7.5) = 8

  detail <- data.frame(
    ori = rep("CA9990001", 3),
    agency_name = "Test Agency",
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = "TESTONIA",
    state_abbr = "CA",
    county_fips = "06999",
    offense = rep("V", 3),
    period = periods,
    count = counts,
    population = part_pops,
    participated_population = part_pops,
    rate = rates,
    reported = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  gap <- out[out$period == "02-2021", , drop = FALSE]
  expect_true(gap$imputed)
  # Rate-based interpolation with half-pop → count ≈ 8, not 15.
  expect_equal(gap$count, 8L)
})

test_that("explicit zero counts are not treated as gaps", {
  # Agency reported 0 in 02-2021 (reported = TRUE, count = 0).
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(10, 0, 14)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  # No gaps to fill — all periods are reported.
  expect_false(any(out$imputed))

  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_equal(feb$count, 0L)
  expect_true(feb$reported)
  expect_false(feb$imputed)
})

test_that("single reported period (no interpolation possible) leaves gaps unfilled", {
  # Only one reported period — can't interpolate with a single point.
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "02-2021", "03-2021"),
    counts = c(NA, 12, NA)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  expect_false(any(out$imputed))
  # The reported row is untouched.
  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_equal(feb$count, 12L)
  expect_false(feb$imputed)
})

test_that("unsorted input fills the real gap and never overwrites a reported row", {
  # Rows arrive out of period order (01, 03, 02, 04) — the API does not promise
  # sorted output. The gap is 02-2021; 03-2021 is genuinely reported.
  detail <- make_detail(
    ori = "CA9990001",
    periods = c("01-2021", "03-2021", "02-2021", "04-2021"),
    counts = c(10, 30, NA, 40)
  )

  out <- impute_reporting_gaps(detail, method = "interpolate")

  # Only the true gap is imputed.
  expect_equal(out$period[out$imputed], "02-2021")

  gap <- out[out$period == "02-2021", , drop = FALSE]
  expect_false(is.na(gap$count))
  expect_equal(gap$count, 20L)  # midpoint rate of 10 and 30, constant pop
  expect_equal(gap$impute_method, "interpolate")

  # Every reported row keeps its original count and is not flagged.
  mar <- out[out$period == "03-2021", , drop = FALSE]
  expect_equal(mar$count, 30L)
  expect_false(mar$imputed)
  expect_true(is.na(mar$impute_method))

  reported <- out[out$reported, , drop = FALSE]
  expect_equal(reported$count, c(10L, 30L, 40L)[order(reported$period)])
  expect_false(any(reported$imputed))
})

test_that("invalid input types are rejected", {
  expect_error(impute_reporting_gaps("not a data.frame"),
               "must be a data.frame")
  expect_error(impute_reporting_gaps(data.frame(x = 1)),
               "missing required columns")
  expect_error(impute_reporting_gaps(
    make_detail("CA9990001", periods = "01-2021", counts = 10),
    method = "extrapolate"
  ), "Unsupported method")
})

test_that("empty detail frame is returned unchanged with new columns", {
  empty <- .empty_detail_frame()
  out <- impute_reporting_gaps(empty)

  expect_equal(nrow(out), 0L)
  expect_true("imputed" %in% names(out))
  expect_true("impute_method" %in% names(out))
})
