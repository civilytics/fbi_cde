# ---- .resolve_cbsa() -------------------------------------------------------

test_that("an exact CBSA title resolves", {
  out <- .resolve_cbsa("Pittsburgh, PA", NULL)
  expect_gt(nrow(out), 1L)                       # Pittsburgh spans counties
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
})

test_that("exact title matching is case- and whitespace-insensitive", {
  a <- .resolve_cbsa("Pittsburgh, PA", NULL)
  b <- .resolve_cbsa("  pittsburgh, pa  ", NULL)
  expect_equal(a$county_fips, b$county_fips)
})

test_that("an unambiguous short name resolves", {
  out <- .resolve_cbsa("Pittsburgh", NULL)
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
})

test_that("an ambiguous short name errors listing the candidates", {
  # Albany is a CBSA name in GA, OR and NY.
  err <- tryCatch(.resolve_cbsa("Albany", NULL), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "ambiguous")
  expect_match(conditionMessage(err), "Albany")
})

test_that("state disambiguates an ambiguous short name", {
  out <- .resolve_cbsa("Albany", "OR")
  expect_equal(unique(out$cbsa_title), "Albany, OR")
})

test_that("an unknown metro resolves to zero rows", {
  expect_equal(nrow(.resolve_cbsa("Nowhere Metro", NULL)), 0L)
})

# ---- metro_agencies() ------------------------------------------------------

test_that("metro_agencies unions agencies across member counties", {
  out <- metro_agencies("Pittsburgh, PA")

  expect_s3_class(out, "data.frame")
  # Measured against the real data: Pittsburgh, PA is 8 counties / 336
  # agencies. Asserting a floor rather than the exact count leaves room for
  # CDE agency-table updates without making the test meaningless.
  expect_gt(nrow(out), 300L)
  expect_equal(length(unique(out$county_fips)), 8L)
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
  expect_equal(unique(out$cbsa_type), "metro")
  expect_true(all(nchar(out$county_fips) == 5L))
})

test_that("metro_agencies returns the documented columns in order", {
  out <- metro_agencies("Pittsburgh, PA")
  expect_equal(names(out), .METRO_AGENCY_COLS)
})

test_that("metro_agencies preserves county-level classification semantics", {
  out <- metro_agencies("Pittsburgh, PA")
  expect_true(all(out$agency_class %in%
    c("county_primary", "municipal", "campus", "state", "tribal", "special")))
  # A metro is a set of whole counties, so sheriffs belong and are default.
  expect_true(any(out$agency_class == "county_primary"))
  expect_true(all(out$default_member[out$agency_class == "county_primary"]))
})

test_that("metro_agencies carries central/outlying from the delineation", {
  out <- metro_agencies("Pittsburgh, PA")
  expect_true(all(out$central_outlying %in% c("Central", "Outlying")))
  expect_true(any(out$central_outlying == "Central"))
})

test_that("metro_agencies warns and returns a typed empty frame for an unknown metro", {
  expect_warning(out <- metro_agencies("Nowhere Metro"), "No CBSA")
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_AGENCY_COLS)
})

test_that("metro_agencies resolves a single-county micro area", {
  out <- metro_agencies("Aberdeen, WA")
  expect_gt(nrow(out), 0L)
  expect_equal(unique(out$cbsa_type), "micro")
  expect_equal(length(unique(out$county_name)), 1L)
})

test_that("a Connecticut metro warns rather than returning silently empty", {
  # The 2023 delineation uses CT planning regions (09110-09190); the CDE
  # reports traditional CT counties (09001-09015). They do not join, so all
  # five CT metros resolve to nothing. That MUST be loud: a quiet zero-row
  # frame would read as "no agencies report in Hartford", which is false.
  expect_warning(
    out <- metro_agencies("Hartford-West Hartford-East Hartford, CT"),
    "Connecticut"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_AGENCY_COLS)
})

test_that("partial county coverage warns with the counts", {
  expect_warning(
    metro_agencies("New Haven, CT"),
    "counties but only"
  )
})

test_that("EVERY CBSA containing a Connecticut county warns", {
  # Derived from the shipped crosswalk itself, not a hardcoded list of seven
  # CT metro names -- so this cannot silently go stale the way the docs did
  # (Gitea whole-branch review, I1). Any CBSA with a 09xxx (Connecticut)
  # county must warn, because none of Connecticut's planning-region FIPS join
  # the CDE's traditional county names.
  cw <- .cbsa_table()
  ct_titles <- sort(unique(cw$cbsa_title[substr(cw$county_fips, 1L, 2L) == "09"]))
  expect_gt(length(ct_titles), 0L)

  for (title in ct_titles) {
    expect_warning(metro_agencies(title), info = title)
  }
})

# ---- No double-count from multi-county agencies (#56) ----------------------
#
# A multi-county agency legitimately matches several counties, which is correct
# at county level. But a metro unions its member counties, and the Columbus
# CBSA contains all three of Columbus PD's counties -- so without dedup the
# agency would appear three times and get_metro_crime_detail() would
# triple-count its crime. That would break the metro layer's core invariant.

test_that("metro_agencies includes a multi-county agency exactly once", {
  out <- metro_agencies("Columbus, OH")

  expect_true("OHCOP0000" %in% out$ori)
  expect_equal(sum(out$ori == "OHCOP0000"), 1L)
})

test_that("no metro returns a duplicated ORI", {
  # Metros whose member counties overlap a multi-county agency's list.
  metros <- c("Columbus, OH", "Dallas-Fort Worth-Arlington, TX",
              "Portland-Vancouver-Hillsboro, OR-WA")

  for (m in metros) {
    out <- suppressWarnings(metro_agencies(m))
    expect_gt(nrow(out), 0L)
    expect_false(any(duplicated(out$ori)), info = paste("metro:", m))
  }
})
