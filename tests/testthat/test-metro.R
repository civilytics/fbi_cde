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

test_that("a hyphenated or slashed city name resolves as a short name", {
  expect_equal(unique(.resolve_cbsa("Winston-Salem")$cbsa_title),
               "Winston-Salem, NC")
  expect_equal(unique(.resolve_cbsa("Louisville")$cbsa_title),
               "Louisville/Jefferson County, KY-IN")
  expect_equal(unique(.resolve_cbsa("Dallas-Fort Worth-Arlington")$cbsa_title),
               "Dallas-Fort Worth-Arlington, TX")
  expect_equal(unique(.resolve_cbsa("Dallas")$cbsa_title),
               "Dallas-Fort Worth-Arlington, TX")
})

test_that("the ambiguity error suggests a state that works", {
  # It used to suggest the first title's whole suffix, state = "GA-AL",
  # which matches nothing.
  err <- tryCatch(.resolve_cbsa("Columbus"), error = conditionMessage)
  suggested <- sub('.*state = "([^"]+)".*', "\\1", err)
  expect_equal(suggested, "GA")
  expect_equal(unique(.resolve_cbsa("Columbus", suggested)$cbsa_title),
               "Columbus, GA-AL")
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
  # A metro is a set of whole counties, so sheriffs belong.
  expect_true(any(out$agency_class == "county_primary"))
  expect_false("default_member" %in% names(out))
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

test_that("every Connecticut metro resolves through its planning regions", {
  # The 2023 delineation builds Connecticut's CBSAs from planning regions
  # (09110-09190). They resolved to nothing while agencies carried only
  # traditional counties (#52); the CDE's planning-region attribution fixes
  # that. Derived from the shipped crosswalk, not a hardcoded list.
  cw <- .cbsa_table()
  ct_titles <- sort(unique(cw$cbsa_title[substr(cw$county_fips, 1L, 2L) == "09"]))
  expect_equal(length(ct_titles), 7L)

  seen <- character(0)
  for (title in ct_titles) {
    out <- expect_no_warning(metro_agencies(title))
    expect_gt(nrow(out), 0L)
    expect_true(all(grepl("PLANNING REGION$", out$county_name)), info = title)
    seen <- c(seen, out$ori)
  }
  # Each attributed Connecticut agency is in exactly one Connecticut metro.
  expect_false(anyDuplicated(seen) > 0)
  expect_setequal(seen, ct_planning_regions$ori)
})

test_that("partial county coverage warns with the counts", {
  # Puerto Rico's municipios do not join any CDE county name.
  cw <- .cbsa_table()
  pr_title <- cw$cbsa_title[substr(cw$county_fips, 1L, 2L) == "72"][1]
  expect_warning(metro_agencies(pr_title), "counties but only")
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

test_that("the New York and Washington metros include their city police", {
  # Both lost their largest department, and warned of unmatched counties,
  # while the NYPD and DC's police had no county.
  ny <- expect_no_warning(metro_agencies("New York-Newark-Jersey City, NY-NJ"))
  expect_equal(sum(ny$ori == "NY0303000"), 1L)
  # Attributed to its first borough in delineation order.
  expect_equal(ny$county_name[ny$ori == "NY0303000"], "BRONX")

  dc <- expect_no_warning(
    metro_agencies("Washington-Arlington-Alexandria, DC-VA-MD-WV")
  )
  expect_equal(sum(dc$ori == "DCMPD0000"), 1L)
})
