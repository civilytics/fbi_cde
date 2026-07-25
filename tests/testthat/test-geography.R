test_that("classify_agency maps raw agency types to agency_class", {
  expect_equal(classify_agency("County"), "county_primary")
  expect_equal(classify_agency("Parish"), "county_primary")
  expect_equal(classify_agency("City"), "municipal")
  expect_equal(classify_agency("Municipality"), "municipal")
  expect_equal(classify_agency("Borough"), "municipal")
  expect_equal(classify_agency("City and Borough"), "municipal")
  expect_equal(classify_agency("University or College"), "campus")
  expect_equal(classify_agency("State Police"), "state")
  expect_equal(classify_agency("Tribal"), "tribal")
  expect_equal(classify_agency("Other"), "special")
  expect_equal(classify_agency("Other State Agency"), "special")
  expect_equal(classify_agency("Census Area"), "special")
  # Unknown and NA are conservative (excluded by default).
  expect_equal(classify_agency("Something New"), "special")
  expect_equal(classify_agency(NA_character_), "special")
  # Vectorized.
  expect_equal(classify_agency(c("City", "County", "Tribal")),
               c("municipal", "county_primary", "tribal"))
})

test_that("county_agencies resolves and classifies a county (Alameda, CA)", {
  out <- county_agencies("Alameda", "CA")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 21L)
  expect_true(all(c("agency_class", "default_member", "county_fips") %in% names(out)))

  cls <- setNames(out$agency_class, out$ori)
  expect_equal(cls[["CA0010000"]], "county_primary")  # Alameda County Sheriff
  expect_equal(cls[["CA0010900"]], "municipal")        # Oakland PD
  expect_equal(cls[["CA001300X"]], "municipal")        # Dublin PD (letter ORI)
  expect_equal(cls[["CA0012100"]], "special")          # BART
  expect_equal(cls[["CA0019900"]], "state")            # Highway Patrol
  expect_equal(cls[["CA0019700"]], "campus")           # UC Berkeley

  # Default membership = county_primary + municipal = 14 City + 1 County.
  expect_equal(sum(out$default_member), 15L)
  dm <- setNames(out$default_member, out$ori)
  expect_true(dm[["CA0010900"]])
  expect_false(dm[["CA0012100"]])
})

test_that("county_agencies includes county_fips column (Alameda, CA)", {
  out <- county_agencies("Alameda", "CA")
  # county_fips is a character column (preserves leading zero)
  expect_true(is.character(out$county_fips))
  # All rows share the same county FIPS
  expect_true(all(out$county_fips == "06001"))
  # Column order: county_fips appears right after state_abbr
  expect_equal(names(out),
               c("ori", "agency_name", "agency_type_name", "agency_class",
                 "default_member", "county_name", "state_abbr", "county_fips",
                 "latitude", "longitude"))
})

test_that("county_agencies zero-row branch includes county_fips", {
  expect_warning(res <- county_agencies("Nowhere", "CA"), "No agencies")
  expect_equal(nrow(res), 0L)
  expect_true("county_fips" %in% names(res))
  expect_true(is.character(res$county_fips))
  expect_equal(length(res$county_fips), 0L)
})

test_that("county_agencies is case-insensitive on county and state", {
  a <- county_agencies("Alameda", "CA")
  b <- county_agencies("alameda", "ca")
  expect_equal(nrow(a), nrow(b))
  expect_setequal(a$ori, b$ori)
})

test_that("county_agencies validates state and warns on unknown county", {
  expect_error(county_agencies("Alameda", "ZZ"), "Invalid state")
  expect_warning(res <- county_agencies("Nowhere", "CA"), "No agencies")
  expect_equal(nrow(res), 0L)
})

# ---- Multi-county agencies (#56) -------------------------------------------
#
# The CDE stores a multi-county agency's county_name as a semicolon-separated
# list ("DELAWARE; FAIRFIELD; FRANKLIN"). Exact string matching never fired for
# those 617 agencies, so major city departments were silently missing from
# their own county and metro results.

test_that("county_agencies finds an agency whose county_name lists several counties", {
  # Columbus PD polices parts of Delaware, Fairfield and Franklin counties.
  for (cty in c("Franklin", "Delaware", "Fairfield")) {
    out <- county_agencies(cty, "OH")
    expect_true("OHCOP0000" %in% out$ori, info = paste("county:", cty))
  }
})

test_that("county_agencies still excludes agencies from unrelated counties", {
  # Columbus PD is not in Hocking, another Columbus-CBSA county.
  expect_false("OHCOP0000" %in% county_agencies("Hocking", "OH")$ori)
})

test_that("county matching is not substring-based", {
  # "YORK" must not match "NEW YORK": a substring test would wrongly cross-match.
  ny <- county_agencies("New York", "NY")
  expect_gt(nrow(ny), 0L)
  expect_true(all(grepl("NEW YORK", toupper(ny$county_name))))
})

test_that("a single-county agency matches only its own county", {
  alameda <- county_agencies("Alameda", "CA")
  expect_gt(nrow(alameda), 0L)
  expect_true(all(vapply(
    strsplit(toupper(alameda$county_name), ";", fixed = TRUE),
    function(p) "ALAMEDA" %in% trimws(p), logical(1)
  )))
})
