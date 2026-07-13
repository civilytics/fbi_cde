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
  expect_true(all(c("agency_class", "default_member") %in% names(out)))

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
