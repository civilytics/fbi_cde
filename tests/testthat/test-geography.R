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
