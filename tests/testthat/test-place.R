# ---- derive_place_name() ---------------------------------------------------

test_that("derive_place_name strips the common police-department suffixes", {
  expect_equal(derive_place_name("Lufkin Police Department"), "Lufkin")
  expect_equal(derive_place_name("Aransas Pass Police Department"), "Aransas Pass")
  expect_equal(derive_place_name("Colusa Police Dept"), "Colusa")
  expect_equal(derive_place_name("Colusa Police Dept."), "Colusa")
  expect_equal(derive_place_name("Sitka Police"), "Sitka")
  expect_equal(derive_place_name("Dover Department of Public Safety"), "Dover")
  expect_equal(derive_place_name("Dover Public Safety Department"), "Dover")
  expect_equal(derive_place_name("Nome Marshal's Office"), "Nome")
})

test_that("derive_place_name leaves bare place names untouched", {
  expect_equal(derive_place_name("Concord"), "Concord")
  expect_equal(derive_place_name("Cherry Valley"), "Cherry Valley")
  expect_equal(derive_place_name("Kansas"), "Kansas")
})

test_that("derive_place_name is vectorised and trims whitespace", {
  expect_equal(
    derive_place_name(c("Lufkin Police Department", "Concord", " Troy Police ")),
    c("Lufkin", "Concord", "Troy")
  )
})

test_that("derive_place_name only strips a suffix, never an interior match", {
  # "Police" inside the name must survive; only a trailing suffix is stripped.
  expect_equal(derive_place_name("Police Jury Police Department"), "Police Jury")
})

test_that("derive_place_name handles NA and empty input", {
  expect_true(is.na(derive_place_name(NA_character_)))
  expect_equal(derive_place_name(character(0)), character(0))
})

test_that("derive_place_name strips a doubled suffix to a fixed point", {
  # Two real CDE records carry the suffix twice.
  expect_equal(
    derive_place_name("Las Vegas Metropolitan Police Department Police Department"),
    "Las Vegas Metropolitan"
  )
  expect_equal(
    derive_place_name("Northeast Police Department Police Department"),
    "Northeast"
  )
})

test_that("derive_place_name never strips a name down to nothing", {
  # The pattern requires a leading space, so a bare "Police" is a fixed point.
  expect_equal(derive_place_name("Police"), "Police")
  expect_equal(derive_place_name("Police Police Department"), "Police")
})

# ---- Drift guards over the bundled agency table ----------------------------
#
# These assert the empirical facts the whole place model rests on (spec §1).
# If the CDE changes its agency naming, these fail rather than the resolver
# silently degrading.

test_that("municipal-tier agencies are present in the expected volume", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  # Guard against the guard going vacuous.
  expect_gt(nrow(mun), 10000)
})

test_that("place-name derivation resolves effectively every municipal agency", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  places <- derive_place_name(mun$agency_name)

  expect_false(any(is.na(places)))
  expect_true(all(nzchar(places)))
  # No derived place name may still carry a police-department suffix.
  expect_equal(sum(grepl(.PLACE_SUFFIX_PATTERN, places)), 0L)
})

test_that("(state, county, place) is a collision-free key", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  key <- paste(mun$state_abbr, mun$county_name, derive_place_name(mun$agency_name))
  expect_gt(length(key), 10000)
  expect_equal(sum(table(key) > 1), 0L)
})

test_that("only the two known (state, place) keys are ambiguous", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  key <- paste(mun$state_abbr, derive_place_name(mun$agency_name))
  ambiguous <- sort(names(which(table(key) > 1)))
  expect_equal(ambiguous, c("PA Foster Township", "PA Jefferson Township"))
})
