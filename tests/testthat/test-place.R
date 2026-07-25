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
  # Guard against the guard going vacuous.
  expect_gt(nrow(mun), 10000)

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

# ---- classify_place_agency() -----------------------------------------------

test_that("classify_place_agency maps municipal types to place_primary", {
  expect_equal(
    classify_place_agency(c("City", "Municipality", "Borough", "City and Borough")),
    rep("place_primary", 4)
  )
})

test_that("classify_place_agency maps embedded types", {
  expect_equal(classify_place_agency("University or College"), "campus")
  expect_equal(classify_place_agency("Other"), "special")
  expect_equal(classify_place_agency("Other State Agency"), "special")
})

test_that("classify_place_agency maps non-place types to NA", {
  # A sheriff or state police agency is never a place member, so it has no
  # place-level class at all.
  expect_true(is.na(classify_place_agency("County")))
  expect_true(is.na(classify_place_agency("State Police")))
  expect_true(is.na(classify_place_agency("Tribal")))
})

# ---- place_agencies() ------------------------------------------------------

test_that("place_agencies resolves a place to its own municipal agency", {
  out <- place_agencies("Lufkin", "TX")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$place_name, "Lufkin")
  expect_equal(out$agency_class, "place_primary")
  expect_true(out$default_member)
  expect_equal(out$attribution, "name_identity")
  expect_equal(out$state_abbr, "TX")
  expect_match(out$ori, "^[A-Z]{2}[A-Z0-9]{7}$")
})

test_that("place_agencies returns the documented columns in order", {
  out <- place_agencies("Lufkin", "TX")
  expect_equal(names(out), .PLACE_AGENCY_COLS)
})

test_that("place_agencies is case- and whitespace-insensitive", {
  a <- place_agencies("Lufkin", "TX")
  b <- place_agencies("  lufkin  ", "tx")
  expect_equal(a$ori, b$ori)
})

test_that("place_agencies never returns sheriffs, state police, or tribal agencies", {
  # Los Angeles has a city PD; LASD (County) must not appear.
  out <- place_agencies("Los Angeles", "CA")
  expect_true(all(out$agency_class == "place_primary"))
  expect_false(any(out$agency_type_name %in% c("County", "Parish", "State Police", "Tribal")))
})

test_that("place_agencies errors on an ambiguous place without county", {
  expect_error(
    place_agencies("Foster Township", "PA"),
    "ambiguous"
  )
  # The error must name the candidate counties so the user can disambiguate.
  err <- tryCatch(place_agencies("Foster Township", "PA"), error = function(e) e)
  expect_match(conditionMessage(err), "county =")
})

test_that("place_agencies resolves an ambiguous place when county is supplied", {
  amb <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES &
      fbi_api_agencies$state_abbr == "PA", , drop = FALSE
  ]
  amb$place <- derive_place_name(amb$agency_name)
  counties <- amb$county_name[amb$place == "Foster Township"]
  expect_gt(length(counties), 1L)

  out <- place_agencies("Foster Township", "PA", county = counties[1])
  expect_equal(nrow(out), 1L)
  expect_equal(toupper(out$county_name), toupper(counties[1]))
})

test_that("place_agencies warns and returns an empty typed frame for an unknown place", {
  expect_warning(
    out <- place_agencies("Nowheresville", "TX"),
    "No municipal agency"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_AGENCY_COLS)
})

test_that("place_agencies rejects an invalid state", {
  expect_error(place_agencies("Lufkin", "ZZ"), "Invalid state")
})

test_that(".empty_place_agency_frame types match a populated result", {
  # Asserted against the populated frame rather than a hardcoded list: an
  # earlier version of this test hardcoded numeric latitude/longitude and so
  # passed while the empty frame disagreed with the populated one, which stores
  # coordinates as character. The contract is agreement, not a fixed guess.
  empty <- .empty_place_agency_frame()
  populated <- place_agencies("Lufkin", "TX")

  expect_gt(nrow(populated), 0L)
  expect_equal(names(empty), names(populated))
  expect_equal(
    vapply(empty, function(x) class(x)[1], character(1)),
    vapply(populated, function(x) class(x)[1], character(1))
  )
  expect_equal(vapply(empty, class, character(1))[["default_member"]], "logical")
})

test_that("an unmatched-place empty frame rbinds cleanly against a real result", {
  # Regression: the empty frame was previously built from matrix(nrow = 0, ...),
  # which typed every unoverridden column as logical(0). rbind() against a
  # populated, character-typed result would then break or silently coerce.
  empty <- suppressWarnings(place_agencies("Nowheresville", "TX"))
  real <- place_agencies("Lufkin", "TX")

  out <- rbind(empty, real)

  expect_equal(nrow(out), 1L)
  classes <- vapply(out, class, character(1))
  expect_equal(classes[["ori"]], "character")
  expect_equal(out$ori, real$ori)
})
