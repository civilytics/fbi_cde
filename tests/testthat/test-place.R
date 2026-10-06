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

test_that("every (state, county, place) key is unique", {
  # Same-named townships are common within a state (once their ", X County"
  # disambiguators are stripped), but never within a county, which is what
  # place_agencies(county = ) relies on.
  mun <- .municipal_agencies()
  key <- paste(mun$state_abbr, mun$county_name, toupper(mun$place_name))
  expect_false(anyDuplicated(key) > 0)
})

test_that("derive_place_name drops a county disambiguator in either position", {
  expect_equal(
    derive_place_name("Clay Township Police Department, Montgomery County"),
    "Clay Township"
  )
  expect_equal(
    derive_place_name("Hamilton Township, Mercer County Police Department"),
    "Hamilton Township"
  )
  # No derived place name keeps a disambiguator.
  expect_false(any(grepl(",", .municipal_agencies()$place_name)))
})

test_that("a disambiguated township resolves by name with its county", {
  expect_error(place_agencies("Hamilton Township", "NJ"), "ambiguous")
  out <- place_agencies("Hamilton Township", "NJ", county = "Mercer")
  expect_equal(out$ori, "NJ0110300")
})

# ---- Census codes (#46) -----------------------------------------------------

test_that("place_agencies carries the Census code of the unit it polices", {
  lufkin <- place_agencies("Lufkin", "TX")
  expect_equal(names(lufkin), .PLACE_AGENCY_COLS)
  expect_equal(lufkin$place_type, "incorporated")
  expect_equal(lufkin$place_fips, "4845072")
  expect_true(is.na(lufkin$cousub_fips))

  # A township is a county subdivision, not a Census place.
  hamilton <- place_agencies("Hamilton Township", "NJ", county = "Mercer")
  expect_equal(hamilton$place_type, "county_subdivision")
  expect_true(is.na(hamilton$place_fips))
  expect_equal(hamilton$cousub_fips, "3402129310")

  # So is a New England town.
  reading <- place_agencies("Reading", "MA")
  expect_equal(reading$place_type, "county_subdivision")
  expect_equal(reading$cousub_fips, "2501756130")

  # New York City spans five counties; its place code is the city's.
  expect_equal(place_agencies("New York City", "NY")$place_fips, "3651000")
})

test_that("an agency matching more than one unit in its county gets NA", {
  # Superior, WI is both a city and a village in Douglas County.
  superior <- place_agencies("Superior", "WI")
  expect_true(is.na(superior$place_type))
  expect_true(is.na(superior$place_fips))
  expect_true(is.na(superior$cousub_fips))
})

test_that("the place crosswalk keeps its invariants", {
  cw <- place_crosswalk
  mun <- .municipal_agencies()

  expect_false(anyDuplicated(cw$ori) > 0)
  expect_true(all(cw$ori %in% mun$ori))
  expect_true(all(cw$place_type %in% c("incorporated", "cdp", "county_subdivision")))
  # Exactly one code per row, the one its type calls for.
  expect_true(all(xor(is.na(cw$place_fips), is.na(cw$cousub_fips))))
  expect_equal(cw$place_type == "county_subdivision", !is.na(cw$cousub_fips))
  expect_true(all(grepl("^[0-9]{7}$", cw$place_fips[!is.na(cw$place_fips)])))
  expect_true(all(grepl("^[0-9]{10}$", cw$cousub_fips[!is.na(cw$cousub_fips)])))

  # Every code is in the agency's own state, and a county subdivision is in
  # one of the agency's own counties.
  ag <- mun[match(cw$ori, mun$ori), ]
  code <- ifelse(is.na(cw$place_fips), cw$cousub_fips, cw$place_fips)
  state_fips <- vapply(ag$state_abbr, .state_abbr_to_fips, "")
  expect_equal(unname(substr(code, 1L, 2L)), unname(state_fips))
  sub_rows <- which(!is.na(cw$cousub_fips))
  in_county <- vapply(sub_rows, function(i) {
    counties <- trimws(strsplit(ag$county_name[i], ";", fixed = TRUE)[[1]])
    fips <- vapply(counties, function(cty) county_to_fips(ag$state_abbr[i], cty), "")
    substr(cw$cousub_fips[i], 1L, 5L) %in% fips
  }, logical(1))
  expect_true(all(in_county))

  # Coverage the documentation promises.
  expect_gte(nrow(cw) / nrow(mun), 0.98)
  expect_equal(attr(cw, "vintage"), PLACE_VINTAGE)
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
  # Other state agencies (a state park's rangers) class as "state", queried
  # only with include_statewide = TRUE.
  expect_equal(classify_place_agency("Other State Agency"), "state")
  # Alaska city police departments carry the "Census Area" type.
  expect_equal(classify_place_agency("Census Area"), "place_primary")
})

test_that("an Alaska city police department resolves as a place", {
  out <- place_agencies("Nome", "AK")
  expect_equal(out$ori, "AK0010600")
  expect_equal(out$agency_class, "place_primary")
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
  expect_false("default_member" %in% names(out))
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
  # earlier version of this test hardcoded the coordinate types and so passed
  # while the empty frame disagreed with the populated one. The contract is
  # agreement, not a fixed guess.
  empty <- .empty_place_agency_frame()
  populated <- place_agencies("Lufkin", "TX")

  expect_gt(nrow(populated), 0L)
  expect_equal(names(empty), names(populated))
  expect_equal(
    vapply(empty, function(x) class(x)[1], character(1)),
    vapply(populated, function(x) class(x)[1], character(1))
  )
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

test_that("place_agencies county filter matches a multi-county agency", {
  # Columbus PD's county_name is "DELAWARE; FAIRFIELD; FRANKLIN"; an exact
  # comparison with county = "Franklin" used to find nothing.
  out <- place_agencies("Columbus", "OH", county = "Franklin")
  expect_equal(out$ori, "OHCOP0000")
  expect_warning(
    none <- place_agencies("Columbus", "OH", county = "Hocking"),
    "No municipal agency"
  )
  expect_equal(nrow(none), 0L)
})
