# ---- Degradation path (runs everywhere, no sf/tigris needed) ---------------

test_that("add_place_spatial_members rejects non-data.frame input", {
  expect_error(add_place_spatial_members("nope"), "must be a data.frame")
})

test_that("add_place_spatial_members rejects a frame missing required columns", {
  expect_error(add_place_spatial_members(data.frame(x = 1)),
               "missing required columns")
})

test_that("add_place_spatial_members returns input unchanged when sf is absent", {
  x <- place_agencies("Lufkin", "TX")

  # Force the unavailable-dependency branch regardless of what is installed.
  testthat::local_mocked_bindings(
    .spatial_deps_available = function() FALSE,
    .package = "fbiCDE"
  )

  expect_message(out <- add_place_spatial_members(x), "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(out$ori, x$ori)
  # Blocker 2: the degradation path must still carry place_type/place_fips,
  # so downstream code like out[!is.na(out$place_type), ] behaves the same
  # whether or not sf/tigris are installed.
  expect_true(all(c("place_type", "place_fips") %in% names(out)))
  expect_true(all(is.na(out$place_type)))
  expect_true(all(is.na(out$place_fips)))
})

test_that("add_place_spatial_members is idempotent", {
  x <- place_agencies("Lufkin", "TX")
  once <- x
  once$place_type <- NA_character_
  once$place_fips <- NA_character_
  once$attribution <- "point_in_polygon"

  expect_message(twice <- add_place_spatial_members(once),
                 "already has spatially-attributed members")
  expect_equal(twice, once)
})

test_that("add_place_spatial_members widens the required-columns check to catch a get_place_crime_detail() result", {
  # A get_place_crime_detail()-shaped frame carries agency_class etc. but not
  # latitude/longitude, so it should fail the *validation* with a clear
  # message rather than dying later with a cryptic subscript error.
  bad <- data.frame(
    ori = "TX1234567", agency_name = "x", agency_type_name = "City",
    agency_class = "place_primary",
    place_name = "Lufkin", county_name = "ANGELINA", state_abbr = "TX",
    attribution = "name_identity",
    offense = "V", period = "01-2021", count = 1,
    stringsAsFactors = FALSE
  )
  expect_error(add_place_spatial_members(bad), "missing required columns")
})

# ---- Point-in-polygon (needs sf; tigris replaced by the seam) --------------

# A one-square-degree polygon around a known point, as an sf frame in the shape
# tigris::places() returns.
fixture_places <- function() {
  skip_if_not_installed("sf")
  sq <- function(cx, cy) {
    sf::st_polygon(list(cbind(
      c(cx - 0.5, cx + 0.5, cx + 0.5, cx - 0.5, cx - 0.5),
      c(cy - 0.5, cy - 0.5, cy + 0.5, cy + 0.5, cy - 0.5)
    )))
  }
  sf::st_sf(
    GEOID = c("4845384", "4800001"),
    NAME = c("Lufkin", "Elsewhere"),
    CLASSFP = c("C1", "U1"),
    geometry = sf::st_sfc(sq(-94.7, 31.3), sq(-100.0, 35.0), crs = 4326)
  )
}

test_that("add_place_spatial_members attributes an embedded agency inside the place", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")

  # One campus agency whose HQ falls inside the Lufkin fixture polygon.
  fake_agencies <- data.frame(
    ori = "TX1234567",
    agency_name = "Angelina College",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 31.3,
    longitude = -94.7,
    stringsAsFactors = FALSE
  )
  # Return ONLY the synthetic rows. Stacking these onto the real 18,459-row
  # table would make the assertions hostage to real agencies that happen to fall
  # inside the fixture polygon — Stephen F. Austin State University really does
  # sit inside a 1-degree box around Lufkin.
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())

  expect_gt(nrow(out), nrow(x))
  added <- out[out$attribution == "point_in_polygon", , drop = FALSE]
  expect_equal(nrow(added), 1L)
  expect_equal(added$ori, "TX1234567")
  expect_equal(added$agency_class, "campus")
  expect_equal(added$place_type, "incorporated")
  expect_equal(added$place_fips, "4845384")
})

test_that("add_place_spatial_members excludes agencies outside the polygon", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  fake_agencies <- data.frame(
    ori = "TX7654321",
    agency_name = "Far Away University",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 20.0,       # well outside the fixture polygon
    longitude = -80.0,
    stringsAsFactors = FALSE
  )
  # Return ONLY the synthetic rows. Stacking these onto the real 18,459-row
  # table would make the assertions hostage to real agencies that happen to fall
  # inside the fixture polygon — Stephen F. Austin State University really does
  # sit inside a 1-degree box around Lufkin.
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())
  expect_false("TX7654321" %in% out$ori)
})

test_that("add_place_spatial_members never attributes sheriffs or state police", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  # A sheriff and a state-police agency sitting exactly on the place centroid.
  fake_agencies <- data.frame(
    ori = c("TX1111111", "TX2222222"),
    agency_name = c("Angelina County Sheriff", "Texas DPS Lufkin"),
    agency_type_name = c("County", "State Police"),
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 31.3,
    longitude = -94.7,
    stringsAsFactors = FALSE
  )
  # Return ONLY the synthetic rows. Stacking these onto the real 18,459-row
  # table would make the assertions hostage to real agencies that happen to fall
  # inside the fixture polygon — Stephen F. Austin State University really does
  # sit inside a 1-degree box around Lufkin.
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())
  expect_false(any(c("TX1111111", "TX2222222") %in% out$ori))
})

test_that("add_place_spatial_members flags a CDP match via place_type", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  x$place_name <- "Elsewhere"   # point the resolver row at the CDP polygon

  fake_agencies <- data.frame(
    ori = "TX3333333",
    agency_name = "Elsewhere Community College",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 35.0,
    longitude = -100.0,
    stringsAsFactors = FALSE
  )
  # Return ONLY the synthetic rows. Stacking these onto the real 18,459-row
  # table would make the assertions hostage to real agencies that happen to fall
  # inside the fixture polygon — Stephen F. Austin State University really does
  # sit inside a 1-degree box around Lufkin.
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())
  added <- out[out$attribution == "point_in_polygon", , drop = FALSE]
  expect_equal(added$place_type, "cdp")
})

test_that("add_place_spatial_members skips agencies with unusable coordinates", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")

  # The bundled table stores missing coordinates as NA.
  fake_agencies <- data.frame(
    ori = c("TX4444444", "TX5555555"),
    agency_name = c("Good Coords College", "No Coords College"),
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = c(31.3, NA),
    longitude = c(-94.7, NA),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())

  expect_true("TX4444444" %in% out$ori)
  expect_false("TX5555555" %in% out$ori)
})

test_that("add_place_spatial_members resolves the containing polygon when a name matches more than one", {
  skip_if_not_installed("sf")

  # Two same-named "Lufkin" polygons at disjoint locations — the realistic
  # case is a CDP and an incorporated place sharing a name. The first (index
  # 1) is a CDP the agency does NOT fall in; the second (index 2) is
  # incorporated and DOES contain the agency. Under the old `match_poly[1, ]`
  # behaviour this would incorrectly tag the agency with the first polygon's
  # place_type/place_fips.
  fixture_places_dup <- function() {
    sq <- function(cx, cy) {
      sf::st_polygon(list(cbind(
        c(cx - 0.1, cx + 0.1, cx + 0.1, cx - 0.1, cx - 0.1),
        c(cy - 0.1, cy - 0.1, cy + 0.1, cy + 0.1, cy - 0.1)
      )))
    }
    sf::st_sf(
      GEOID = c("4800001", "4800002"),
      NAME = c("Lufkin", "Lufkin"),
      CLASSFP = c("U1", "C1"),
      geometry = sf::st_sfc(sq(-94.7, 31.3), sq(-96.0, 32.0), crs = 4326)
    )
  }

  x <- place_agencies("Lufkin", "TX")
  fake_agencies <- data.frame(
    ori = "TX9999999",
    agency_name = "Second Polygon College",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 32.0,
    longitude = -96.0,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  out <- add_place_spatial_members(
    x, places_fun = function(state, vintage) fixture_places_dup()
  )
  added <- out[out$attribution == "point_in_polygon", , drop = FALSE]
  expect_equal(nrow(added), 1L)
  expect_equal(added$place_fips, "4800002")
  expect_equal(added$place_type, "incorporated")
})

test_that("add_place_spatial_members leaves name_identity rows with NA place_fips", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())

  primary <- out[out$attribution == "name_identity", , drop = FALSE]
  expect_equal(nrow(primary), 1L)
  expect_true(is.na(primary$place_fips))
  expect_true(is.na(primary$place_type))
})

# ---- Defensiveness: polygon frame is not trusted blindly ------------------

test_that("add_place_spatial_members errors clearly when the polygon frame is missing required columns", {
  x <- place_agencies("Lufkin", "TX")
  bad_polys <- data.frame(NAME = "Lufkin", stringsAsFactors = FALSE)  # no GEOID/CLASSFP

  expect_error(
    add_place_spatial_members(x, places_fun = function(state, vintage) bad_polys),
    "missing required columns.*GEOID.*CLASSFP"
  )
})

test_that("add_place_spatial_members errors clearly when places_fun does not return a data.frame", {
  x <- place_agencies("Lufkin", "TX")

  expect_error(
    add_place_spatial_members(x, places_fun = function(state, vintage) "not a frame"),
    "must return a data.frame"
  )
})

test_that("add_place_spatial_members does not produce a phantom match from an NA place NAME", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  polys_with_na <- fixture_places()
  polys_with_na$NAME[1] <- NA_character_  # was "Lufkin" — the very name being matched

  fake_agencies <- data.frame(
    ori = "TX1234567",
    agency_name = "Angelina College",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 31.3,
    longitude = -94.7,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() fake_agencies,
    .package = "fbiCDE"
  )

  expect_warning(
    out <- add_place_spatial_members(x, places_fun = function(state, vintage) polys_with_na),
    "No Census place polygon named"
  )
  expect_equal(nrow(out), nrow(x))
})
