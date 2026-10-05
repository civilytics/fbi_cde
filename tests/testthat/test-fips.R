library(testthat)
test_that("county_to_fips resolves common counties", {
  expect_equal(county_to_fips("CA", "LOS ANGELES"), "06037")
  expect_equal(county_to_fips("TX", "HARRIS"), "48201")
  expect_equal(county_to_fips("NY", "NEW YORK"), "36061")
  expect_equal(county_to_fips("IL", "COOK"), "17031")
})

test_that("county_to_fips handles TX name variants", {
  # DeWitt (capital W, no space) in TX
  expect_equal(county_to_fips("TX", "DEWITT"), "48123")
  # La Salle (space, period) in TX
  expect_equal(county_to_fips("TX", "LA SALLE"), "48283")
  # Multi-county agency (takes first county)
  expect_equal(county_to_fips("TX", "DEWITT; LAVACA"), "48123")
})

test_that("county_to_fips handles independent cities", {
  # MD Baltimore City vs County
  expect_equal(county_to_fips("MD", "BALTIMORE CITY"), "24510")
  expect_equal(county_to_fips("MD", "BALTIMORE"), "24005")
  
  # MO St. Louis City vs County
  expect_equal(county_to_fips("MO", "ST LOUIS CITY"), "29510")
  expect_equal(county_to_fips("MO", "ST LOUIS"), "29189")
  expect_equal(county_to_fips("MO", "ST LOUIS; FRANKLIN"), "29189")
  
  # MN St. Louis County (not an independent city state)
  expect_equal(county_to_fips("MN", "ST LOUIS"), "27137")
})

test_that("county_to_fips handles Virginia independent cities", {
  expect_equal(county_to_fips("VA", "ALEXANDRIA CITY"), "51510")
  expect_equal(county_to_fips("VA", "NORFOLK CITY"), "51710")
  expect_equal(county_to_fips("VA", "VIRGINIA BEACH CITY"), "51810")
  # VA county (not a city)
  expect_equal(county_to_fips("VA", "ACCOMACK"), "51001")
})

test_that("county_to_fips handles Connecticut planning regions", {
  # CT uses planning regions instead of traditional counties
  expect_equal(county_to_fips("CT", "HARTFORD"), "09003")
  expect_equal(county_to_fips("CT", "FAIRFIELD"), "09001")
  expect_equal(county_to_fips("CT", "NEW HAVEN"), "09009")
})

test_that("county_to_fips handles Alaska boroughs", {
  expect_equal(county_to_fips("AK", "ANCHORAGE"), "02020")
  expect_equal(county_to_fips("AK", "FAIRBANKS NORTH STAR"), "02090")
  expect_equal(county_to_fips("AK", "ALEUTIANS WEST"), "02016")
})

test_that("county_to_fips handles ST./ST. name variants", {
  expect_equal(county_to_fips("IL", "ST CLAIR"), "17163")
  expect_equal(county_to_fips("LA", "ST CHARLES"), "22089")
  expect_equal(county_to_fips("MO", "STE GENEVIEVE"), "29186")
})

test_that("county_to_fips returns NA for unresolvable cases", {
  # N/A county (state police, tribal, etc.)
  expect_true(is.na(county_to_fips("AK", "N/A")))
  expect_true(is.na(county_to_fips("TX", "N/A")))
  
  # VALDEZ-CORDOVA (dissolved AK borough)
  expect_true(is.na(county_to_fips("AK", "VALDEZ-CORDOVA")))
})

test_that("county_to_fips handles state FIPS codes", {
  expect_equal(county_to_fips("06", "LOS ANGELES"), "06037")
  expect_equal(county_to_fips("48", "HARRIS"), "48201")
  expect_equal(county_to_fips("24", "BALTIMORE CITY"), "24510")
})

test_that("county_to_fips is case-insensitive", {
  expect_equal(county_to_fips("ca", "los angeles"), "06037")
  expect_equal(county_to_fips("Ca", "Los Angeles"), "06037")
  expect_equal(county_to_fips("CA", "Los Angeles"), "06037")
})

# ---- Structural invariant --------------------------------------------------
#
# A county FIPS code begins with its state's 2-digit FIPS. That is total, cheap
# to check, and would have caught the Connecticut cross-state leak in #50 at
# build time. Asserted here over the bundled asset so it runs on every CI run,
# not only when the crosswalk is regenerated.

test_that("every crosswalk FIPS begins with its own state's FIPS prefix", {
  cw <- fbiCDE:::crosswalk
  cw <- cw[!is.na(cw$county_fips), , drop = FALSE]

  # Guard against the guard: an empty crosswalk would pass trivially.
  expect_gt(nrow(cw), 3000L)

  expected <- suppressWarnings(fbiCDE:::.state_abbr_to_fips(cw$state_abbr))
  mismatched <- cw[substr(cw$county_fips, 1L, 2L) != expected, , drop = FALSE]

  # Name the offenders, so a failure is diagnosable without re-deriving it.
  expect_equal(
    nrow(mismatched), 0L,
    info = paste0(
      "rows whose FIPS state prefix disagrees with their state: ",
      paste(sprintf("%s|%s->%s", mismatched$state_abbr,
                    mismatched$county_name, mismatched$county_fips),
            collapse = ", ")
    )
  )
})

test_that("counties sharing a name across states resolve to their own state (#50)", {
  # Connecticut retains historical county names that also exist elsewhere; the
  # crosswalk previously returned CT's FIPS for all of them.
  expect_equal(county_to_fips("MA", "MIDDLESEX"), "25017")
  expect_equal(county_to_fips("NJ", "MIDDLESEX"), "34023")
  expect_equal(county_to_fips("VA", "MIDDLESEX"), "51119")
  expect_equal(county_to_fips("OH", "FAIRFIELD"), "39045")
  expect_equal(county_to_fips("SC", "FAIRFIELD"), "45039")
  expect_equal(county_to_fips("VT", "WINDHAM"), "50025")

  # Multi-county variants take the first county, still in the right state.
  expect_equal(county_to_fips("OH", "FAIRFIELD; LICKING"), "39045")
})

test_that("Connecticut's own counties are not broken by the #50 fix", {
  # The fix must not overcorrect: these CT codes are legitimately 09xxx.
  expect_equal(county_to_fips("CT", "MIDDLESEX"), "09007")
  expect_equal(county_to_fips("CT", "FAIRFIELD"), "09001")
  expect_equal(county_to_fips("CT", "WINDHAM"), "09015")
})

test_that("Virginia independent cities that share a county's name resolve to the city", {
  # The patch logic stripped " CITY" and landed on the same-named county.
  expect_equal(county_to_fips("VA", "RICHMOND CITY"), "51760")
  expect_equal(county_to_fips("VA", "FAIRFAX CITY"), "51600")
  expect_equal(county_to_fips("VA", "FRANKLIN CITY"), "51620")
  expect_equal(county_to_fips("VA", "ROANOKE CITY"), "51770")
  # ...while the counties themselves are unchanged.
  expect_equal(county_to_fips("VA", "RICHMOND"), "51159")
  expect_equal(county_to_fips("VA", "FAIRFAX"), "51059")
  expect_equal(county_to_fips("VA", "FRANKLIN"), "51067")
  expect_equal(county_to_fips("VA", "ROANOKE"), "51161")
  # Case-insensitive, as county_agencies() passes user input through.
  expect_equal(county_to_fips("va", "Richmond City"), "51760")
})

test_that("county_to_fips agrees with the crosswalk for every CDE county name", {
  ag <- fbi_api_agencies
  pairs <- unique(ag[, c("state_abbr", "county_name")])
  got <- mapply(county_to_fips, pairs$state_abbr, pairs$county_name,
                USE.NAMES = FALSE)
  cw <- crosswalk
  want <- cw$county_fips[match(paste(pairs$state_abbr, pairs$county_name),
                               paste(cw$state_abbr, cw$county_name))]
  expect_identical(got, want)
})

test_that("counties_with_fips returns one row per resolvable county", {
  out <- counties_with_fips()
  expect_equal(names(out), c("state_abbr", "county_name", "county_fips"))
  expect_gt(nrow(out), 3000L)
  expect_false(any(grepl(";", out$county_name, fixed = TRUE)))
  expect_false(anyNA(out$county_fips))
  expect_false(any(duplicated(out$county_fips)))
  expect_equal(
    out$county_fips[out$state_abbr == "VA" & out$county_name == "RICHMOND CITY"],
    "51760"
  )
})
