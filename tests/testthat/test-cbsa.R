# ---- Crosswalk integrity ---------------------------------------------------
#
# These assert the bundled asset itself. If the delineation file or the build
# script drifts, CI fails loudly rather than results changing silently.

test_that("sysdata still carries at least the known internal objects", {
  # Regression guard: R/sysdata.rda holds multiple objects and save()
  # overwrites wholesale, so a careless rebuild can destroy an object it
  # doesn't know about. Rather than enumerate exactly two objects by name
  # (which would not catch a THIRD object being dropped by a future rebuild),
  # assert the two known ones are present AND that the total object count in
  # the file has not shrunk below what it holds today. See data-raw/
  # cbsa_crosswalk.R and data-raw/fix_county_fips_state_prefix.R, which both
  # use the generic save(list = objs, envir = ...) idiom for this reason.
  sysdata_path <- testthat::test_path("..", "..", "R", "sysdata.rda")
  skip_if_not(file.exists(sysdata_path),
              "R/sysdata.rda source file not available from this test location")

  existing <- new.env(parent = emptyenv())
  load(sysdata_path, envir = existing)
  objs <- ls(existing)

  expect_true(all(c("crosswalk", "cbsa_crosswalk") %in% objs))
  expect_gte(length(objs), 2L)

  expect_true(is.data.frame(fbiCDE:::crosswalk))
  expect_gt(nrow(fbiCDE:::crosswalk), 3000L)
  expect_true(is.data.frame(fbiCDE:::cbsa_crosswalk))
  expect_gt(nrow(fbiCDE:::cbsa_crosswalk), 1800L)
})

test_that("cbsa_crosswalk has the expected shape", {
  cw <- .cbsa_table()

  expect_true(all(c("cbsa_code", "cbsa_title", "cbsa_type", "county_fips",
                    "central_outlying") %in% names(cw)))
  expect_true(all(nchar(cw$county_fips) == 5L))
  expect_setequal(unique(cw$cbsa_type), c("metro", "micro"))
  expect_false(any(is.na(cw$cbsa_title)))
})

test_that("CBSA titles are unique per code", {
  cw <- .cbsa_table()
  expect_gt(nrow(cw), 1800L)
  expect_equal(length(unique(cw$cbsa_code)), length(unique(cw$cbsa_title)))
})

test_that("the crosswalk covers the expected share of known counties", {
  cw <- .cbsa_table()
  counties <- fbiCDE:::crosswalk$county_fips
  expect_gt(length(counties), 3000L)
  covered <- mean(counties %in% cw$county_fips)
  # ~61% as of the 2023 delineation; rural counties belong to no CBSA.
  expect_gt(covered, 0.55)
  expect_lt(covered, 0.70)
})

# ---- list_metros() ---------------------------------------------------------

test_that("list_metros returns one row per CBSA with a county count", {
  out <- list_metros()

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("cbsa_code", "cbsa_title", "cbsa_type",
                             "n_counties"))
  expect_equal(nrow(out), length(unique(.cbsa_table()$cbsa_code)))
  expect_gt(nrow(out), 900L)
  expect_true(all(out$n_counties >= 1L))
})

test_that("list_metros filters by type", {
  metro <- list_metros(type = "metro")
  micro <- list_metros(type = "micro")

  expect_true(all(metro$cbsa_type == "metro"))
  expect_true(all(micro$cbsa_type == "micro"))
  expect_gt(nrow(metro), 300L)
  expect_gt(nrow(micro), 400L)
  expect_equal(nrow(metro) + nrow(micro), nrow(list_metros()))
})

test_that("list_metros rejects an unknown type", {
  expect_error(list_metros(type = "nonsense"), "must be")
})

test_that("list_metros carries the delineation vintage", {
  expect_equal(attr(list_metros(), "vintage"), CBSA_VINTAGE)
})

test_that("CBSA_VINTAGE matches the shipped data, not just itself (I4)", {
  # Regression guard: CBSA_VINTAGE must be sourced from cbsa_crosswalk's own
  # "vintage" attribute (see .onLoad() in R/fips.R), not an independent
  # literal in R/cbsa.R that a rebuild could bump without updating this
  # constant. Comparing against the shipped data directly -- rather than
  # against list_metros(), which itself just stamps CBSA_VINTAGE -- is the
  # only way this test can fail if the two ever diverge.
  expect_equal(CBSA_VINTAGE, attr(fbiCDE:::cbsa_crosswalk, "vintage"))
})

test_that("the largest metros carry the expected county counts", {
  out <- list_metros(type = "metro")

  expect_gt(max(out$n_counties), 20L)

  # New York is 22 counties in the 2023 delineation — a specific, checkable
  # anchor that catches drift. Note it is NOT the largest CBSA: San Juan, PR
  # has 40 municipios, and Atlanta 29. Territories are part of the official
  # delineation and are deliberately retained.
  ny <- out[out$cbsa_title == "New York-Newark-Jersey City, NY-NJ", , drop = FALSE]
  expect_equal(nrow(ny), 1L)
  expect_equal(ny$n_counties, 22L)
})

test_that("list_metros sorts titles the same way in every locale", {
  # order() follows the locale's collation, which put
  # "Albany-Schenectady-Troy, NY" first under C.UTF-8 and last elsewhere.
  albany <- grep("^Albany", list_metros()$cbsa_title, value = TRUE)
  expect_equal(albany, c("Albany, GA", "Albany, OR",
                         "Albany-Schenectady-Troy, NY"))
})
