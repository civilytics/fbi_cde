# ---- Crosswalk integrity ---------------------------------------------------
#
# These assert the bundled asset itself. If the delineation file or the build
# script drifts, CI fails loudly rather than results changing silently.

test_that("sysdata still carries BOTH internal crosswalks", {
  # Regression guard: R/sysdata.rda holds multiple objects and save()
  # overwrites wholesale, so a careless rebuild can destroy the county FIPS
  # table. Both must survive.
  expect_true(is.data.frame(fbi:::crosswalk))
  expect_gt(nrow(fbi:::crosswalk), 3000L)
  expect_true(is.data.frame(fbi:::cbsa_crosswalk))
  expect_gt(nrow(fbi:::cbsa_crosswalk), 1800L)
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
  counties <- fbi:::crosswalk$county_fips
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
