test_that("enumerate_periods lists inclusive MM-YYYY months", {
  expect_equal(enumerate_periods("01-2019", "03-2019"),
               c("01-2019", "02-2019", "03-2019"))
  expect_equal(enumerate_periods("11-2020", "02-2021"),
               c("11-2020", "12-2020", "01-2021", "02-2021"))
  expect_equal(enumerate_periods("05-2022", "05-2022"), "05-2022")
})

test_that("parse_agency_detail builds per-period rows with reported flag", {
  # simplifyVector = FALSE shape, as cde_request() returns. 02-2021 is a hole.
  response <- list(
    offenses = list(
      actuals = list(
        "Testville PD Offenses"   = list("01-2021" = 10, "03-2021" = 14),
        "Testville PD Clearances" = list("01-2021" = 3,  "03-2021" = 5)
      ),
      rates = list(
        "Testville PD Offenses" = list("01-2021" = 50, "03-2021" = 70),
        "California Offenses"    = list("01-2021" = 40)  # comparison — ignored
      )
    ),
    populations = list(
      population = list(
        "Testville PD" = list("01-2021" = 20000, "02-2021" = 20000,
                              "03-2021" = 20000)
      ),
      participated_population = list(
        "Testville PD" = list("01-2021" = 20000, "02-2021" = 20000,
                              "03-2021" = 20000)
      )
    )
  )

  out <- parse_agency_detail(response, ori = "CA9999999", offense = "V",
                             from = "01-2021", to = "03-2021")

  expect_equal(out$period, c("01-2021", "02-2021", "03-2021"))
  expect_equal(out$count, c(10, NA, 14))
  expect_equal(out$reported, c(TRUE, FALSE, TRUE))
  expect_equal(out$population, c(20000, 20000, 20000))
  expect_equal(out$offense, rep("V", 3))
  expect_equal(out$ori, rep("CA9999999", 3))
  # rate = count / participated_population * 1e5, per period.
  expect_equal(out$rate[1], 10 / 20000 * 1e5)
  expect_true(is.na(out$rate[2]))
  # Comparison series never leak in.
  expect_false(any(grepl("California", as.character(unlist(out)))))
})
