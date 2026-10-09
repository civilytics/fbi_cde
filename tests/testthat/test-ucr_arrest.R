# Offline tests for arrest functions using fixtures.
#
# Fixtures recorded live from arrest/{level}/all?type=counts for
# 01-2019..03-2019 (see tests/testthat/fixtures/README.md). At agency level
# the response includes state/national comparison series alongside the
# agency's own counts; they are dropped unless comparison = TRUE -- see
# test-ucr_crime.R for the same pattern on summarized crime.

test_that("get_arrest_count returns only the agency's own series by default", {
  local_fbi_fixture("arrest-agency-CA0010900-counts.json")
  result <- get_arrest_count("CA0010900", from = "01-2019", to = "03-2019")

  expect_equal(names(result),
               c("geography", "offense", "measure", "period", "count", "rate",
                 "population", "participated_population"))
  expect_equal(nrow(result), 3)
  expect_equal(unique(result$geography), "CA0010900")
  expect_equal(unique(result$offense), "all")
  expect_equal(unique(result$measure), "arrests")
  expect_equal(result$period, c("01-2019", "02-2019", "03-2019"))
  expect_equal(result$count, c(823, 774, 762))
  expect_false(anyNA(result$count))
})

test_that("get_arrest_count(comparison = TRUE) labels state and national series", {
  local_fbi_fixture("arrest-agency-CA0010900-counts.json")
  result <- get_arrest_count("CA0010900", from = "01-2019", to = "03-2019",
                             comparison = TRUE)

  expect_equal(nrow(result), 9)
  expect_equal(unique(result$series), c("agency", "state", "national"))
  expect_equal(unique(result$series_name),
               c("Oakland Police Department", "California", "United States"))
  expect_true(all(is.na(result$count[result$series != "agency"])))
  expect_false(anyNA(result$rate))
})

test_that("get_arrest_count parses national-level counts response", {
  local_fbi_fixture("arrest-national-all-counts.json")
  result <- get_arrest_count(from = "01-2019", to = "03-2019")

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_equal(unique(result$geography), "US")
  expect_equal(nrow(result), 3)
  expect_equal(result$count, c(807594, 758199, 866055))
  expect_equal(unique(result$measure), "arrests")
})

test_that("get_arrest_demographics parses demographics response", {
  local_fbi_fixture("arrest-national-all-totals.json")
  result <- get_arrest_demographics()

  expect_s3_class(result, "data.frame")
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
  expect_true("period" %in% names(result))
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
  expect_true("count" %in% names(result))
  expect_equal(result$geography[1], "US")
  expect_true(nrow(result) > 0)
  # demographics are aggregated across all offenses
  expect_true(all(result$demographic_type %in%
    c("Arrestee Sex", "Arrestee Race",
      "Male Arrests By Age", "Female Arrests By Age")))
})

# Serve recorded arrest/state/OH/{code} fixtures by path and type, recording
# the paths requested.
local_arrest_code_fixtures <- function(env = parent.frame()) {
  requested <- new.env()
  requested$paths <- character(0)
  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      requested$paths <- c(requested$paths, path)
      code <- sub("^arrest/state/OH/", "", path)
      read_fixture(sprintf("arrest-state-OH-%s-%s.json", code, query$type))
    },
    .package = "fbiCDE",
    .env = env
  )
  requested
}

test_that("get_arrest_demographics breaks one offense down by age and sex", {
  req <- local_arrest_code_fixtures()
  result <- get_arrest_demographics(state_abb = "OH", offense = "larceny",
                                    from = "01-2023", to = "12-2023")
  expect_equal(req$paths, "arrest/state/OH/70")
  expect_equal(unique(result$offense), "Larceny")
  sex <- result[result$demographic_type == "Arrestee Sex", ]
  expect_equal(sum(sex$count), 19500)
  under18 <- c("Under 10", "10-12", "13-14", "15", "16", "17")
  by_age <- result[grepl("By Age", result$demographic_type) &
                     result$demographic_value %in% under18, ]
  expect_equal(sum(by_age$count), 1666)
})

test_that("get_arrest_demographics sums the codes of a multi-code offense", {
  req <- local_arrest_code_fixtures()
  result <- get_arrest_demographics(state_abb = "OH",
                                    offense = "Homicide Offenses",
                                    from = "01-2023", to = "12-2023")
  expect_setequal(req$paths, c("arrest/state/OH/11", "arrest/state/OH/12"))
  sex <- result[result$demographic_type == "Arrestee Sex", ]
  # One row per demographic value, summed: murder 314 + negligent 12.
  expect_false(anyDuplicated(sex$demographic_value) > 0)
  expect_equal(sum(sex$count), 326)
})

test_that("get_arrest_demographics returns empty for no totals data", {
  local_mocked_bindings(
    cde_request = function(...) list(cde_properties = list()),
    .package = "fbiCDE"
  )
  result <- get_arrest_demographics("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_arrest_count returns a monthly series for one offense", {
  req <- local_arrest_code_fixtures()
  result <- get_arrest_count(state_abb = "OH", offense = "Larceny",
                             from = "01-2023", to = "03-2023")
  expect_equal(req$paths, "arrest/state/OH/70")
  expect_equal(result$period, c("01-2023", "02-2023", "03-2023"))
  expect_equal(unique(result$offense), "Larceny")
  expect_equal(unique(result$measure), "arrests")
  fx <- read_fixture("arrest-state-OH-70-counts.json")
  expect_equal(result$count,
               unname(vapply(fx$actuals[["Ohio Arrests"]], as.numeric, 1)))
  expect_false(anyNA(result$participated_population))
})

test_that("get_arrest_count sums the codes of a multi-code offense", {
  req <- local_arrest_code_fixtures()
  result <- get_arrest_count(state_abb = "OH", offense = "homicide offenses",
                             from = "01-2023", to = "03-2023")
  expect_setequal(req$paths, c("arrest/state/OH/11", "arrest/state/OH/12"))
  expect_equal(unique(result$offense), "Homicide Offenses")
  expect_equal(nrow(result), 3L)
  per_code <- vapply(c("11", "12"), function(code) {
    fx <- read_fixture(sprintf("arrest-state-OH-%s-counts.json", code))
    sum(vapply(fx$actuals[["Ohio Arrests"]], as.numeric, 1))
  }, numeric(1))
  expect_equal(sum(result$count), sum(per_code))
})

test_that("offense names resolve at every level to their codes", {
  sel <- .arrest_offense_selection
  expect_equal(sel("all")$codes, "all")
  expect_equal(sel("Larceny")$codes, "70")
  expect_equal(sort(sel("Drug/Narcotic Offenses")$codes),
               as.character(150:160))
  expect_equal(sel("drug possession - marijuana")$codes, "158")
  expect_equal(sel("70")$offense, "Larceny - Theft (Not Specified)")
  # A name at two levels resolves as an offense name first.
  expect_equal(sel("Sex Offenses")$codes, "240")
  expect_error(sel("Rape"), "no arrests under 'Rape'.*Rape \\(Legacy\\)")
  expect_error(sel("Runaway"), "no arrests under 'Runaway'")
  expect_error(sel(c("Larceny", "Robbery")), "single string")
})

test_that("every listed offense name with arrests has a code", {
  codes <- ucr_arrest_offense_codes
  expect_false(anyDuplicated(codes$code) > 0)
  covered <- unique(c(codes$name, codes$category, codes$breakdown))
  expect_setequal(setdiff(list_ucr_arrest_offenses(), covered),
                  c("Rape", "Runaway", "Rape - Not Specified"))
})

test_that("the bundled arrest offense names match the recorded response, by level", {
  totals <- read_fixture("arrest-national-all-totals.json")
  expect_setequal(list_ucr_arrest_offenses("name"),
                  names(totals[["Offense Name"]]))
  expect_setequal(list_ucr_arrest_offenses("category"),
                  names(totals[["Offense Category"]]))
  expect_setequal(list_ucr_arrest_offenses("breakdown"),
                  names(totals[["Offense Breakdown"]]))
  expect_setequal(list_ucr_arrest_offenses(),
                  unique(c(names(totals[["Offense Name"]]),
                           names(totals[["Offense Category"]]),
                           names(totals[["Offense Breakdown"]]))))
  expect_error(list_ucr_arrest_offenses("subcategory"))
})

test_that("get_arrest_count rejects an unknown offense before any request", {
  testthat::local_mocked_bindings(
    cde_request = function(...) stop("no request expected"),
    .package = "fbiCDE"
  )
  expect_error(
    get_arrest_count(offense = "jaywalking"),
    "Invalid arrest offense"
  )
})

# Validation tests

test_that("get_arrest_count validates ORI", {
  expect_error(get_arrest_count("bad-ori"), "Invalid ORI code")
})

test_that("get_arrest_count validates state abbreviation", {
  expect_error(get_arrest_count(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_arrest_count rejects inverted date ranges", {
  expect_error(
    get_arrest_count(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_arrest_demographics validates ORI", {
  expect_error(
    get_arrest_demographics("bad-ori", offense = "robbery"),
    "Invalid ORI code"
  )
})

test_that("get_arrest_demographics validates state abbreviation", {
  expect_error(
    get_arrest_demographics(state_abb = "XX", offense = "robbery"),
    "Invalid state abbreviation"
  )
})

test_that("get_arrest_demographics defaults to offense = all", {
  local_fbi_fixture("arrest-national-all-totals.json")
  expect_silent(result <- get_arrest_demographics("CA0010900"))
  expect_s3_class(result, "data.frame")
})

# Live tests

test_that("get_arrest_count returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_arrest_count("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("geography" %in% names(result))
  expect_true("offense" %in% names(result))
})

test_that("get_arrest_demographics returns expected shape from live API", {
  skip_if_no_fbi_api()
  result <- get_arrest_demographics("CA0010900")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("demographic_type" %in% names(result))
  expect_true("demographic_value" %in% names(result))
})

test_that("one offense by code reconciles with the all-offense totals, live", {
  skip_if_no_fbi_api()
  series <- get_arrest_count(state_abb = "OH", offense = "Drug/Narcotic Offenses",
                             from = "01-2023", to = "12-2023")
  expect_equal(nrow(series), 12L)
  totals <- cde_request(cde_path("arrest", "state/OH", "all"),
                        list(from = "01-2023", to = "12-2023", type = "totals"))
  expect_equal(sum(series$count),
               as.numeric(totals[["Offense Category"]][["Drug/Narcotic Offenses"]]))

  demo <- get_arrest_demographics(state_abb = "OH", offense = "Larceny",
                                  from = "01-2023", to = "12-2023")
  sex <- demo[demo$demographic_type == "Arrestee Sex", ]
  expect_equal(sum(sex$count),
               as.numeric(totals[["Offense Name"]][["Larceny"]]))
})

test_that("all-offense demographics omit exactly the Unspecified codes, live", {
  # Documented in ?get_arrest_demographics. If this fails, the API has
  # changed and the documentation must change with it.
  skip_if_no_fbi_api()
  q <- list(from = "01-2023", to = "12-2023", type = "totals")
  all <- cde_request(cde_path("arrest", "state/OH", "all"), q)
  gap <- sum(unlist(all[["Offense Name"]])) - sum(unlist(all[["Arrestee Sex"]]))
  codes <- ucr_arrest_offense_codes
  unspecified <- codes$code[grepl("(Unspecified)", codes$breakdown, fixed = TRUE)]
  expect_equal(sort(unspecified), c("140", "150", "151", "156", "170"))
  in_unspecified <- sum(vapply(unspecified, function(code) {
    r <- cde_request(cde_path("arrest", "state/OH", code), q)
    sum(unlist(r[["Arrestee Sex"]]))
  }, numeric(1)))
  expect_gt(gap, 0)
  expect_equal(gap, in_unspecified)
})

test_that("every bundled arrest code is served live", {
  skip_if_no_fbi_api()
  for (code in ucr_arrest_offense_codes$code) {
    r <- cde_request(cde_path("arrest", "national", code),
                     list(from = "01-2023", to = "12-2023", type = "totals"))
    expect_true(!is.null(r[["Offense Name"]]), info = code)
  }
})

test_that("list_ucr_arrest_offenses returns a character vector", {
  result <- list_ucr_arrest_offenses()
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("arrest series carry population and participated_population", {
  local_fbi_fixture("arrest-agency-CA0010900-counts.json")
  fx <- read_fixture("arrest-agency-CA0010900-counts.json")
  result <- get_arrest_count("CA0010900", from = "01-2019", to = "03-2019")
  expected <- vapply(result$period, function(p) {
    as.numeric(fx$populations$participated_population[[
      "Oakland Police Department"]][[p]])
  }, numeric(1), USE.NAMES = FALSE)
  expect_equal(result$participated_population, expected)
  expect_false(anyNA(result$population))
})


