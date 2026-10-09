# Offline tests for NIBRS functions, using responses recorded from the live
# API for 2023 (nibrs/{level}/{offense}?type=totals):
#
#   nibrs-agency-OHCOP0000-BUR.json  Columbus PD, burglary: populated
#   nibrs-state-OH-BUR.json          Ohio, burglary: populated
#   nibrs-agency-CA0010900-ROB.json  Oakland PD, robbery: every count null,
#                                    which is how the API answers for a
#                                    geography with no NIBRS data that period
#
# These replace hand-written fixtures whose variables ("count", "bias") and
# age buckets the API never returns, which is how the package shipped
# defaults that could not return data.

test_that("get_nibrs_victim parses an agency's victim breakdown", {
  local_fbi_fixture("nibrs-agency-OHCOP0000-BUR.json")
  result <- get_nibrs_victim("OHCOP0000", offense = "BUR", variable = "age",
                             from = "01-2023", to = "12-2023")

  expect_equal(names(result),
               c("geography", "offense", "period", "demographic_type",
                 "demographic_value", "count"))
  expect_equal(unique(result$geography), "OHCOP0000")
  expect_equal(unique(result$offense), "BUR")
  expect_equal(unique(result$demographic_type), "age")
  expect_equal(result$count[result$demographic_value == "20-29"], 967)
})

test_that("get_nibrs_offender parses a state's offender ages", {
  local_fbi_fixture("nibrs-state-OH-BUR.json")
  result <- get_nibrs_offender(state_abb = "OH", offense = "BUR",
                               variable = "age",
                               from = "01-2023", to = "12-2023")

  expect_equal(unique(result$geography), "OH")
  # Ten-year buckets: "10-19" straddles 18, so NIBRS cannot isolate juveniles.
  expect_true(all(c("0-9", "10-19", "Unknown") %in% result$demographic_value))
  expect_gt(sum(result$count), 0)
})

test_that("get_nibrs_offense defaults to weapons", {
  local_fbi_fixture("nibrs-state-OH-BUR.json")
  result <- get_nibrs_offense(state_abb = "OH", offense = "BUR",
                              from = "01-2023", to = "12-2023")

  expect_equal(unique(result$demographic_type), "weapons")
  expect_gt(nrow(result), 0)
})

test_that("a response with every count null returns zero rows and a message", {
  local_fbi_fixture("nibrs-agency-CA0010900-ROB.json")
  expect_message(
    result <- get_nibrs_victim("CA0010900", offense = "ROB"),
    "returned no data"
  )
  expect_equal(nrow(result), 0)
  expect_equal(names(result),
               c("geography", "offense", "period", "demographic_type",
                 "demographic_value", "count"))
})

test_that("a response without the section returns zero rows and a message", {
  local_mocked_bindings(cde_request = function(...) list(),
                        .package = "fbiCDE")
  expect_message(result <- get_nibrs_offender(offense = "BUR"),
                 "returned no data")
  expect_equal(nrow(result), 0)
})

test_that("the bundled variable lists match a recorded response", {
  x <- read_fixture("nibrs-state-OH-BUR.json")
  expect_setequal(list_nibrs_victim_variables(), names(x$victim))
  expect_setequal(list_nibrs_offender_variables(), names(x$offender))
  expect_setequal(list_nibrs_offense_variables(), names(x$offense))
})

# ---- Offense and variable checks ----

test_that("offense names and 'all' are rejected before any request", {
  local_mocked_bindings(
    cde_request = function(...) stop("no request expected"),
    .package = "fbiCDE"
  )
  expect_error(get_nibrs_victim(offense = "robbery"), 'Did you mean "ROB"')
  expect_error(get_nibrs_offense(offense = "burglary-breaking-and-entering"),
               'Did you mean "BUR"')
  expect_error(get_nibrs_offender(offense = "all"), "no all-offenses total")
  expect_error(get_nibrs_offender(offense = "jaywalking"),
               "not a NIBRS offense code")
})

test_that("offense codes are case-insensitive and sent upper-case", {
  path <- NULL
  local_mocked_bindings(
    cde_request = function(p, ...) {
      path <<- p
      read_fixture("nibrs-state-OH-BUR.json")
    },
    .package = "fbiCDE"
  )
  result <- get_nibrs_offender(state_abb = "OH", offense = "bur",
                               variable = "sex")
  expect_equal(path, "nibrs/state/OH/BUR")
  expect_equal(unique(result$offense), "BUR")
})

test_that("an unknown variable is rejected with the valid choices", {
  expect_error(get_nibrs_offender(offense = "BUR", variable = "count"),
               "must be one of: age, ethnicity, race, sex")
  expect_error(get_nibrs_offense(offense = "BUR", variable = "bias"),
               "related_offenses, weapons")
})

# ---- Validation tests ----

test_that("get_nibrs_victim validates ORI", {
  expect_error(get_nibrs_victim("bad-ori"), "Invalid ORI code")
})

test_that("get_nibrs_victim validates state abbreviation", {
  expect_error(get_nibrs_victim(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_nibrs_victim rejects inverted date ranges", {
  expect_error(
    get_nibrs_victim(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_nibrs_offender validates ORI", {
  expect_error(get_nibrs_offender("bad-ori"), "Invalid ORI code")
})

test_that("get_nibrs_offender validates state abbreviation", {
  expect_error(get_nibrs_offender(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_nibrs_offender rejects inverted date ranges", {
  expect_error(
    get_nibrs_offender(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

test_that("get_nibrs_offense validates ORI", {
  expect_error(get_nibrs_offense("bad-ori"), "Invalid ORI code")
})

test_that("get_nibrs_offense validates state abbreviation", {
  expect_error(get_nibrs_offense(state_abb = "XX"), "Invalid state abbreviation")
})

test_that("get_nibrs_offense rejects inverted date ranges", {
  expect_error(
    get_nibrs_offense(from = "12-2020", to = "01-2015"),
    "Invalid date range"
  )
})

# ---- list_* functions ----

test_that("list_nibrs_offenses returns codes and labels", {
  result <- list_nibrs_offenses()
  expect_equal(names(result), c("code", "label"))
  expect_true(all(c("V", "P", "ROB", "BUR", "13B", "35A") %in% result$code))
  expect_false(any(duplicated(result$code)))
  # Every listed code passes the offense check.
  expect_equal(vapply(result$code, .check_nibrs_offense, character(1),
                      USE.NAMES = FALSE), result$code)
})

test_that("list_nibrs_*_variables return the response's keys", {
  expect_equal(list_nibrs_victim_variables(),
               c("age", "ethnicity", "location", "race", "relationship", "sex"))
  expect_equal(list_nibrs_offender_variables(),
               c("age", "ethnicity", "race", "sex"))
  expect_equal(list_nibrs_offense_variables(),
               c("related_offenses", "weapons"))
})

# ---- Live tests ----

test_that("get_nibrs_victim returns data from the live API", {
  skip_if_no_fbi_api()
  result <- get_nibrs_victim(state_abb = "OH", offense = "ROB",
                             variable = "race",
                             from = "01-2023", to = "12-2023")
  expect_gt(nrow(result), 0)
  expect_gt(sum(result$count), 0)
})

test_that("get_nibrs_offender returns data from the live API", {
  skip_if_no_fbi_api()
  result <- get_nibrs_offender(state_abb = "OH", offense = "BUR",
                               variable = "age",
                               from = "01-2023", to = "12-2023")
  expect_gt(nrow(result), 0)
  expect_gt(sum(result$count), 0)
})

test_that("get_nibrs_offense returns data from the live API", {
  skip_if_no_fbi_api()
  result <- get_nibrs_offense(state_abb = "OH", offense = "ROB",
                              from = "01-2023", to = "12-2023")
  expect_gt(nrow(result), 0)
})

test_that("the live response has exactly the bundled variables", {
  skip_if_no_fbi_api()
  x <- cde_request(cde_path("nibrs", "state/OH", "ROB"),
                   cde_query("01-2023", "12-2023", type = "totals"))
  expect_setequal(list_nibrs_victim_variables(), names(x$victim))
  expect_setequal(list_nibrs_offender_variables(), names(x$offender))
  expect_setequal(list_nibrs_offense_variables(), names(x$offense))
})
