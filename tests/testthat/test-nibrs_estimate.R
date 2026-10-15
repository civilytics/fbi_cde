test_that("the estimates lookup parses into the bundled vocabularies", {
  parsed <- .parse_estimation_lookup(
    read_fixture("nibrs-estimation-lookup-all.json")
  )
  expect_equal(parsed$states, nibrs_estimation_states)
  expect_equal(parsed$indicators, nibrs_estimation_indicators)
  expect_equal(parsed$years, nibrs_estimation_years)
})

test_that("estimate state ids are the lookup's own, not FIPS", {
  st <- nibrs_estimation_states
  ids <- st$id
  names(ids) <- st$state_abbr
  expect_equal(unname(ids[c("AK", "AL", "CA", "OH", "TX", "DC")]),
               c(1L, 2L, 6L, 39L, 48L, 10L))
  # "Federal" (98) and "Other" (99) have no state abbreviation.
  expect_false(any(c("Federal", "Other") %in% st$state_name))
  expect_equal(nrow(st), 57L)
  expect_false(anyNA(st$state_abbr))
})

test_that("every supported offense maps to exactly one estimate indicator", {
  ind <- nibrs_estimation_indicators
  for (code in names(.ESTIMATE_OFFENSES)) {
    hits <- ind$id[ind$indicator == .ESTIMATE_OFFENSES[[code]]$indicator]
    expect_length(hits, 1L)
  }
  expect_equal(
    ind$id[ind$indicator == .ESTIMATE_OFFENSES$V$indicator], 133L
  )
  expect_equal(.ESTIMATE_OFFENSES$V$nibrs,
               c("09A", "11A", "11B", "11C", "120", "13A"))
})

test_that("the bundled estimate vocabularies match the live lookup", {
  skip_if_no_fbi_api()
  # Fails when the FBI adds a year: rebuild with data-raw/api_vocabularies.R.
  live <- .parse_estimation_lookup(cde_request("nibrs-estimation/lookup/all"))
  expect_equal(live$years, nibrs_estimation_years)
  expect_equal(live$states, nibrs_estimation_states)
  expect_equal(live$indicators, nibrs_estimation_indicators)
})
