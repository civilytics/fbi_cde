# Shared agency fan-out machinery (Gitea #54).
#
# The loop behind get_county_crime_detail(), get_place_crime_detail(), and
# get_metro_crime_detail() used to exist in three copies, which drifted five
# ways. One divergence was a real bug: the metro copy returned a silent
# zero-row frame where its siblings warned that a filter had emptied a
# non-empty agency set.
#
# The cross-level test below is the guard against that specific class of drift
# recurring. It asserts the invariant at every level from one place, so a
# fourth geographic level cannot quietly omit it.

# A minimal agency response in the nested-list shape cde_request() returns.
# Defined locally: top-level helpers in one test file are not visible to
# another, so this file must stand alone under a filtered run.
make_response <- function(name, counts, pop = 20000) {
  offense_key <- paste(name, "Offenses")
  actuals <- stats::setNames(list(as.list(counts)), offense_key)
  pop_series <- as.list(stats::setNames(rep(pop, length(counts)), names(counts)))
  pop_list <- stats::setNames(list(pop_series), name)
  list(
    offenses = list(actuals = actuals, rates = list()),
    populations = list(population = pop_list,
                       participated_population = pop_list)
  )
}

# ---- .filter_agency_members() ----------------------------------------------

test_that(".filter_agency_members keeps only the requested classes", {
  agencies <- data.frame(
    ori = c("A", "B", "C"),
    agency_class = c("municipal", "campus", "county_primary"),
    default_member = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  expect_equal(.filter_agency_members(agencies, "campus")$ori, "B")
  expect_equal(
    .filter_agency_members(agencies, c("municipal", "campus"))$ori,
    c("A", "B")
  )
})

test_that(".filter_agency_members honours default_only", {
  agencies <- data.frame(
    ori = c("A", "B", "C"),
    agency_class = c("municipal", "campus", "county_primary"),
    default_member = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  expect_equal(.filter_agency_members(agencies, NULL, TRUE)$ori, c("A", "C"))
  # agency_class wins over default_only, matching the documented precedence.
  expect_equal(.filter_agency_members(agencies, "campus", TRUE)$ori, "B")
  # Neither filter: unchanged.
  expect_equal(nrow(.filter_agency_members(agencies)), 3L)
})

test_that(".filter_agency_members can return zero rows", {
  agencies <- data.frame(
    ori = "A", agency_class = "municipal", default_member = TRUE,
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(.filter_agency_members(agencies, "tribal")), 0L)
})

# ---- .filter_desc() --------------------------------------------------------

test_that(".filter_desc names the filter responsible", {
  expect_equal(.filter_desc("campus"), "agency_class = campus")
  expect_equal(.filter_desc(c("campus", "state")),
               "agency_class = campus, state")
  expect_equal(.filter_desc(NULL), "default_only = TRUE")
})

# ---- The cross-level invariant ---------------------------------------------

test_that("every geography level warns when a filter empties a non-empty set", {
  # "tribal" resolves to nothing in all three fixtures, while each geography
  # itself is non-empty -- so reaching zero rows is the filter's doing, and
  # every level must say so rather than returning a silent empty frame.
  #
  # No network: each function returns before issuing any request.
  levels <- list(
    list(
      name = "county",
      members = function() county_agencies("Alameda", "CA"),
      call = function() {
        get_county_crime_detail("Alameda", "CA", agency_class = "tribal",
                                from = "01-2021", to = "01-2021")
      }
    ),
    list(
      name = "place",
      members = function() place_agencies("Lufkin", "TX"),
      call = function() {
        get_place_crime_detail("Lufkin", "TX", agency_class = "tribal",
                               from = "01-2021", to = "01-2021")
      }
    ),
    list(
      name = "metro",
      members = function() metro_agencies("Aberdeen, WA"),
      call = function() {
        get_metro_crime_detail("Aberdeen, WA", agency_class = "tribal",
                               from = "01-2021", to = "01-2021",
                               progress = FALSE)
      }
    )
  )

  # Guard against the guard: if a level ever stops covering all three, or a
  # fixture goes empty, this test must fail rather than pass vacuously.
  expect_equal(length(levels), 3L)

  for (lvl in levels) {
    members <- lvl$members()
    expect_gt(nrow(members), 0L)
    expect_false("tribal" %in% members$agency_class)

    expect_warning(out <- lvl$call(), "after filtering",
                   info = paste("level:", lvl$name))
    expect_equal(nrow(out), 0L, info = paste("level:", lvl$name))
  }
})

test_that("the emptied-filter warning names which filter was responsible", {
  expect_warning(
    get_place_crime_detail("Lufkin", "TX", agency_class = "tribal",
                           from = "01-2021", to = "01-2021"),
    "agency_class = tribal"
  )
  expect_warning(
    get_metro_crime_detail("Aberdeen, WA", agency_class = "tribal",
                           from = "01-2021", to = "01-2021", progress = FALSE),
    "agency_class = tribal"
  )
})

# ---- .fanout_agency_detail() ------------------------------------------------

test_that(".fanout_agency_detail copies exactly the requested metadata columns", {
  agencies <- data.frame(
    ori = "CA0000001",
    agency_name = "Alpha PD",
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = "TESTONIA",
    state_abbr = "CA",
    extra_col = "should not be copied",
    stringsAsFactors = FALSE
  )
  response <- make_response("Alpha PD", c("01-2021" = 10))

  testthat::local_mocked_bindings(
    cde_request = function(...) response,
    .package = "fbiCDE"
  )

  out <- .fanout_agency_detail(
    agencies = agencies,
    meta_cols = c("agency_name", "agency_class", "county_name"),
    cols = c("ori", "agency_name", "agency_class", "county_name",
             "offense", "period", "count"),
    empty_fn = function() data.frame(),
    offense = "V", from = "01-2021", to = "01-2021"
  )

  expect_equal(out$agency_name, "Alpha PD")
  expect_equal(out$county_name, "TESTONIA")
  expect_false("extra_col" %in% names(out))
  # state_abbr was not requested, so it must not appear.
  expect_false("state_abbr" %in% names(out))
})

test_that(".fanout_agency_detail returns the typed empty frame when all fail", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002"),
    agency_name = c("Alpha PD", "Beta PD"),
    stringsAsFactors = FALSE
  )
  sentinel <- function() {
    data.frame(ori = character(0), sentinel = logical(0),
               stringsAsFactors = FALSE)
  }

  testthat::local_mocked_bindings(
    cde_request = function(...) stop("503"),
    .package = "fbiCDE"
  )

  expect_warning(
    out <- .fanout_agency_detail(
      agencies = agencies,
      meta_cols = "agency_name",
      cols = c("ori", "agency_name"),
      empty_fn = sentinel,
      offense = "V", from = "01-2021", to = "01-2021"
    ),
    "Dropped 2 agencies"
  )

  # The level's own empty-frame constructor is used, not a generic one.
  expect_true("sentinel" %in% names(out))
  expect_equal(nrow(out), 0L)
  expect_equal(attr(out, "dropped"), c("CA0000001", "CA0000002"))
})

test_that(".fanout_agency_detail keeps survivors when only some agencies fail", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002"),
    agency_name = c("Alpha PD", "Beta PD"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      if (grepl("CA0000002", path)) stop("503")
      make_response("Alpha PD", c("01-2021" = 10))
    },
    .package = "fbiCDE"
  )

  expect_warning(
    out <- .fanout_agency_detail(
      agencies = agencies,
      meta_cols = "agency_name",
      cols = c("ori", "agency_name", "period", "count"),
      empty_fn = function() data.frame(),
      offense = "V", from = "01-2021", to = "01-2021"
    ),
    "Dropped 1 agency"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$ori, "CA0000001")
  expect_equal(attr(out, "dropped"), "CA0000002")
})
