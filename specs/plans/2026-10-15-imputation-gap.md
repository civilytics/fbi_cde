# Imputation-gap capstone Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rename `get_estimated_*()` to `get_reported_*()`, wrap the CDE's
NIBRS estimates (`get_nibrs_estimate()`), and ship `imputation_gap()`, which
shows how much of the FBI's published state or national figure was modelled
rather than reported, with the terms *reported*, *estimated* and *imputed*
defined consistently across the user-facing documentation.

**Architecture:** Three layers over the existing single network seam
`cde_request()`. Bundled lookup vocabularies (built by
`data-raw/api_vocabularies.R`) map state abbreviations and summary offense
codes to the estimates API's numeric IDs. `get_nibrs_estimate()` is a thin
wrapper over `nibrs-estimation/...`. `imputation_gap()` combines one estimate
request, one `summarized` request and one `nibrs/...?type=counts` request per
NIBRS code in the offense, per year.

**Tech Stack:** R (>= 3.5.0), base R, `httr`, `jsonlite`, `testthat` 3e,
roxygen2, knitr (precomputed vignettes), pkgdown.

**Spec:** `specs/2026-10-09-capstone-imputation-gap-design.md` — read §3.1,
§3.2, §3.3, §4 and §6 before starting. §6 holds the decisions; where §4 and
§6 differ, §6 wins.

## Global Constraints

- `R (>= 3.5.0)`. Base R only. `Imports` stay exactly `httr`, `jsonlite`,
  `datasets`, `utils`: no new imports, and no `stats::` calls (use
  `names(x) <- ...` instead of `stats::setNames()`).
- **All HTTP goes through `cde_request()`** (`R/http.R`). Build paths with
  `cde_path()`. Never call `httr` directly.
- `cde_request()` returns nested lists (`simplifyVector = FALSE`) and
  `read_fixture()` parses fixtures the same way. Index with `[[ ]]` /
  `names()`. Do not add `is.data.frame()` branches.
- **Every data path gets both** an offline fixture test (mocks
  `cde_request`) **and** a `skip_if_no_fbi_api()`-guarded live test.
- **Fixtures are verbatim recorded responses, never synthetic.** All the
  fixtures this plan needs are already recorded in
  `tests/testthat/fixtures/` (committed with this plan, 2026-10-15):
  `nibrs-estimation-lookup-all.json`,
  `nibrs-estimation-national-133-2021.json`,
  `nibrs-estimation-state-6-133-2021.json`,
  `nibrs-estimation-state-6-133-2022.json`,
  `nibrs-estimation-state-39-133-2022.json` (Ohio; has null and negative
  bounds), `summarized-state-CA-V-2021.json`, `summarized-state-CA-V-2022.json`,
  and `nibrs-counts-state-CA-{09A,11A,11B,11C,120,13A}-{2021,2022}.json`.
  Do not edit them. If a test value disagrees with this plan, the fixture is
  right and the plan is wrong: report it, do not re-record.
- Bundled vocabularies are built by `data-raw/api_vocabularies.R` from the
  live API, never by hand.
- **Rd files must be ASCII-safe** (`tests/testthat/test-ascii-docs.R`): no
  `±`, `≈`, `−` (Unicode minus), `Σ` in roxygen. Em-dashes and curly quotes
  are allowed.
- **User-facing prose** (roxygen, README, vignettes, NEWS): US spelling
  (modeled, neighboring); plain declarative sentences; numbers under 10
  spelled out, 10 and above as numerals; `U.S.` not `US` in prose (the
  `geography` value `"US"` is code and stays).
- **The three terms, defined once and used everywhere with exactly these
  meanings** (spec §6, decision 1):
  - *reported*: the sum of what agencies submitted to the FBI, in either
    format (NIBRS, or the older summary format). It covers only the agencies
    that reported.
  - *estimated*: the FBI's published figure. It adds a statistical model's
    prediction for everything outside NIBRS: agencies that did not report
    and, in 2022, agencies that reported only in the older summary format.
    It comes with a 95% confidence interval.
  - *imputed*: a value filled in for a missing report. The FBI's estimate
    imputes, and so does `impute_reporting_gaps()`, which fills an agency's
    missing months from its own neighboring months. Always say which one is
    meant.
- Conventional commits (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`).
  GitHub issues are written `GH-N`; bare `#N` means a Gitea issue. Never put
  a closing keyword (`fixes #N`) in a commit message. Record user-visible
  changes in `NEWS.md` under the development-version heading.
- Never write under `docs/` (pkgdown output) and never create
  `docs/superpowers/`. Specs in `specs/` are historical: do not rename
  `get_estimated_*` inside them.
- In a cloud session: work on a `claude/*` branch, open a pull request, do
  not merge it.

## Review Focus

1. **The counting residual makes `reported_other` and `gap` negative when
   nothing was modelled.** California 2021 has `reported_other = -504`
   (everything was NIBRS; the two series differ by counting rules); Texas
   2022 has a gap of about -900. These must come back negative, never
   clamped to 0 or `NA`. Pinned in Task 4, Step 1.
2. **A year the estimates do not cover (2020, or 2023 today) fails before
   any other request**, with the API's own message naming the available
   years. Pinned in Task 4, Step 1, and live in Task 3, Step 6.
3. **A lower-case state abbreviation** (`"ca"`) gives the same result as
   `"CA"`; the estimate's numeric state ID lookup must upper-case first.
   Pinned in Task 4, Step 1.
4. **An offense `imputation_gap()` cannot split by format** (`"13A"`,
   `"SHR"`, `c("V", "P")`) is refused with the supported codes listed, before
   any request. Pinned in Task 4, Step 1.
5. **Null and negative confidence bounds** (Ohio 2022: `Under 5` arrestees
   `0` with null bounds; American Indian arrestees `30 [-4.2, 64.5]`) parse
   to `NA` and to the negative value as published. Pinned in Task 3, Step 1.

---

### Task 1: Rename `get_estimated_*()` to `get_reported_*()`

**Files:**
- Modify: `R/ucr_crime.R:61-144`
- Create: `R/deprecated.R`
- Modify: `R/police_employment.R:7` (`@inheritParams get_estimated_arson`)
- Modify: `tests/testthat/test-ucr_crime.R` (tests at lines 50-105, 117-156)
- Modify: `tests/testthat/helper-fbiCDE.R:53-57` (comment example)
- Modify: `tests/testthat/fixtures/README.md` (example)
- Modify: `README.Rmd:106-120`, then regenerate `README.md`
- Modify: `vignettes/fbi.Rmd.orig:70-113` and `vignettes/fbi.Rmd` (same text)
- Modify: `pkgdown/_pkgdown.yml:27-28`
- Modify: `NEWS.md`

**Interfaces:**
- Produces: `get_reported_crime(state_abb = NULL, from = "01-2015", to =
  "12-2020", offense = "V", comparison = FALSE)` and
  `get_reported_arson(state_abb = NULL, from = "01-2015", to = "12-2020",
  comparison = FALSE)`, returning the same data.frame
  `get_estimated_crime()` returned. `get_estimated_crime(...)` and
  `get_estimated_arson(...)` remain exported, warn via `.Deprecated()`, and
  forward to the new names.

- [ ] **Step 1: Write the failing tests**

In `tests/testthat/test-ucr_crime.R`, rename every `get_estimated_crime` to
`get_reported_crime` and every `get_estimated_arson` to
`get_reported_arson` in the existing tests (test names included). Then
append:

```r
test_that("get_estimated_crime() still works, with a deprecation warning", {
  local_fbi_fixture("summarized-state-CA-V.json")
  expect_warning(
    old <- get_estimated_crime("CA", from = "01-2019", to = "03-2019"),
    "get_reported_crime"
  )
  expect_equal(old, get_reported_crime("CA", from = "01-2019", to = "03-2019"))
})

test_that("get_estimated_arson() still works, with a deprecation warning", {
  local_fbi_fixture("summarized-national-ARS.json")
  expect_warning(old <- get_estimated_arson(), "get_reported_arson")
  expect_equal(old, get_reported_arson())
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "ucr_crime")'`
Expected: FAIL, `could not find function "get_reported_crime"`.

- [ ] **Step 3: Rename the functions**

In `R/ucr_crime.R`, replace lines 61-144 (from `#' Get state- or
national-level reported crime counts` to the end of the file) with:

```r
#' Get state- or national-level reported crime counts
#'
#' Returns crime *reported* to the FBI: the sum of what the agencies that
#' reported each month sent, in either format (NIBRS, or the older summary
#' format). It is not the FBI's *estimate*, which adds a model's prediction
#' for the agencies that did not report. Compare `participated_population`
#' with `population` to see how much of the state or nation a month covers.
#' The FBI's state and national summary-format estimates are published as a
#' bulk download only, which the package does not read; the
#' [download recipe](https://github.com/civilytics/fbi_cde/blob/main/specs/2026-10-06-bulk-downloads-decision.md)
#' fetches it.
#'
#' This function was called `get_estimated_crime()` until fbiCDE
#' 0.1.0.9000; the old name still works and warns.
#'
#' @family UCR crime functions
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param from Start date in MM-YYYY format (default "01-2015").
#' @param to End date in MM-YYYY format (default "12-2020").
#' @param offense Offense code (default "V" for violent crime). See
#'   `get_offense_codes()` for available codes.
#' @param comparison If `TRUE`, a state query also returns the national
#'   comparison series (rate only). Default `FALSE` returns only the state's
#'   own series. Has no effect on a national query.
#'
#' @return A data.frame with the columns described in [get_agency_crime()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_reported_crime("CA")
#' }
get_reported_crime <- function(state_abb = NULL,
                               from = "01-2015",
                               to = "12-2020",
                               offense = "V",
                               comparison = FALSE) {
  if (!is.null(state_abb) && !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", state_abb, call. = FALSE)
  }

  cde_validate_dates(from, to, "mm-yyyy")

  if (is.null(state_abb)) {
    level <- "national"
  } else {
    level <- paste0("state/", toupper(state_abb))
  }

  path <- cde_path("summarized", level, offense)
  query <- list(from = from, to = to, type = "counts")

  response <- cde_request(path, query)

  geo <- if (is.null(state_abb)) "US" else toupper(state_abb)
  parse_summarized_response(response, geography = geo, offense = offense,
                            level = if (is.null(state_abb)) "national" else "state",
                            comparison = comparison)
}

#' Get state- or national-level reported arson counts
#'
#' [get_reported_crime()] for arson (`offense = "ARS"`): reported sums, not
#' the FBI's estimates. Called `get_estimated_arson()` until fbiCDE
#' 0.1.0.9000; the old name still works and warns.
#'
#' @family UCR crime functions
#' @inheritParams get_reported_crime
#'
#' @return A data.frame with the columns described in [get_agency_crime()].
#' @export
#'
#' @examples
#' \dontrun{
#' get_reported_arson("CA")
#' }
get_reported_arson <- function(state_abb = NULL,
                               from = "01-2015",
                               to = "12-2020",
                               comparison = FALSE) {
  get_reported_crime(
    state_abb = state_abb,
    from = from,
    to = to,
    offense = "ARS",
    comparison = comparison
  )
}
```

Create `R/deprecated.R`:

```r
#' Deprecated functions
#'
#' `get_estimated_crime()` and `get_estimated_arson()` were renamed
#' [get_reported_crime()] and [get_reported_arson()]: they return the crime
#' agencies reported to the FBI, not the FBI's estimates. The old names
#' still work and warn.
#'
#' @param ... Arguments passed to the new function.
#' @return The new function's result.
#' @name fbiCDE-deprecated
#' @keywords internal
NULL

#' @rdname fbiCDE-deprecated
#' @export
get_estimated_crime <- function(...) {
  .Deprecated("get_reported_crime", package = "fbiCDE")
  get_reported_crime(...)
}

#' @rdname fbiCDE-deprecated
#' @export
get_estimated_arson <- function(...) {
  .Deprecated("get_reported_arson", package = "fbiCDE")
  get_reported_arson(...)
}
```

In `R/police_employment.R:7`, change `@inheritParams get_estimated_arson` to
`@inheritParams get_reported_arson`.

- [ ] **Step 4: Regenerate documentation and run the tests**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test(filter = "ucr_crime")'`
Expected: PASS. `NAMESPACE` gains `export(get_reported_crime)` and
`export(get_reported_arson)` and keeps the two old exports.

- [ ] **Step 5: Update every other reference**

- `tests/testthat/helper-fbiCDE.R:53-57` and
  `tests/testthat/fixtures/README.md`: change the example to
  `get_reported_crime`.
- `pkgdown/_pkgdown.yml` lines 27-28: `get_reported_crime`,
  `get_reported_arson`. (The deprecated topic is `@keywords internal` and
  must not be listed.)
- `README.Rmd` lines 106-120: table rows become
  `` | `get_reported_crime()` | Get state- or national-level reported crime counts | ``
  and
  `` | `get_reported_arson()` | Get state- or national-level reported arson counts | ``;
  the code comments become `# State-level reported crime`, `# National-level
  reported crime`, `# Reported arson for a state`; the calls become
  `get_reported_crime("CA")`, `get_reported_crime()`,
  `get_reported_arson("CA")`. Regenerate with
  `Rscript -e 'rmarkdown::render("README.Rmd")'`, then delete any
  `README.html` it leaves and confirm `git diff README.md` touches only these
  lines.
- `vignettes/fbi.Rmd.orig` and `vignettes/fbi.Rmd` (make identical text
  edits in both; the outputs do not change, so do not re-knit): replace the
  sentence at `.orig` lines 70-71 with "`get_reported_crime()` does the same
  for a state, or the nation when `state_abb` is `NULL`;
  `get_reported_arson()` is `get_reported_crime()` for arson." and rename the
  two calls (`.orig` lines 104 and 113).

Then check nothing user-facing still uses the old names:

Run: `grep -rn "get_estimated" R/ tests/ README.Rmd README.md vignettes/ pkgdown/ man/ | grep -v "R/deprecated.R\|man/fbiCDE-deprecated.Rd\|still works, with a deprecation"`
Expected: no output except the `get_reported_*` doc sentences that name the
old function ("was called `get_estimated_crime()`").

- [ ] **Step 6: Record the change in NEWS.md**

Add under `# fbiCDE 0.1.0.9000 (development version)`, above the existing
first `##` section:

```markdown
## Renamed: `get_reported_crime()` and `get_reported_arson()`

`get_estimated_crime()` and `get_estimated_arson()` are now
`get_reported_crime()` and `get_reported_arson()`. They always returned the
crime agencies reported to the FBI, never the FBI's estimates, and the new
names say so. The old names still work and warn.
```

- [ ] **Step 7: Run the full offline suite and commit**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 failures.

```bash
git add R/ucr_crime.R R/deprecated.R R/police_employment.R NAMESPACE man/ \
  tests/testthat/test-ucr_crime.R tests/testthat/helper-fbiCDE.R \
  tests/testthat/fixtures/README.md README.Rmd README.md \
  vignettes/fbi.Rmd.orig vignettes/fbi.Rmd pkgdown/_pkgdown.yml NEWS.md
git commit -m "refactor: rename get_estimated_*() to get_reported_*() (#42)"
```

---

### Task 2: Bundled vocabularies for the NIBRS estimates

**Files:**
- Create: `R/nibrs_estimate.R` (this task adds the lookup parser and the
  offense map; Task 3 adds the wrapper to the same file)
- Modify: `data-raw/api_vocabularies.R`
- Create: `data/nibrs_estimation_states.rda`,
  `data/nibrs_estimation_indicators.rda`, `data/nibrs_estimation_years.rda`
  (written by the data-raw script)
- Modify: `R/data.R`
- Modify: `pkgdown/_pkgdown.yml` (Data section)
- Test: `tests/testthat/test-nibrs_estimate.R` (create)

**Interfaces:**
- Produces:
  - `.parse_estimation_lookup(response)` → `list(states, indicators,
    years)`.
  - `nibrs_estimation_states`: data.frame, 57 rows, columns `state_abbr`
    (chr), `state_name` (chr), `id` (int), ordered by `id`. Alaska 1,
    Alabama 2, California 6, Ohio 39, Texas 48.
  - `nibrs_estimation_indicators`: data.frame, 71 rows, columns `group`
    (chr), `indicator` (chr), `id` (int), ordered by `group` then `id` in C
    locale.
  - `nibrs_estimation_years`: integer vector, `c(2021L, 2022L)`.
  - `.ESTIMATE_OFFENSES`: named list keyed by summary code (`V`, `P`, `ROB`,
    `ASS`, `BUR`, `LAR`, `MVT`, `ARS`, `HOM`, `RPE`), each
    `list(indicator = <name in nibrs_estimation_indicators>, nibrs = <chr
    NIBRS codes>)`.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-nibrs_estimate.R`:

```r
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
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "nibrs_estimate")'`
Expected: FAIL, `could not find function ".parse_estimation_lookup"`.

- [ ] **Step 3: Write the parser and the offense map**

Create `R/nibrs_estimate.R`:

```r
# The CDE's NIBRS estimates: nibrs-estimation/{national,state/{id}}/{indicator}
# with a required ?year=YYYY. The path is used by the CDE web app's "NIBRS
# Estimates" page and is absent from the CDE's public API documentation. State
# and indicator IDs are the lookup's own numbers, not FIPS (Alaska is 1,
# California 6). See specs/2026-10-09-capstone-imputation-gap-design.md.

# Names the estimates lookup uses that datasets::state.name does not.
.ESTIMATE_STATE_EXTRAS <- c(
  "District of Columbia" = "DC", "Puerto Rico" = "PR", "Guam" = "GU",
  "U.S. Virgin Islands" = "VI", "American Samoa" = "AS",
  "Canal Zone" = "CZ", "Mariana Islands" = "MP"
)

# Parse nibrs-estimation/lookup/all into the three bundled vocabularies.
# "Federal" and "Other" have no state abbreviation and are left out.
.parse_estimation_lookup <- function(response) {
  data <- response[[1]]$data

  abbr <- datasets::state.abb
  names(abbr) <- datasets::state.name
  abbr <- c(abbr, .ESTIMATE_STATE_EXTRAS)

  st <- data$States
  states <- data.frame(
    state_abbr = unname(abbr[names(st)]),
    state_name = names(st),
    id = as.integer(unlist(st, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  states <- states[!is.na(states$state_abbr), , drop = FALSE]
  states <- states[order(states$id), , drop = FALSE]
  rownames(states) <- NULL

  ind <- data$Indicators
  indicators <- do.call(rbind, lapply(names(ind), function(group) {
    data.frame(
      group = group,
      indicator = names(ind[[group]]),
      id = as.integer(unlist(ind[[group]], use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  }))
  indicators <- indicators[order(indicators$group, indicators$id,
                                 method = "radix"), , drop = FALSE]
  rownames(indicators) <- NULL

  years <- sort(as.integer(unlist(data$Years, use.names = FALSE)))

  list(states = states, indicators = indicators, years = years)
}

# Summary offense codes the estimates and imputation_gap() support: the
# estimate indicator each maps to (by its name in nibrs_estimation_indicators)
# and the NIBRS offense codes whose counts make up the same offense. Summary
# property crime is burglary, larceny and motor vehicle theft (arson is
# separate); summary rape is the revised definition (11A, 11B, 11C). Each
# code was checked live against summarized/ on 2026-10-15 (spec 3.3).
.ESTIMATE_OFFENSES <- list(
  V   = list(indicator = "Violent Crime",
             nibrs = c("09A", "11A", "11B", "11C", "120", "13A")),
  P   = list(indicator = "Property Crime",
             nibrs = c("220", "23A", "23B", "23C", "23D", "23E", "23F",
                       "23G", "23H", "240")),
  ROB = list(indicator = "Robbery", nibrs = "120"),
  ASS = list(indicator = "Aggravated Assault", nibrs = "13A"),
  BUR = list(indicator = "Burglary/B&E", nibrs = "220"),
  LAR = list(indicator = "Larceny/Theft Offenses",
             nibrs = c("23A", "23B", "23C", "23D", "23E", "23F", "23G",
                       "23H")),
  MVT = list(indicator = "Motor Vehicle Theft", nibrs = "240"),
  ARS = list(indicator = "Arson", nibrs = "200"),
  HOM = list(indicator = "Murder and Non-negligent Manslaughter",
             nibrs = "09A"),
  RPE = list(indicator = "Rape", nibrs = c("11A", "11B", "11C"))
)
```

- [ ] **Step 4: Build the bundled data from the live API**

In `data-raw/api_vocabularies.R`, add to the header comment's list:

```r
#   nibrs_estimation_states      state abbreviation -> the estimates' own ID
#   nibrs_estimation_indicators  estimate indicator (offense) names and IDs
#   nibrs_estimation_years       years nibrs-estimation/ serves
```

Insert before the final `for (obj in ...)` loop:

```r
# ---- NIBRS estimation vocabularies -----------------------------------------
# nibrs-estimation/lookup/all: numeric state and indicator IDs (not FIPS) and
# the years the estimates cover. Parsed by the package's own parser so the
# bundled data and the offline test read the response the same way.
est <- .parse_estimation_lookup(cde_request("nibrs-estimation/lookup/all"))
nibrs_estimation_states <- est$states
nibrs_estimation_indicators <- est$indicators
nibrs_estimation_years <- est$years
```

and add `"nibrs_estimation_states", "nibrs_estimation_indicators",
"nibrs_estimation_years"` to the loop's vector.

Run: `Rscript data-raw/api_vocabularies.R` (needs network; a few minutes).
Then `git status data/`. The three new `.rda` files must appear. If any
**existing** `.rda` changed, inspect it with `load()` and compare to `git
show HEAD:data/<file>`: a genuine API change gets its own NEWS line and
commit; anything else is reverted with `git checkout data/<file>`.

- [ ] **Step 5: Document the three datasets**

Append to `R/data.R`:

```r
#' States the FBI's NIBRS estimates cover, with their estimate IDs.
#'
#' The estimates API identifies a state by its own number, in roughly
#' alphabetical order of the postal code (Alaska 1, Alabama 2, California
#' 6), not by FIPS. Built from `nibrs-estimation/lookup/all` by
#' `data-raw/api_vocabularies.R`. The lookup's "Federal" and "Other" entries
#' have no state abbreviation and are left out.
#'
#' @format A data frame with 57 rows and 3 variables:
#' \describe{
#'   \item{state_abbr}{Postal abbreviation, as passed to `state_abb`}
#'   \item{state_name}{Name as the lookup gives it}
#'   \item{id}{The estimates API's state ID}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all}
"nibrs_estimation_states"

#' Offenses ("indicators") the FBI's NIBRS estimates cover.
#'
#' Built from `nibrs-estimation/lookup/all` by `data-raw/api_vocabularies.R`.
#' [get_nibrs_estimate()] accepts an `id` from this table, or one of the
#' summary codes `"V"`, `"P"`, `"ROB"`, `"ASS"`, `"BUR"`, `"LAR"`, `"MVT"`,
#' `"ARS"`, `"HOM"` and `"RPE"`.
#'
#' @format A data frame with 71 rows and 3 variables:
#' \describe{
#'   \item{group}{The lookup's group: `"Person"`, `"Property"`, `"Society"`
#'     or `"GroupB"`}
#'   \item{indicator}{Offense name, e.g. `"Violent Crime"`}
#'   \item{id}{The estimates API's indicator ID, e.g. 133}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all}
"nibrs_estimation_indicators"

#' Years the FBI's NIBRS estimates cover in the API.
#'
#' Built from `nibrs-estimation/lookup/all` by `data-raw/api_vocabularies.R`.
#' The API publishes estimates for 2021 and 2022 only; a live test fails when
#' the FBI adds a year, so the package can be rebuilt.
#'
#' @format An integer vector.
#' @source \url{https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all}
"nibrs_estimation_years"
```

In `pkgdown/_pkgdown.yml`, add the three names to the `Data` section's
`contents`, after `regions`.

- [ ] **Step 6: Run the tests**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test(filter = "nibrs_estimate")'`
Expected: PASS (the live test skips).

Run: `FBI_CDE_LIVE=true Rscript -e 'devtools::test(filter = "nibrs_estimate")'`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add R/nibrs_estimate.R R/data.R data-raw/api_vocabularies.R \
  data/nibrs_estimation_states.rda data/nibrs_estimation_indicators.rda \
  data/nibrs_estimation_years.rda man/ pkgdown/_pkgdown.yml \
  tests/testthat/test-nibrs_estimate.R tests/testthat/fixtures/
git commit -m "feat: bundled vocabularies for the FBI's NIBRS estimates (#42)"
```

(The fixtures were recorded on 2026-10-15 and committed with the plan; if
`git status` shows them already tracked, `git add` is a no-op.)

---

### Task 3: `get_nibrs_estimate()`

**Files:**
- Modify: `R/nibrs_estimate.R` (append)
- Modify: `pkgdown/_pkgdown.yml` (new section)
- Modify: `NEWS.md`
- Test: `tests/testthat/test-nibrs_estimate.R` (append)

**Interfaces:**
- Consumes: `nibrs_estimation_states`, `nibrs_estimation_indicators`,
  `.ESTIMATE_OFFENSES` (Task 2).
- Produces:
  - `get_nibrs_estimate(state_abb = NULL, year, offense = "V", table =
    "Offense")` → data.frame with columns `geography` (chr, `"US"` or the
    upper-cased abbreviation), `year` (int), `offense` (chr, as given,
    upper-cased), `indicator` (int), `table` (chr), `variable` (chr),
    `category` (chr), `estimate` (num), `lower_bound` (num),
    `upper_bound` (num). The offense total is the row with
    `variable == "Offense Count - Total"` (category `"Count - Total"`) in
    the `"Offense"` table.
  - Internal helpers used by Task 4: `.estimate_level(state_abb)` →
    `"national"` or `"state/<id>"`; `.check_estimate_year(year)` → integer.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-nibrs_estimate.R`:

```r
# Capture the request a call makes, without needing a response.
capture_request <- function(env = parent.frame()) {
  seen <- new.env()
  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$path <- path
      seen$query <- query
      stop("captured")
    },
    .package = "fbiCDE", .env = env
  )
  seen
}

test_that("get_nibrs_estimate parses a state's offense table", {
  seen <- new.env()
  fixture <- read_fixture("nibrs-estimation-state-6-133-2022.json")
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$path <- path
      seen$query <- query
      fixture
    },
    .package = "fbiCDE"
  )
  out <- get_nibrs_estimate("CA", year = 2022)
  expect_equal(seen$path, "nibrs-estimation/state/6/133")
  expect_equal(seen$query, list(year = 2022L))
  expect_equal(names(out), c("geography", "year", "offense", "indicator",
                             "table", "variable", "category", "estimate",
                             "lower_bound", "upper_bound"))
  total <- out[out$variable == "Offense Count - Total", ]
  expect_equal(nrow(total), 1L)
  expect_equal(total$category, "Count - Total")
  expect_equal(total$estimate, 207381)
  expect_equal(total$lower_bound, 183100.636344443)
  expect_equal(total$upper_bound, 231662.98194076)
  expect_equal(unique(out$geography), "CA")
  expect_equal(unique(out$year), 2022L)
  expect_equal(unique(out$indicator), 133L)
  expect_equal(unique(out$table), "Offense")
})

test_that("get_nibrs_estimate parses the national estimate", {
  seen <- new.env()
  fixture <- read_fixture("nibrs-estimation-national-133-2021.json")
  local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$path <- path
      fixture
    },
    .package = "fbiCDE"
  )
  out <- get_nibrs_estimate(year = 2021)
  expect_equal(seen$path, "nibrs-estimation/national/133")
  expect_equal(unique(out$geography), "US")
  expect_equal(out$estimate[out$variable == "Offense Count - Total"],
               1325991)
})

test_that("null bounds parse to NA and negative bounds are kept", {
  local_fbi_fixture("nibrs-estimation-state-39-133-2022.json")
  out <- get_nibrs_estimate("OH", year = 2022, table = "Arrest")
  under5 <- out[out$variable == "Arrestee age" & out$category == "Under 5", ]
  expect_equal(under5$estimate, 0)
  expect_true(is.na(under5$lower_bound))
  expect_true(is.na(under5$upper_bound))
  aian <- out[out$variable == "Arrestee race" &
                out$category == "American Indian or Alaska Native", ]
  expect_equal(aian$estimate, 30)
  expect_equal(aian$lower_bound, -4.22302049047399)
})

test_that("offense takes a summary code or an indicator id", {
  seen <- capture_request()
  expect_error(get_nibrs_estimate("TX", 2022, offense = "rob"), "captured")
  expect_equal(seen$path, "nibrs-estimation/state/48/121")
  expect_error(get_nibrs_estimate(year = 2022, offense = 58), "captured")
  expect_equal(seen$path, "nibrs-estimation/national/58")
  expect_error(get_nibrs_estimate(year = 2022, offense = "58"), "captured")
  expect_equal(seen$path, "nibrs-estimation/national/58")
})

test_that("bad arguments are refused before any request", {
  local_mocked_bindings(
    cde_request = function(...) stop("no request expected"),
    .package = "fbiCDE"
  )
  expect_error(get_nibrs_estimate("XX", 2022), "Invalid state")
  expect_error(get_nibrs_estimate("CA", 2022, offense = "13A"),
               "must be one of")
  expect_error(get_nibrs_estimate("CA", 2022, offense = 99999),
               "must be one of")
  expect_error(get_nibrs_estimate("CA", "2022"), "single year")
  expect_error(get_nibrs_estimate("CA", 2021:2022), "single year")
  expect_error(get_nibrs_estimate("CA", 2022, table = "Bogus"))
})

test_that("get_nibrs_estimate returns a total for every supported offense live", {
  skip_if_no_fbi_api()
  for (code in names(.ESTIMATE_OFFENSES)) {
    out <- get_nibrs_estimate("TX", year = 2022, offense = code)
    total <- out$estimate[out$variable == "Offense Count - Total"]
    expect_length(total, 1L)
    expect_gt(total, 0)
  }
})

test_that("a year without estimates fails with the API's list of years live", {
  skip_if_no_fbi_api()
  expect_error(get_nibrs_estimate("CA", year = 2020), "2021")
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "nibrs_estimate")'`
Expected: FAIL, `could not find function "get_nibrs_estimate"`.

- [ ] **Step 3: Write the wrapper and parser**

Append to `R/nibrs_estimate.R`:

```r
.ESTIMATE_COLS <- c("geography", "year", "offense", "indicator", "table",
                    "variable", "category", "estimate", "lower_bound",
                    "upper_bound")

# "national" or "state/<the estimates' own state id>".
.estimate_level <- function(state_abb) {
  if (is.null(state_abb)) {
    return("national")
  }
  if (length(state_abb) != 1L || !is_valid_state(state_abb)) {
    stop("Invalid state abbreviation: ", paste(state_abb, collapse = ", "),
         call. = FALSE)
  }
  st <- fbiCDE::nibrs_estimation_states
  id <- st$id[st$state_abbr == toupper(state_abb)]
  if (length(id) != 1L) {
    stop("The FBI publishes no NIBRS estimate for ", toupper(state_abb), ".",
         call. = FALSE)
  }
  paste0("state/", id)
}

# One whole-number year. Membership in nibrs_estimation_years is not checked
# here: the API answers an unsupported year with HTTP 400 naming the years it
# has, which stays right when the FBI adds one and the bundled list is stale.
.check_estimate_year <- function(year) {
  if (!is.numeric(year) || length(year) != 1L || is.na(year) ||
      year != round(year)) {
    stop("'year' must be a single year, e.g. 2022.", call. = FALSE)
  }
  as.integer(year)
}

# A summary code from .ESTIMATE_OFFENSES, or an id in
# nibrs_estimation_indicators -> the indicator id.
.estimate_indicator <- function(offense) {
  ind <- fbiCDE::nibrs_estimation_indicators
  if (length(offense) == 1L && !is.na(offense)) {
    code <- toupper(as.character(offense))
    if (code %in% names(.ESTIMATE_OFFENSES)) {
      return(ind$id[ind$indicator == .ESTIMATE_OFFENSES[[code]]$indicator])
    }
    if (grepl("^[0-9]+$", code) && as.integer(code) %in% ind$id) {
      return(as.integer(code))
    }
  }
  stop("'offense' must be one of ",
       paste(names(.ESTIMATE_OFFENSES), collapse = ", "),
       ", or an id from nibrs_estimation_indicators.", call. = FALSE)
}

# Parse one nibrs-estimation response: [{"data": {<table>: {<variable>:
# [{<category>: n, "lower_bound": x, "upper_bound": y}, ...]}}}]. Bounds can
# be null (a 0 cell) or negative (small cells); both are kept as published.
parse_nibrs_estimate_response <- function(response, geography, year, offense,
                                          indicator, table) {
  tbl <- if (length(response) > 0) response[[1]]$data[[table]] else NULL
  rows <- lapply(names(tbl), function(variable) {
    do.call(rbind, lapply(tbl[[variable]], function(entry) {
      category <- setdiff(names(entry), c("lower_bound", "upper_bound"))
      if (length(category) != 1L) {
        stop("Unexpected NIBRS estimate entry under '", variable, "'.",
             call. = FALSE)
      }
      data.frame(
        variable = variable,
        category = category,
        estimate = as.numeric(entry[[category]] %||% NA),
        lower_bound = as.numeric(entry$lower_bound %||% NA),
        upper_bound = as.numeric(entry$upper_bound %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(variable = character(0), category = character(0),
                      estimate = numeric(0), lower_bound = numeric(0),
                      upper_bound = numeric(0), stringsAsFactors = FALSE)
  }
  n <- nrow(out)
  out <- cbind(
    data.frame(geography = rep(geography, n), year = rep(year, n),
               offense = rep(offense, n), indicator = rep(indicator, n),
               table = rep(table, n), stringsAsFactors = FALSE),
    out
  )
  out[, .ESTIMATE_COLS, drop = FALSE]
}

#' Get the FBI's published NIBRS estimates for a state or the nation
#'
#' Returns the FBI's *estimate*: its published figure for a state or the
#' nation, which adds a statistical model's prediction for everything outside
#' NIBRS to what agencies reported in NIBRS. Each value comes with its 95%
#' confidence interval. Compare it with crime *reported* to the FBI using
#' [imputation_gap()].
#'
#' The API publishes these estimates for the years in
#' [nibrs_estimation_years] (2021 and 2022), and only for states and the
#' nation: nothing for counties, cities or metro areas. A year it does not
#' cover fails with the API's own message listing the years it has. The
#' estimates come from `nibrs-estimation/`, the endpoint behind the CDE's
#' "NIBRS Estimates" page, which the CDE's public API documentation does not
#' list.
#'
#' @family NIBRS functions
#' @param state_abb String for state abbreviation. If `NULL` (default) returns
#'   national data.
#' @param year A single year, e.g. `2022`.
#' @param offense A summary offense code (`"V"`, the default, for violent
#'   crime; `"P"`, `"ROB"`, `"ASS"`, `"BUR"`, `"LAR"`, `"MVT"`, `"ARS"`,
#'   `"HOM"` or `"RPE"`) or an `id` from [nibrs_estimation_indicators].
#' @param table Which of the estimate's tables to return: `"Offense"` (the
#'   default), `"Victim"`, `"Incident"` or `"Arrest"`.
#'
#' @return A data.frame with one row per category: `geography`, `year`,
#'   `offense`, `indicator` (the estimates API's offense id), `table`,
#'   `variable` (e.g. `"Victim age"`), `category` (e.g. `"25-34"`),
#'   `estimate`, `lower_bound` and `upper_bound`. The offense total is the
#'   `"Offense"` table's row with `variable` `"Offense Count - Total"`.
#'   Bounds are as published: `NA` where the FBI gives none, and below zero
#'   for some small counts.
#' @export
#'
#' @examples
#' \dontrun{
#' ca <- get_nibrs_estimate("CA", year = 2021)
#' ca[ca$variable == "Offense Count - Total", ]
#' }
get_nibrs_estimate <- function(state_abb = NULL, year, offense = "V",
                               table = "Offense") {
  level <- .estimate_level(state_abb)
  year <- .check_estimate_year(year)
  indicator <- .estimate_indicator(offense)
  table <- match.arg(table, c("Offense", "Victim", "Incident", "Arrest"))

  response <- cde_request(cde_path("nibrs-estimation", level, indicator),
                          list(year = year))
  geography <- if (is.null(state_abb)) "US" else toupper(state_abb)
  parse_nibrs_estimate_response(response, geography, year,
                                toupper(as.character(offense)), indicator,
                                table)
}
```

In `pkgdown/_pkgdown.yml`, add a section after `Summarized Crime`:

```yaml
  - title: Published estimates
    desc: >
      The FBI's NIBRS estimates, and how much of them was modeled rather
      than reported
    contents:
      - get_nibrs_estimate
```

- [ ] **Step 4: Run the tests**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test(filter = "nibrs_estimate")'`
Expected: PASS.

Run: `FBI_CDE_LIVE=true Rscript -e 'devtools::test(filter = "nibrs_estimate")'`
Expected: PASS.

- [ ] **Step 5: NEWS**

Add under the development-version heading, after the rename section:

```markdown
## The FBI's published estimates

- `get_nibrs_estimate()` returns the FBI's NIBRS estimates for a state or
  the nation, with 95% confidence intervals, by offense table (offenses,
  victims, incidents, arrests). The API has them for 2021 and 2022 only.
  The new bundled tables `nibrs_estimation_states`,
  `nibrs_estimation_indicators` and `nibrs_estimation_years` hold the
  estimates' own state and offense IDs.
```

- [ ] **Step 6: Commit**

```bash
git add R/nibrs_estimate.R NAMESPACE man/ pkgdown/_pkgdown.yml NEWS.md \
  tests/testthat/test-nibrs_estimate.R
git commit -m "feat: get_nibrs_estimate() for the FBI's NIBRS estimates (#42)"
```

---

### Task 4: `imputation_gap()`

**Files:**
- Create: `R/imputation_gap.R`
- Modify: `pkgdown/_pkgdown.yml` (Published estimates section)
- Modify: `NEWS.md`
- Test: `tests/testthat/test-imputation_gap.R` (create)

**Interfaces:**
- Consumes: `get_nibrs_estimate()`, `.estimate_level()`,
  `.ESTIMATE_OFFENSES` (Tasks 2-3); `get_reported_crime()` (Task 1);
  `parse_summarized_response()` (`R/ucr_crime.R`), which parses
  `nibrs/...?type=counts` responses unchanged (same `offenses$actuals` and
  `populations` shape as `summarized`).
- Produces: `imputation_gap(state_abb = NULL, year, offense = "V")` →
  data.frame, one row per year, columns in this order: `geography`,
  `year`, `offense`, `reported`, `reported_nibrs`, `reported_other`,
  `coverage`, `coverage_nibrs`, `published`, `published_lower`,
  `published_upper`, `gap`, `gap_lower`, `gap_upper`, `share_estimated`,
  `share_lower`, `share_upper`.

Column definitions (these go in the roxygen `@return`):
- `reported`: sum of the year's monthly counts from `get_reported_crime()`
  (either format).
- `reported_nibrs`: sum over the offense's NIBRS codes of
  `nibrs/{level}/{code}?type=counts` (NIBRS submissions only).
- `reported_other`: `reported - reported_nibrs`: reports in the older
  summary format, plus the counting-rule residual. Can be negative.
- `coverage`: mean monthly `participated_population / population` from
  the `summarized` response (agencies reporting in either format).
- `coverage_nibrs`: the same, from the first NIBRS code's response
  (agencies reporting in NIBRS).
- `published`, `published_lower`, `published_upper`: the estimate's
  `"Offense Count - Total"` and its 95% bounds.
- `gap = published - reported_nibrs`; `gap_lower`, `gap_upper` the same
  with the bounds.
- `share_estimated = gap / published`; `share_lower = gap_lower /
  published_lower`; `share_upper = gap_upper / published_upper`; each `NA`
  where its denominator is `NA` or not positive.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-imputation_gap.R`:

```r
# Answer each request imputation_gap() makes for California with the matching
# recorded fixture, and record the order of the requests.
local_gap_fixtures <- function(env = parent.frame()) {
  seen <- new.env()
  seen$paths <- character(0)
  testthat::local_mocked_bindings(
    cde_request = function(path, query = list(), ...) {
      seen$paths <- c(seen$paths, path)
      year <- if (!is.null(query$year)) query$year else
        sub("^\\d{2}-", "", query$from)
      name <- if (path == "nibrs-estimation/state/6/133") {
        sprintf("nibrs-estimation-state-6-133-%s.json", year)
      } else if (path == "summarized/state/CA/V") {
        sprintf("summarized-state-CA-V-%s.json", year)
      } else if (startsWith(path, "nibrs/state/CA/")) {
        stopifnot(identical(query$type, "counts"))
        sprintf("nibrs-counts-state-CA-%s-%s.json", basename(path), year)
      } else {
        stop("unexpected request: ", path)
      }
      read_fixture(name)
    },
    .package = "fbiCDE", .env = env
  )
  seen
}

test_that("imputation_gap splits California 2022 by reporting format", {
  local_gap_fixtures()
  out <- imputation_gap("CA", year = 2022)
  expect_equal(names(out), c(
    "geography", "year", "offense", "reported", "reported_nibrs",
    "reported_other", "coverage", "coverage_nibrs", "published",
    "published_lower", "published_upper", "gap", "gap_lower", "gap_upper",
    "share_estimated", "share_lower", "share_upper"
  ))
  expect_equal(nrow(out), 1L)
  expect_equal(out$geography, "CA")
  expect_equal(out$year, 2022L)
  expect_equal(out$offense, "V")
  expect_equal(out$reported, 193824)
  expect_equal(out$reported_nibrs, 71625)
  expect_equal(out$reported_other, 122199)
  expect_equal(out$coverage, 0.987835, tolerance = 1e-6)
  expect_equal(out$coverage_nibrs, 0.465859, tolerance = 1e-6)
  expect_equal(out$published, 207381)
  expect_equal(out$gap, 135756)
  expect_equal(out$gap_lower, 183100.636344443 - 71625)
  expect_equal(out$gap_upper, 231662.98194076 - 71625)
  expect_equal(out$share_estimated, 0.6546212, tolerance = 1e-6)
  expect_equal(out$share_lower, 0.6088217, tolerance = 1e-6)
  expect_equal(out$share_upper, 0.6908224, tolerance = 1e-6)
})

test_that("a NIBRS-only year gives a negative reported_other, not zero", {
  local_gap_fixtures()
  out <- imputation_gap("CA", year = 2021)
  # In 2021 the FBI took NIBRS only; the two reported series differ by
  # counting rules alone.
  expect_equal(out$reported, 36439)
  expect_equal(out$reported_nibrs, 36943)
  expect_equal(out$reported_other, -504)
  expect_equal(out$coverage, out$coverage_nibrs, tolerance = 1e-6)
  expect_equal(out$coverage, 0.263218, tolerance = 1e-6)
  expect_equal(out$gap, 180596)
  expect_equal(out$share_estimated, 0.8301776, tolerance = 1e-6)
})

test_that("several years give one row each, estimate request first", {
  seen <- local_gap_fixtures()
  out <- imputation_gap("CA", year = c(2021, 2022))
  expect_equal(out$year, c(2021L, 2022L))
  expect_equal(out$published, c(217539, 207381))
  expect_equal(seen$paths[1], "nibrs-estimation/state/6/133")
  expect_equal(seen$paths[2], "summarized/state/CA/V")
  expect_equal(seen$paths[3:8],
               paste0("nibrs/state/CA/",
                      c("09A", "11A", "11B", "11C", "120", "13A")))
  expect_length(seen$paths, 16L)
})

test_that("a lower-case state gives the same result", {
  local_gap_fixtures()
  expect_equal(imputation_gap("ca", year = 2022),
               imputation_gap("CA", year = 2022))
})

test_that("a year without estimates fails before any other request", {
  seen <- new.env()
  seen$paths <- character(0)
  local_mocked_bindings(
    cde_request = function(path, ...) {
      seen$paths <- c(seen$paths, path)
      stop("HTTP 400 - Bad request, an invalid year was sent. NIBRS ",
           "Estimation currently only supports the years: 2021,2022")
    },
    .package = "fbiCDE"
  )
  expect_error(imputation_gap("CA", year = 2020), "2021,2022")
  expect_equal(seen$paths, "nibrs-estimation/state/6/133")
})

test_that("unsupported arguments are refused before any request", {
  local_mocked_bindings(
    cde_request = function(...) stop("no request expected"),
    .package = "fbiCDE"
  )
  expect_error(imputation_gap("CA", 2022, offense = "13A"), "must be one of")
  expect_error(imputation_gap("CA", 2022, offense = "SHR"), "must be one of")
  expect_error(imputation_gap("CA", 2022, offense = c("V", "P")),
               "must be one of")
  expect_error(imputation_gap("XX", 2022), "Invalid state")
  expect_error(imputation_gap("CA", "2022"), "years")
  expect_error(imputation_gap("CA", 2022.5), "years")
})

test_that("Texas 2022 is almost entirely reported in NIBRS live", {
  skip_if_no_fbi_api()
  out <- imputation_gap("TX", year = 2022)
  expect_gt(out$coverage_nibrs, 0.95)
  expect_lt(abs(out$reported_other) / out$reported, 0.02)
  expect_lt(abs(out$share_estimated), 0.02)
})

test_that("the national gap covers both estimate years live", {
  skip_if_no_fbi_api()
  out <- imputation_gap(year = 2021:2022)
  expect_equal(out$geography, c("US", "US"))
  expect_true(all(out$reported_nibrs > 9e5))
  expect_true(all(out$published > out$reported_nibrs))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "imputation_gap")'`
Expected: FAIL, `could not find function "imputation_gap"`.

- [ ] **Step 3: Write the implementation**

Create `R/imputation_gap.R`:

```r
# Total count and mean monthly coverage of a parsed series' offense rows. A
# geography with no rows (nothing reported) totals 0 with coverage NA.
.series_total <- function(x) {
  off <- x[x$measure %in% "offenses", , drop = FALSE]
  if (nrow(off) == 0) {
    return(list(count = 0, coverage = NA_real_))
  }
  coverage <- mean(off$participated_population / off$population,
                   na.rm = TRUE)
  list(count = sum(off$count, na.rm = TRUE),
       coverage = if (is.nan(coverage)) NA_real_ else coverage)
}

# One NIBRS offense code's monthly counts for a year, NIBRS submissions only:
# nibrs/{level}/{code}?type=counts answers in the summarized/ shape.
.nibrs_counts <- function(state_abb, code, year) {
  national <- is.null(state_abb)
  level <- if (national) "national" else paste0("state/", state_abb)
  response <- cde_request(
    cde_path("nibrs", level, code),
    cde_query(sprintf("01-%d", year), sprintf("12-%d", year),
              type = "counts")
  )
  parse_summarized_response(response,
                            geography = if (national) "US" else state_abb,
                            offense = code,
                            level = if (national) "national" else "state")
}

# A ratio of a gap to its published figure; NA where that is not positive.
.gap_share <- function(gap, published) {
  if (is.na(published) || published <= 0) NA_real_ else gap / published
}

.imputation_gap_row <- function(state_abb, year, offense) {
  # The estimate first: an unsupported year fails before the other requests.
  est <- get_nibrs_estimate(state_abb, year = year, offense = offense)
  total <- est[est$variable == "Offense Count - Total", , drop = FALSE]
  if (nrow(total) != 1L) {
    stop("The FBI's estimate for ", offense, " in ", year,
         " has no offense total.", call. = FALSE)
  }

  reported <- .series_total(get_reported_crime(
    state_abb, from = sprintf("01-%d", year), to = sprintf("12-%d", year),
    offense = offense
  ))
  nibrs <- lapply(.ESTIMATE_OFFENSES[[offense]]$nibrs, function(code) {
    .series_total(.nibrs_counts(state_abb, code, year))
  })
  reported_nibrs <- sum(vapply(nibrs, function(n) n$count, numeric(1)))

  published <- total$estimate
  lower <- total$lower_bound
  upper <- total$upper_bound
  data.frame(
    geography = if (is.null(state_abb)) "US" else state_abb,
    year = year,
    offense = offense,
    reported = reported$count,
    reported_nibrs = reported_nibrs,
    reported_other = reported$count - reported_nibrs,
    coverage = reported$coverage,
    coverage_nibrs = nibrs[[1]]$coverage,
    published = published,
    published_lower = lower,
    published_upper = upper,
    gap = published - reported_nibrs,
    gap_lower = lower - reported_nibrs,
    gap_upper = upper - reported_nibrs,
    share_estimated = .gap_share(published - reported_nibrs, published),
    share_lower = .gap_share(lower - reported_nibrs, lower),
    share_upper = .gap_share(upper - reported_nibrs, upper),
    stringsAsFactors = FALSE
  )
}

#' How much of the FBI's published crime figure was estimated
#'
#' Compares the FBI's *estimate* for a state or the nation with the crime
#' agencies *reported* to the FBI, and says how much of the estimate the
#' FBI's model supplied. The FBI builds its estimate from NIBRS reports only
#' and models everything else: agencies that did not report and, in 2022,
#' agencies that reported only in the older summary format. So the gap is
#' measured against `reported_nibrs`, and `reported_other` shows the
#' summary-format reports the model predicted rather than used.
#'
#' The API publishes estimates for 2021 and 2022 only (see
#' [nibrs_estimation_years]), and for states and the nation only. A year it
#' does not cover fails with the API's message listing the years it has.
#'
#' The NIBRS and summary-format counts differ slightly even where every
#' agency reported in NIBRS, because the two count offenses differently
#' (the summary format counts only the most serious offense in an incident).
#' Measured nationally for 2021, that difference is about 1% for violent
#' crime and larger for some offenses (about 5% for burglary, 7% for
#' homicide); `vignette("estimates")` gives the full table. A `gap` or
#' `reported_other` smaller than that difference, including a negative one,
#' means nothing was modeled.
#'
#' Each row takes one estimate request, one `summarized` request and one
#' `nibrs` request per NIBRS offense code that makes up `offense` (six for
#' violent crime).
#'
#' @param state_abb String for state abbreviation. If `NULL` (default)
#'   compares national figures.
#' @param year One or more years, e.g. `2021:2022`.
#' @param offense A summary offense code: `"V"` (violent crime, the default),
#'   `"P"`, `"ROB"`, `"ASS"`, `"BUR"`, `"LAR"`, `"MVT"`, `"ARS"`, `"HOM"` or
#'   `"RPE"`.
#'
#' @return A data.frame with one row per year:
#' \describe{
#'   \item{geography, year, offense}{What was compared}
#'   \item{reported}{Crime reported to the FBI in either format: the year's
#'     sum from [get_reported_crime()]}
#'   \item{reported_nibrs}{Crime reported in NIBRS only}
#'   \item{reported_other}{`reported - reported_nibrs`: summary-format
#'     reports, plus the counting difference. Can be negative}
#'   \item{coverage}{Mean monthly share of the population covered by
#'     agencies reporting in either format}
#'   \item{coverage_nibrs}{The same, for agencies reporting in NIBRS}
#'   \item{published, published_lower, published_upper}{The FBI's estimate
#'     and the bounds of its confidence interval}
#'   \item{gap, gap_lower, gap_upper}{`published - reported_nibrs`: what
#'     the FBI's model supplied, with bounds from the estimate's interval}
#'   \item{share_estimated, share_lower, share_upper}{`gap / published`: the
#'     share of the published figure the model supplied, with bounds}
#' }
#' @family NIBRS functions
#' @export
#'
#' @examples
#' \dontrun{
#' # 2021, when the FBI accepted NIBRS reports only
#' imputation_gap("CA", year = 2021:2022)
#' imputation_gap(year = 2021:2022)
#' }
imputation_gap <- function(state_abb = NULL, year, offense = "V") {
  if (!is.character(offense) || length(offense) != 1L || is.na(offense) ||
      !toupper(offense) %in% names(.ESTIMATE_OFFENSES)) {
    stop("'offense' must be one of: ",
         paste(names(.ESTIMATE_OFFENSES), collapse = ", "), ".",
         call. = FALSE)
  }
  offense <- toupper(offense)
  if (!is.null(state_abb)) {
    .estimate_level(state_abb)  # validates, and checks an estimate exists
    state_abb <- toupper(state_abb)
  }
  if (!is.numeric(year) || length(year) == 0L || anyNA(year) ||
      any(year != round(year))) {
    stop("'year' must be one or more years, e.g. 2021:2022.", call. = FALSE)
  }

  rows <- lapply(unique(as.integer(year)), function(y) {
    .imputation_gap_row(state_abb, y, offense)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
```

Add `imputation_gap` to the `Published estimates` section of
`pkgdown/_pkgdown.yml`, after `get_nibrs_estimate`.

- [ ] **Step 4: Run the tests**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test(filter = "imputation_gap")'`
Expected: PASS.

Run: `FBI_CDE_LIVE=true Rscript -e 'devtools::test(filter = "imputation_gap")'`
Expected: PASS.

- [ ] **Step 5: NEWS**

Append to the `## The FBI's published estimates` section:

```markdown
- `imputation_gap()` shows how much of the FBI's published state or
  national figure its model supplied. It splits reported crime by format,
  because the FBI's estimate is built from NIBRS reports only: in 2022 it
  models agencies that reported in the older summary format as well as
  agencies that did not report. For California, the model supplied 83% of
  the 2021 violent-crime estimate and 65% of 2022's.
```

- [ ] **Step 6: Commit**

```bash
git add R/imputation_gap.R NAMESPACE man/ pkgdown/_pkgdown.yml NEWS.md \
  tests/testthat/test-imputation_gap.R
git commit -m "feat: imputation_gap(), published estimate against reported crime (#42)"
```

---

### Task 5: Define the terms in the user-facing documentation

**Files:**
- Create: `vignettes/estimates.Rmd.orig`, then knit `vignettes/estimates.Rmd`
- Modify: `README.Rmd` (new section), regenerate `README.md`
- Modify: roxygen in `R/ucr_crime.R` (`get_reported_crime`),
  `R/imputation.R` (`impute_reporting_gaps`), `R/county_crime.R`
  (`get_county_crime`), `R/nibrs_estimate.R`, `R/imputation_gap.R`:
  add `@seealso` lines
- Modify: `vignettes/fbi.Rmd.orig` / `vignettes/fbi.Rmd`,
  `vignettes/geography.Rmd.orig` / `vignettes/geography.Rmd` where wording
  conflicts with the definitions
- Modify: `CLAUDE.md` (roadmap entry), `NEWS.md`

**Interfaces:**
- Consumes: everything from Tasks 1-4.
- Produces: `vignette("estimates")`, referenced by the `imputation_gap()`
  docs from Task 4.

- [ ] **Step 1: Write the vignette source**

Create `vignettes/estimates.Rmd.orig` with exactly this content (the
numbers in the prose are computed inline; do not hard-code them):

````markdown
---
title: "Reported, estimated and imputed crime"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Reported, estimated and imputed crime}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
```

```{r setup}
library(fbiCDE)
```

Three words describe where a crime count comes from, and this package uses
each with one meaning.

- **Reported**: the sum of what agencies submitted to the FBI, in either
  format (NIBRS, or the older summary format). It covers only the agencies
  that reported. `get_reported_crime()` returns it.
- **Estimated**: the FBI's published figure. It adds a statistical model's
  prediction for everything outside NIBRS: agencies that did not report
  and, in 2022, agencies that reported only in the older summary format. It
  comes with a 95% confidence interval. `get_nibrs_estimate()` returns it.
- **Imputed**: a value filled in for a missing report. The FBI's estimate
  imputes, and so does `impute_reporting_gaps()`, which fills an agency's
  missing months from its own neighboring months. This package always says
  which one it means.

## Where the FBI publishes estimates

The API publishes the FBI's NIBRS estimates for 2021 and 2022, for each
state and the nation. It publishes none for counties, cities or metro
areas. The FBI's older summary-format estimates, 1979 onward, are a bulk
download that this package does not read.

## How much of the published figure was modeled

`imputation_gap()` compares the estimate with reported crime. The FBI
builds the estimate from NIBRS reports alone, so the gap is measured
against crime reported in NIBRS.

```{r gap}
ca <- imputation_gap("CA", year = 2021:2022)
ca[, c("year", "reported", "reported_nibrs", "reported_other",
       "coverage", "coverage_nibrs", "published", "share_estimated")]
```

In 2021 the FBI accepted NIBRS reports only, and most California agencies
had not switched to NIBRS. Agencies reporting in NIBRS covered
`r round(100 * ca$coverage_nibrs[1])`% of the state's population, and the
FBI's model supplied `r round(100 * ca$share_estimated[1])`% of its
published figure of `r format(ca$published[1], big.mark = ",")` violent
crimes.

In 2022 the FBI accepted both formats. Agencies reporting in either format
covered `r round(100 * ca$coverage[2])`% of the population, but those
reporting in NIBRS covered `r round(100 * ca$coverage_nibrs[2])`%. The
FBI's model treated the rest as missing, including the
`r format(ca$reported_other[2], big.mark = ",")` violent crimes reported in
the older format, and supplied `r round(100 * ca$share_estimated[2])`% of
the published figure.

Those older-format reports are a check on the model. The model supplied
`r format(ca$gap[2], big.mark = ",")` violent crimes for the agencies
outside NIBRS, which reported
`r format(ca$reported_other[2], big.mark = ",")` themselves.

California's own count gives a second check for 2021. The California
Department of Justice collected 2021 reports in both formats and published
183,546 violent crimes ([Crime in California 2021, Table
3](https://data-openjustice.doj.ca.gov/sites/default/files/2022-08/Crime%20In%20CA%202021_0.pdf)).
That is below the lower bound of the FBI's 95% interval,
`r format(round(ca$published_lower[1]), big.mark = ",")`.

```{r national}
imputation_gap(year = 2021:2022)[, c("year", "coverage_nibrs",
                                     "published", "share_estimated",
                                     "share_lower", "share_upper")]
```

## The counting difference

The two formats count offenses differently: the summary format counts only
the most serious offense in an incident. In 2021 every report reaching the
FBI was NIBRS, so the national difference between the two reported series
is the counting difference alone.

```{r residual}
codes <- c("V", "P", "ROB", "ASS", "BUR", "LAR", "MVT", "ARS", "HOM", "RPE")
residual <- do.call(rbind, lapply(codes, function(code) {
  g <- imputation_gap(year = 2021, offense = code)
  data.frame(offense = code, reported = g$reported,
             reported_nibrs = g$reported_nibrs,
             difference = round(100 * g$reported_other / g$reported_nibrs, 1))
}))
residual
```

A `gap` or `reported_other` within this difference, including a negative
one, means the model supplied nothing beyond counting rules.
````

Then knit only this vignette (it makes about 90 live requests; do not
re-run `data-raw/precompute_vignettes.R`, which re-knits every vignette):

Run: `Rscript -e 'pkgload::load_all(quiet = TRUE); setwd("vignettes"); knitr::knit("estimates.Rmd.orig", output = "estimates.Rmd")'`
Expected: `vignettes/estimates.Rmd` with the outputs baked in. Read it:
California 2021 `share_estimated` about 0.83, 2022 about 0.65; the
residual table's `difference` within about 2.5 for most offenses, about
5.4 for `BUR` and 7.5 for `HOM` (spec §3.3 and the plan's measurements of
2026-10-15). If a number in the knitted prose contradicts the sentence
around it (for example the 2021 lower bound is no longer above 183,546),
stop and report it rather than rewording to fit.

- [ ] **Step 2: README section**

In `README.Rmd`, add after the `## Quick Start` section's code block:

```markdown
## Reported, estimated and imputed

- **Reported**: the sum of what agencies submitted to the FBI, in either
  format (NIBRS, or the older summary format). `get_reported_crime()`.
- **Estimated**: the FBI's published figure, which adds a model's
  prediction for everything outside NIBRS, with a 95% confidence interval.
  `get_nibrs_estimate()`, for 2021 and 2022.
- **Imputed**: a value filled in for a missing report, by the FBI's model
  or by `impute_reporting_gaps()`.

`imputation_gap()` shows how much of the FBI's published figure its model
supplied. See `vignette("estimates")`.
```

and in the function table near line 106 add rows:

```markdown
| `get_nibrs_estimate()` | Get the FBI's published NIBRS estimates, 2021-2022 |
| `imputation_gap()` | How much of the FBI's published figure was estimated |
```

with an example in the following code block:

```r
# How much of California's published violent crime was modeled
imputation_gap("CA", year = 2021:2022)
```

Regenerate: `Rscript -e 'rmarkdown::render("README.Rmd")'`; delete any
`README.html`; check `git diff README.md`.

- [ ] **Step 3: Cross-reference the reference pages**

Add `#' @seealso vignette("estimates") for reported, estimated and imputed
crime.` to the roxygen of `get_reported_crime()` (`R/ucr_crime.R`),
`impute_reporting_gaps()` (`R/imputation.R`), `get_county_crime()`
(`R/county_crime.R`), `get_nibrs_estimate()` and `imputation_gap()`.

In `impute_reporting_gaps()`'s description, make sure it says it *imputes*
an agency's missing months and that this is not the FBI's estimate; add
one sentence if it does not.

- [ ] **Step 4: Sweep the other vignettes and README for loose use of the terms**

Run: `grep -n -i "estimat\|imput" vignettes/fbi.Rmd.orig vignettes/geography.Rmd.orig vignettes/juvenile-arrests.Rmd.orig README.Rmd`
For each hit, check it against the three definitions in Global
Constraints. Fix wording that calls a reported sum an estimate, or that
says "imputed" without saying by whom. Make each edit identically in the
`.Rmd.orig` and the shipped `.Rmd` (text only; do not re-knit those
vignettes).

- [ ] **Step 5: CLAUDE.md and NEWS**

In `CLAUDE.md`'s Roadmap, replace the `**Capstone (#42)**` bullet's
"**Design decided 2026-10-15, not yet planned:**" with "**shipped.**" and
add after its spec path: "`get_nibrs_estimate()` and `imputation_gap()`;
plan `specs/plans/2026-10-15-imputation-gap.md`."

Add to NEWS.md's `## The FBI's published estimates` section:

```markdown
- `vignette("estimates")` defines *reported*, *estimated* and *imputed*,
  which the package's documentation now uses consistently, and shows the
  California and national comparisons.
```

- [ ] **Step 6: Run the checks and commit**

Run: `Rscript -e 'devtools::document()' && Rscript -e 'devtools::test()'`
Expected: 0 failures (including `test-ascii-docs.R`).

```bash
git add vignettes/estimates.Rmd.orig vignettes/estimates.Rmd README.Rmd \
  README.md R/ man/ vignettes/ CLAUDE.md NEWS.md
git commit -m "docs: define reported, estimated and imputed; estimates vignette (#42)"
```

---

### Task 6: Full verification

**Files:** none new.

- [ ] **Step 1: Offline suite and package check**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 failures.

Run: `R CMD build . && R CMD check --no-manual fbiCDE_*.tar.gz`
Expected: 0 errors, 0 warnings. Notes only for environment reasons
(missing optional packages, no time server); list any other note.

- [ ] **Step 2: Live suite**

Run: `FBI_CDE_LIVE=true Rscript -e 'devtools::test()'`
Expected: 0 failures, 0 skips beyond those that need optional packages.

- [ ] **Step 3: pkgdown index**

Run: `Rscript -e 'pkgdown::check_pkgdown()'`
Expected: "No problems found." Every exported topic not marked internal is
in `pkgdown/_pkgdown.yml`.

- [ ] **Step 4: Clean up and report**

Delete the `fbiCDE_*.tar.gz` and `fbiCDE.Rcheck/` the check left. Report:
test counts (offline and live), the check result, and the knitted
vignette's California and national numbers, so the reviewer can compare
them with spec §3.3.
