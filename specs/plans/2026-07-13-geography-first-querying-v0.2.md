# Geography-first querying (v0.2a + v0.2) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add county-level agency membership classification and an itemized,
unsummed county-crime detail view to the `fbi` package, on a `data.table`-free
base — the foundation for later aggregation, imputation, and Census-join phases.

**Architecture:** Three layers built bottom-up. Layer 0 is a pure membership
resolver (`county_agencies()`) that classifies bundled agencies into
`agency_class` with a conservative `default_member` flag. Layer 1
(`get_county_crime_detail()`) fans out one live request per member ORI, parses
each into per-period rows carrying count, population, participated-population,
rate, and a `reported` flag, and stacks them with **no summation**. A prep phase
first removes the `data.table` dependency and fixes an ORI-validation bug that
currently makes `get_agency_crime()` throw for ~10% of agencies.

**Tech Stack:** R (base only for new code), `testthat` 3e (offline fixtures +
`skip_if_no_fbi_api()`-guarded live tests), `roxygen2` for docs. Network goes
through the single `cde_request()` seam, mocked in tests.

## Global Constraints

- **R >= 3.5.0**; targeting CRAN. Keep `R CMD check` clean.
- **No new `Imports`.** New code is base R only. This plan *removes* `data.table`.
- **Single network seam:** all HTTP goes through `cde_request()` (`R/http.R`).
  Mock it in tests; never hit the network in offline tests.
- **Every data path gets both** an offline test (runs on CI) **and** a
  `skip_if_no_fbi_api()`-guarded live test.
- **Parse defensively:** the CDE schema drifts and agency/state responses include
  comparison rows (state + national series with `NA` counts). Strip them.
- **Bundled data:** `fbi_api_agencies` — `county_name` is stored **UPPERCASE**;
  a county sheriff's `agency_type_name` is `County`; ORIs are 9 chars, positions
  3–9 may be **letters** (state/tribal/campus/contract-city ORIs).
- **Commit style:** conventional commits (`feat:`, `fix:`, `refactor:`,
  `test:`, `docs:`). Attribution disabled globally.
- **Do not run `git commit`/`push` until the user has approved execution.** Work
  on branch `design/geography-first`.

---

### Task 0: Branch and commit the design + plan

**Files:**
- Modify: (none — git only)

- [ ] **Step 1: Create the working branch (we are on `master`)**

Run:
```bash
git checkout -b design/geography-first
```

- [ ] **Step 2: Commit the design doc and this plan**

```bash
git add specs/2026-07-13-geography-first-querying-design.md \
        specs/plans/2026-07-13-geography-first-querying-v0.2.md
git commit -m "docs: geography-first querying design + v0.2 implementation plan"
```

Expected: one commit on `design/geography-first`.

---

## Part A — v0.2a prep PR (foundation cleanups)

### Task 1: Relax `is_valid_ori()` to accept letter-bearing ORIs

**Why:** the regex `^[A-Z]{2}[0-9]{7}$` rejects 1,805 of 18,459 bundled ORIs
(all state police, all tribal, most university/college, and letter-bearing city
ORIs like `CA001300X`). `is_valid_ori()` gates `get_agency_crime()`, so those
agencies currently error — a hard blocker for the Layer-1 fan-out.

**Files:**
- Modify: `R/http.R` (function `is_valid_ori`, ~line 205–230; roxygen above it)
- Modify: `R/ucr_crime.R:63-66` (error message text)
- Test: `tests/testthat/test-utils.R` (existing `is_valid_ori` test, ~line 25)

**Interfaces:**
- Produces: `is_valid_ori(ori)` — accepts a 9-char ORI: 2 leading letters + 7
  alphanumerics. Same vectorized logical return as before.

- [ ] **Step 1: Extend the failing test in `tests/testthat/test-utils.R`**

Replace the existing `is_valid_ori` test block with:

```r
test_that("is_valid_ori checks ORI format (2 letters + 7 alphanumerics)", {
  expect_true(is_valid_ori("CA0010900"))
  expect_true(is_valid_ori("NY1234567"))
  # Letter-bearing ORIs are valid: contract cities, state, tribal, campus.
  expect_true(is_valid_ori("CA001300X"))   # Dublin PD (Alameda)
  expect_true(is_valid_ori("CA0191H0X"))   # West Hollywood PD (LASD contract)
  expect_true(is_valid_ori("ARASP0000"))   # Arkansas State Police
  expect_false(is_valid_ori("not-an-ori"))
  expect_false(is_valid_ori("ABC123"))     # too short
  expect_false(is_valid_ori("A0010900"))   # only 1 leading letter
  expect_false(is_valid_ori("CA010900"))   # 8 chars
  expect_false(is_valid_ori("CA00109!0"))  # illegal char
  # Case-insensitive.
  expect_true(is_valid_ori("ca0010900"))
  expect_true(is_valid_ori("ca001300x"))
  # Vectorized.
  expect_equal(is_valid_ori(c("CA0010900", "not-an-ori")), c(TRUE, FALSE))
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-utils.R')"`
Expected: FAIL — `is_valid_ori("CA001300X")` returns `FALSE` (old regex requires digits).

- [ ] **Step 3: Update the regex in `R/http.R`**

Change the body of `is_valid_ori()`:

```r
is_valid_ori <- function(ori) {
  grepl("^[A-Z]{2}[A-Z0-9]{7}$", toupper(ori))
}
```

Update its roxygen: change the description/`@details` wording from
"2 letters + 7 digits" to "9 characters: 2 letters followed by 7 alphanumerics
(letters allowed for state, tribal, campus, and some city ORIs)". Keep the
`@examples` lines, and add `#' is_valid_ori("CA001300X")`.

- [ ] **Step 4: Update the error message in `R/ucr_crime.R:63-66`**

```r
    stop(
      "Invalid ORI code: ", ori,
      ". Must be 9 characters: 2 letters followed by 7 alphanumerics",
      " (e.g., CA0010900 or CA001300X)",
      call. = FALSE
    )
```

- [ ] **Step 5: Run the full suite to confirm no regressions**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS (all files). Any test asserting a specific old error string for a
letter ORI would surface here; none exists.

- [ ] **Step 6: Commit**

```bash
git add R/http.R R/ucr_crime.R tests/testthat/test-utils.R
git commit -m "fix: is_valid_ori accepts letter-bearing ORIs (state/tribal/campus/contract)"
```

---

### Task 2: Add a base-R `rbind_fill()` helper

**Why:** the `data.table` removal (Task 3) needs a base replacement for
`rbindlist(..., fill = TRUE)` — stacking data.frames with differing columns.

**Files:**
- Modify: `R/utils.R` (add function near the top, after any existing helpers)
- Test: `tests/testthat/test-utils.R`

**Interfaces:**
- Produces: `rbind_fill(dfs)` — takes a list of data.frames (may contain `NULL`
  entries), returns one data.frame whose columns are the union of all inputs'
  columns, missing cells filled with `NA`, rows in input order. Empty/`NULL`-only
  input returns `data.frame()`.

- [ ] **Step 1: Write the failing test**

```r
test_that("rbind_fill unions columns and fills missing with NA", {
  a <- data.frame(x = 1L, y = "a", stringsAsFactors = FALSE)
  b <- data.frame(x = 2L, z = TRUE, stringsAsFactors = FALSE)
  out <- rbind_fill(list(a, NULL, b))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_setequal(names(out), c("x", "y", "z"))
  expect_equal(out$x, c(1L, 2L))
  expect_true(is.na(out$z[1]))   # a had no z
  expect_true(is.na(out$y[2]))   # b had no y
})

test_that("rbind_fill returns empty frame for empty or all-NULL input", {
  expect_equal(nrow(rbind_fill(list())), 0L)
  expect_equal(nrow(rbind_fill(list(NULL, NULL))), 0L)
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-utils.R')"`
Expected: FAIL — `could not find function "rbind_fill"`.

- [ ] **Step 3: Implement `rbind_fill()` in `R/utils.R`**

```r
# Stack a list of data.frames with differing columns (base-R replacement for
# data.table::rbindlist(fill = TRUE)). NULL entries are dropped; the union of
# all columns is used, with missing cells filled NA and rows kept in order.
rbind_fill <- function(dfs) {
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0) {
    return(data.frame())
  }
  all_cols <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(df) {
    for (col in setdiff(all_cols, names(df))) {
      df[[col]] <- NA
    }
    df[all_cols]
  })
  do.call(rbind, dfs)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-utils.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/utils.R tests/testthat/test-utils.R
git commit -m "feat: add base-R rbind_fill helper (data.table rbindlist replacement)"
```

---

### Task 3: Remove the `data.table` dependency

**Why:** no object exceeds a few thousand rows and there are no complex grouped
joins; the only non-trivial usage (`melt`/`dcast`) is dead code.

**Files:**
- Modify: `R/lookups.R:31-39` and `R/lookups.R:103`
- Modify: `R/ucr_participation.R:50`, `:53`, `:194`
- Modify: `R/leoka.R:87`
- Modify: `R/utils.R` (delete `srs_long_to_wide()` and `make_url()`)
- Modify: `DESCRIPTION` (remove `data.table,` from `Imports`)
- Test: existing suite must stay green (no new test file)

**Interfaces:**
- Consumes: `rbind_fill()` from Task 2.

- [ ] **Step 1: Confirm the dead helpers are unreferenced**

Run:
```bash
grep -rn "srs_long_to_wide\|make_url" R/ tests/
```
Expected: only their definitions in `R/utils.R`, no call sites. If any call site
appears, STOP and reassess.

- [ ] **Step 2: Replace `rbindlist(fill = TRUE)` in `R/lookups.R:31-38`**

Replace the `combined <- data.table::rbindlist(lapply(...), fill = TRUE)` block
and the following `setorder` line with:

```r
  # Combine all agency lists into a single data.frame
  combined <- rbind_fill(lapply(all_agencies, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  combined <- combined[order(combined$ori), , drop = FALSE]
  combined[] <- lapply(combined, as.character)
  rownames(combined) <- NULL
  combined
```

- [ ] **Step 3: Replace `rbindlist(rows)` in `R/lookups.R:103`**

```r
  result <- do.call(rbind, rows)
  as.data.frame(result)
```

- [ ] **Step 4: Replace the three `rbindlist` calls in `R/ucr_participation.R`**

`:50` (same columns):
```r
    do.call(rbind, agency_rows)
```
`:53` (ragged columns → fill):
```r
  result <- rbind_fill(rows)
  as.data.frame(result)
```
`:194` (same columns):
```r
  combined <- do.call(rbind, state_results)
```

- [ ] **Step 5: Replace `rbindlist(rows)` in `R/leoka.R:87`**

```r
  result <- do.call(rbind, rows)
  as.data.frame(result)
```

- [ ] **Step 6: Delete dead helpers in `R/utils.R`**

Delete the entire `make_url <- function(...) { ... }` and
`srs_long_to_wide <- function(.data) { ... }` definitions (the only users of
`data.table::melt`/`dcast`).

- [ ] **Step 7: Remove `data.table` from `DESCRIPTION`**

Delete the `    data.table,` line under `Imports:` (keep `httr`, `jsonlite`,
`datasets`, `utils`).

- [ ] **Step 8: Confirm no `data.table` references remain**

Run:
```bash
grep -rn "data.table" R/ DESCRIPTION NAMESPACE
```
Expected: no matches.

- [ ] **Step 9: Run the full suite and R CMD check**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_dir('tests/testthat')"`
Expected: PASS (all files).
Run: `Rscript -e "devtools::check(document = TRUE, args = '--no-manual')"`
Expected: 0 errors, 0 warnings (a NOTE about the removed import is acceptable if
it appears; there should be none for a cleanly dropped `Imports` entry).

- [ ] **Step 10: Commit**

```bash
git add R/lookups.R R/ucr_participation.R R/leoka.R R/utils.R DESCRIPTION
git commit -m "refactor: remove data.table dependency; use base R (delete dead reshape helpers)"
```

---

## Part B — v0.2 resolver + detail

### Task 4: `classify_agency()` — map `agency_type_name` to `agency_class`

**Files:**
- Create: `R/agency_class.R`
- Test: `tests/testthat/test-geography.R` (new)

**Interfaces:**
- Produces: `classify_agency(agency_type_name)` — vectorized; maps each raw
  `agency_type_name` string to one of `"county_primary"`, `"municipal"`,
  `"campus"`, `"state"`, `"tribal"`, `"special"`. Unknown/`NA` → `"special"`
  (conservative: excluded by default).
- Produces: the constant `DEFAULT_MEMBER_CLASSES <- c("county_primary", "municipal")`.

- [ ] **Step 1: Write the failing test in `tests/testthat/test-geography.R`**

```r
test_that("classify_agency maps raw agency types to agency_class", {
  expect_equal(classify_agency("County"), "county_primary")
  expect_equal(classify_agency("Parish"), "county_primary")
  expect_equal(classify_agency("City"), "municipal")
  expect_equal(classify_agency("Municipality"), "municipal")
  expect_equal(classify_agency("Borough"), "municipal")
  expect_equal(classify_agency("City and Borough"), "municipal")
  expect_equal(classify_agency("University or College"), "campus")
  expect_equal(classify_agency("State Police"), "state")
  expect_equal(classify_agency("Tribal"), "tribal")
  expect_equal(classify_agency("Other"), "special")
  expect_equal(classify_agency("Other State Agency"), "special")
  expect_equal(classify_agency("Census Area"), "special")
  # Unknown and NA are conservative (excluded by default).
  expect_equal(classify_agency("Something New"), "special")
  expect_equal(classify_agency(NA_character_), "special")
  # Vectorized.
  expect_equal(classify_agency(c("City", "County", "Tribal")),
               c("municipal", "county_primary", "tribal"))
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-geography.R')"`
Expected: FAIL — `could not find function "classify_agency"`.

- [ ] **Step 3: Implement `R/agency_class.R`**

```r
# Coarse, filterable grouping of the CDE's raw `agency_type_name` values.
# This is where the membership model's opinion is *declared* (see the design
# doc). Unknown or missing types fall through to "special" so they are excluded
# from the conservative default set rather than silently counted.

DEFAULT_MEMBER_CLASSES <- c("county_primary", "municipal")

.AGENCY_CLASS_MAP <- c(
  "County"                = "county_primary",
  "Parish"                = "county_primary",
  "City"                  = "municipal",
  "Municipality"          = "municipal",
  "Borough"               = "municipal",
  "City and Borough"      = "municipal",
  "University or College" = "campus",
  "State Police"          = "state",
  "Tribal"                = "tribal",
  "Other"                 = "special",
  "Other State Agency"    = "special",
  "Census Area"           = "special"
)

#' Classify an agency type into a membership class
#'
#' Maps the CDE's raw `agency_type_name` to a coarse, filterable `agency_class`
#' used by the geography functions. Unknown or missing values map to `"special"`.
#'
#' @param agency_type_name Character vector of raw `agency_type_name` values.
#' @return Character vector of `agency_class` values: one of `"county_primary"`,
#'   `"municipal"`, `"campus"`, `"state"`, `"tribal"`, `"special"`.
#' @keywords internal
classify_agency <- function(agency_type_name) {
  out <- unname(.AGENCY_CLASS_MAP[as.character(agency_type_name)])
  out[is.na(out)] <- "special"
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-geography.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/agency_class.R tests/testthat/test-geography.R
git commit -m "feat: classify_agency maps agency_type_name to agency_class"
```

---

### Task 5: `county_agencies()` — the membership resolver (Layer 0)

**Files:**
- Create: `R/geography.R`
- Test: `tests/testthat/test-geography.R`

**Interfaces:**
- Consumes: `classify_agency()`, `DEFAULT_MEMBER_CLASSES` (Task 4);
  `is_valid_state()` (`R/http.R`); bundled `fbi_api_agencies`.
- Produces: `county_agencies(county, state)` — returns a data.frame with columns
  `ori, agency_name, agency_type_name, agency_class, default_member, county_name,
  state_abbr, latitude, longitude`, one row per agency whose (case-folded)
  `county_name` + `state_abbr` match. `default_member` is logical.

- [ ] **Step 1: Write the failing test in `tests/testthat/test-geography.R`**

```r
test_that("county_agencies resolves and classifies a county (Alameda, CA)", {
  out <- county_agencies("Alameda", "CA")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 21L)
  expect_true(all(c("agency_class", "default_member") %in% names(out)))

  cls <- setNames(out$agency_class, out$ori)
  expect_equal(cls[["CA0010000"]], "county_primary")  # Alameda County Sheriff
  expect_equal(cls[["CA0010900"]], "municipal")        # Oakland PD
  expect_equal(cls[["CA001300X"]], "municipal")        # Dublin PD (letter ORI)
  expect_equal(cls[["CA0012100"]], "special")          # BART
  expect_equal(cls[["CA0019900"]], "state")            # Highway Patrol
  expect_equal(cls[["CA0019700"]], "campus")           # UC Berkeley

  # Default membership = county_primary + municipal = 14 City + 1 County.
  expect_equal(sum(out$default_member), 15L)
  dm <- setNames(out$default_member, out$ori)
  expect_true(dm[["CA0010900"]])
  expect_false(dm[["CA0012100"]])
})

test_that("county_agencies is case-insensitive on county and state", {
  a <- county_agencies("Alameda", "CA")
  b <- county_agencies("alameda", "ca")
  expect_equal(nrow(a), nrow(b))
  expect_setequal(a$ori, b$ori)
})

test_that("county_agencies validates state and warns on unknown county", {
  expect_error(county_agencies("Alameda", "ZZ"), "Invalid state")
  expect_warning(res <- county_agencies("Nowhere", "CA"), "No agencies")
  expect_equal(nrow(res), 0L)
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-geography.R')"`
Expected: FAIL — `could not find function "county_agencies"`.

- [ ] **Step 3: Implement `R/geography.R`**

```r
# Layer 0 of the geography model: the membership resolver. Pure, no network.
# Reads the bundled agency table and returns the classified candidate set for a
# county. All opinion is *declared* here (agency_class, default_member); none is
# applied (no filtering, summation, or fetching).

# `fbi_api_agencies` is a lazy-loaded package dataset; declare it to satisfy
# R CMD check's global-variable analysis.
utils::globalVariables("fbi_api_agencies")

# Internal accessor so the source of the agency table is swappable in tests.
agencies_table <- function() {
  fbi_api_agencies
}

.COUNTY_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "latitude", "longitude"
)

#' List the law-enforcement agencies attributed to a county
#'
#' Returns every agency in the bundled agency table whose county and state match,
#' classified by `agency_class` and flagged with a conservative `default_member`
#' indicator. This is an *attribution* model, not a spatial one: it reports the
#' agencies attributed to a county, not crime known to have occurred there.
#'
#' @param county County name (case-insensitive; e.g. `"Alameda"`).
#' @param state Two-letter state abbreviation (e.g. `"CA"`).
#' @return A data.frame with columns `ori`, `agency_name`, `agency_type_name`,
#'   `agency_class`, `default_member`, `county_name`, `state_abbr`, `latitude`,
#'   `longitude`. `default_member` is `TRUE` for `county_primary` and `municipal`
#'   agencies. Returns a zero-row frame (with a warning) if no agencies match.
#' @export
#' @examples
#' \dontrun{
#' county_agencies("Alameda", "CA")
#' }
county_agencies <- function(county, state) {
  if (!is_valid_state(state)) {
    stop("Invalid state abbreviation: ", state, call. = FALSE)
  }

  ag <- agencies_table()
  county_key <- toupper(trimws(county))
  state_key <- toupper(trimws(state))

  keep <- toupper(trimws(ag$county_name)) == county_key &
    toupper(trimws(ag$state_abbr)) == state_key
  sel <- ag[keep, , drop = FALSE]

  if (nrow(sel) == 0) {
    warning("No agencies match county '", county, "' in state '", state, "'",
            call. = FALSE)
    empty <- as.data.frame(
      matrix(nrow = 0, ncol = length(.COUNTY_AGENCY_COLS),
             dimnames = list(NULL, .COUNTY_AGENCY_COLS)),
      stringsAsFactors = FALSE
    )
    empty$default_member <- logical(0)
    return(empty)
  }

  sel$agency_class <- classify_agency(sel$agency_type_name)
  sel$default_member <- sel$agency_class %in% DEFAULT_MEMBER_CLASSES

  out <- sel[, .COUNTY_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-geography.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/geography.R tests/testthat/test-geography.R
git commit -m "feat: county_agencies membership resolver (Layer 0)"
```

---

### Task 6: `enumerate_periods()` — MM-YYYY period grid

**Why:** the detail parser needs the full requested period range to distinguish
"reported 0" from "did not report" (a missing period → `reported = FALSE`).

**Files:**
- Create: `R/county_crime.R` (this helper starts the file)
- Test: `tests/testthat/test-county_crime.R` (new)

**Interfaces:**
- Produces: `enumerate_periods(from, to)` — takes two `"MM-YYYY"` strings,
  returns the inclusive character vector of every `"MM-YYYY"` month between them
  (chronological order).

- [ ] **Step 1: Write the failing test in `tests/testthat/test-county_crime.R`**

```r
test_that("enumerate_periods lists inclusive MM-YYYY months", {
  expect_equal(enumerate_periods("01-2019", "03-2019"),
               c("01-2019", "02-2019", "03-2019"))
  expect_equal(enumerate_periods("11-2020", "02-2021"),
               c("11-2020", "12-2020", "01-2021", "02-2021"))
  expect_equal(enumerate_periods("05-2022", "05-2022"), "05-2022")
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: FAIL — `could not find function "enumerate_periods"`.

- [ ] **Step 3: Implement `enumerate_periods()` at the top of `R/county_crime.R`**

```r
# Layer 1 of the geography model: itemized, unsummed county-crime detail.

# Every "MM-YYYY" month from `from` to `to`, inclusive, in chronological order.
# Used to build the reporting grid so a missing month reads as "did not report"
# rather than "reported zero".
enumerate_periods <- function(from, to) {
  f <- as.integer(strsplit(from, "-", fixed = TRUE)[[1]])
  t <- as.integer(strsplit(to, "-", fixed = TRUE)[[1]])
  start <- f[2] * 12L + (f[1] - 1L)
  end <- t[2] * 12L + (t[1] - 1L)
  idx <- seq.int(start, end)
  sprintf("%02d-%d", idx %% 12L + 1L, idx %/% 12L)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/county_crime.R tests/testthat/test-county_crime.R
git commit -m "feat: enumerate_periods helper for the reporting grid"
```

---

### Task 7: `parse_agency_detail()` — one agency's response → per-period rows

**Files:**
- Modify: `R/county_crime.R` (add after `enumerate_periods()`)
- Test: `tests/testthat/test-county_crime.R`

**Interfaces:**
- Consumes: `enumerate_periods()` (Task 6); `%||%` (existing, `R/utils.R`).
- Produces: `parse_agency_detail(response, ori, offense, from, to)` — takes a
  parsed CDE `summarized/agency/{ori}/{offense}` response (as `cde_request()`
  returns it: nested named lists), returns a data.frame with columns
  `ori, offense, period, count, population, participated_population, rate,
  reported`, one row per period in `enumerate_periods(from, to)`. `count`/`rate`
  are `NA` and `reported = FALSE` for un-reported periods. `rate` =
  `count / participated_population * 1e5` (per-period), `NA` when the denominator
  is missing or zero. Comparison series (state/national) are never read — only
  the agency's own `* Offenses` actuals.

- [ ] **Step 1: Write the failing test in `tests/testthat/test-county_crime.R`**

```r
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
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: FAIL — `could not find function "parse_agency_detail"`.

- [ ] **Step 3: Implement `parse_agency_detail()` in `R/county_crime.R`**

```r
# Pick the agency's own "<name> Offenses" series from the actuals, defensively
# excluding national/state comparison series that may appear under schema drift.
.agency_offense_key <- function(actual_names) {
  cand <- grep(" Offenses$", actual_names, value = TRUE)
  comparison <- c("United States Offenses",
                  paste(datasets::state.name, "Offenses"))
  cand <- setdiff(cand, comparison)
  if (length(cand) == 0) NA_character_ else cand[1]
}

parse_agency_detail <- function(response, ori, offense, from, to) {
  periods <- enumerate_periods(from, to)
  n <- length(periods)

  actuals <- response$offenses$actuals %||% response$offenses$counts
  offense_key <- if (is.null(actuals)) NA_character_ else
    .agency_offense_key(names(actuals))
  count_series <- if (is.na(offense_key)) NULL else actuals[[offense_key]]
  label <- if (is.na(offense_key)) NA_character_ else
    sub(" Offenses$", "", offense_key)

  pop_series <- if (is.na(label)) NULL else
    response$populations$population[[label]]
  part_series <- if (is.na(label)) NULL else
    response$populations$participated_population[[label]]

  num <- function(series, period) {
    v <- if (is.null(series)) NULL else series[[period]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }

  count <- vapply(periods, function(p) num(count_series, p), numeric(1))
  population <- vapply(periods, function(p) num(pop_series, p), numeric(1))
  participated <- vapply(periods, function(p) num(part_series, p), numeric(1))
  reported <- !is.na(count)
  rate <- ifelse(!is.na(count) & !is.na(participated) & participated > 0,
                 count / participated * 1e5, NA_real_)

  data.frame(
    ori = ori,
    offense = offense,
    period = periods,
    count = count,
    population = population,
    participated_population = participated,
    rate = rate,
    reported = reported,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/county_crime.R tests/testthat/test-county_crime.R
git commit -m "feat: parse_agency_detail parses one agency response to per-period rows"
```

---

### Task 8: `get_county_crime_detail()` — the fan-out (Layer 1 headline)

**Files:**
- Modify: `R/county_crime.R` (add the exported function)
- Test: `tests/testthat/test-county_crime.R` (offline + live)

**Interfaces:**
- Consumes: `county_agencies()` (Task 5), `parse_agency_detail()` (Task 7),
  `rbind_fill()` (Task 2), `cde_path()`/`cde_request()`/`cde_validate_dates()`
  (`R/http.R`), `DEFAULT_MEMBER_CLASSES` (Task 4).
- Produces: `get_county_crime_detail(county, state, offense = "V",
  from = "01-2015", to = "12-2020", agency_class = NULL, default_only = FALSE,
  progress = FALSE)` — returns a data.frame with columns `ori, agency_name,
  agency_type_name, agency_class, default_member, county_name, state_abbr,
  offense, period, count, population, participated_population, rate, reported`,
  one row per (member ORI, period). No summation. ORIs whose request or parse
  fails are dropped with a warning and recorded in `attr(out, "dropped")`.

- [ ] **Step 1: Write the failing offline test in `tests/testthat/test-county_crime.R`**

```r
# Helper: a minimal one-agency response for the inline fan-out mock. `counts` is
# a named numeric vector, e.g. c("01-2021" = 10, "02-2021" = 12).
make_agency_response <- function(name, counts, pop = 20000) {
  offense_key <- paste(name, "Offenses")
  actuals <- setNames(list(as.list(counts)), offense_key)
  pop_series <- as.list(setNames(rep(pop, length(counts)), names(counts)))
  pop_list <- setNames(list(pop_series), name)
  list(
    offenses = list(actuals = actuals, rates = list()),
    populations = list(population = pop_list,
                       participated_population = pop_list)
  )
}

test_that("get_county_crime_detail fans out, filters, and reports drops", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002", "CA0000003"),
    agency_name = c("Alpha PD", "Beta University", "Gamma PD"),
    agency_type_name = c("City", "University or College", "City"),
    agency_class = c("municipal", "campus", "municipal"),
    default_member = c(TRUE, FALSE, TRUE),
    county_name = "TESTONIA",
    state_abbr = "CA",
    latitude = 0, longitude = 0,
    stringsAsFactors = FALSE
  )
  responses <- list(
    CA0000001 = make_agency_response(
      "Alpha PD", c("01-2021" = 10, "02-2021" = 12)),
    CA0000003 = make_agency_response(
      "Gamma PD", c("01-2021" = 4, "02-2021" = 6))
    # CA0000002 intentionally absent -> its request errors -> dropped.
  )

  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) {
      ori <- sub("^summarized/agency/([A-Za-z0-9]{9})/.*$", "\\1", path)
      r <- responses[[ori]]
      if (is.null(r)) stop("no data for ", ori)
      r
    },
    .package = "fbiCDE"
  )

  out <- get_county_crime_detail("Testonia", "CA", offense = "V",
                                 from = "01-2021", to = "02-2021")

  expect_s3_class(out, "data.frame")
  # 2 successful agencies x 2 periods = 4 rows; campus errored and was dropped.
  expect_equal(nrow(out), 4L)
  expect_setequal(unique(out$ori), c("CA0000001", "CA0000003"))
  expect_equal(attr(out, "dropped"), "CA0000002")
  expect_true(all(c("agency_class", "count", "population", "rate",
                    "reported") %in% names(out)))
})

test_that("get_county_crime_detail default_only keeps only default members", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000002"),
    agency_name = c("Alpha PD", "Beta University"),
    agency_type_name = c("City", "University or College"),
    agency_class = c("municipal", "campus"),
    default_member = c(TRUE, FALSE),
    county_name = "TESTONIA", state_abbr = "CA",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  responses <- list(
    CA0000001 = make_agency_response("Alpha PD", c("01-2021" = 10)),
    CA0000002 = make_agency_response("Beta University", c("01-2021" = 99))
  )
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    cde_request = function(path, query = list(), ...) {
      ori <- sub("^summarized/agency/([A-Za-z0-9]{9})/.*$", "\\1", path)
      responses[[ori]]
    },
    .package = "fbiCDE"
  )

  out <- get_county_crime_detail("Testonia", "CA", from = "01-2021",
                                 to = "01-2021", default_only = TRUE)
  expect_equal(unique(out$ori), "CA0000001")
  expect_false("CA0000002" %in% out$ori)
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: FAIL — `could not find function "get_county_crime_detail"`.

- [ ] **Step 3: Implement `get_county_crime_detail()` in `R/county_crime.R`**

```r
.DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

#' Itemized crime detail for every agency attributed to a county
#'
#' Fans out one request per member agency and returns their crime series
#' **unsummed** — one row per agency-period — with each agency's `agency_class`,
#' coverage (`population`, `participated_population`, `reported`), and a
#' per-agency `rate` (`count / participated_population * 1e5`). This is the
#' transparent, power-user primitive; it applies no aggregation and takes no
#' stance on denominators. Filter with `agency_class`/`default_only`; the default
#' returns *every* attributed agency, typed.
#'
#' @param county County name (case-insensitive).
#' @param state Two-letter state abbreviation.
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector; keep only these classes
#'   (`"county_primary"`, `"municipal"`, `"campus"`, `"state"`, `"tribal"`,
#'   `"special"`).
#' @param default_only If `TRUE`, keep only default members (`county_primary` +
#'   `municipal`). Ignored if `agency_class` is supplied.
#' @param progress If `TRUE`, print a simple progress line per agency.
#' @return A data.frame (columns listed in Details). Agencies whose request or
#'   parse fails are dropped with a warning and recorded in `attr(x, "dropped")`.
#' @export
#' @examples
#' \dontrun{
#' get_county_crime_detail("Alameda", "CA", from = "01-2019", to = "12-2019")
#' }
get_county_crime_detail <- function(county, state, offense = "V",
                                    from = "01-2015", to = "12-2020",
                                    agency_class = NULL, default_only = FALSE,
                                    progress = FALSE) {
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- county_agencies(county, state)
  if (!is.null(agency_class)) {
    agencies <- agencies[agencies$agency_class %in% agency_class, , drop = FALSE]
  } else if (isTRUE(default_only)) {
    agencies <- agencies[agencies$default_member, , drop = FALSE]
  }

  if (nrow(agencies) == 0) {
    warning("No agencies to query for '", county, "', ", state,
            " after filtering", call. = FALSE)
    empty <- as.data.frame(
      matrix(nrow = 0, ncol = length(.DETAIL_COLS),
             dimnames = list(NULL, .DETAIL_COLS)),
      stringsAsFactors = FALSE
    )
    return(empty)
  }

  dropped <- character(0)
  parts <- vector("list", nrow(agencies))
  for (i in seq_len(nrow(agencies))) {
    ori <- agencies$ori[i]
    if (isTRUE(progress)) {
      message(sprintf("[%d/%d] %s", i, nrow(agencies), ori))
    }
    path <- cde_path("summarized", paste0("agency/", ori), offense)
    query <- list(from = from, to = to, type = "counts")

    res <- tryCatch(
      parse_agency_detail(cde_request(path, query), ori, offense, from, to),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      dropped <- c(dropped, ori)
      next
    }
    res$agency_name <- agencies$agency_name[i]
    res$agency_type_name <- agencies$agency_type_name[i]
    res$agency_class <- agencies$agency_class[i]
    res$default_member <- agencies$default_member[i]
    res$county_name <- agencies$county_name[i]
    res$state_abbr <- agencies$state_abbr[i]
    parts[[i]] <- res
  }

  out <- rbind_fill(parts)
  out <- out[, intersect(.DETAIL_COLS, names(out)), drop = FALSE]
  rownames(out) <- NULL

  if (length(dropped) > 0) {
    warning("Dropped ", length(dropped),
            " agenc", if (length(dropped) == 1) "y" else "ies",
            " that returned no data: ", paste(dropped, collapse = ", "),
            call. = FALSE)
    attr(out, "dropped") <- dropped
  }
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: PASS.

- [ ] **Step 5: Add the live test (guarded) in `tests/testthat/test-county_crime.R`**

```r
test_that("get_county_crime_detail works live for a small county", {
  skip_if_no_fbi_api()
  out <- get_county_crime_detail("Alameda", "CA", offense = "V",
                                 from = "01-2019", to = "03-2019",
                                 default_only = TRUE)
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)
  expect_true(all(c("ori", "count", "population", "reported") %in% names(out)))
  expect_true(all(out$agency_class %in% c("county_primary", "municipal")))
  # Oakland PD is a default member of Alameda County.
  expect_true("CA0010900" %in% out$ori)
})
```

- [ ] **Step 6: Run the live test locally (requires FBI_API_KEY)**

Run: `FBI_API_KEY=1 Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: PASS (or SKIP if no key/CI). If it fails on a schema mismatch, inspect
one live response and adjust `parse_agency_detail()`, not the test.

- [ ] **Step 7: Commit**

```bash
git add R/county_crime.R tests/testthat/test-county_crime.R
git commit -m "feat: get_county_crime_detail fans out itemized county crime (Layer 1)"
```

---

### Task 9: `get_county_agency_crime()` — the county's own agency

**Files:**
- Modify: `R/county_crime.R`
- Test: `tests/testthat/test-county_crime.R`

**Interfaces:**
- Consumes: `county_agencies()` (Task 5), `get_agency_crime()` (`R/ucr_crime.R`).
- Produces: `get_county_agency_crime(county, state, offense = "V",
  from = "01-2015", to = "12-2020")` — resolves the single `county_primary` ORI
  and returns `get_agency_crime()` for it. Errors if none; warns and uses the
  first if several.

- [ ] **Step 1: Write the failing offline test**

```r
test_that("get_county_agency_crime resolves the county_primary ORI", {
  agencies <- data.frame(
    ori = c("CA0000001", "CA0000009"),
    agency_name = c("Alpha PD", "Testonia County Sheriff"),
    agency_type_name = c("City", "County"),
    agency_class = c("municipal", "county_primary"),
    default_member = c(TRUE, TRUE),
    county_name = "TESTONIA", state_abbr = "CA",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  called <- new.env()
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies,
    get_agency_crime = function(ori, ...) {
      called$ori <- ori
      data.frame(geography = ori, offense = "x", period = "01-2021",
                 count = 1, rate = 1, stringsAsFactors = FALSE)
    },
    .package = "fbiCDE"
  )
  out <- get_county_agency_crime("Testonia", "CA")
  expect_equal(called$ori, "CA0000009")   # the sheriff, not the city
  expect_s3_class(out, "data.frame")
})

test_that("get_county_agency_crime errors when no county_primary exists", {
  agencies <- data.frame(
    ori = "CA0000001", agency_name = "Alpha PD",
    agency_type_name = "City", agency_class = "municipal",
    default_member = TRUE, county_name = "TESTONIA", state_abbr = "CA",
    latitude = 0, longitude = 0, stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    county_agencies = function(county, state) agencies, .package = "fbiCDE")
  expect_error(get_county_agency_crime("Testonia", "CA"),
               "No county-primary")
})
```

- [ ] **Step 2: Run it and watch it fail**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: FAIL — `could not find function "get_county_agency_crime"`.

- [ ] **Step 3: Implement `get_county_agency_crime()` in `R/county_crime.R`**

```r
#' Crime reported by a county's own primary agency (sheriff/parish)
#'
#' Resolves the single `county_primary` ORI for a county and returns its own
#' `get_agency_crime()` series. This disambiguates "the county sheriff's own
#' reported crime" from "crime aggregated across the county"
#' (`get_county_crime_detail()`).
#'
#' @inheritParams get_county_crime_detail
#' @return The `get_agency_crime()` data.frame for the county's primary agency.
#' @export
#' @examples
#' \dontrun{
#' get_county_agency_crime("Alameda", "CA")
#' }
get_county_agency_crime <- function(county, state, offense = "V",
                                    from = "01-2015", to = "12-2020") {
  agencies <- county_agencies(county, state)
  prim <- agencies[agencies$agency_class == "county_primary", , drop = FALSE]

  if (nrow(prim) == 0) {
    stop("No county-primary (sheriff/parish) agency found for '", county,
         "', ", state, call. = FALSE)
  }
  if (nrow(prim) > 1) {
    warning("Multiple county-primary agencies for '", county, "', ", state,
            "; using ", prim$ori[1], call. = FALSE)
  }
  get_agency_crime(prim$ori[1], from = from, to = to, offense = offense)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e "devtools::load_all('.'); testthat::test_file('tests/testthat/test-county_crime.R')"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/county_crime.R tests/testthat/test-county_crime.R
git commit -m "feat: get_county_agency_crime returns the county's own agency series"
```

---

### Task 10: Documentation, NAMESPACE, and full check

**Files:**
- Modify: `NAMESPACE` (regenerated), `man/*.Rd` (regenerated)
- Modify: `NEWS.md`
- Modify: `_pkgdown.yml` (if present — add a reference section)

- [ ] **Step 1: Regenerate documentation and NAMESPACE**

Run: `Rscript -e "devtools::document()"`
Expected: `NAMESPACE` now exports `county_agencies`, `get_county_crime_detail`,
`get_county_agency_crime`; new `man/*.Rd` files created. `classify_agency`,
`parse_agency_detail`, `enumerate_periods`, `rbind_fill`, `agencies_table` remain
internal (no `@export`).

- [ ] **Step 2: Add a NEWS.md entry**

Add under the development-version heading:

```markdown
## Geography-first querying (v0.2)

- Removed the `data.table` dependency; the package is now base-R only (dead
  `srs_long_to_wide()`/`make_url()` reshape helpers deleted).
- `is_valid_ori()` now accepts letter-bearing ORIs (`^[A-Z]{2}[A-Z0-9]{7}$`),
  which unblocks `get_agency_crime()` for state, tribal, campus, and
  contract-city agencies (~10% of the agency universe were wrongly rejected).
- Added `county_agencies()` — a pure membership resolver that classifies every
  agency attributed to a county into `agency_class` (`county_primary`,
  `municipal`, `campus`, `state`, `tribal`, `special`) with a conservative
  `default_member` flag (county_primary + municipal).
- Added `get_county_crime_detail()` — itemized, **unsummed** county crime, one
  row per agency-period, carrying `agency_class`, `population`,
  `participated_population`, a per-agency `rate`, and a `reported` flag that
  distinguishes "reported zero" from "did not report". Filter with
  `agency_class`/`default_only`; failed agencies are dropped with a warning and
  recorded in `attr(x, "dropped")`.
- Added `get_county_agency_crime()` — the county's own primary agency
  (sheriff/parish) series, disambiguating it from the county-wide detail.
```

- [ ] **Step 3: Add a pkgdown reference group (only if `_pkgdown.yml` exists)**

Run:
```bash
test -f _pkgdown.yml && echo "edit _pkgdown.yml" || echo "no pkgdown; skip"
```
If it exists, add a reference section (match the file's existing indentation):

```yaml
  - title: "Geography"
    desc: "County-level membership and itemized detail"
    contents:
      - county_agencies
      - get_county_crime_detail
      - get_county_agency_crime
```

- [ ] **Step 4: Run the full check**

Run: `Rscript -e "devtools::check(args = '--no-manual')"`
Expected: 0 errors, 0 warnings, 0 notes (or only pre-existing notes unrelated to
this work). Fix any `no visible binding` notes by confirming
`utils::globalVariables("fbi_api_agencies")` is present in `R/geography.R`.

- [ ] **Step 5: Commit**

```bash
git add NAMESPACE man NEWS.md _pkgdown.yml
git commit -m "docs: document geography functions; NEWS for v0.2"
```

---

## Self-Review Notes

**Spec coverage** (design doc §-by-§ → task):
- §3 Layer 0 resolver → Tasks 4, 5. §3 Layer 1 detail → Tasks 6, 7, 8.
  Disambiguation verb → Task 9.
- §4 classification + default policy → Task 4 (`classify_agency`,
  `DEFAULT_MEMBER_CLASSES`); state/tribal/campus default-exclude is enforced by
  `default_member` (Task 5) and `default_only` (Task 8).
- §5 return shape (incl. `rate`, both populations, `reported`) → Tasks 7, 8.
- §7 mechanical decisions: comparison-row stripping → Task 7
  (`.agency_offense_key`); fan-out partial-failure + drop reporting → Task 8;
  coverage columns → Task 7; FIPS deferred (absent by design) → not in scope.
- §10 `data.table` removal → Tasks 2, 3. §10a `is_valid_ori` fix → Task 1.
- §11 testing discipline (offline + live per path) → every task; live tests in
  Task 8 (detail) and via existing suite (resolver is pure/offline).
- **Deferred (not in this plan, by design):** aggregate rate/denominator model
  (§6, v0.3), imputation (§9a, v0.2b), Census join/FIPS (§8/§9, v0.3+), place &
  metro (v0.4+), capstone. The `reported`/coverage columns this plan ships are
  the substrate those phases consume.

**Placeholder scan:** none — every code step has complete code; every run step
has an exact command and expected result.

**Type/name consistency:** `agency_class` values, `DEFAULT_MEMBER_CLASSES`,
`.DETAIL_COLS`, and the `parse_agency_detail()` output columns are consistent
across Tasks 4–9. `county_agencies()` output columns (Task 5) match what
`get_county_crime_detail()` reads by name (Task 8). `cde_request()` mock
signature `(path, query, ...)` matches the real seam and both offline tests.

**Note on rate semantics:** `rate` is per-period (`count / participated_population
× 1e5`), mirroring the CDE's per-month payload. If annualized rates are wanted
later, that is an additive refinement, not a change to this shape.
