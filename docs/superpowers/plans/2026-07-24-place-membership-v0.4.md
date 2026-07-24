# Place/Municipal Membership (v0.4) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add place/municipal-level agency membership and itemized crime detail to the `fbi` package, keyed by place name rather than ORI.

**Architecture:** Three units. A pure, offline resolver (`place_agencies()`) derives a place name from each municipal agency's name and matches it — no network, no new dependencies. A fan-out layer (`get_place_crime_detail()`) reuses the existing county fan-out machinery unchanged. A separate opt-in transform (`add_place_spatial_members()`) attributes embedded campus/transit agencies by point-in-polygon, behind `sf`/`tigris` in `Suggests`.

**Tech Stack:** Base R (>= 3.5.0). `httr`, `jsonlite`, `datasets`, `utils` in Imports. `sf`, `tigris` added to Suggests. `testthat` 3rd edition.

**Spec:** `docs/superpowers/specs/2026-07-24-place-membership-v0.4-design.md`

## Global Constraints

- **All HTTP goes through `cde_request()`** (`R/http.R`). Never call `httr` directly elsewhere. Compose URLs with `cde_path()` / `cde_query()`.
- **Base R only in `Imports`.** New heavy dependencies (`sf`, `tigris`) go in `Suggests` and must be guarded at runtime with `requireNamespace(..., quietly = TRUE)`.
- **R >= 3.5.0.** No native pipe (`|>`), no `\(x)` lambda, no base `%||%` (use the package-local `%||%` in `R/utils.R`).
- **Every data path gets an offline test that runs on CI.** Mock `cde_request()` via `local_fbi_fixture()`; fixtures live in `tests/testthat/fixtures/`.
- **No vacuous tests.** A test that iterates a collection must assert the collection is non-empty. A test whose only assertions sit inside an `if` must use `skip_if_not_installed()` instead, so a skip is visible rather than a silent pass.
- **`R CMD check` must stay clean:** 0 errors, 0 warnings, 0 notes.
- **Conventional commits** (`feat:`/`fix:`/`test:`/`docs:`). Record user-facing changes in `NEWS.md`.
- **Deviation from spec §3, ratified:** the place index is derived at call time by a pure function, not built into `R/sysdata.rda`. `sysdata.rda` currently holds exactly one object (`crosswalk`); adding to it risks clobbering the FIPS crosswalk. The spec's build-script drift guard becomes a test over the bundled table (Task 1, Step 5), which runs on every CI run rather than only on data regeneration.
- **Deviation from spec §3, ratified:** an unmatched place produces a **warning + zero-row frame**, not an error, matching the existing `county_agencies()` precedent. We hold no independent place universe, so "place does not exist" and "place has no reporting agency" are indistinguishable from our data; the message says so rather than implying a distinction we cannot make.

## File Structure

| File | Responsibility |
|---|---|
| `R/place.R` (create) | Place-name derivation + `place_agencies()` resolver. Pure, no network. |
| `R/place_crime.R` (create) | `get_place_crime_detail()` — Layer 1 fan-out. |
| `R/place_spatial.R` (create) | `add_place_spatial_members()` — opt-in PIP transform. |
| `R/agency_class.R` (modify) | Add `classify_place_agency()` beside the existing county classifier. |
| `tests/testthat/test-place.R` (create) | Derivation + resolver tests, incl. drift guards. |
| `tests/testthat/test-place_crime.R` (create) | Fan-out tests (mocked `cde_request`). |
| `tests/testthat/test-place_spatial.R` (create) | Seam-injected PIP tests + degradation tests. |
| `DESCRIPTION` (modify) | Add `sf`, `tigris` to Suggests. |
| `NEWS.md` (modify) | v0.4 entry. |

Three separate R files rather than one: the resolver must stay dependency-free and offline, while the spatial unit pulls heavy Suggests. Keeping them in separate files makes that boundary visible and keeps each file well under the 400-line house target.

---

### Task 1: Place-name derivation

**Files:**
- Create: `R/place.R`
- Test: `tests/testthat/test-place.R`

**Interfaces:**
- Consumes: `fbi_api_agencies` (bundled dataset), `agencies_table()` from `R/geography.R`.
- Produces: `derive_place_name(agency_name)` → character vector; `.MUNICIPAL_TYPES` → character vector of the four municipal `agency_type_name` values; `.PLACE_SUFFIX_PATTERN` → the regex constant.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-place.R`:

```r
# ---- derive_place_name() ---------------------------------------------------

test_that("derive_place_name strips the common police-department suffixes", {
  expect_equal(derive_place_name("Lufkin Police Department"), "Lufkin")
  expect_equal(derive_place_name("Aransas Pass Police Department"), "Aransas Pass")
  expect_equal(derive_place_name("Colusa Police Dept"), "Colusa")
  expect_equal(derive_place_name("Colusa Police Dept."), "Colusa")
  expect_equal(derive_place_name("Sitka Police"), "Sitka")
  expect_equal(derive_place_name("Dover Department of Public Safety"), "Dover")
  expect_equal(derive_place_name("Dover Public Safety Department"), "Dover")
  expect_equal(derive_place_name("Nome Marshal's Office"), "Nome")
})

test_that("derive_place_name leaves bare place names untouched", {
  expect_equal(derive_place_name("Concord"), "Concord")
  expect_equal(derive_place_name("Cherry Valley"), "Cherry Valley")
  expect_equal(derive_place_name("Kansas"), "Kansas")
})

test_that("derive_place_name is vectorised and trims whitespace", {
  expect_equal(
    derive_place_name(c("Lufkin Police Department", "Concord", " Troy Police ")),
    c("Lufkin", "Concord", "Troy")
  )
})

test_that("derive_place_name only strips a suffix, never an interior match", {
  # "Police" inside the name must survive; only a trailing suffix is stripped.
  expect_equal(derive_place_name("Police Jury Police Department"), "Police Jury")
})

test_that("derive_place_name handles NA and empty input", {
  expect_true(is.na(derive_place_name(NA_character_)))
  expect_equal(derive_place_name(character(0)), character(0))
})

test_that("derive_place_name strips a doubled suffix to a fixed point", {
  # Two real CDE records carry the suffix twice.
  expect_equal(
    derive_place_name("Las Vegas Metropolitan Police Department Police Department"),
    "Las Vegas Metropolitan"
  )
  expect_equal(
    derive_place_name("Northeast Police Department Police Department"),
    "Northeast"
  )
})

test_that("derive_place_name never strips a name down to nothing", {
  # The pattern requires a leading space, so a bare "Police" is a fixed point.
  expect_equal(derive_place_name("Police"), "Police")
  expect_equal(derive_place_name("Police Police Department"), "Police")
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `R -q -e 'devtools::test(filter = "place")'`
Expected: FAIL — `could not find function "derive_place_name"`.

- [ ] **Step 3: Write the minimal implementation**

Create `R/place.R`:

```r
# Layer 0 of the place geography model: the municipal membership resolver.
# Pure, no network, no new dependencies.
#
# The municipal tier is a name-identity problem, not a spatial one: CDE city
# agencies are named "<Place> Police Department", so the agency *is* the place.
# See docs/superpowers/specs/2026-07-24-place-membership-v0.4-design.md §1.

# The four agency_type_name values that constitute the municipal tier.
.MUNICIPAL_TYPES <- c("City", "Municipality", "Borough", "City and Borough")

# Trailing agency-name suffixes stripped to recover the bare place name.
# Order matters: longer, more specific alternatives must precede shorter ones
# so "Police Department" is not truncated to "Department" by an earlier match.
.PLACE_SUFFIX_PATTERN <- paste0(
  " (",
  paste(
    c(
      "Police Department",
      "Police Dept\\.?",
      "Department of Public Safety",
      "Public Safety Department",
      "Marshal's Office",
      "Police"
    ),
    collapse = "|"
  ),
  ")$"
)

# Recover the place name from a municipal agency's name by stripping recognized
# trailing suffixes. Names matching no suffix are returned as-is (the CDE stores
# a few hundred agencies under a bare place name).
#
# Stripping repeats to a fixed point because two real records carry a doubled
# suffix — "Las Vegas Metropolitan Police Department Police Department"
# (NV0020100) and "Northeast Police Department Police Department" (PA0081200).
# A single pass would leave a residual suffix in the derived place name.
# The loop is safe: the pattern requires a space before the matched suffix, so a
# name that is only "Police" is a fixed point rather than being stripped empty.
derive_place_name <- function(agency_name) {
  out <- trimws(agency_name)
  repeat {
    stripped <- trimws(sub(.PLACE_SUFFIX_PATTERN, "", out, perl = TRUE))
    if (identical(stripped, out)) {
      break
    }
    out <- stripped
  }
  out
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `R -q -e 'devtools::test(filter = "place")'`
Expected: PASS.

- [ ] **Step 5: Add the drift guards over the real bundled table**

These replace the build-script assertions from spec §3. They run on every CI run, so a CDE naming change fails loudly. Append to `tests/testthat/test-place.R`:

```r
# ---- Drift guards over the bundled agency table ----------------------------
#
# These assert the empirical facts the whole place model rests on (spec §1).
# If the CDE changes its agency naming, these fail rather than the resolver
# silently degrading.

test_that("municipal-tier agencies are present in the expected volume", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  # Guard against the guard going vacuous.
  expect_gt(nrow(mun), 10000)
})

test_that("place-name derivation resolves effectively every municipal agency", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  places <- derive_place_name(mun$agency_name)

  expect_false(any(is.na(places)))
  expect_true(all(nzchar(places)))
  # No derived place name may still carry a police-department suffix.
  expect_equal(sum(grepl(.PLACE_SUFFIX_PATTERN, places)), 0L)
})

test_that("(state, county, place) is a collision-free key", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  key <- paste(mun$state_abbr, mun$county_name, derive_place_name(mun$agency_name))
  expect_gt(length(key), 10000)
  expect_equal(sum(table(key) > 1), 0L)
})

test_that("only the two known (state, place) keys are ambiguous", {
  mun <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE
  ]
  key <- paste(mun$state_abbr, derive_place_name(mun$agency_name))
  ambiguous <- sort(names(which(table(key) > 1)))
  expect_equal(ambiguous, c("PA Foster Township", "PA Jefferson Township"))
})
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `R -q -e 'devtools::test(filter = "place")'`
Expected: PASS. If the collision or ambiguity guard fails, the CDE agency table has drifted — stop and report, do not relax the assertion.

- [ ] **Step 7: Commit**

```bash
git add R/place.R tests/testthat/test-place.R
git commit -m "feat: derive place names from municipal agency names

CDE city agencies are named '<Place> Police Department', so the municipal
tier is a name-identity problem rather than a spatial one. Adds
derive_place_name() plus drift guards asserting ~100% derivation and a
collision-free (state, county, place) key over the bundled agency table."
```

---

### Task 2: `place_agencies()` resolver

**Files:**
- Modify: `R/agency_class.R` (append `classify_place_agency()`)
- Modify: `R/place.R` (append the resolver)
- Test: `tests/testthat/test-place.R` (append)

**Interfaces:**
- Consumes: `derive_place_name()`, `.MUNICIPAL_TYPES` (Task 1); `agencies_table()` and `is_valid_state()` (existing); `classify_agency()` (existing, `R/agency_class.R`).
- Produces: `classify_place_agency(agency_type_name)` → character vector of `"place_primary"` / `"campus"` / `"special"`; `place_agencies(place, state, county = NULL)` → data.frame with columns `.PLACE_AGENCY_COLS`; `.PLACE_AGENCY_COLS` → the column-order constant; `.empty_place_agency_frame()` → zero-row frame.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-place.R`:

```r
# ---- classify_place_agency() -----------------------------------------------

test_that("classify_place_agency maps municipal types to place_primary", {
  expect_equal(
    classify_place_agency(c("City", "Municipality", "Borough", "City and Borough")),
    rep("place_primary", 4)
  )
})

test_that("classify_place_agency maps embedded types", {
  expect_equal(classify_place_agency("University or College"), "campus")
  expect_equal(classify_place_agency("Other"), "special")
  expect_equal(classify_place_agency("Other State Agency"), "special")
})

test_that("classify_place_agency maps non-place types to NA", {
  # A sheriff or state police agency is never a place member, so it has no
  # place-level class at all.
  expect_true(is.na(classify_place_agency("County")))
  expect_true(is.na(classify_place_agency("State Police")))
  expect_true(is.na(classify_place_agency("Tribal")))
})

# ---- place_agencies() ------------------------------------------------------

test_that("place_agencies resolves a place to its own municipal agency", {
  out <- place_agencies("Lufkin", "TX")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_equal(out$place_name, "Lufkin")
  expect_equal(out$agency_class, "place_primary")
  expect_true(out$default_member)
  expect_equal(out$attribution, "name_identity")
  expect_equal(out$state_abbr, "TX")
  expect_match(out$ori, "^[A-Z]{2}[A-Z0-9]{7}$")
})

test_that("place_agencies returns the documented columns in order", {
  out <- place_agencies("Lufkin", "TX")
  expect_equal(names(out), .PLACE_AGENCY_COLS)
})

test_that("place_agencies is case- and whitespace-insensitive", {
  a <- place_agencies("Lufkin", "TX")
  b <- place_agencies("  lufkin  ", "tx")
  expect_equal(a$ori, b$ori)
})

test_that("place_agencies never returns sheriffs, state police, or tribal agencies", {
  # Los Angeles has a city PD; LASD (County) must not appear.
  out <- place_agencies("Los Angeles", "CA")
  expect_true(all(out$agency_class == "place_primary"))
  expect_false(any(out$agency_type_name %in% c("County", "Parish", "State Police", "Tribal")))
})

test_that("place_agencies errors on an ambiguous place without county", {
  expect_error(
    place_agencies("Foster Township", "PA"),
    "ambiguous"
  )
  # The error must name the candidate counties so the user can disambiguate.
  err <- tryCatch(place_agencies("Foster Township", "PA"), error = function(e) e)
  expect_match(conditionMessage(err), "county =")
})

test_that("place_agencies resolves an ambiguous place when county is supplied", {
  amb <- fbi_api_agencies[
    fbi_api_agencies$agency_type_name %in% .MUNICIPAL_TYPES &
      fbi_api_agencies$state_abbr == "PA", , drop = FALSE
  ]
  amb$place <- derive_place_name(amb$agency_name)
  counties <- amb$county_name[amb$place == "Foster Township"]
  expect_gt(length(counties), 1L)

  out <- place_agencies("Foster Township", "PA", county = counties[1])
  expect_equal(nrow(out), 1L)
  expect_equal(toupper(out$county_name), toupper(counties[1]))
})

test_that("place_agencies warns and returns an empty typed frame for an unknown place", {
  expect_warning(
    out <- place_agencies("Nowheresville", "TX"),
    "No municipal agency"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_AGENCY_COLS)
})

test_that("place_agencies rejects an invalid state", {
  expect_error(place_agencies("Lufkin", "ZZ"), "Invalid state")
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `R -q -e 'devtools::test(filter = "place")'`
Expected: FAIL — `could not find function "classify_place_agency"`.

- [ ] **Step 3: Add the place classifier**

Append to `R/agency_class.R`:

```r
# Place-level classification. Distinct from `classify_agency()` because the
# tiers differ: at place level the municipal agency IS the place's primary
# reporter, and county/state/tribal agencies are not place members at all.
#
# Sheriffs police the unincorporated remainder and contract cities report under
# their own city ORI (design §14), so a place's crime is carried entirely by its
# own ORI. Mapping them to NA keeps them structurally unable to become members.
.PLACE_AGENCY_CLASS_MAP <- c(
  "City"                  = "place_primary",
  "Municipality"          = "place_primary",
  "Borough"               = "place_primary",
  "City and Borough"      = "place_primary",
  "University or College" = "campus",
  "Other"                 = "special",
  "Other State Agency"    = "special",
  "Census Area"           = "special"
)

#' Classify an agency type into a place-level membership class
#'
#' Maps the CDE's raw `agency_type_name` to a place-level `agency_class`.
#' Types that can never be place members (`County`, `Parish`, `State Police`,
#' `Tribal`) map to `NA`.
#'
#' @param agency_type_name Character vector of raw `agency_type_name` values.
#' @return Character vector of `"place_primary"`, `"campus"`, `"special"`, or
#'   `NA` for types that are not place members.
#' @keywords internal
classify_place_agency <- function(agency_type_name) {
  unname(.PLACE_AGENCY_CLASS_MAP[as.character(agency_type_name)])
}
```

- [ ] **Step 4: Add the resolver**

Append to `R/place.R`:

```r
.PLACE_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "place_name", "county_name", "state_abbr", "attribution",
  "latitude", "longitude"
)

# A 0-row, .PLACE_AGENCY_COLS-shaped frame with the correct column types.
.empty_place_agency_frame <- function() {
  out <- as.data.frame(
    matrix(nrow = 0, ncol = length(.PLACE_AGENCY_COLS),
           dimnames = list(NULL, .PLACE_AGENCY_COLS)),
    stringsAsFactors = FALSE
  )
  out$default_member <- logical(0)
  out$latitude <- numeric(0)
  out$longitude <- numeric(0)
  out
}

# The municipal-tier slice of the agency table, with place names derived.
# Recomputed per call: ~11,600 regex operations, negligible, and it keeps the
# derivation rules in reviewable R source rather than frozen in sysdata.rda.
.municipal_agencies <- function() {
  ag <- agencies_table()
  mun <- ag[ag$agency_type_name %in% .MUNICIPAL_TYPES, , drop = FALSE]
  mun$place_name <- derive_place_name(mun$agency_name)
  mun
}

#' List the law-enforcement agencies attributed to a place
#'
#' Resolves a municipality to the agency that reports crime for it. CDE city
#' agencies are named `"<Place> Police Department"`, so the place name is
#' recovered from the agency name — the agency *is* the place. This is an
#' *attribution* model, not a spatial one.
#'
#' Sheriffs, state police, and tribal agencies are never place members: a
#' sheriff's population is the unincorporated remainder, and contract cities
#' report under their own city ORI. A place's crime is therefore carried
#' entirely by its own agency, with no double-count and no gap.
#'
#' Campus and special-district agencies sit *inside* places but cannot be
#' attributed from names alone (name-matching achieves only ~26% recall). Add
#' them with [add_place_spatial_members()], which is opt-in and requires `sf`
#' and `tigris`.
#'
#' @param place Place name (case-insensitive; e.g. `"Lufkin"`). Supply either
#'   the bare name or the full agency name — both resolve.
#' @param state Two-letter state abbreviation (e.g. `"TX"`).
#' @param county Optional county name, needed only to disambiguate a place name
#'   that occurs in more than one county of the same state.
#' @return A data.frame with columns `ori`, `agency_name`, `agency_type_name`,
#'   `agency_class`, `default_member`, `place_name`, `county_name`,
#'   `state_abbr`, `attribution`, `latitude`, `longitude`. `attribution` records
#'   how the row earned membership: `"name_identity"` here, or
#'   `"point_in_polygon"` for rows added by [add_place_spatial_members()].
#'   Returns a zero-row frame (with a warning) when no municipal agency matches.
#' @seealso [county_agencies()] for the county-level resolver,
#'   [get_place_crime_detail()] for the crime series.
#' @export
#' @examples
#' place_agencies("Lufkin", "TX")
place_agencies <- function(place, state, county = NULL) {
  if (!is_valid_state(state)) {
    stop("Invalid state abbreviation: ", state, call. = FALSE)
  }

  mun <- .municipal_agencies()
  state_key <- toupper(trimws(state))
  # Accept either a bare place name or a full agency name.
  place_key <- toupper(derive_place_name(place))

  keep <- toupper(trimws(mun$state_abbr)) == state_key &
    toupper(mun$place_name) == place_key
  keep[is.na(keep)] <- FALSE
  sel <- mun[keep, , drop = FALSE]

  if (!is.null(county)) {
    county_key <- toupper(trimws(county))
    in_county <- toupper(trimws(sel$county_name)) == county_key
    in_county[is.na(in_county)] <- FALSE
    sel <- sel[in_county, , drop = FALSE]
  }

  if (nrow(sel) == 0) {
    warning("No municipal agency matches place '", place, "' in state '", state,
            "'. Either the place has no reporting agency, or its name differs ",
            "in the CDE agency table.", call. = FALSE)
    return(.empty_place_agency_frame())
  }

  if (nrow(sel) > 1) {
    stop("Place '", place, "' is ambiguous in ", state_key,
         ": it occurs in ", nrow(sel), " counties (",
         paste(sel$county_name, collapse = ", "),
         "). Disambiguate with county = \"", sel$county_name[1], "\".",
         call. = FALSE)
  }

  sel$agency_class <- classify_place_agency(sel$agency_type_name)
  sel$default_member <- sel$agency_class == "place_primary"
  sel$attribution <- "name_identity"

  out <- sel[, .PLACE_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}
```

- [ ] **Step 5: Document and run the tests**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test(filter = "place")'`
Expected: PASS.

If `place_agencies("Los Angeles", "CA")` returns more than one row, the ambiguity error will fire and that test will fail — in that case supply the county in the test rather than weakening the resolver.

- [ ] **Step 6: Commit**

```bash
git add R/place.R R/agency_class.R man/ NAMESPACE tests/testthat/test-place.R
git commit -m "feat: add place_agencies() municipal membership resolver

Pure, offline resolver keyed on (state, county, place). Sheriffs, state
police, and tribal agencies map to NA at place level so they are
structurally unable to become place members. Ambiguous place names error
with the candidate counties rather than silently picking one."
```

---

### Task 3: `get_place_crime_detail()` fan-out

**Files:**
- Create: `R/place_crime.R`
- Test: `tests/testthat/test-place_crime.R`

**Interfaces:**
- Consumes: `place_agencies()`, `.empty_place_agency_frame()` (Task 2); `parse_agency_detail()`, `enumerate_periods()` (existing, `R/county_crime.R`); `cde_validate_dates()`, `cde_path()`, `cde_request()` (existing, `R/http.R`); `rbind_fill()` (existing, `R/utils.R`).
- Produces: `get_place_crime_detail(place, state, county = NULL, offense = "V", from = "01-2015", to = "12-2020", agency_class = NULL, default_only = TRUE, progress = FALSE)` → data.frame with columns `.PLACE_DETAIL_COLS`; `.PLACE_DETAIL_COLS`; `.empty_place_detail_frame()`.

Note `default_only` defaults to **`TRUE`** here, unlike `get_county_crime_detail()` where it is `FALSE`. At place level the non-default classes only exist after an explicit opt-in spatial enrichment, so defaulting to the primary agency is the honest default.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-place_crime.R`:

```r
# A minimal `summarized/agency/{ori}/{offense}` response in the nested-list
# shape cde_request() returns (simplifyVector = FALSE). Includes a state
# comparison series that must be stripped.
fake_agency_response <- function(label = "Lufkin Police Department",
                                 counts = list(`01-2021` = 10, `02-2021` = 12),
                                 pops = list(`01-2021` = 20000, `02-2021` = 20000)) {
  offense_key <- paste(label, "Offenses")
  actuals <- list()
  actuals[[offense_key]] <- counts
  actuals[["Texas Offenses"]] <- list(`01-2021` = 999, `02-2021` = 999)

  populations <- list(
    population = stats::setNames(list(pops), label),
    participated_population = stats::setNames(list(pops), label)
  )
  list(offenses = list(actuals = actuals), populations = populations)
}

test_that("get_place_crime_detail returns per-period rows for the place's agency", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbi"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_equal(out$period, c("01-2021", "02-2021"))
  expect_equal(out$count, c(10, 12))
  expect_equal(unique(out$place_name), "Lufkin")
  expect_equal(unique(out$agency_class), "place_primary")
  expect_equal(unique(out$attribution), "name_identity")
  expect_true(all(out$reported))
})

test_that("get_place_crime_detail returns the documented columns in order", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbi"
  )
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  expect_equal(names(out), .PLACE_DETAIL_COLS)
})

test_that("get_place_crime_detail strips state comparison rows", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(),
    .package = "fbi"
  )
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  # The Texas comparison series carries 999; it must never reach the output.
  expect_false(any(out$count == 999))
})

test_that("get_place_crime_detail flags a non-reporting period rather than zero", {
  testthat::local_mocked_bindings(
    cde_request = function(...) fake_agency_response(
      counts = list(`01-2021` = 10)  # 02-2021 absent entirely
    ),
    .package = "fbi"
  )

  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021")
  feb <- out[out$period == "02-2021", , drop = FALSE]
  expect_true(is.na(feb$count))
  expect_false(feb$reported)
})

test_that("get_place_crime_detail drops a failing ORI with a warning and records it", {
  testthat::local_mocked_bindings(
    cde_request = function(...) stop("503"),
    .package = "fbi"
  )

  expect_warning(
    out <- get_place_crime_detail("Lufkin", "TX", from = "01-2021", to = "02-2021"),
    "Dropped"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_DETAIL_COLS)
  expect_true(length(attr(out, "dropped")) == 1L)
})

test_that("get_place_crime_detail warns and returns an empty frame for an unknown place", {
  # The warning comes from place_agencies(); assert it, then assert the shape.
  expect_warning(
    out <- get_place_crime_detail("Nowheresville", "TX",
                                  from = "01-2021", to = "02-2021"),
    "No municipal agency"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .PLACE_DETAIL_COLS)
})

test_that("get_place_crime_detail validates the date range", {
  expect_error(
    get_place_crime_detail("Lufkin", "TX", from = "2021-01", to = "02-2021")
  )
})

# ---- Live API -------------------------------------------------------------

test_that("get_place_crime_detail works against the live API", {
  skip_if_no_fbi_api()
  out <- get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), .PLACE_DETAIL_COLS)
  expect_gt(nrow(out), 0L)
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `R -q -e 'devtools::test(filter = "place_crime")'`
Expected: FAIL — `could not find function "get_place_crime_detail"`.

- [ ] **Step 3: Write the implementation**

Create `R/place_crime.R`:

```r
# Layer 1 of the place geography model: itemized, unsummed place-crime detail.
# Reuses the county fan-out machinery (parse_agency_detail, comparison-row
# stripping, partial-failure handling) unchanged.

.PLACE_DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "place_name", "county_name", "state_abbr", "attribution",
  "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

.empty_place_detail_frame <- function() {
  as.data.frame(
    matrix(nrow = 0, ncol = length(.PLACE_DETAIL_COLS),
           dimnames = list(NULL, .PLACE_DETAIL_COLS)),
    stringsAsFactors = FALSE
  )
}

#' Itemized crime detail for every agency attributed to a place
#'
#' Fans out one request per member agency and returns their crime series
#' **unsummed** — one row per agency-period — carrying `agency_class`, coverage
#' (`population`, `participated_population`, `reported`), a per-agency `rate`,
#' and the `attribution` that earned each agency its membership.
#'
#' In the common case a place has exactly one member agency, so this is a
#' by-name wrapper over that agency's series. That is the value: ORI discovery
#' is the package's primary friction, and this removes it.
#'
#' No place-level aggregate is provided. Summing an opted-in campus agency into
#' a city total is a modeling choice, not arithmetic; see the county Layer 2
#' (`get_county_crime()`) for the shape that decision takes.
#'
#' @inheritParams place_agencies
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector; keep only these place classes
#'   (`"place_primary"`, `"campus"`, `"special"`).
#' @param default_only If `TRUE` (the default), keep only default members
#'   (`place_primary`). Ignored if `agency_class` is supplied.
#' @param progress If `TRUE`, print a simple progress line per agency.
#' @return A data.frame with the columns listed in Details. Agencies whose
#'   request or parse fails are dropped with a warning and recorded in
#'   `attr(x, "dropped")`.
#' @seealso [place_agencies()], [add_place_spatial_members()],
#'   [impute_reporting_gaps()] for filling reporting gaps in the result.
#' @export
#' @examples
#' \dontrun{
#' get_place_crime_detail("Lufkin", "TX", from = "01-2019", to = "12-2019")
#' }
get_place_crime_detail <- function(place, state, county = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL, default_only = TRUE,
                                   progress = FALSE) {
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- place_agencies(place, state, county = county)
  if (!is.null(agency_class)) {
    agencies <- agencies[agencies$agency_class %in% agency_class, , drop = FALSE]
  } else if (isTRUE(default_only)) {
    agencies <- agencies[agencies$default_member, , drop = FALSE]
  }

  if (nrow(agencies) == 0) {
    return(.empty_place_detail_frame())
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
    res$place_name <- agencies$place_name[i]
    res$county_name <- agencies$county_name[i]
    res$state_abbr <- agencies$state_abbr[i]
    res$attribution <- agencies$attribution[i]
    parts[[i]] <- res
  }

  out <- rbind_fill(parts)
  out <- out[, intersect(.PLACE_DETAIL_COLS, names(out)), drop = FALSE]
  rownames(out) <- NULL

  if (nrow(out) == 0 || ncol(out) == 0) {
    out <- .empty_place_detail_frame()
  }

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

- [ ] **Step 4: Document and run the tests**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test(filter = "place")'`
Expected: PASS (both `test-place.R` and `test-place_crime.R`).

Fixture facts verified against the bundled table before this plan was written, so these are safe to assert: Lufkin TX is `TX0030400` in ANGELINA county and is unique; Los Angeles CA is unique; Foster Township PA is genuinely ambiguous across MCKEAN and SCHUYLKILL.

- [ ] **Step 5: Commit**

```bash
git add R/place_crime.R man/ NAMESPACE tests/testthat/test-place_crime.R
git commit -m "feat: add get_place_crime_detail() place-level fan-out

Reuses the county fan-out machinery unchanged: parse_agency_detail(),
comparison-row stripping, and drop-warn-record partial-failure handling.
default_only defaults to TRUE at place level, since non-default classes
only exist after an explicit spatial opt-in."
```

---

### Task 4: `add_place_spatial_members()` opt-in spatial tier

**Files:**
- Create: `R/place_spatial.R`
- Modify: `DESCRIPTION` (add `sf`, `tigris` to Suggests)
- Test: `tests/testthat/test-place_spatial.R`

**Interfaces:**
- Consumes: `place_agencies()` output, `.PLACE_AGENCY_COLS` (Task 2); `classify_place_agency()` (Task 2); `agencies_table()` (existing).
- Produces: `add_place_spatial_members(x, vintage = NULL, places_fun = NULL)` → the input frame with spatially-attributed rows appended and two columns added (`place_type`, `place_fips`); `.EMBEDDED_TYPES`.

The `places_fun` seam mirrors `cde_request(get_fun = )` and `join_census_pop(census_fun = )`. It is called as `places_fun(state, vintage)` and must return an `sf` polygon frame with at least `GEOID`, `NAME`, and `CLASSFP` columns.

- [ ] **Step 1: Add the Suggests entries**

Modify `DESCRIPTION` — the Suggests block becomes:

```
Suggests:
    testthat (>= 3.1.7),
    covr,
    withr,
    knitr,
    rmarkdown,
    censusapi,
    sf,
    tigris
```

- [ ] **Step 2: Write the failing test**

Create `tests/testthat/test-place_spatial.R`:

```r
# ---- Degradation path (runs everywhere, no sf/tigris needed) ---------------

test_that("add_place_spatial_members rejects non-data.frame input", {
  expect_error(add_place_spatial_members("nope"), "must be a data.frame")
})

test_that("add_place_spatial_members rejects a frame missing required columns", {
  expect_error(add_place_spatial_members(data.frame(x = 1)),
               "missing required columns")
})

test_that("add_place_spatial_members returns input unchanged when sf is absent", {
  x <- place_agencies("Lufkin", "TX")

  # Force the unavailable-dependency branch regardless of what is installed.
  testthat::local_mocked_bindings(
    .spatial_deps_available = function() FALSE,
    .package = "fbi"
  )

  expect_message(out <- add_place_spatial_members(x), "sf")
  expect_equal(nrow(out), nrow(x))
  expect_equal(out$ori, x$ori)
})

# ---- Point-in-polygon (needs sf; tigris replaced by the seam) --------------

# A one-square-degree polygon around a known point, as an sf frame in the shape
# tigris::places() returns.
fixture_places <- function() {
  skip_if_not_installed("sf")
  sq <- function(cx, cy) {
    sf::st_polygon(list(cbind(
      c(cx - 0.5, cx + 0.5, cx + 0.5, cx - 0.5, cx - 0.5),
      c(cy - 0.5, cy - 0.5, cy + 0.5, cy + 0.5, cy - 0.5)
    )))
  }
  sf::st_sf(
    GEOID = c("4845384", "4800001"),
    NAME = c("Lufkin", "Elsewhere"),
    CLASSFP = c("C1", "U1"),
    geometry = sf::st_sfc(sq(-94.7, 31.3), sq(-100.0, 35.0), crs = 4326)
  )
}

test_that("add_place_spatial_members attributes an embedded agency inside the place", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")

  # One campus agency whose HQ falls inside the Lufkin fixture polygon.
  fake_agencies <- data.frame(
    ori = "TX1234567",
    agency_name = "Angelina College",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 31.3,
    longitude = -94.7,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() rbind_fill(list(fbi_api_agencies, fake_agencies)),
    .package = "fbi"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())

  expect_gt(nrow(out), nrow(x))
  added <- out[out$attribution == "point_in_polygon", , drop = FALSE]
  expect_equal(nrow(added), 1L)
  expect_equal(added$ori, "TX1234567")
  expect_equal(added$agency_class, "campus")
  expect_false(added$default_member)
  expect_equal(added$place_type, "incorporated")
  expect_equal(added$place_fips, "4845384")
})

test_that("add_place_spatial_members excludes agencies outside the polygon", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  fake_agencies <- data.frame(
    ori = "TX7654321",
    agency_name = "Far Away University",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 20.0,       # well outside the fixture polygon
    longitude = -80.0,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() rbind_fill(list(fbi_api_agencies, fake_agencies)),
    .package = "fbi"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())
  expect_false("TX7654321" %in% out$ori)
})

test_that("add_place_spatial_members never attributes sheriffs or state police", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  # A sheriff and a state-police agency sitting exactly on the place centroid.
  fake_agencies <- data.frame(
    ori = c("TX1111111", "TX2222222"),
    agency_name = c("Angelina County Sheriff", "Texas DPS Lufkin"),
    agency_type_name = c("County", "State Police"),
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 31.3,
    longitude = -94.7,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() rbind_fill(list(fbi_api_agencies, fake_agencies)),
    .package = "fbi"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())
  expect_false(any(c("TX1111111", "TX2222222") %in% out$ori))
})

test_that("add_place_spatial_members flags a CDP match via place_type", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  x$place_name <- "Elsewhere"   # point the resolver row at the CDP polygon

  fake_agencies <- data.frame(
    ori = "TX3333333",
    agency_name = "Elsewhere Community College",
    agency_type_name = "University or College",
    state_abbr = "TX",
    county_name = x$county_name,
    latitude = 35.0,
    longitude = -100.0,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    agencies_table = function() rbind_fill(list(fbi_api_agencies, fake_agencies)),
    .package = "fbi"
  )

  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())
  added <- out[out$attribution == "point_in_polygon", , drop = FALSE]
  expect_equal(added$place_type, "cdp")
})

test_that("add_place_spatial_members leaves name_identity rows with NA place_fips", {
  skip_if_not_installed("sf")

  x <- place_agencies("Lufkin", "TX")
  out <- add_place_spatial_members(x, places_fun = function(state, vintage) fixture_places())

  primary <- out[out$attribution == "name_identity", , drop = FALSE]
  expect_equal(nrow(primary), 1L)
  expect_true(is.na(primary$place_fips))
  expect_true(is.na(primary$place_type))
})
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `R -q -e 'devtools::test(filter = "place_spatial")'`
Expected: FAIL — `could not find function "add_place_spatial_members"`.

- [ ] **Step 4: Write the implementation**

Create `R/place_spatial.R`:

```r
# Opt-in spatial attribution of embedded agencies to a place.
#
# Separate from the resolver on purpose: it needs heavy Suggests (sf, tigris)
# AND a network fetch, and it is best-effort inference. Keeping it a distinct
# call makes place_agencies() offline and deterministic, and makes the
# inference structurally opt-in — the impute_reporting_gaps() precedent.

# Agency types eligible for point-in-polygon attribution. Sheriffs and state
# police are deliberately absent: their HQ point says nothing about their
# jurisdiction (design §14), so a PIP hit would be meaningless.
.EMBEDDED_TYPES <- c("University or College", "Other", "Other State Agency")

# Isolated so tests can force the unavailable-dependency branch.
.spatial_deps_available <- function() {
  requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("tigris", quietly = TRUE)
}

# Default polygon source. Only reached when tigris is installed.
# Kept to the two arguments tigris::places() has carried stably (`state`,
# `year`) — optional cosmetic arguments have moved between tigris versions.
.default_places_fun <- function(state, vintage) {
  args <- list(state = state)
  if (!is.null(vintage)) {
    args$year <- vintage
  }
  do.call(tigris::places, args)
}

#' Add spatially-attributed embedded agencies to a place membership frame
#'
#' Attributes campus, transit, airport, and other special-district agencies to a
#' place by point-in-polygon of their headquarters coordinates against Census
#' place boundaries. Appends them to a [place_agencies()] result.
#'
#' This is opt-in and best-effort. It requires `sf` and `tigris` (both in
#' \sQuote{Suggests}) and downloads boundary shapefiles. Without them the
#' function emits a message and returns its input unchanged.
#'
#' Sheriffs and state police are never candidates: unlike a university police
#' department — whose headquarters genuinely sits on its campus inside the city
#' — a sheriff's HQ point carries no information about its jurisdiction. That
#' restriction is what makes point-in-polygon defensible for this tier.
#'
#' @param x A data.frame as returned by [place_agencies()].
#' @param vintage Optional Census boundary year passed to `tigris::places()`.
#'   Boundaries change with annexations and new incorporations, so pin this when
#'   reproducibility matters. `NULL` uses the `tigris` default.
#' @param places_fun Function used to fetch place polygons, called as
#'   `places_fun(state, vintage)` and expected to return an `sf` frame with
#'   `GEOID`, `NAME`, and `CLASSFP` columns. Defaults to `tigris::places()`.
#'   Exposed for testing; you should not need to set it.
#' @return `x` with spatially-attributed rows appended and two columns added:
#'   \itemize{
#'     \item `place_type`: `"incorporated"` or `"cdp"` (Census Designated
#'       Place, i.e. unincorporated). `NA` on `name_identity` rows.
#'     \item `place_fips`: the matched polygon's GEOID. **Best-effort
#'       enrichment, not a promised join key** — it is `NA` on `name_identity`
#'       rows, so it does not cover the municipal tier.
#'   }
#'   Appended rows carry `attribution = "point_in_polygon"` and
#'   `default_member = FALSE`.
#' @seealso [place_agencies()]
#' @export
#' @examples
#' \dontrun{
#' x <- place_agencies("Berkeley", "CA")
#' add_place_spatial_members(x)
#' }
add_place_spatial_members <- function(x, vintage = NULL, places_fun = NULL) {
  if (!inherits(x, "data.frame")) {
    stop("'x' must be a data.frame", call. = FALSE)
  }
  required <- c("ori", "place_name", "state_abbr", "county_name", "attribution")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("'x' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (is.null(places_fun)) {
    if (!.spatial_deps_available()) {
      message("Packages 'sf' and 'tigris' are required for ",
              "add_place_spatial_members().\n",
              "Install them with: install.packages(c('sf', 'tigris'))")
      return(x)
    }
    places_fun <- .default_places_fun
  }

  x$place_type <- NA_character_
  x$place_fips <- NA_character_

  if (nrow(x) == 0) {
    return(x)
  }

  state <- x$state_abbr[1]
  target <- toupper(x$place_name[1])

  polys <- places_fun(state, vintage)
  match_poly <- polys[toupper(polys$NAME) == target, , drop = FALSE]
  if (nrow(match_poly) == 0) {
    warning("No Census place polygon named '", x$place_name[1], "' in ", state,
            "; returning input unchanged.", call. = FALSE)
    return(x)
  }

  cand <- .embedded_candidates(state)
  if (nrow(cand) == 0) {
    return(x)
  }

  inside <- .points_in_polygon(cand$longitude, cand$latitude, match_poly)
  cand <- cand[inside, , drop = FALSE]
  if (nrow(cand) == 0) {
    return(x)
  }

  # CLASSFP "U*" denotes a Census Designated Place (unincorporated).
  is_cdp <- substr(as.character(match_poly$CLASSFP[1]), 1L, 1L) == "U"

  added <- data.frame(
    ori = cand$ori,
    agency_name = cand$agency_name,
    agency_type_name = cand$agency_type_name,
    agency_class = classify_place_agency(cand$agency_type_name),
    default_member = FALSE,
    place_name = x$place_name[1],
    county_name = cand$county_name,
    state_abbr = cand$state_abbr,
    attribution = "point_in_polygon",
    latitude = cand$latitude,
    longitude = cand$longitude,
    place_type = if (is_cdp) "cdp" else "incorporated",
    place_fips = as.character(match_poly$GEOID[1]),
    stringsAsFactors = FALSE
  )

  out <- rbind(x[, names(added), drop = FALSE], added)
  rownames(out) <- NULL
  out
}

# Embedded-tier agencies in a state, with usable coordinates.
.embedded_candidates <- function(state) {
  ag <- agencies_table()
  keep <- ag$agency_type_name %in% .EMBEDDED_TYPES &
    toupper(trimws(ag$state_abbr)) == toupper(trimws(state)) &
    !is.na(ag$latitude) & !is.na(ag$longitude)
  keep[is.na(keep)] <- FALSE
  ag[keep, , drop = FALSE]
}

# TRUE for each (lon, lat) falling inside any polygon of `polys`.
.points_in_polygon <- function(lon, lat, polys) {
  pts <- sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = 4326
  )
  polys <- sf::st_transform(polys, 4326)
  hits <- sf::st_within(pts, sf::st_union(sf::st_geometry(polys)))
  lengths(hits) > 0
}
```

- [ ] **Step 5: Document and run the tests**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test(filter = "place_spatial")'`
Expected: PASS, or SKIP on the PIP tests if `sf` is not installed. A skip is acceptable; a silent pass is not — if the PIP tests neither pass nor skip, something is wrong.

- [ ] **Step 6: Commit**

```bash
git add R/place_spatial.R DESCRIPTION man/ NAMESPACE tests/testthat/test-place_spatial.R
git commit -m "feat: add add_place_spatial_members() opt-in PIP attribution

Attributes campus/transit/airport agencies to a place by point-in-polygon
against Census place boundaries, behind sf + tigris in Suggests. Sheriffs
and state police are excluded outright: their HQ point says nothing about
jurisdiction. place_fips is surfaced as best-effort enrichment, explicitly
not a promised join key. A places_fun seam keeps tests offline."
```

---

### Task 5: Documentation, NEWS, and full verification

**Files:**
- Modify: `NEWS.md`
- Modify: `CLAUDE.md` (roadmap status)

- [ ] **Step 1: Add the NEWS entry**

Insert immediately below the `# fbi 0.1.0.9000 (development version)` heading in `NEWS.md`:

```markdown
## Place/municipal membership (v0.4)

- Added `place_agencies()` — a pure, offline resolver mapping a municipality to
  the agency that reports crime for it. CDE city agencies are named
  `"<Place> Police Department"`, so the place name is recovered from the agency
  name: ~100% of the 11,635 municipal-tier agencies resolve, and
  `(state, county, place)` is collision-free. No new dependencies.
- Added `get_place_crime_detail()` — itemized, unsummed place crime, reusing the
  county fan-out machinery (comparison-row stripping, `reported` flag,
  drop-warn-record partial-failure handling). Composes with
  `impute_reporting_gaps()`.
- Added `add_place_spatial_members()` — opt-in attribution of embedded campus
  and special-district agencies by point-in-polygon against Census place
  boundaries. Requires `sf` and `tigris` (both `Suggests`); returns its input
  unchanged with a message when they are absent. Adds `place_type`
  (`"incorporated"` / `"cdp"`) and a best-effort `place_fips` that is
  explicitly **not** a promised join key.
- Sheriffs, state police, and tribal agencies are never place members: a
  sheriff polices the unincorporated remainder, and contract cities report
  under their own city ORI, so a place's crime is carried entirely by its own
  agency.
```

- [ ] **Step 2: Update the roadmap in CLAUDE.md**

In the `## Roadmap` section, replace the `**v0.4 → 1.0**` bullet with:

```markdown
- **v0.4** — **shipped.** Place/municipal membership: `place_agencies()`,
  `get_place_crime_detail()`, and opt-in `add_place_spatial_members()`
  (`sf`/`tigris` in `Suggests`). Spec:
  `docs/superpowers/specs/2026-07-24-place-membership-v0.4-design.md`.
- **Metro (CBSA)** — union of member counties; needs a county→CBSA crosswalk
  and a delineation-vintage decision (Gitea #45).
- **Place FIPS** as a promised join key — likely a name-based crosswalk rather
  than `sf` (Gitea #46).
```

- [ ] **Step 3: Run the full test suite**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test()'`
Expected: 0 failures. Skips are acceptable only for `sf`-guarded and live-API tests.

- [ ] **Step 4: Run R CMD check**

Run: `R -q -e 'devtools::check(document = FALSE, args = c("--no-manual", "--as-cran"))'`
Expected: **0 errors, 0 warnings, 0 notes.**

Common causes if a note appears: a new file not covered by `.Rbuildignore`, an undocumented exported function, or a `Suggests` package used without a `requireNamespace()` guard.

- [ ] **Step 5: Commit**

```bash
git add NEWS.md CLAUDE.md
git commit -m "docs: NEWS and roadmap for v0.4 place membership"
```

---

## Self-Review

**Spec coverage:**

| Spec section | Task |
|---|---|
| §1 empirical findings (derivation rate, collision-freedom) | Task 1 Step 5 (drift guards) |
| §3 resolver, return shape, `attribution` | Task 2 |
| §3 sheriff/state/tribal exclusion | Task 2 (`classify_place_agency` → `NA`) |
| §3 ambiguous key errors with candidates | Task 2 |
| §3 empty result for a place with no agency | Task 2 (warning + typed empty frame) |
| §4 Layer 1 fan-out, reuse of county machinery | Task 3 |
| §4 no place-level aggregate | Task 3 (documented in the roxygen block) |
| §5 PIP confined to embedded tier | Task 4 (`.EMBEDDED_TYPES`) |
| §5 `place_type` incl. CDPs | Task 4 |
| §5 `place_fips` best-effort, unpromised | Task 4 |
| §5 degradation without `sf`/`tigris` | Task 4 |
| §6 offline tests, `places_fun` seam, non-vacuous | Tasks 1–4 |
| §7 non-goals (metro, place FIPS key, aggregate) | Not implemented, by design |

**Placeholder scan:** No TBD/TODO. Every code step carries runnable code.

**Type consistency:** `.PLACE_AGENCY_COLS` (Task 2) is a strict prefix of `.PLACE_DETAIL_COLS` minus the crime columns and plus/minus lat-long — verified: Task 3 assigns exactly the eight metadata columns the detail frame names, and Task 4 appends rows via `x[, names(added)]`, which requires `added` to carry every `.PLACE_AGENCY_COLS` name plus the two new ones. It does. `classify_place_agency()` is named identically in Tasks 2 and 4. `agencies_table()` is the mocked seam in Task 4 and the real accessor in Task 2.

**Known risk to watch during execution:** Task 4's `rbind(x[, names(added)], added)` requires `x` to already carry `place_type`/`place_fips` — it does, because they are assigned before the early returns. If a future edit moves those assignments below the polygon fetch, this breaks.
