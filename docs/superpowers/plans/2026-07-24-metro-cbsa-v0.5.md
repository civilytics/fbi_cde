# Metro (CBSA) Geography (v0.5) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add metro (CBSA) level agency membership and crime detail to the `fbi` package, where a metro is the union of its member counties' agency sets.

**Architecture:** A bundled county→CBSA crosswalk derived from the public-domain 2023 OMB delineation file, a pure resolver that unions `county_agencies()` across member counties, and a guarded fan-out that refuses to issue hundreds of requests without an explicit opt-in.

**Tech Stack:** Base R (>= 3.5.0). Imports stay `httr`, `jsonlite`, `datasets`, `utils`. `readxl` is used **only** in `data-raw/` (not a package dependency — `data-raw/` is in `.Rbuildignore`).

**Spec:** `docs/superpowers/specs/2026-07-24-metro-cbsa-v0.5-design.md`

## Global Constraints

- **All HTTP goes through `cde_request()`** (`R/http.R`). Never call `httr` directly. Compose paths with `cde_path()`.
- **No new package dependencies.** `readxl` is a `data-raw/` build-time tool only; it must not appear in DESCRIPTION.
- **R >= 3.5.0.** No native pipe (`|>`), no `\(x)` lambda, no base `%||%` (use the package-local one in `R/utils.R`).
- **Zero-row frames are built column-by-column with types matching a populated result** — never from `matrix(nrow = 0, ...)`, whose default mode is `logical`. See `.empty_place_agency_frame()` in `R/place.R` for the pattern and issue #48 for why.
- **Every data path gets an offline test that runs on CI**, plus a `skip_if_no_fbi_api()`-guarded live test where it touches the network.
- **No vacuous tests.** A test that iterates a collection must assert the collection is non-empty. A test whose only assertions sit inside an `if` must use `skip_if_not_installed()` so a skip is visible.
- **`R CMD check` must stay clean:** 0 errors, 0 warnings, 0 notes.
- **Conventional commits.** Record user-facing changes in `NEWS.md`.

## Critical hazard: `R/sysdata.rda`

`R/sysdata.rda` holds **exactly one** object today: `crosswalk` (county FIPS, 3,733 rows). `save()` overwrites the entire file. A build script that saves only the new object **silently destroys the county FIPS crosswalk**, breaking `county_to_fips()`, `county_agencies()`, and everything above them — with no error at build time.

Task 1 must load the existing objects, add the new one, and re-save **all** of them, then assert both are present. Task 1's tests assert it again from the installed package.

## File Structure

| File | Responsibility |
|---|---|
| `data-raw/cbsa_crosswalk.R` (create) | Build the county→CBSA crosswalk into `R/sysdata.rda` |
| `R/cbsa.R` (create) | Crosswalk accessors + `list_metros()` |
| `R/metro.R` (create) | `metro_agencies()` resolver |
| `R/metro_crime.R` (create) | `get_metro_crime_detail()` guarded fan-out |
| `tests/testthat/test-cbsa.R` (create) | Crosswalk integrity + `list_metros()` |
| `tests/testthat/test-metro.R` (create) | Resolver tests |
| `tests/testthat/test-metro_crime.R` (create) | Fan-out + guard tests |
| `tests/testthat/test-empty-frames.R` (modify) | Extend to the two new frames |
| `NEWS.md`, `CLAUDE.md` (modify) | Document v0.5 |

---

### Task 1: County→CBSA crosswalk data asset

**Files:**
- Create: `data-raw/cbsa_crosswalk.R`
- Modify: `R/sysdata.rda` (regenerated)
- Create: `R/cbsa.R`
- Create: `tests/testthat/test-cbsa.R`

**Interfaces:**
- Produces: internal data object `cbsa_crosswalk` (data.frame) in `R/sysdata.rda`; `.cbsa_table()` accessor in `R/cbsa.R`; `CBSA_VINTAGE` constant.

- [ ] **Step 1: Write the build script**

Create `data-raw/cbsa_crosswalk.R`:

```r
# ============================================================================
# data-raw/cbsa_crosswalk.R
# ============================================================================
# Build the county -> CBSA crosswalk from the public-domain OMB/Census
# delineation file (OMB Bulletin 23-01, the 2023 vintage).
#
# Run with: Rscript data-raw/cbsa_crosswalk.R
#
# Output: R/sysdata.rda
#
# WARNING: R/sysdata.rda holds MULTIPLE internal objects and save() overwrites
# the whole file. This script loads the existing objects, adds its own, and
# re-saves everything. Saving only `cbsa_crosswalk` would silently destroy the
# county FIPS `crosswalk` and break county_to_fips() and all of geography.
# ============================================================================

CBSA_VINTAGE <- 2023L
url <- paste0(
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/",
  "reference-files/2023/delineation-files/list1_2023.xlsx"
)

tmp <- tempfile(fileext = ".xlsx")
utils::download.file(url, tmp, mode = "wb", quiet = TRUE)

raw <- readxl::read_excel(tmp, skip = 2)
names(raw) <- c(
  "cbsa_code", "md_code", "csa_code", "cbsa_title", "cbsa_type_raw",
  "md_title", "csa_title", "county", "state_name", "st_fips", "cty_fips",
  "central_outlying"
)

# Drop the trailing footnote rows the file carries below the data block.
raw <- raw[!is.na(raw$cbsa_code) & !is.na(raw$st_fips), , drop = FALSE]

cbsa_crosswalk <- data.frame(
  cbsa_code = as.character(raw$cbsa_code),
  cbsa_title = as.character(raw$cbsa_title),
  cbsa_type = ifelse(
    grepl("^Metropolitan", raw$cbsa_type_raw), "metro", "micro"
  ),
  csa_code = as.character(raw$csa_code),
  csa_title = as.character(raw$csa_title),
  md_code = as.character(raw$md_code),
  md_title = as.character(raw$md_title),
  county_fips = paste0(
    formatC(as.integer(raw$st_fips), width = 2L, flag = "0"),
    formatC(as.integer(raw$cty_fips), width = 3L, flag = "0")
  ),
  central_outlying = as.character(raw$central_outlying),
  stringsAsFactors = FALSE
)

attr(cbsa_crosswalk, "vintage") <- CBSA_VINTAGE

# ---- Assertions: fail the build loudly rather than shipping a bad asset ----
stopifnot(
  nrow(cbsa_crosswalk) > 1800,
  all(nchar(cbsa_crosswalk$county_fips) == 5L),
  !any(is.na(cbsa_crosswalk$cbsa_title)),
  all(cbsa_crosswalk$cbsa_type %in% c("metro", "micro")),
  # Titles must be unique per code, and codes unique per title.
  length(unique(cbsa_crosswalk$cbsa_code)) ==
    length(unique(cbsa_crosswalk$cbsa_title))
)

# ---- Re-save ALL internal objects (see the WARNING above) -----------------
existing <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = existing)
cat("existing sysdata objects:", paste(ls(existing), collapse = ", "), "\n")

crosswalk <- get("crosswalk", envir = existing)
stopifnot(is.data.frame(crosswalk), nrow(crosswalk) > 3000)

save(crosswalk, cbsa_crosswalk,
     file = "R/sysdata.rda", compress = "bzip2", version = 2)

cat("wrote R/sysdata.rda with", nrow(crosswalk), "county FIPS rows and",
    nrow(cbsa_crosswalk), "CBSA rows\n")
```

- [ ] **Step 2: Run the build script**

Run: `cd /home/jared/Nextcloud/Civilytics/Code/Civilytics/fbiCDE && Rscript data-raw/cbsa_crosswalk.R`

Expected: it prints the existing sysdata objects (should include `crosswalk`), then confirms it wrote both. If `crosswalk` is missing from the existing file, STOP — do not write, and report.

- [ ] **Step 3: Write the failing test**

Create `tests/testthat/test-cbsa.R`:

```r
# ---- Crosswalk integrity ---------------------------------------------------
#
# These assert the bundled asset itself. If the delineation file or the build
# script drifts, CI fails loudly rather than results changing silently.

test_that("sysdata still carries BOTH internal crosswalks", {
  # Regression guard: R/sysdata.rda holds multiple objects and save()
  # overwrites wholesale, so a careless rebuild can destroy the county FIPS
  # table. Both must survive.
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
```

- [ ] **Step 4: Run to verify it fails**

Run: `R -q -e 'devtools::test(filter = "cbsa")'`
Expected: FAIL — `could not find function ".cbsa_table"`.

- [ ] **Step 5: Write the implementation**

Create `R/cbsa.R`:

```r
# County -> CBSA crosswalk accessors and metro discovery.
#
# The crosswalk is derived at build time from the public-domain OMB/Census
# delineation file and shipped in R/sysdata.rda. See data-raw/cbsa_crosswalk.R.

#' The OMB delineation vintage the bundled CBSA crosswalk was built from
#'
#' CBSA definitions are revised periodically and counties move between metros,
#' so results are only reproducible against a stated vintage.
#'
#' @format An integer scalar.
#' @export
CBSA_VINTAGE <- 2023L

# Internal accessor so the crosswalk source is swappable in tests.
.cbsa_table <- function() {
  cbsa_crosswalk
}

#' List the metropolitan and micropolitan statistical areas
#'
#' Returns every CBSA in the bundled crosswalk, with how many counties each
#' contains. This is the discovery counterpart to [metro_agencies()]: it answers
#' "what may I pass as `metro`?"
#'
#' @param type Optionally filter to `"metro"` (Metropolitan Statistical Areas)
#'   or `"micro"` (Micropolitan Statistical Areas). `NULL` (default) returns
#'   both.
#' @return A data.frame with columns `cbsa_code`, `cbsa_title`, `cbsa_type`, and
#'   `n_counties`, carrying the delineation vintage in `attr(x, "vintage")`.
#' @seealso [metro_agencies()], [counties_with_fips()] for the county analogue.
#' @export
#' @examples
#' head(list_metros(type = "metro"))
list_metros <- function(type = NULL) {
  if (!is.null(type) && !type %in% c("metro", "micro")) {
    stop("'type' must be \"metro\", \"micro\", or NULL", call. = FALSE)
  }

  cw <- .cbsa_table()
  if (!is.null(type)) {
    cw <- cw[cw$cbsa_type == type, , drop = FALSE]
  }

  counts <- table(cw$cbsa_code)
  keys <- !duplicated(cw$cbsa_code)

  out <- data.frame(
    cbsa_code = cw$cbsa_code[keys],
    cbsa_title = cw$cbsa_title[keys],
    cbsa_type = cw$cbsa_type[keys],
    n_counties = as.integer(counts[cw$cbsa_code[keys]]),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$cbsa_title), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "vintage") <- CBSA_VINTAGE
  out
}
```

- [ ] **Step 6: Document and run the tests**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test(filter = "cbsa")'`
Expected: PASS.

Note `utils::globalVariables("cbsa_crosswalk")` may be needed to satisfy `R CMD check`'s global-variable analysis; `R/geography.R` does the same for `fbi_api_agencies`. Add it in `R/cbsa.R` if check complains.

- [ ] **Step 7: Verify the county FIPS crosswalk still works**

Run: `R -q -e 'pkgload::load_all(quiet = TRUE); print(county_to_fips("CA", "LOS ANGELES")); print(nrow(county_agencies("Alameda", "CA")))'`
Expected: `"06037"` and a non-zero row count. If either fails, the rebuild destroyed the county crosswalk — STOP and report.

- [ ] **Step 8: Commit**

```bash
git add data-raw/cbsa_crosswalk.R R/cbsa.R R/sysdata.rda man/ NAMESPACE tests/testthat/test-cbsa.R
git commit -m "feat: add county-to-CBSA crosswalk and list_metros()

Derived at build time from the public-domain 2023 OMB/Census delineation
file (Bulletin 23-01) and bundled in R/sysdata.rda, following the county
FIPS crosswalk precedent. 935 CBSAs over 1,915 county rows, covering 61%
of known counties - rural counties belong to no CBSA by construction.

The build script re-saves every internal object, because sysdata.rda holds
more than one and save() overwrites wholesale; a test asserts both survive."
```

---

### Task 2: `metro_agencies()` resolver

**Files:**
- Create: `R/metro.R`
- Test: `tests/testthat/test-metro.R`

**Interfaces:**
- Consumes: `.cbsa_table()`, `CBSA_VINTAGE` (Task 1); `county_agencies()` and `.empty_county_agency_frame()` (existing, `R/geography.R`); `rbind_fill()` (`R/utils.R`).
- Produces: `metro_agencies(metro, state = NULL)`; `.METRO_AGENCY_COLS`; `.empty_metro_agency_frame()`; `.resolve_cbsa(metro, state)` (internal, returns the matched crosswalk rows).

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-metro.R`:

```r
# ---- .resolve_cbsa() -------------------------------------------------------

test_that("an exact CBSA title resolves", {
  out <- .resolve_cbsa("Pittsburgh, PA", NULL)
  expect_gt(nrow(out), 1L)                       # Pittsburgh spans counties
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
})

test_that("exact title matching is case- and whitespace-insensitive", {
  a <- .resolve_cbsa("Pittsburgh, PA", NULL)
  b <- .resolve_cbsa("  pittsburgh, pa  ", NULL)
  expect_equal(a$county_fips, b$county_fips)
})

test_that("an unambiguous short name resolves", {
  out <- .resolve_cbsa("Pittsburgh", NULL)
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
})

test_that("an ambiguous short name errors listing the candidates", {
  # Albany is a CBSA name in GA, OR and NY.
  err <- tryCatch(.resolve_cbsa("Albany", NULL), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "ambiguous")
  expect_match(conditionMessage(err), "Albany")
})

test_that("state disambiguates an ambiguous short name", {
  out <- .resolve_cbsa("Albany", "OR")
  expect_equal(unique(out$cbsa_title), "Albany, OR")
})

test_that("an unknown metro resolves to zero rows", {
  expect_equal(nrow(.resolve_cbsa("Nowhere Metro", NULL)), 0L)
})

# ---- metro_agencies() ------------------------------------------------------

test_that("metro_agencies unions agencies across member counties", {
  out <- metro_agencies("Pittsburgh, PA")

  expect_s3_class(out, "data.frame")
  # Measured against the real data: Pittsburgh, PA is 8 counties / 336
  # agencies. Asserting a floor rather than the exact count leaves room for
  # CDE agency-table updates without making the test meaningless.
  expect_gt(nrow(out), 300L)
  expect_equal(length(unique(out$county_fips)), 8L)
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
  expect_equal(unique(out$cbsa_type), "metro")
  expect_true(all(nchar(out$county_fips) == 5L))
})

test_that("metro_agencies returns the documented columns in order", {
  out <- metro_agencies("Pittsburgh, PA")
  expect_equal(names(out), .METRO_AGENCY_COLS)
})

test_that("metro_agencies preserves county-level classification semantics", {
  out <- metro_agencies("Pittsburgh, PA")
  expect_true(all(out$agency_class %in%
    c("county_primary", "municipal", "campus", "state", "tribal", "special")))
  # A metro is a set of whole counties, so sheriffs belong and are default.
  expect_true(any(out$agency_class == "county_primary"))
  expect_true(all(out$default_member[out$agency_class == "county_primary"]))
})

test_that("metro_agencies carries central/outlying from the delineation", {
  out <- metro_agencies("Pittsburgh, PA")
  expect_true(all(out$central_outlying %in% c("Central", "Outlying")))
  expect_true(any(out$central_outlying == "Central"))
})

test_that("metro_agencies warns and returns a typed empty frame for an unknown metro", {
  expect_warning(out <- metro_agencies("Nowhere Metro"), "No CBSA")
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_AGENCY_COLS)
})

test_that("metro_agencies resolves a single-county micro area", {
  out <- metro_agencies("Aberdeen, WA")
  expect_gt(nrow(out), 0L)
  expect_equal(unique(out$cbsa_type), "micro")
  expect_equal(length(unique(out$county_name)), 1L)
})

test_that("a Connecticut metro warns rather than returning silently empty", {
  # The 2023 delineation uses CT planning regions (09110-09190); the CDE
  # reports traditional CT counties (09001-09015). They do not join, so all
  # five CT metros resolve to nothing. That MUST be loud: a quiet zero-row
  # frame would read as "no agencies report in Hartford", which is false.
  expect_warning(
    out <- metro_agencies("Hartford-West Hartford-East Hartford, CT"),
    "Connecticut"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_AGENCY_COLS)
})

test_that("partial county coverage warns with the counts", {
  expect_warning(
    metro_agencies("New Haven, CT"),
    "counties but only"
  )
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `R -q -e 'devtools::test(filter = "metro")'`
Expected: FAIL — `could not find function ".resolve_cbsa"`.

- [ ] **Step 3: Write the implementation**

Create `R/metro.R`:

```r
# Layer 0 of the metro geography model: the CBSA membership resolver.
# Pure, no network. A metro is the union of its member counties' agency sets,
# so this delegates to county_agencies() and adds the CBSA columns.

.METRO_AGENCY_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "county_fips", "latitude", "longitude",
  "cbsa_code", "cbsa_title", "cbsa_type", "central_outlying"
)

# A 0-row frame whose column types match a populated result. Built
# column-by-column, not from matrix(nrow = 0, ...) — see issue #48.
.empty_metro_agency_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    default_member = logical(0),
    county_name = character(0),
    state_abbr = character(0),
    county_fips = character(0),
    latitude = character(0),
    longitude = character(0),
    cbsa_code = character(0),
    cbsa_title = character(0),
    cbsa_type = character(0),
    central_outlying = character(0),
    stringsAsFactors = FALSE
  )
}

# Resolve a user-supplied metro name to its crosswalk rows.
#
# Exact title first (unique nationally), then the short name before the first
# comma or dash. An ambiguous short name errors listing candidates rather than
# guessing. Returns 0 rows when nothing matches.
.resolve_cbsa <- function(metro, state = NULL) {
  cw <- .cbsa_table()
  key <- toupper(trimws(metro))

  hit <- cw[toupper(cw$cbsa_title) == key, , drop = FALSE]
  if (nrow(hit) > 0) {
    return(hit)
  }

  # Short-name path: match the portion before the first comma or dash.
  short <- toupper(trimws(sub("[-,].*$", "", cw$cbsa_title)))
  hit <- cw[short == key, , drop = FALSE]

  if (nrow(hit) > 0 && !is.null(state)) {
    # CBSA titles end in a comma-separated list of state abbreviations.
    st <- toupper(trimws(state))
    suffix <- toupper(sub("^.*,\\s*", "", hit$cbsa_title))
    keep <- vapply(strsplit(suffix, "-", fixed = TRUE),
                   function(z) st %in% trimws(z), logical(1))
    hit <- hit[keep, , drop = FALSE]
  }

  titles <- unique(hit$cbsa_title)
  if (length(titles) > 1) {
    stop("Metro '", metro, "' is ambiguous: it matches ", length(titles),
         " CBSAs (", paste(titles, collapse = ", "),
         "). Pass the full title, or disambiguate with state = \"",
         sub("^.*,\\s*", "", titles[1]), "\".", call. = FALSE)
  }

  hit
}

#' List the law-enforcement agencies in a metropolitan area
#'
#' Resolves a Core Based Statistical Area (CBSA) to the union of its member
#' counties' agency sets. A metro is a set of *whole counties*, so this simply
#' stacks [county_agencies()] across them — `agency_class` and `default_member`
#' keep exactly the meaning they have at county level, including that a county
#' sheriff is a default member.
#'
#' CBSA titles are unique nationally, so `state` is only needed to disambiguate
#' a short name shared by several metros (`"Albany"` matches GA, OR and NY).
#'
#' @param metro A CBSA title (e.g. `"Pittsburgh, PA"`), or a short name
#'   (`"Pittsburgh"`). See [list_metros()] for the available values.
#' @param state Optional two-letter state abbreviation, used only to
#'   disambiguate an ambiguous short name.
#' @return A data.frame with the [county_agencies()] columns plus `cbsa_code`,
#'   `cbsa_title`, `cbsa_type` (`"metro"`/`"micro"`), and `central_outlying`
#'   (whether the agency's county is central or outlying in the CBSA). Returns a
#'   zero-row frame with a warning when the metro is unknown.
#' @seealso [list_metros()] to discover metro names,
#'   [get_metro_crime_detail()] for the crime series, [county_agencies()].
#' @export
#' @examples
#' \dontrun{
#' metro_agencies("Pittsburgh, PA")
#' }
metro_agencies <- function(metro, state = NULL) {
  hit <- .resolve_cbsa(metro, state)

  if (nrow(hit) == 0) {
    warning("No CBSA matches '", metro,
            "'. See list_metros() for available metros.", call. = FALSE)
    return(.empty_metro_agency_frame())
  }

  fips_to_county <- .cbsa_county_lookup(hit$county_fips)

  parts <- lapply(seq_len(nrow(hit)), function(i) {
    key <- fips_to_county[[hit$county_fips[i]]]
    if (is.null(key)) {
      return(NULL)
    }
    ag <- suppressWarnings(county_agencies(key$county_name, key$state_abbr))
    if (nrow(ag) == 0) {
      return(NULL)
    }
    ag$cbsa_code <- hit$cbsa_code[i]
    ag$cbsa_title <- hit$cbsa_title[i]
    ag$cbsa_type <- hit$cbsa_type[i]
    ag$central_outlying <- hit$central_outlying[i]
    ag
  })

  # Coverage must never be silent. A CBSA whose counties do not join our
  # crosswalk would otherwise return a quiet zero-row frame, which reads as
  # "no agencies report here" — false, and materially misleading. The live case
  # is Connecticut: the 2023 delineation uses planning regions (09110-09190)
  # while the CDE reports traditional counties (09001-09015), so all five CT
  # metros resolve to nothing.
  resolved <- sum(hit$county_fips %in% names(fips_to_county))
  if (resolved < nrow(hit)) {
    warning("Metro '", hit$cbsa_title[1], "' lists ", nrow(hit),
            " counties but only ", resolved,
            " could be matched to CDE county names",
            if (any(substr(hit$county_fips, 1L, 2L) == "09")) {
              paste0(". Connecticut is delineated by planning regions, which ",
                     "the CDE does not use, so its metros are not supported")
            } else {
              ""
            },
            ". Results are incomplete.", call. = FALSE)
  }

  out <- rbind_fill(parts)
  if (is.null(out) || nrow(out) == 0) {
    return(.empty_metro_agency_frame())
  }

  out <- out[, .METRO_AGENCY_COLS, drop = FALSE]
  rownames(out) <- NULL
  out
}

# Map 5-digit county FIPS back to the (county_name, state_abbr) pair
# county_agencies() takes, using the bundled county FIPS crosswalk.
#
# The crosswalk is keyed by the CDE's raw county_name, which includes
# multi-county strings ("FAIRFIELD; LICKING"), so county_fips is NOT unique
# across all rows. Filtering to names without a semicolon gives the canonical
# entry: 3,131 such rows for 3,131 distinct FIPS — an exact 1:1, with every
# FIPS represented. Skipping that filter would sometimes pick a multi-county
# row, and county_agencies() matches county_name exactly, so it would return a
# subset of the county rather than the county.
.cbsa_county_lookup <- function(fips) {
  cw <- crosswalk
  sel <- cw[!is.na(cw$county_fips) &
              !grepl(";", cw$county_name, fixed = TRUE) &
              cw$county_fips %in% fips, , drop = FALSE]
  sel <- sel[!duplicated(sel$county_fips), , drop = FALSE]
  stats::setNames(
    lapply(seq_len(nrow(sel)), function(i) {
      list(county_name = sel$county_name[i], state_abbr = sel$state_abbr[i])
    }),
    sel$county_fips
  )
}
```

- [ ] **Step 4: Document and run the tests**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test(filter = "metro")'`
Expected: PASS.

If a test's expected agency count or county count is wrong for the real data, report the actual value rather than loosening the assertion to something meaningless — these numbers are the point.

- [ ] **Step 5: Commit**

```bash
git add R/metro.R man/ NAMESPACE tests/testthat/test-metro.R
git commit -m "feat: add metro_agencies() CBSA membership resolver

A metro is the union of its member counties' agency sets, so this stacks
county_agencies() across them and keeps agency_class/default_member
semantics identical at every geographic level.

CBSA titles are unique nationally, so no state argument is required; it
exists only to disambiguate a short name shared by several metros, which
errors listing candidates rather than guessing."
```

---

### Task 3: `get_metro_crime_detail()` guarded fan-out

**Files:**
- Create: `R/metro_crime.R`
- Test: `tests/testthat/test-metro_crime.R`

**Interfaces:**
- Consumes: `metro_agencies()`, `.METRO_AGENCY_COLS` (Task 2); `parse_agency_detail()`, `enumerate_periods()` (`R/county_crime.R`); `cde_validate_dates()`, `cde_path()`, `cde_request()` (`R/http.R`); `rbind_fill()`.
- Produces: `get_metro_crime_detail(metro, state = NULL, offense = "V", from = "01-2015", to = "12-2020", agency_class = NULL, default_only = TRUE, max_agencies = 150, progress = TRUE)`; `.METRO_DETAIL_COLS`; `.empty_metro_detail_frame()`.

**The guard is the point of this task.** New York is 459 agencies, each one sequential request. The guard must fire **before any request is issued**, and a test must prove it by asserting the mocked `cde_request` was never called.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-metro_crime.R`:

```r
# A minimal agency response in the nested-list shape cde_request() returns.
make_metro_response <- function(name, counts, pop = 20000) {
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

# A two-agency metro membership frame, so the fan-out is exercised with N >= 2.
fake_metro_agencies <- function() {
  data.frame(
    ori = c("PA0000001", "PA0000002"),
    agency_name = c("Alpha PD", "Beta PD"),
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = c("ALLEGHENY", "BUTLER"),
    state_abbr = "PA",
    county_fips = c("42003", "42019"),
    latitude = "0", longitude = "0",
    cbsa_code = "38300",
    cbsa_title = "Pittsburgh, PA",
    cbsa_type = "metro",
    central_outlying = c("Central", "Outlying"),
    stringsAsFactors = FALSE
  )
}

# ---- The max_agencies guard ------------------------------------------------

test_that("the guard errors before issuing ANY request", {
  called <- 0L
  big <- fake_metro_agencies()[rep(1:2, 60), , drop = FALSE]   # 120 agencies
  big$ori <- sprintf("PA%07d", seq_len(nrow(big)))

  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) big,
    cde_request = function(...) {
      called <<- called + 1L
      stop("must not be reached")
    },
    .package = "fbiCDE"
  )

  expect_error(
    get_metro_crime_detail("Big Metro", max_agencies = 10,
                           from = "01-2021", to = "01-2021"),
    "max_agencies"
  )
  # The whole point of the guard: no network work happened.
  expect_equal(called, 0L)
})

test_that("the guard error names the metro and the agency count", {
  big <- fake_metro_agencies()[rep(1:2, 60), , drop = FALSE]
  big$ori <- sprintf("PA%07d", seq_len(nrow(big)))

  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) big,
    .package = "fbiCDE"
  )

  err <- tryCatch(
    get_metro_crime_detail("Big Metro", max_agencies = 10,
                           from = "01-2021", to = "01-2021"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "Big Metro")
  expect_match(conditionMessage(err), "120")
})

test_that("max_agencies = Inf disables the guard", {
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(...) make_metro_response("Alpha PD",
                                                    c("01-2021" = 5)),
    .package = "fbiCDE"
  )

  out <- get_metro_crime_detail("Pittsburgh, PA", max_agencies = Inf,
                                from = "01-2021", to = "01-2021",
                                progress = FALSE)
  expect_gt(nrow(out), 0L)
})

# ---- Fan-out ---------------------------------------------------------------

test_that("get_metro_crime_detail returns per-agency-period rows", {
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(path, query = list(), ...) {
      make_metro_response("Alpha PD", c("01-2021" = 10, "02-2021" = 12))
    },
    .package = "fbiCDE"
  )

  out <- get_metro_crime_detail("Pittsburgh, PA", from = "01-2021",
                                to = "02-2021", progress = FALSE)

  expect_equal(nrow(out), 4L)                       # 2 agencies x 2 periods
  expect_setequal(unique(out$ori), c("PA0000001", "PA0000002"))
  expect_equal(unique(out$cbsa_title), "Pittsburgh, PA")
  expect_setequal(unique(out$central_outlying), c("Central", "Outlying"))
  expect_equal(names(out), .METRO_DETAIL_COLS)
})

test_that("a failing ORI is dropped with a warning and recorded", {
  agencies <- fake_metro_agencies()
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(path, query = list(), ...) {
      if (grepl("PA0000002", path)) stop("503")
      make_metro_response("Alpha PD", c("01-2021" = 10))
    },
    .package = "fbiCDE"
  )

  expect_warning(
    out <- get_metro_crime_detail("Pittsburgh, PA", from = "01-2021",
                                  to = "01-2021", progress = FALSE),
    "Dropped"
  )
  expect_equal(nrow(out), 1L)                       # the surviving agency
  expect_equal(attr(out, "dropped"), "PA0000002")
})

test_that("an unknown metro returns a typed empty frame", {
  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) .empty_metro_agency_frame(),
    .package = "fbiCDE"
  )
  out <- get_metro_crime_detail("Nowhere", from = "01-2021", to = "01-2021",
                                progress = FALSE)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), .METRO_DETAIL_COLS)
})

test_that("get_metro_crime_detail validates the date range", {
  expect_error(
    get_metro_crime_detail("Pittsburgh, PA", from = "2021-01", to = "01-2021")
  )
})

# ---- Live API --------------------------------------------------------------

test_that("get_metro_crime_detail works against the live API", {
  skip_if_no_fbi_api()
  # A small micro area keeps the live fan-out cheap.
  out <- get_metro_crime_detail("Aberdeen, WA", from = "01-2019",
                                to = "03-2019", progress = FALSE)
  expect_s3_class(out, "data.frame")
  expect_equal(names(out), .METRO_DETAIL_COLS)
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `R -q -e 'devtools::test(filter = "metro_crime")'`
Expected: FAIL — `could not find function "get_metro_crime_detail"`.

- [ ] **Step 3: Write the implementation**

Create `R/metro_crime.R`:

```r
# Layer 1 of the metro geography model: itemized, unsummed metro-crime detail.
#
# Reuses parse_agency_detail() and mirrors the county fan-out structure. The
# loop is duplicated rather than shared because the column contracts differ;
# see R/place_crime.R for the same trade-off.
#
# Unlike its county and place siblings this one is guarded: a metro can be
# hundreds of agencies (New York is 459, each a sequential request), so an
# unbounded call would hang for minutes with no explanation.

.METRO_DETAIL_COLS <- c(
  "ori", "agency_name", "agency_type_name", "agency_class", "default_member",
  "county_name", "state_abbr", "county_fips",
  "cbsa_code", "cbsa_title", "cbsa_type", "central_outlying",
  "offense", "period", "count",
  "population", "participated_population", "rate", "reported"
)

.empty_metro_detail_frame <- function() {
  data.frame(
    ori = character(0),
    agency_name = character(0),
    agency_type_name = character(0),
    agency_class = character(0),
    default_member = logical(0),
    county_name = character(0),
    state_abbr = character(0),
    county_fips = character(0),
    cbsa_code = character(0),
    cbsa_title = character(0),
    cbsa_type = character(0),
    central_outlying = character(0),
    offense = character(0),
    period = character(0),
    count = numeric(0),
    population = numeric(0),
    participated_population = numeric(0),
    rate = numeric(0),
    reported = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Itemized crime detail for every agency in a metropolitan area
#'
#' Fans out one request per member agency across the metro's counties and
#' returns their crime series **unsummed** — one row per agency-period — with
#' coverage columns and the CBSA metadata.
#'
#' **This can be an expensive call.** A metro is the union of whole counties, so
#' the largest are very large: New York-Newark-Jersey City resolves to roughly
#' 459 agencies, Chicago 356. Every agency is one sequential request. The median
#' CBSA is only 7 agencies, so the cost is highly skewed — which is why
#' `max_agencies` refuses the large cases up front rather than letting them run
#' silently for minutes.
#'
#' No metro-level aggregate is provided. Summing across a metro raises the same
#' denominator question the county aggregate deferred, and a metro's is harder
#' (multi-state, mixed coverage).
#'
#' @inheritParams metro_agencies
#' @param offense Offense code (default `"V"`; see `get_offense_codes()`).
#' @param from,to Date range in `MM-YYYY` format.
#' @param agency_class Optional character vector; keep only these classes.
#' @param default_only If `TRUE` (the default), keep only default members
#'   (`county_primary` + `municipal`). Ignored if `agency_class` is supplied.
#' @param max_agencies Refuse to run if the filtered agency set is larger than
#'   this (default `150`), erroring **before any request is issued**. Set to
#'   `Inf` to disable, or narrow the set with `agency_class`.
#' @param progress If `TRUE` (the default here, unlike the county and place
#'   equivalents), print a progress line per agency. At metro scale silence is
#'   indistinguishable from a hang.
#' @return A data.frame with one row per agency-period. Agencies whose request
#'   or parse fails are dropped with a warning and recorded in
#'   `attr(x, "dropped")`.
#' @seealso [metro_agencies()], [list_metros()], [get_county_crime_detail()].
#' @export
#' @examples
#' \dontrun{
#' # A small micropolitan area is cheap.
#' get_metro_crime_detail("Aberdeen, WA", from = "01-2019", to = "12-2019")
#' }
get_metro_crime_detail <- function(metro, state = NULL, offense = "V",
                                   from = "01-2015", to = "12-2020",
                                   agency_class = NULL, default_only = TRUE,
                                   max_agencies = 150, progress = TRUE) {
  cde_validate_dates(from, to, "mm-yyyy")

  agencies <- metro_agencies(metro, state)
  if (!is.null(agency_class)) {
    agencies <- agencies[agencies$agency_class %in% agency_class, , drop = FALSE]
  } else if (isTRUE(default_only)) {
    agencies <- agencies[agencies$default_member, , drop = FALSE]
  }

  if (nrow(agencies) == 0) {
    return(.empty_metro_detail_frame())
  }

  # Guard BEFORE any request is issued.
  if (nrow(agencies) > max_agencies) {
    stop("Metro '", metro, "' resolves to ", nrow(agencies),
         " agencies, above max_agencies = ", max_agencies,
         ". Each agency is a separate request, so this would take a while. ",
         "Raise the limit (max_agencies = Inf), or narrow the set with ",
         "agency_class.", call. = FALSE)
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
    res$county_fips <- agencies$county_fips[i]
    res$cbsa_code <- agencies$cbsa_code[i]
    res$cbsa_title <- agencies$cbsa_title[i]
    res$cbsa_type <- agencies$cbsa_type[i]
    res$central_outlying <- agencies$central_outlying[i]
    parts[[i]] <- res
  }

  out <- rbind_fill(parts)
  if (is.null(out) || nrow(out) == 0 || ncol(out) == 0) {
    out <- .empty_metro_detail_frame()
  } else {
    out <- out[, .METRO_DETAIL_COLS, drop = FALSE]
    rownames(out) <- NULL
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

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test(filter = "metro")'`
Expected: PASS (both `test-metro.R` and `test-metro_crime.R`).

- [ ] **Step 5: Commit**

```bash
git add R/metro_crime.R man/ NAMESPACE tests/testthat/test-metro_crime.R
git commit -m "feat: add get_metro_crime_detail() guarded fan-out

Mirrors the county and place detail functions, plus CBSA metadata. Unlike
them it is guarded: New York resolves to ~459 agencies and each is a
sequential request, so max_agencies (default 150) errors before issuing
any request rather than hanging for minutes. progress defaults to TRUE
for the same reason. A test asserts no request is made when the guard
fires."
```

---

### Task 4: Empty-frame coverage, docs, and verification

**Files:**
- Modify: `tests/testthat/test-empty-frames.R`
- Modify: `NEWS.md`, `CLAUDE.md`

- [ ] **Step 1: Extend the empty-frame agreement tests**

Append to `tests/testthat/test-empty-frames.R`:

```r
# ---- metro_agencies() ------------------------------------------------------

test_that("metro_agencies empty frame agrees with its populated frame", {
  populated <- metro_agencies("Pittsburgh, PA")
  empty <- suppressWarnings(metro_agencies("Nowhere Metro"))

  expect_frames_agree(empty, populated)
})

# ---- get_metro_crime_detail() ----------------------------------------------

test_that("get_metro_crime_detail empty frame agrees with its populated frame", {
  agencies <- data.frame(
    ori = "PA0000001",
    agency_name = "Alpha PD",
    agency_type_name = "City",
    agency_class = "municipal",
    default_member = TRUE,
    county_name = "ALLEGHENY",
    state_abbr = "PA",
    county_fips = "42003",
    latitude = "0", longitude = "0",
    cbsa_code = "38300",
    cbsa_title = "Pittsburgh, PA",
    cbsa_type = "metro",
    central_outlying = "Central",
    stringsAsFactors = FALSE
  )
  response <- make_response("Alpha PD", c("01-2021" = 10, "02-2021" = 12))

  testthat::local_mocked_bindings(
    metro_agencies = function(metro, state = NULL) agencies,
    cde_request = function(path, query = list(), ...) response,
    .package = "fbiCDE"
  )

  populated <- get_metro_crime_detail("Pittsburgh, PA", from = "01-2021",
                                      to = "02-2021", progress = FALSE)
  expect_frames_agree(.empty_metro_detail_frame(), populated)
})
```

- [ ] **Step 2: Add the NEWS entry**

Insert immediately below the `# fbi 0.1.0.9000 (development version)` heading in `NEWS.md`:

```markdown
## Metro (CBSA) geography (v0.5, Issue #45)

- Added `metro_agencies()` — resolves a Core Based Statistical Area to the union
  of its member counties' agency sets. Because a metro is a set of *whole*
  counties, `agency_class` and `default_member` keep exactly their county-level
  meaning, including that a sheriff is a default member. Rows carry
  `cbsa_code`, `cbsa_title`, `cbsa_type` (`"metro"`/`"micro"`), and
  `central_outlying`.
- Added `get_metro_crime_detail()` — itemized, unsummed metro crime. **Guarded:**
  a metro can be hundreds of agencies (New York resolves to ~459, Chicago 356),
  each a sequential request, so `max_agencies` (default `150`) errors *before
  issuing any request* rather than hanging for minutes. Set `max_agencies = Inf`
  to override. `progress` defaults to `TRUE` here, unlike the county and place
  equivalents.
- Added `list_metros()` — the discovery counterpart: every CBSA with its county
  count, filterable by type.
- Added an internal county→CBSA crosswalk derived from the public-domain
  **2023** OMB/Census delineation file (Bulletin 23-01): 935 CBSAs (393
  metropolitan, 542 micropolitan) over 1,915 county rows. It covers 61% of known
  counties — rural counties belong to no CBSA by construction, which is a
  property of the delineation, not a gap in the data. The vintage is pinned and
  exposed as `CBSA_VINTAGE` and as an attribute on `list_metros()`, since CBSA
  definitions are revised periodically and counties move between metros.
- No metro-level aggregate. Summing across a metro raises the same denominator
  question the county aggregate deferred, and a metro's is harder (multi-state,
  mixed reporting coverage).
```

- [ ] **Step 3: Update CLAUDE.md**

In the `## Roadmap` section, add below the v0.4 entry:

```markdown
- **v0.5** — **shipped.** Metro (CBSA) geography: `metro_agencies()`,
  `get_metro_crime_detail()` (guarded by `max_agencies`), `list_metros()`, and
  a bundled 2023 OMB delineation crosswalk. Spec:
  `docs/superpowers/specs/2026-07-24-metro-cbsa-v0.5-design.md`.
```

Also note in the architecture section that `R/sysdata.rda` now holds **two** internal objects (`crosswalk`, `cbsa_crosswalk`) and that any build script touching it must re-save both.

- [ ] **Step 4: Run the full suite**

Run: `R -q -e 'devtools::document(quiet = TRUE); devtools::test()'`
Expected: 0 failures. Note any skips.

- [ ] **Step 5: Run R CMD check**

Run: `R -q -e 'devtools::check(document = FALSE, args = c("--no-manual", "--as-cran"))'`
Expected: **0 errors, 0 warnings, 0 notes.**

Likely causes if a note appears: `cbsa_crosswalk` needs `utils::globalVariables()`; a new exported object needs documentation; or `R/sysdata.rda` grew large enough to warrant compression tuning.

- [ ] **Step 6: Commit**

```bash
git add tests/testthat/test-empty-frames.R NEWS.md CLAUDE.md
git commit -m "docs: NEWS and roadmap for v0.5 metro geography"
```

---

## Self-Review

**Spec coverage:**

| Spec section | Task |
|---|---|
| §3 crosswalk asset, sysdata footgun, vintage | Task 1 (build script re-saves all; test asserts both objects) |
| §4 resolver, exact/short/ambiguous matching | Task 2 |
| §4 union of county_agencies, semantics preserved | Task 2 |
| §4 typed zero-row frames | Tasks 2, 3, and Task 4 agreement tests |
| §5 fan-out, `max_agencies` guard, `progress = TRUE` | Task 3 |
| §5 no metro aggregate | Not implemented, documented in roxygen and NEWS |
| §6 `list_metros()` discovery | Task 1 |
| §7 testing discipline incl. guard-fires-before-request | Tasks 1–4 |

**Placeholder scan:** No TBD/TODO; every code step carries runnable code.

**Type consistency:** `.METRO_AGENCY_COLS` (14) is the county agency columns (10) plus 4 CBSA columns. `.METRO_DETAIL_COLS` (19) drops `latitude`/`longitude`, keeps `county_fips` and the 4 CBSA columns, and adds the 7 series columns — Task 3's loop assigns exactly the 11 metadata columns not produced by `parse_agency_detail()`. `.resolve_cbsa()` is named identically in Tasks 2 and 3's mocks. `make_response()` in Task 4 is the helper already defined in `test-empty-frames.R` by #48.

**Known risk to watch:** Task 2's `.cbsa_county_lookup()` maps FIPS back to `(county_name, state_abbr)` via the county crosswalk. Counties present in the CBSA file but absent from our crosswalk yield `NULL` and are skipped silently. That is correct behaviour, but if a large metro loses many counties this way the result would be quietly incomplete — Task 2's test asserting Pittsburgh returns >100 agencies across >1 county is the tripwire.
