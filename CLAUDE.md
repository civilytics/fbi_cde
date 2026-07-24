# CLAUDE.md — `fbi`

R wrapper for the FBI Crime Data Explorer (CDE) API
(`https://cde.ucr.cjis.gov/LATEST/`). No API key required for the current host.
Targeting CRAN. Based on the original `fbi` package by Jacob Kaplan.

## Architecture & house rules (do not violate)

- **Single network seam.** *All* HTTP goes through `cde_request()` (`R/http.R`).
  Compose URLs with `cde_path()` / `cde_query()`. Never call `httr` directly
  elsewhere. Tests mock `cde_request()` — this is what makes the package testable
  offline.
- **Test discipline.** Every data path gets **both** an offline fixture test that
  runs on CI (mocks `cde_request` via `local_fbi_fixture()`; fixtures in
  `tests/testthat/fixtures/`) **and** a `skip_if_no_fbi_api()`-guarded live test.
  See `tests/testthat/helper-fbi.R`. `R CMD check` must stay clean.
- **Lean dependencies, base R.** `R >= 3.5.0`. Imports are only `httr`,
  `jsonlite`, `datasets`, `utils` — **`data.table` was removed**; new code is base
  R. Heavy deps (`sf`, `tidycensus`) must be justified and go in `Suggests`, not
  `Imports`.
- **Parse defensively — the CDE schema drifts.** Several lookup endpoints have
  silently changed shape. Use `%||%` for drift tolerance (e.g.
  `actuals %||% counts`). Agency- and state-level `summarized`/`arrest` responses
  include **comparison rows** (state + national series with a real `rate` but
  `NA` count) — strip them to the geography's own series.
- **`cde_request()` uses `simplifyVector = FALSE`** (nested lists). Index parsers
  with `names()` / `[[ ]]` so they work for both the live shape and fixtures.

## API shape & data facts (non-obvious)

- The CDE knows only three geographic levels: **national**, **state/{ABBR}**,
  **agency/{ORI}**. County/metro/place are *our* constructs (see the geography
  feature below).
- **ORIs are 9 chars; positions 3–9 may be letters** (all state police, tribal,
  university, and some city/contract ORIs, e.g. `CA001300X`). `is_valid_ori()`
  accepts `^[A-Z]{2}[A-Z0-9]{7}$`. Do not narrow it back to digits-only.
- Bundled `fbi_api_agencies` (~18,459 rows): has `county_name` (stored
  **UPPERCASE**), lat/long, `agency_type_name` — but **no FIPS** and no
  place/municipality codes. A county sheriff's `agency_type_name` is `County`.
- **Population semantics:** in `summarized` responses, a sheriff's `population` is
  the *unincorporated remainder* it polices, **not** the whole county; contract
  cities (e.g. LASD's Compton, West Hollywood) report under their **own** city
  ORI. So county-level population partitions cleanly (cities + sheriff-remainder).
  `population` vs `participated_population` is the reporting-coverage gap.
- Dates are `MM-YYYY` except police employment (`YYYY`).

## Geography feature (v0.2–v0.4, shipped)

County- and place-level agency membership + itemized crime, an *attribution*
model (which agencies are attributed to a geography — not spatial-truth).
Layers:
- `county_agencies(county, state)` — pure resolver; classifies each agency into
  `agency_class` (`county_primary`/`municipal`/`campus`/`state`/`tribal`/`special`)
  with a conservative `default_member` flag (`county_primary` + `municipal`).
- `get_county_crime_detail(...)` — fans out one request per member ORI, returns
  **unsummed** per-agency-period rows with coverage columns and a `reported` flag
  (distinguishes "reported 0" from "did not report" — critical for the 2021 CA
  NIBRS-transition reporting hole). Filters via `agency_class` / `default_only`;
  failed ORIs are dropped, warned, and listed in `attr(x, "dropped")`.
- `get_county_agency_crime(...)` — the county's own primary (sheriff) series.
- `place_agencies(place, state, county = NULL)` — pure, name-identity resolver
  for the municipal tier (`"<Place> Police Department"` names); classifies
  `agency_class` (`place_primary`/`campus`/`special`).
- `get_place_crime_detail(...)` — the place twin of `get_county_crime_detail()`;
  accepts an optional pre-resolved `agencies` frame (e.g. from
  `add_place_spatial_members()`) so campus/special members can be queried.
- `add_place_spatial_members(x, ...)` — opt-in, `sf`/`tigris`-backed
  point-in-polygon attribution of campus/special agencies to a place; appends
  rows with `attribution = "point_in_polygon"` to a `place_agencies()` result.

## Roadmap

Full design + phased plan: `docs/superpowers/specs/2026-07-13-geography-first-querying-design.md`.
Status: **v0.4 shipped** (county resolver + detail + aggregate; place
resolver + detail + spatial members). Planned phases (each its own
spec → plan → implementation cycle; tracked as Gitea issues):
- **v0.2b** — optional reporting-gap imputation (`impute_reporting_gaps()`, off by default).
- **v0.3** — Layer 2 aggregate (denominator model) + county FIPS crosswalk (derive
  from `county_name` + patch table; **do not ship LEAIC** — licensing/staleness)
  + Census join (`censusapi`, `Suggests`).
- **v0.4** — **shipped.** Place/municipal membership: `place_agencies()`,
  `get_place_crime_detail()`, and opt-in `add_place_spatial_members()`
  (`sf`/`tigris` in `Suggests`). Spec:
  `docs/superpowers/specs/2026-07-24-place-membership-v0.4-design.md`.
- **Metro (CBSA)** — union of member counties; needs a county→CBSA crosswalk
  and a delineation-vintage decision (Gitea #45).
- **Place FIPS** as a promised join key — likely a name-based crosswalk rather
  than `sf` (Gitea #46).
- **Capstone** — imputation gap: FBI published aggregate vs. sum of reported
  components (uses the `reported`/coverage columns already collected).

## Workflow

- **Issue-driven** on Gitea (`gitea.civilytics.org/Civilytics/fbi_cde`); default
  branch `main`. Conventional commits (`feat:`/`fix:`/`refactor:`/`test:`/`docs:`).
  Record changes in `NEWS.md`. CI: `.gitea/workflows/R-CMD-check.yaml`.
- **Known issue #33:** NIBRS demographic detail
  (`get_nibrs_victim/offender/offense`) returns no data from the live API — an
  upstream outage, not a parser bug.
