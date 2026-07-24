# fbi 0.1.0.9000 (development version)

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
- The bundled agency table stores `latitude`/`longitude` as character columns,
  and 545 rows hold the literal string `"NULL"` rather than a real missing
  value. `add_place_spatial_members()` coerces and drops those defensively, so
  270 of the 2,324 embedded-tier agencies (11.6%) have no usable coordinates
  and can never be spatially attributed.
- Derived place names come from agency names (stripping a
  `"... Police Department"` suffix), not from Census place names, so they can
  disagree: e.g. the Las Vegas Metropolitan Police Department derives to
  `"Las Vegas Metropolitan"`, not the Census place `"Las Vegas"`. Agreement
  with Census naming is a separate, deferred concern.

## County aggregate and Census join (v0.3, Issues #36-#37)

- Added `get_county_crime()` — Layer 2 aggregate that sums itemized detail from
  [get_county_crime_detail()] into a county-wide series. Never emits a bare rate:
  every row carries the denominator value (`population`), which denominator was
  used (`denominator_type`: `"jurisdiction_pop"`, `"participated_pop"`, or
  `"census_pop"`), and a `coverage_fraction` (`participated_population /
  population`). The default `"jurisdiction_pop"` denominator sums each agency's
  own population column — empirically coherent with CDE semantics (sheriff =
  unincorporated remainder; contract cities report separately).
- Added `join_census_pop()` — optional Census ACS population join keyed by
  county FIPS. Requires `censusapi` (in Suggests) and a free Census API key
  (`CENSUS_KEY` env var). Returns detail with `census_population` column for use
  with `get_county_crime(detail, denominator = "census_pop")`.
- Added `county_fips` column to [county_agencies()] output, derived from the
  bundled FIPS crosswalk (98.6% match rate; see [county_to_fips()]).
- Added `county_to_fips()` and `counties_with_fips()` — public FIPS lookup
  functions backed by a tigris-derived crosswalk + hand-maintained patch table.

## Bug fixes

- Fixed `impute_reporting_gaps()` writing interpolated values to the wrong rows
  when `detail` was not already sorted by period within an agency. The function
  sorts each `(ori, offense)` group internally but mapped results back using the
  pre-sort row indices, so a genuinely reported row could be overwritten with an
  interpolated value and flagged `imputed = TRUE` while the real gap went
  unfilled.
- Fixed `join_census_pop()` calling `censusapi::get_acs()`, which does not
  exist — `get_acs()` is a **tidycensus** function. The join now uses
  `censusapi::getCensus()` with the correct argument shape (`name`, `vintage`,
  `vars`, `region`, `regionin`), issues one request per state (the API takes a
  single `regionin`), and builds the 5-digit key from the returned `state` and
  `county` columns with zero-padding. Previously every call failed, was swallowed
  by `tryCatch()`, and returned an all-`NA` `census_population` column.
- `join_census_pop()` now defaults to the ACS **5-year** release (`"acs/acs5"`,
  overridable via `dataset`); the 1-year release only covers geographies of
  65,000+ people, excluding most counties. The default `year` is now two years
  back, since the ACS release for year `Y` publishes in December of `Y + 1`.
- Fixed a truncated `@param denominator` entry and a missing examples section in
  `get_county_crime()` docs (the roxygen blocks used `#` instead of `#'`).
- Fixed two error messages that embedded literal newlines and source indentation.
- Fixed `.onLoad` namespace error: `setNames()` is in `stats`, not `base`.
- Moved FIPS crosswalk from `data/county_fips.rda` to internal `R/sysdata.rda`
  (was triggering "undocumented data set" warning).
- Fixed non-ASCII character in patch table (CRAN WARNING).

## Reporting-gap imputation (v0.2b, Issue #38)

- Added `impute_reporting_gaps()` — an opt-in, standalone transform that fills
  within-agency temporal holes in detail data. Operates on `rate` (so a drifting
  denominator is respected), then scales by each period's own
  `participated_population`. Every filled value is flagged with `imputed = TRUE`
  and `impute_method`. Agencies that never report in the window are left
  unchanged. Off by default — call explicitly when continuous series are needed.

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
- Caveat: agencies classed `state` or `tribal` (excluded by default) are
  attributed to a county by HQ location; when opted in via `agency_class`,
  their figures are statewide/jurisdiction-wide, not county-specific, and
  should be interpreted accordingly.

## Provenance (Issue #26)

- Package authorship and maintainer updated to Jared E. Knowles (Civilytics)
- Jacob Kaplan has stepped down as author/maintainer per his request
- License year and copyright holder updated to 2026 / Civilytics
- Repository URL updated to gitea.civilytics.org
- Package description now acknowledges the original fbi package by Jacob Kaplan

## Documentation Overhaul (Issue #13)

- Complete rewrite of README.Rmd with current API host, badges, usage examples, and function table of contents
- Set up pkgdown site configuration with categorized reference sections
- Updated DESCRIPTION metadata to reflect current CDE API usage
- Updated all `@source` URLs in data documentation to point to `cde.ucr.cjis.gov`
- Updated user agent string in `cde_request()` to reference new repository URL

## API Migration (Issues #6-#11)

- **Issue #6:** Lookups rewritten for CDE API -- `get_agencies()`, `get_offense_codes()`, `get_states()` now query the CDE API directly instead of the old API
- **Issue #7:** Summarized crime endpoints added -- `get_agency_crime()`, `get_estimated_crime()`, `get_estimated_arson()` for UCR Offenses Known and Clearances data
- **Issue #8:** Arrest endpoints added -- `get_arrest_count()`, `get_arrest_demographics()`, `get_arrest_demographics_all()` for UCR arrest statistics
- **Issue #9:** Police employment endpoint added -- `get_police_employment()` for staffing data
- **Issue #10:** NIBRS endpoints added -- `get_nibrs_victim()`, `get_nibrs_offender()`, `get_nibrs_offense()` for incident-level data
- **Issue #11:** SHR endpoint added -- `get_shr()` for Supplemental Homicide Reports
- **Issue #12:** LEOKA endpoints implemented -- `get_leoka()` and `get_leoka_monthly()` query the live `leoka/ytd` and `leoka/monthly` endpoints (discovered via the CDE web app's client bundle). National only; LEOKA data is only available from 2020 onward. Estimated crime was already covered by `get_estimated_crime()`/`get_estimated_arson()` (Issue #7), which use `summarized/{level}/{offense}` -- confirmed there is no separate estimates endpoint.
- **Issue #13:** Documentation overhaul (see above)
- **Issue #1:** Participation endpoints reimplemented -- the legacy `participation/...` paths are gone, and the `participation/*` namespace still present in the current API is scoped to Use-of-Force agency reporting, not general UCR/NIBRS participation (confirmed via client-bundle inspection and live probing). `get_agency_participation()`, `get_state_participation()`, and `get_region_participation()` now report live NIBRS-reporting status/rate sourced from `agency/byStateAbbr/{state}` in place of the retired year-by-year SRS/NIBRS series.
- **Issue #2:** Closed, not applicable -- no footnote endpoint exists in the current CDE API. Confirmed by exhaustively searching the CDE web app's client bundle (no "footnote" string anywhere) and probing `lookup/*`, whose only valid types are `states`, `offenses`, and `cde_properties`.

## Test Suite (Issue #29)

- `test-police_employment.R`'s live test replaced exact-value comparisons against golden CSVs recorded from the retired `api.usa.gov/crime/fbi/sapi` API (1985-2020) with shape-based assertions (columns, non-empty, non-negative) matching the style of the package's other live tests
- Confirmed live that `actuals` (employee counts) are suppressed upstream at every geography except agency-level -- not just national as previously documented; state- and region-level `get_police_employment()` correctly return 0 rows, now covered by an offline fixture (`pe-state-CA.json`) and a live test
- Removed the now-unreferenced `prep_ucr_crime_test()` helper and the 123 `*_police-employment-breakout.csv` golden files it read

## Test Suite (Issue #30)

- Re-recorded the summarized, arrest-counts, and police-employment offline fixtures from the live current CDE API (`summarized-agency-CA0010900-V.json`, `summarized-state-CA-V.json`, `summarized-national-V.json`, `summarized-national-ARS.json` -- renamed from `summarized-national-AR.json`, the real offense code is `ARS`, `arrest-agency-CA0010900-counts.json`, `arrest-national-all-counts.json`, `pe-agency-CA0010900.json`, `pe-national.json`); all previously encoded a synthetic/legacy shape and exercised only the package's `%||%` back-compat fallback path, not the real current API shape
- Discovered in the process that agency- and state-level `summarized`/`arrest` responses include comparison rows (state/national rates alongside the geography's own counts), which a full outer join surfaces as extra rows with a real `rate` but `NA` `count` -- documented in the relevant offline tests and reflected in updated `nrow`/value assertions
- Confirmed `pe-national.json`'s employee-count suppression matches the live API (see Issue #29) and updated its offline test accordingly, replacing the old non-suppressed synthetic fixture
- Did not switch `read_fixture()` to `simplifyVector = FALSE`: still blocked on the lookups parser, which turns out to have a live bug of its own -- `lookup/offenses?type=crime-trend` now returns a nested `crimeGroups` structure that `get_offense_codes()` does not handle (confirmed live; filed as Issue #32)

## Bug Fix (Issue #32)

- `get_offense_codes()` fixed to parse the current `lookup/offenses` response shape -- `{crimeGroups: [{label, crimes: [{label, value}]}]}` -- instead of the retired flat `{code: label}` dict. The old parser silently returned garbage against the live API (every row had `code = "crimeGroups"` with group/crime labels and codes jumbled into one column via `unlist()`). Verified across `type = "crime-trend"` (72 codes), `"arrest"` (48 codes), `"hate-crime"` (35 codes), and `"nibrs"` (empty, `crimeGroups: null`). `lookup-offenses.json` re-recorded from the live current shape.

## Known Issue (Issue #33)

- `get_nibrs_victim()`/`get_nibrs_offender()`/`get_nibrs_offense()` currently return no data from the live API -- confirmed not a parsing bug (the parser matches the documented contract exactly, and offline fixture tests with real synthetic data parse correctly), but `nibrs/{level}/{offense}?type=totals` returns an all-null payload for every offense/level/date-range tried, including nonsense offense strings, while the identical mechanism works fine for `get_arrest_demographics()`. These functions now emit a `message()` whenever they return 0 rows, pointing at Issue #33, so an empty result isn't mistaken for a genuine zero count while this is unresolved.

## Bug Fix (Issue #34)

- `get_states()` fixed to parse the current `lookup/states` response shape -- `{get_states: {cde_states_query: {states: [{abbr, name}]}}}` -- instead of the retired flat `{ABBR: "Name"}` dict. Discovered while dry-running the getting-started vignette; the old parser produced garbage rows (a literal `"get_states"` string and a run timestamp mixed in with real state data). `lookup-states.json` re-recorded from the live current shape; added a live-guarded test (previously `get_states()` had none).

## Vignette (Issue #31)

- Added `vignettes/fbi.Rmd` (source: `fbi.Rmd.orig`, rendered with live data and committed per the pre-rendered-vignette pattern used by `httr2`/`gargle`, so CRAN/CI never need network access to build it) covering lookups, summarized crime, arrests (including the offense-count vs. all-offenses-only demographics distinction), NIBRS (documented as currently non-functional, see Issue #33), SHR, police employment (including the counts-suppressed-above-agency-level limitation), NIBRS participation, and LEOKA
- Examples span six agencies across six states (CA, TX, NY, WA, FL, MA) to show real coverage rather than repeating a single agency
- Added `knitr`/`rmarkdown` to `Suggests` and `VignetteBuilder: knitr` to DESCRIPTION

## Architecture

- `cde_request()` established as the single network seam for all API calls
- `cde_base_url()`, `cde_path()`, and `cde_query()` provide composable URL building
- No API key required for the current CDE host (`cde.ucr.cjis.gov`)
- All date parameters use MM-YYYY format except police employment (YYYY)
