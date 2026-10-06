# fbiCDE 0.1.0.9000 (development version)

## Census codes for places (#46)

- `place_agencies()` and `get_place_crime_detail()` now carry the Census code
  of the unit each municipal agency polices, as a promised join key:
  `place_fips` (7 digits, state + place) for a city, town or village;
  `cousub_fips` (10 digits, state + county + subdivision) for a township or a
  New England or New York town, which are governments but not Census places;
  and `place_type` (`"incorporated"`, `"county_subdivision"` or `"cdp"`)
  saying which applies. 98.7% of the 11,646 municipal agencies resolve; the
  rest, mostly regional departments, get `NA` rather than a guess.
- Codes come from a bundled crosswalk built from the Census Bureau's 2020
  reference code files (`data-raw/place_fips_crosswalk.R`; vintage in the new
  `PLACE_VINTAGE`). An agency is matched by name only among Census units in
  its own county; agency coordinates are not used, being too unreliable.
  Design: `specs/2026-10-06-place-fips-design.md`.
- `add_place_spatial_members()` keeps the place agency's own codes instead of
  resetting them to `NA`; the `place_fips` it gives the campus and special
  agencies it adds is still best-effort (where the headquarters sits).
- `derive_place_name()` strips the ", <Name> County" that 146 Pennsylvania,
  Ohio, New Jersey and Michigan agencies carry to tell same-named townships
  apart. Those places were unreachable by name; now
  `place_agencies("Hamilton Township", "NJ", county = "Mercer")` finds its
  department.

## Documentation, distribution and clean-up

- **`get_police_employment()` returns state and national staffing.** It
  requested `pe/state/{ST}` and `pe/national`, which the CDE answers with
  every value null, so state and national calls returned no rows, and the
  package's own tests had recorded those empty answers as "suppressed
  upstream". The endpoint takes `pe/{ST}` for a state and plain `pe` for the
  nation (an agency is `pe/{ST}/{ORI}`). No region form returns data, so
  `region` is now an error. An empty result comes with a message.
- `get_police_employment()` adds `participated_population` and
  `employees_per_1000`. A state's counts are sums over the agencies that
  reported, so they move with coverage: Texas's employee count rose 39% from
  2018 to 2020 while its rate held near 3.4 per 1,000.
- Help pages render their markdown. The roxygen comments were written in
  markdown that the package never enabled, so the help showed backticks and
  `[fn()]` literally, and any text after a `%` was silently dropped (an
  unescaped `%` starts an Rd comment).
- The package is distributed through r-universe
  (<https://civilytics.r-universe.dev/fbiCDE>), not CRAN. DESCRIPTION's `URL`
  and `BugReports` point at the public GitHub repository, the maintainer
  address is real, and the README installs from r-universe. The version is
  now `0.1.0.9000`, development past the `v0.1.0` release r-universe builds,
  and this file separates the two.
- The pkgdown reference index lists every exported topic (place, metro,
  LEOKA and the county aggregate functions were missing).
- Removed four unexported helpers left over from the retired api.data.gov
  API (`make_state()`, `make_year()`, `clean_column_names()`,
  `combine_url_section()`).
- Design specs moved from `docs/superpowers/specs/` to `specs/`, and plans to
  `specs/plans/` (#61), out of pkgdown's output folder. A Claude Code hook
  (`.claude/settings.json`) and a Gitea CI step keep them from drifting back
  to the superpowers default.

## Vignettes

All three vignettes were re-run against the live API, and their prose now
reads its numbers from the results instead of hard-coding them.

- **Getting Started** (`vignette("fbi")`) was rewritten around the current
  interface: offense codes, the `comparison` argument, reporting coverage,
  arrest offense levels, NIBRS codes (with the error an offense name now
  gives), SHR's reporting gaps (Florida sent 9 homicides for 2019; Georgia
  484), and LEOKA counting
  officers killed, not assaulted.
- **Juvenile arrests** (`vignette("juvenile-arrests")`) corrected:
  - Reporting coverage comes from `participated_population / population`, not
    the share of agencies on NIBRS, which measured something else.
  - The offense ranking covers all 34 offense names at one level, instead of
    a hand-picked list that left out the largest ("All Other Offenses") and
    counted drugs through "Drug Abuse Violations" (880 Ohio arrests in 2023),
    which is only the remainder not classed as possession or sale. The drug
    total, "Drug/Narcotic Offenses", was 20,997.
  - The Columbus profile checks that all 12 months reported before using the
    annual total.
  - The metro fan-out includes campus police (the default classes), and
    records the agencies whose requests failed instead of dropping them
    silently.
  - Figure alt text is built from the data.
- **Counties, cities and metro areas** (`vignette("geography")`) is new. It
  covers membership and agency classes, `get_county_crime_detail()` and the
  `reported` flag through California's 2021 NIBRS transition, coverage in
  `get_county_crime()`, `impute_reporting_gaps()`, multi-county agencies,
  places, metro areas and the `max_agencies` guard.

## Reporting coverage and arrest offense levels

- `get_agency_crime()`, `get_estimated_crime()`, `get_estimated_arson()` and
  `get_arrest_count()` now return `population` and `participated_population`
  for each row, from the response's own populations map. Their ratio is the
  reporting coverage, so a state or national count can be read for what it
  is: the sum of the agencies that reported. (Pennsylvania's 2023 arrests
  cover 96% of its population even though only 17% of its agencies report
  through NIBRS, so NIBRS participation is not a coverage measure.) A
  single-offense arrest total has no populations; those columns are `NA`.
- `list_ucr_arrest_offenses(level = )` lists one level of the CDE's arrest
  names: `"name"` (34), `"category"` (29) or `"breakdown"` (49); the default
  `"all"` lists every name. Counts within a level do not overlap, so ranking
  offenses means ranking one level. The `ucr_arrest_offenses` dataset is now
  a data frame of `offense` and `level`.

## Fixes found by checking against the live API

- **NIBRS works; it never had an outage (#33).** The `nibrs/` endpoint takes
  short offense codes -- summary groups (`"V"`, `"P"`, `"ROB"`, `"BUR"`, ...)
  and NIBRS codes (`"13B"`, `"35A"`, `"120"`, ...) -- and answers anything else
  with an all-null payload. The package documented long names
  (`list_nibrs_offenses()` returned `"robbery"`,
  `"burglary-breaking-and-entering"`, ...) and defaulted to `"robbery"` and
  `"all"`, so every call with documented arguments returned nothing, which
  read as an upstream outage. Now:
  - `list_nibrs_offenses()` returns a data.frame of `code` and `label`.
  - `get_nibrs_victim()`, `get_nibrs_offender()` and `get_nibrs_offense()`
    default to `offense = "V"`, take codes case-insensitively, and reject an
    offense name or `"all"` before any request, suggesting the code
    (`"robbery"` -> `Did you mean "ROB" (Robbery)?`).
  - The variable lists now match the response: victim `age`, `ethnicity`,
    `location`, `race`, `relationship`, `sex`; offender `age`, `ethnicity`,
    `race`, `sex`; offense `related_offenses`, `weapons`. The old lists
    offered `count`, `bias` and five other variables that do not exist, and
    `count` was the offender and offense default. Defaults are now `"race"`
    (victim, offender) and `"weapons"` (offense); an unknown variable is an
    error.
  - The NIBRS test fixtures were hand-written, with variables the API never
    returns; they are replaced by recorded responses.
- **`get_arrest_count(offense = )` accepts every name the API reports.** It
  validated against the 34 offense names only, so the 29 categories and 49
  breakdowns were rejected -- including `"Drug/Narcotic Offenses"`, the only
  total of drug arrests. (`"Drug Abuse Violations"` is just the drug arrests
  not classed as possession or sale: 880 of Ohio's 20,997 in 2023.) Names are
  now checked against the response itself; `ucr_arrest_offenses` lists all 80.
- **`impute_reporting_gaps()` now fills real gaps.** It scaled interpolated
  rates by `participated_population`, which the CDE reports as missing for
  every month an agency did not report, so it filled nothing on real data
  (0 of 232 gaps in Alameda County, 2021). It now falls back to the agency's
  `population` in those months (39 of 184 interior gaps filled for Alameda,
  2020-2021).
- `get_leoka()` documents that its totals are officers *feloniously killed*
  (they match the FBI's published 46 in 2020 and 73 in 2021), not assaults.
- `R/data.R` defined its own copies of `nibrs_offenses`, `ucr_arrest_offenses`,
  `regions` and the NIBRS variable lists as package objects, a second, stale
  source of truth that unqualified references picked up. The bundled datasets
  are now the only copy, rebuilt from the live API by
  `data-raw/api_vocabularies.R`.
- **The NYPD, DC's police and the Baltimore City Sheriff are back in their
  counties.** The CDE gives these three agencies no county (`"N/A"`, or "NOT
  SPECIFIED" in its live directory), so `county_agencies("District of
  Columbia", "DC")` found nothing, Manhattan returned a SUNY campus and the
  State Police, and the New York and Washington metros lacked their largest
  department. The package now attributes them itself, by ORI, wherever the
  CDE leaves the county as `"N/A"`:
  - The NYPD goes to all five boroughs (Bronx, Kings, New York, Queens and
    Richmond counties). It reports one citywide series, so like any
    multi-county agency it counts in full in each: **a borough's results are
    New York City's**, with the city's population.
  - DC's Metropolitan Police goes to the District of Columbia, and the
    Baltimore City Sheriff to Baltimore city.

  The county FIPS crosswalk gains Queens (36081), Richmond (36085) and the
  District of Columbia (11001), which no agency named before
  (`data-raw/crosswalk_attributed_counties.R`). `fbi_api_agencies` itself
  still holds the CDE's values.

## Interface changes (breaking)

- **Comparison rows are stripped by default.** For an agency or state, the
  CDE also sends its state's and the nation's series (a rate but no count).
  `get_agency_crime()`, `get_estimated_crime()`, `get_estimated_arson()`,
  `get_arrest_count()` and `get_county_agency_crime()` returned them mixed in
  with the geography's own rows (an Oakland query gave 18 rows, 6 of them
  Oakland's), so `mean(rate)` or a plot by period silently mixed geographies.
  They now return only the queried geography's own series; `comparison = TRUE`
  brings the others back, labelled by new `series` (`"agency"`, `"state"`,
  `"national"`) and `series_name` columns.
- **New output columns for those functions:** `offense` is now the requested
  code (e.g. `"V"`, or `"all"` for arrests) rather than a series label such as
  `"Oakland Police Department Offenses"`, and a new `measure` column says
  `"offenses"`, `"clearances"` or `"arrests"`. Periods are sorted
  chronologically (they had been sorted as strings, which misorders across
  years).
- **`default_only` and `default_member` are gone; `include_statewide` replaces
  them.** `get_county_crime_detail()`, `get_place_crime_detail()` and
  `get_metro_crime_detail()` now query, by default, the agencies whose
  jurisdiction is a specific area below the state: sheriffs, city police and
  campus police. `include_statewide = TRUE` adds state agencies (state police
  and highway patrol, other state agencies). Special-purpose agencies
  (transit, school, airport, port, park and railroad police, task forces) and
  tribal agencies are queried only when named in `agency_class`. Membership is
  decided by agency type alone, never by parsing names. What changes:
  - County: previously every attributed agency was queried; special, state and
    tribal agencies are now opt-in.
  - Place and metro: campus police are now included by default (place: when
    supplied via `add_place_spatial_members()`).
  - `agency_class` values are validated; an unknown class is an error.
- `agency_class` corrections: the 11 `"Census Area"` agencies are Alaska city
  police departments (Nome, Bethel, ...) and are now `"municipal"` (and place
  members) instead of `"special"`; `"Other State Agency"` is now `"state"`
  instead of `"special"`.
- **`fbi_api_agencies` has real column types:** `nibrs` is logical,
  `latitude`/`longitude` numeric, `nibrs_start_date` a Date, with `NA` where
  the source had the string `"NULL"`. One placeholder coordinate (`-9, -9`) is
  now `NA`. The documentation now notes the table is a snapshot whose NIBRS
  start dates run to September 2019. The conversion is recorded in
  `data-raw/fbi_api_agencies_types.R`.
- Tribal agencies' attribution to counties (reservations often cross county
  lines) and the special-purpose class (single-site agencies mixed with
  multi-county task forces) are deferred for a later design.

## Network resilience and test infrastructure

- `cde_request()` now retries transient failures with exponential backoff
  (1, 2, 4 seconds, capped at 30): network errors and timeouts, and HTTP 408,
  429, 500, 502, 503 and 504. A `Retry-After` header in seconds is honoured
  (capped at 60); other statuses fail immediately, as before. Each attempt has
  a timeout. Previously a single dropped connection failed the call, and in a
  county or metro fan-out silently removed that agency from the totals. Tune
  with `options(fbiCDE.max_retries = 3)` (`0` disables retries) and
  `options(fbiCDE.timeout = 60)` (seconds).
- Offline fixtures are now parsed exactly as `cde_request()` parses live
  responses (`simplifyVector = FALSE`). They used to be simplified to
  data.frames, so tests exercised a shape production never sees; the parser
  branches that existed only for that shape are removed.
- Live API tests are switched on with `FBI_CDE_LIVE=true`. They used to require
  `FBI_API_KEY` (the CDE needs no key) and always skipped on CI, so nothing
  automated ever ran them. `.gitea/live-api.yaml.example` is a ready-made
  weekly Gitea workflow for them (inert until moved into `.gitea/workflows/`);
  they deliberately do not run on GitHub.
- Removed the legacy test scaffolding inherited from the original package:
  `tests/testthat/setup.R` and the 86 CSVs plus one `.rda` in `inst/testdata`
  (recorded from the retired `api.usa.gov` API, shipped in every build, and
  unused by any test), along with the internal reader for them.

## Correctness fixes from the package review

Several of these are regressions from the multi-county matching fix (#56): an
agency that polices several counties now matches each of them, but its raw
`county_name` (`"DELAWARE; FAIRFIELD; FRANKLIN"`) was still treated as the
row's county downstream.

- **Behaviour change:** `county_agencies()` now sets `county_name` to the
  queried county on every row, and keeps the CDE's own list in a new
  `agency_county_names` column (also on `metro_agencies()`). Previously a
  multi-county agency carried its raw list, which:
  - split `get_county_crime()` into one row per distinct string. Franklin
    County, OH aggregated to 7 rows per period instead of 1.
  - gave `county_agencies()` the wrong `county_fips` whenever the first
    matching agency was multi-county. Licking County, OH got Fairfield's FIPS
    (39045 instead of 39089), which then fed the wrong population to
    `join_census_pop()`.
  - left `metro_agencies()` rows internally inconsistent: `county_fips` and
    `central_outlying` described one county while `county_name` listed three.
- `get_county_crime_detail()` now returns `county_fips`. Without it,
  `join_census_pop()` errored on real detail output, so the documented
  `denominator = "census_pop"` workflow never worked; the tests had been adding
  the column by hand.
- `county_to_fips()` now looks the name up in the bundled crosswalk before
  applying its patch table. Four Virginia independent cities that share a name
  with a county resolved to the county: Richmond city gave 51159 (Richmond
  County) instead of 51760, and likewise Fairfax, Franklin and Roanoke cities.
  It now agrees with the crosswalk for every county name the CDE uses (a new
  test checks all 3,733).
- `counties_with_fips()` returned a zero-row data.frame. It now returns one row
  per resolvable county (`state_abbr`, `county_name`, `county_fips`).
- `get_agencies()` requested `agency/{state}` and coerced the response with
  `as.data.frame()`, which on the live county-keyed shape gave one junk row per
  state. It now uses `agency/byStateAbbr/{state}` (the endpoint the
  participation functions already use), returns one row per agency, and skips a
  failing state with a warning rather than erroring. Its offline fixture
  (`agency-byStateAbbr-CA.json`) was synthetic and did not match the endpoint's
  real shape; the test now uses the recorded Rhode Island response, parsed both
  ways, and checks the request path.
- `place_agencies(county = )` compared the county exactly, so
  `place_agencies("Columbus", "OH", county = "Franklin")` found nothing.
- The agency fan-out behind `get_*_crime_detail()` now keeps each failed
  agency's error in `attr(x, "dropped_reasons")`, and its warning no longer
  says "returned no data" for what may be an HTTP or parse error.
  `get_county_crime()` warns when its input carries dropped agencies and keeps
  `attr(x, "dropped")`: those agencies are missing from the denominator too,
  so `coverage_fraction` cannot account for them.
- README: `get_arrest_demographics(offense = "murder")` always errors (only
  `"all"` is supported); the NIBRS offender example now uses a call known to
  return data.
- `?cde_base_url` named the pre-rename option `fbi.cde.base_url`; it is
  `fbiCDE.cde.base_url`.

# fbiCDE 0.1.0

## Metro (CBSA) geography (v0.5, Issue #45)

- Added `metro_agencies()` -- resolves a Core Based Statistical Area to the union
  of its member counties' agency sets. Because a metro is a set of *whole*
  counties, `agency_class` and `default_member` keep exactly their county-level
  meaning, including that a sheriff is a default member. Rows carry
  `cbsa_code`, `cbsa_title`, `cbsa_type` (`"metro"`/`"micro"`), and
  `central_outlying`.
- Added `get_metro_crime_detail()` -- itemized, unsummed metro crime. **Guarded:**
  a metro can be hundreds of agencies (New York resolves to ~489, Chicago ~314),
  each a sequential request, so `max_agencies` (default `150`) errors *before
  issuing any request* rather than hanging for minutes. Set `max_agencies = Inf`
  to override. `progress` defaults to `TRUE` here, unlike the county and place
  equivalents.
- Added `list_metros()` -- the discovery counterpart: every CBSA with its county
  count, filterable by type.
- Added an internal county->CBSA crosswalk derived from the public-domain
  **2023** OMB/Census delineation file (Bulletin 23-01): 935 CBSAs (393
  metropolitan, 542 micropolitan) over 1,915 county rows. That is 61% of the
  3,131 counties `county_agencies()` knows about, counted as crosswalk rows;
  counted as distinct, CDE-reachable counties instead it is about 58.5%
  (1,831 of 3,131) -- rural counties belong to no CBSA by construction, which
  is a property of the delineation, not a gap in the data. The vintage is
  pinned and exposed as `CBSA_VINTAGE` (read from the shipped data's own
  `"vintage"` attribute, not duplicated as a literal) and as an attribute on
  `list_metros()`, since CBSA definitions are revised periodically and
  counties move between metros.
- No metro-level aggregate. Summing across a metro raises the same denominator
  question the county aggregate deferred, and a metro's is harder (multi-state,
  mixed reporting coverage).
- **Connecticut's seven metros are not supported.** The 2023 delineation
  delineates Connecticut by planning regions (FIPS 09110-09190), which
  replaced its counties in 2022; the CDE still reports Connecticut agencies by
  traditional county (09001-09015). The two vocabularies do not join, so all
  seven CT metros -- Bridgeport-Stamford-Danbury, Hartford-West
  Hartford-East Hartford, New Haven, Norwich-New London-Willimantic, Putnam,
  Torrington, and Waterbury-Shelton -- resolve to zero counties.
  `metro_agencies()` warns explicitly rather than returning a silent empty
  frame, because a quiet zero-row result would read as "no agencies report in
  Hartford", which is false. Tracked as Gitea issue #52.
- Puerto Rico's 10 CBSAs are unmapped too, but that is academic: the CDE has
  exactly one PR agency.
- Agencies whose `county_name` is `"N/A"` (state police, tribal agencies, and
  the District of Columbia) cannot be reached from any county-keyed geography,
  so a metro that includes one is incomplete even when its counties join
  cleanly -- Washington-Arlington-Alexandria, DC-VA-MD-WV lists 23 counties
  but only 22 resolve, because DC's 3 agencies all carry `county_name = "N/A"`.

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
- `get_place_crime_detail()` gains an `agencies` argument so a caller-supplied
  membership frame (e.g. `add_place_spatial_members()`'s output) can be fanned
  out directly, making `agency_class = "campus"`/`"special"` reachable; an
  unsatisfiable filter now warns instead of returning an empty frame silently.
- `add_place_spatial_members()` is now idempotent (a frame that already has
  `point_in_polygon` rows is returned unchanged with a message rather than
  duplicated), validates its input against the full membership column set, and
  returns the same columns (`place_type`, `place_fips`) whether or not
  `sf`/`tigris` are installed. It also validates the polygon frame it is given
  and no longer produces a phantom match from an `NA` place name.

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

- Fixed `R CMD check` failure on CI (and CRAN win-builder): the test for
  `join_census_pop()` adding an NA column when `censusapi` is unavailable was
  environment-dependent — `skip_on_ci()` alone didn't make it deterministic,
  and when `censusapi` happens to be installed (as it is in CI, via
  `setup-r-dependencies`), the function proceeds past the availability check
  and errors on the missing Census API key. The dependency check is now
  isolated into `.census_deps_available()` (mirroring the existing
  `.spatial_deps_available()` pattern for `sf`/`tigris`), which the test mocks
  directly via `local_mocked_bindings(.package = "fbiCDE")` — mocking
  `requireNamespace()` itself does not work here because it doesn't intercept
  the unqualified call inside the package's own namespace.
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
