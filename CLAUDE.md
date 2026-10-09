# CLAUDE.md — `fbiCDE`

R wrapper for the FBI Crime Data Explorer (CDE) API
(`https://cde.ucr.cjis.gov/LATEST/`). No API key required for the current host.
Distributed through r-universe (`civilytics.r-universe.dev`, built from a
release tag of `github.com/civilytics/fbi_cde`), **not CRAN**. Based on the
original `fbi` package by Jacob Kaplan.

## Architecture & house rules (do not violate)

- **Single network seam.** *All* HTTP goes through `cde_request()` (`R/http.R`).
  Compose URLs with `cde_path()` / `cde_query()`. Never call `httr` directly
  elsewhere. Tests mock `cde_request()` — this is what makes the package testable
  offline.
- **Test discipline.** Every data path gets **both** an offline fixture test that
  runs on CI (mocks `cde_request` via `local_fbi_fixture()`; fixtures in
  `tests/testthat/fixtures/`) **and** a `skip_if_no_fbi_api()`-guarded live test
  (runs only with `FBI_CDE_LIVE=true`; never on GitHub infrastructure. A
  scheduled Gitea workflow is provided as `.gitea/live-api.yaml.example`).
  See `tests/testthat/helper-fbiCDE.R`. `R CMD check` must stay clean.
  Fixtures are verbatim recorded responses, never synthetic.
- **Lean dependencies, base R.** `R >= 3.5.0`. Imports are only `httr`,
  `jsonlite`, `datasets`, `utils` — **`data.table` was removed**; new code is base
  R. Heavy deps (`sf`, `tidycensus`) must be justified and go in `Suggests`, not
  `Imports`.
- **Parse defensively — the CDE schema drifts.** Several lookup endpoints have
  silently changed shape. Use `%||%` for drift tolerance (e.g.
  `actuals %||% counts`). Agency- and state-level `summarized`/`arrest` responses
  include **comparison rows** (state + national series with a real `rate` but
  `NA` count). `R/series.R` strips them by default (own series = labels present
  in the counts map); `comparison = TRUE` keeps them, labelled by `series`.
- **`cde_request()` uses `simplifyVector = FALSE`** (nested lists), and
  `read_fixture()` parses fixtures the same way, so parsers only ever see that
  shape. Index with `names()` / `[[ ]]`; do not add `is.data.frame()` branches
  for a simplified shape.
- **`cde_request()` retries** transient failures (network errors, 408/429/5xx)
  with backoff; 4xx other than 408/429 fail immediately.
- **`R/sysdata.rda` holds four internal objects**: `crosswalk` (county FIPS),
  `cbsa_crosswalk` (county->CBSA), `place_crosswalk` (ORI -> Census place
  / county subdivision) and `ct_planning_regions` (ORI -> Connecticut
  planning region). Any build script that touches it must `load()` and
  re-`save()` **every** object together (the `data-raw/` scripts do it with
  `ls()` on a loaded environment), or it will silently destroy the ones it did
  not know about.

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
  **by `agency_type_name` only — never by parsing agency names.**
- **Which classes are queried:** by default `county_primary`, `municipal`,
  `campus` (place level: `place_primary`, `campus`) — agencies with a specific
  sub-state jurisdiction. `include_statewide = TRUE` adds `state` (State Police
  + Other State Agency). `special` ("Other": transit, school, port, railroad,
  multi-county task forces) and `tribal` (attribution unresolved, deferred)
  only via `agency_class`. There is no "default member" flag; don't add one.
- `get_county_crime_detail(...)` — fans out one request per member ORI, returns
  **unsummed** per-agency-period rows with coverage columns and a `reported` flag
  (distinguishes "reported 0" from "did not report" — critical for the 2021 CA
  NIBRS-transition reporting hole). Filters via `agency_class` /
  `include_statewide`; failed ORIs are dropped, warned, and listed in
  `attr(x, "dropped")`.
- `get_county_agency_crime(...)` — the county's own primary (sheriff) series.
- **Attribution invariant:** a resolver row's `county_name`/`county_fips` is the
  county the row is *attributed* to (the queried county; in a metro, the first
  member county in delineation order). A multi-county agency's raw CDE list
  (`"DELAWARE; FAIRFIELD; FRANKLIN"`) lives in `agency_county_names`. Never
  group or derive FIPS from the raw list.
- **County attributions for `"N/A"` agencies.** The CDE gives the NYPD, DC's
  Metropolitan Police and the Baltimore City Sheriff no county.
  `.AGENCY_COUNTY_ATTRIBUTIONS` (`R/geography.R`, keyed by ORI) fills their
  `county_name` inside `agencies_table()`, only while the CDE's value is
  `"N/A"`; the bundled `fbi_api_agencies` is left as the CDE has it. The NYPD
  goes to all five boroughs, so a borough returns citywide figures; that is
  documented, not a bug. A county added there must also be in the FIPS
  crosswalk: run `data-raw/crosswalk_attributed_counties.R`.
- **Connecticut planning regions (#52).** Connecticut's county equivalents
  since 2022, and the units of its 2023 CBSAs. `ct_planning_regions` records
  each bundled CT agency's region as the CDE's live directory reports it
  (`data-raw/ct_planning_regions.R`); `agencies_table()` appends it to the
  agency's county list, so an agency is reachable by traditional county and by
  region. The two systems overlap completely: never sum across them.
- `place_agencies(place, state, county = NULL)` — pure, name-identity resolver
  for the municipal tier (`"<Place> Police Department"` names); classifies
  `agency_class` (`place_primary`/`campus`/`special`).
- **Place codes (#46):** `place_agencies()` rows carry `place_type`,
  `place_fips` (7-digit) or `cousub_fips` (10-digit, for townships and New
  England/NY towns), from `place_crosswalk` keyed by ORI
  (`data-raw/place_fips_crosswalk.R`, Census 2020 reference files,
  `PLACE_VINTAGE`). Matched by name **within the agency's county**, never by
  coordinates (the HQ points are too unreliable). Unresolved = `NA`, not a
  guess. Spec: `specs/2026-10-06-place-fips-design.md`.
- `get_place_crime_detail(...)` — the place twin of `get_county_crime_detail()`;
  accepts an optional pre-resolved `agencies` frame (e.g. from
  `add_place_spatial_members()`) so campus/special members can be queried.
- `add_place_spatial_members(x, ...)` — opt-in, `sf`/`tigris`-backed
  point-in-polygon attribution of campus/special agencies to a place; appends
  rows with `attribution = "point_in_polygon"` to a `place_agencies()` result.

## Roadmap

Full design + phased plan: `specs/2026-07-13-geography-first-querying-design.md`.
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
  `specs/2026-07-24-place-membership-v0.4-design.md`.
- **v0.5** — **shipped.** Metro (CBSA) geography: `metro_agencies()`,
  `get_metro_crime_detail()` (guarded by `max_agencies`), `list_metros()`, and
  a bundled 2023 OMB delineation crosswalk. Spec:
  `specs/2026-07-24-metro-cbsa-v0.5-design.md`.
- **Place FIPS** as a promised join key (Gitea #46) — **shipped** as a
  name-and-county crosswalk, no `sf`. Spec:
  `specs/2026-10-06-place-fips-design.md`.
- **Capstone** — imputation gap: FBI published aggregate vs. sum of reported
  components (uses the `reported`/coverage columns already collected).
- **Bulk downloads (#58)** — **decided:** no bulk-file reader for now;
  estimates and LEOKA assaults are bulk-only (recipe and the shape of a future
  reader in `specs/2026-10-06-bulk-downloads-decision.md`). Any reader needs
  its own mockable network seam beside `cde_request()`.

## Workflow

- **Issue-driven** on Gitea (`gitea.civilytics.org/Civilytics/fbiCDE`); default
  branch `main`. Conventional commits (`feat:`/`fix:`/`refactor:`/`test:`/`docs:`).
  Record changes in `NEWS.md`. CI: `.gitea/workflows/R-CMD-check.yaml`.
- **Design docs go in `specs/`, plans in `specs/plans/` — never
  `docs/superpowers/`** (#61). This overrides the superpowers skills' default
  location: write a brainstorming spec to `specs/YYYY-MM-DD-<topic>-design.md`
  and an implementation plan to `specs/plans/YYYY-MM-DD-<feature>.md`. `docs/`
  is pkgdown's output folder. A hook in `.claude/settings.json` blocks writes
  under `docs/superpowers/`, and the Gitea CI fails if that folder exists.
- **Arrest offenses are numeric codes** (`arrest/<level>/<code>`); a name in
  the URL is HTTP 400. `ucr_arrest_offense_codes` maps each code to its name,
  category and breakdown; `.arrest_offense_selection()` resolves a name (name,
  then category, then breakdown) to codes and the functions sum per code.
  Rebuild it with `data-raw/api_vocabularies.R`, never by hand.
- **Police employment paths:** `pe` (nation), `pe/{ST}`, `pe/{ST}/{ORI}`.
  `pe/state/{ST}` and `pe/national` answer all-null; no region form works.
- **NIBRS (#33 resolved):** the `nibrs/` endpoint takes short offense codes
  (`V`, `P`, `ROB`, `BUR`, `13B`, `35A`, ...) and answers anything else — long
  names, `"all"` — with an all-null payload, which once looked like an outage.
  Vocabularies come from `data-raw/api_vocabularies.R`; rebuild rather than
  hand-edit them. A geography with no NIBRS data also returns all-nulls.
