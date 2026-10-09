# Design: Geography-first querying & agency membership for `fbi`

**Date:** 2026-07-13
**Status:** Approved design; ready for implementation planning
**Scope of this doc:** the design and phased plan for moving `fbi` from a
faithful API mirror toward analysis-ready crime data — starting with
**county-level agency membership and itemized detail**. Aggregation, Census
joins, place-level membership, and the imputation-gap capstone are designed here
but deliberately deferred to later phases.

---

## 1. Value hypothesis & north star

v0.1 mirrors the CDE API: one function per endpoint, three geographic levels
(national, state, agency/ORI). The value hypothesis is to move toward
*analysis-ready* crime data. Three directions, in priority order: (1) query by
geography, (2) aggregate agencies together, (3) join external (Census) data.

**The 1.0 north star is the agency-membership model, not the Census join.**
`tidycensus` already won Census access. The defensible, unfilled niche is
cutting through "there are 18,459 agencies and I don't know which ones are
*Alameda County*" — classifying agencies and encoding how each *type* relates to
a geography, transparently and adjustably. The genuinely hard, genuinely
valuable cases are the ones no existing R package handles:

- **Non-place agencies** (state police, transit authorities, airports, parks) —
  jurisdiction spans or floats across geographies.
- **Embedded agencies** (campus PD, hospital PD) — spatially inside a place but
  reporting separately.

**Capstone (post-1.0):** expose the gap between the FBI's *published* aggregate
(which imputes for non-reporters) and the *sum of reported components*, so users
can see how much of an official figure is statistically generated.

### Prior art (confirms the niche is unfilled)
- Jacob Kaplan's ecosystem (`fbi`/`fbiAPI`, `crimeutils`, openICPSR cleaned UCR)
  ships **agency-level** data and leaves rollups and the ORI→FIPS crosswalk to
  the user.
- `crimedata` (Ashby, Crime Open Database) is incident-level open-portal data —
  not FBI UCR, no agency→county rollup, no Census join.
- No CRAN package ships an ORI→FIPS crosswalk or an agency-membership aggregator.

## 2. The core constraint: attribution, not spatial truth

FBI data is **agency-reported counts, not geocoded incidents.** We can never
answer "crime that physically happened inside this polygon." We can only answer
"the agencies *attributed to* this geography." Membership is an **attribution
model**, and the product value is making that attribution transparent and
adjustable — never a black box. (The FBI's own county rollups are an attribution
model with documented rules; we borrow the convention where we can.)

## 3. Architecture — three layers, built bottom-up

Each layer builds on the one below. Opinion is *declared* in Layer 0 and only
*applied* in Layer 2.

### Layer 0 — membership resolver (pure, no network)
`county_agencies(county, state)` reads the bundled `fbi_api_agencies` table,
selects agencies whose `county_name` + `state_abbr` match (normalized), and
returns the classified candidate set:

| column | source |
|---|---|
| `ori`, `agency_name`, `agency_type_name`, `county_name`, `state_abbr`, `latitude`, `longitude` | bundled table |
| `agency_class` | derived (see §4) |
| `default_member` | logical: `TRUE` iff `agency_class ∈ {county_primary, municipal}` |

Exhaustively unit-testable offline. This is where all opinion is *declared*, none
applied.

**Normalization requirements (confirmed against bundled data, 2026-07-13):**
`county_name` is stored **UPPERCASE** (`LOS ANGELES`, `ORANGE`), so matching must
case-fold (and trim). The county sheriff's `agency_type_name` is **`County`** (not
"Sheriff"), which is why `County` → `county_primary` in §4. The resolver's
`county` argument must therefore accept ordinary-case input and normalize both
sides before matching.

### Layer 1 — itemized detail (fan-out, no summation) — **v0.2 headline**
`get_county_crime_detail(county, state, offense, from, to, agency_class = NULL, default_only = FALSE)`

Resolver → fan out one `get_agency_crime()` per selected ORI → return **one row
per (ORI, offense, period)**, unsummed, carrying the Layer-0 classification plus
per-agency coverage and a transparent per-agency rate. Default returns
*everything, typed*; `agency_class` / `default_only` filter by **row selection
only** (no denominators, no summation).

### Layer 2 — aggregate (deferred to v0.3)
`get_county_crime(county, state, ..., tier = ...)` — the opinionated rollup +
denominator model (§6). Not in v0.2.

### Disambiguation verb (v0.2)
`get_county_agency_crime(county, state, ...)` → the single `county_primary` ORI's
own series (thin wrapper over resolver + `get_agency_crime`). Resolves the core
ambiguity: "the county sheriff's own reported crime" vs. "crime aggregated across
the county."

## 4. Agency classification & default membership policy

Every detail row carries **two** classification columns:

- **`agency_type_name`** — raw, straight from the data (nothing lost).
- **`agency_class`** — derived, filterable grouping that encodes the tiers and
  each class's default disposition:

| `agency_class` | maps from | default | rationale |
|---|---|---|---|
| `county_primary` | County sheriff / county PD / Parish | **include** | core of the county |
| `municipal` | City / Municipality / Borough / City and Borough | **include** | core of the county |
| `campus` | University or College | exclude (opt-in) | embedded; additive but usually not what "county crime" means |
| `special` | transit/airport/park, "Other", "Other State Agency", Census Area | exclude (opt-in) | jurisdiction spans/floats across geographies |
| `state` | State Police / Highway Patrol | **exclude** | statewide reporting — HQ county would swallow local agencies in small counties |
| `tribal` | Tribal | **exclude** | genuinely bounded in reality, but not renderable from our data |

**Default membership = `{county_primary, municipal}`** (Tier 1). Everything else
is opt-in. Matches FBI county-rollup convention and protects small counties from
state/tribal distortion.

### Two honesty caveats (must be enforced in output)
- **`state`:** agencies carry only an *HQ* county. A naive `county_name` match
  would dump all of CHP's statewide crime into its HQ county. Even when a user
  opts in, those rows must be flagged `statewide — not county-specific`.
- **`tribal`:** agencies *do* carry an HQ `county_name`, so the naive match would
  silently attribute them — but reservations span counties and are a separate
  (BIA/tribal) jurisdiction. Default-exclude; document openly. Proper AIANNH
  reservation attribution is a post-1.0 Census-boundary problem.

## 5. Return shape (v0.2 detail)

Long/tidy **plain `data.frame`** (no `sf`, no new `Imports`; internally stacked
with base `rbind`). Columns:

```
ori, agency_name, agency_type_name, agency_class, default_member,
county_name, state_abbr, offense, period,
count, population, participated_population, rate, reported
```

- **`rate`** = `count / participated_population × 100k`. Per-agency rates are
  unambiguous, so they ship in v0.2 — but a rate **never** appears without its
  denominator columns beside it (`population`, `participated_population`).
- **`population`** vs **`participated_population`**: both always present. An
  agency that reported only part of the year has a smaller `participated_population`;
  hiding it would silently distort the rate (see §6 worked example).
- **`reported`**: logical — did this ORI return data for this period? **Must
  distinguish "reported 0" from "did not report."** Confirmed necessary
  (2026-07-13): every LA agency (Compton, West Hollywood, LASD) shows 0 violent
  offenses for **2021** — the California NIBRS-transition reporting collapse, not a
  true zero. A naive county sum for 2021 would read ≈0 crime, which is
  catastrophically wrong; `reported` is what lets callers tell a genuine zero from
  a coverage hole. (This is also a prime capstone exhibit.)

## 6. The denominator problem (why the *aggregate* is deferred)

The difficulty is not rate arithmetic — it is the **county-level denominator**.
Worked example: robbery, 2022, "Smallville County," Census pop 700,000 (three
cities = 480k + unincorporated remainder 220k policed by the sheriff).

| agency | class | robberies | jurisdiction pop | months | participated pop |
|---|---|---|---|---|---|
| Smallville City PD | municipal | 90 | 300,000 | 12/12 | 300,000 |
| Junction City PD | municipal | 24 | 120,000 | 12/12 | 120,000 |
| Rivertown PD | municipal | 6 | 60,000 | **6/12** | 30,000 |
| County Sheriff | county_primary | 40 | 220,000 | 12/12 | 220,000 |

**Per-agency rates — trivial, unambiguous (v0.2):** 30.0 / 20.0 / 20.0 (over
participated pop; 10.0 if wrongly over full pop — *why we keep both*) / 18.2.

**County rate — same reported count (160), three defensible denominators:**

| denominator | value | rate/100k | problem |
|---|---|---|---|
| (a) full Census county pop | 700,000 | 22.9 | numerator undercounts (Rivertown's missing months un-imputed) → biased low |
| (b) Σ participated pop | 670,000 | 23.9 | coverage-consistent, but drops uncovered Rivertown-months |
| (c) Σ jurisdiction pop | 700,000 | 22.9 | coherent **iff** the sheriff pop is the unincorporated remainder (confirmed empirically — §14); a *whole-county* sheriff coding would double-count the cities (Σ = 1,180,000 → 13.6), but the CDE does **not** do that |

The FBI's *published* county rate does neither: it **imputes** Rivertown's missing
months (~+6 → 166) over full population → ~23.7. **The gap between our transparent
(a) = 22.9 and the FBI's 23.7 is exactly what the capstone exposes.**

**Design consequence:** Layer 2 must never emit a bare county rate. It returns
the count, the chosen denominator value, *which* denominator was used, and the
coverage fraction — the math is always visible, including where the FBI hides it.
Choosing among (a)/(b)/(c) and handling the sheriff double-count is the
opinionated modeling that justifies deferral, not the rates themselves.

## 7. Mechanical decisions (ratified)

1. **Comparison-row stripping (must-handle).** `get_agency_crime()` returns state
   + national comparison rows (real `rate`, `NA count`). The fan-out keeps **only
   the agency's own rows**, or every county is polluted with duplicated
   state/national rows. (The NEWS.md #30 gotcha.)
2. **Fan-out infrastructure.** Dozens of `cde_request()` calls per county:
   sequential through the single seam, a toggleable progress indicator,
   **partial-failure tolerance** (one ORI errors → warn, drop, continue), and a
   returned/attached record of *which ORIs were dropped and why*. No silent
   truncation.
3. **Coverage columns ride along now; imputation-gap later.** Every row carries
   `population`, `participated_population`, `reported`. We collect the raw
   material in v0.2 but compute the published-vs-summed gap only in the capstone.
4. **FIPS deferred to the Census phase.** County *membership* keys on
   `county_name + state_abbr` — no FIPS needed to answer "which agencies are in
   Alameda." Keeps v0.2 dependency-free.

## 8. Crosswalk strategy (Census phase, v0.3+)

Research (2026-07-13, sources in the research brief) settled this:

- **LEAIC (ICPSR 35158) is out as a shippable/fetchable asset.** The ORI↔FIPS
  *facts* are public domain, but the ICPSR *file* has a no-redistribution
  click-through, requires login (no anonymous runtime fetch), and is **frozen at
  2012**. We may consult it to QA, not bundle or fetch it.
- **County FIPS: derive it ourselves.** `county_name + state_abbr` → the
  public-domain `fips_codes` table (copyable) + a small hand-maintained **patch
  table** for the known collisions: Virginia independent cities (the big one),
  Louisiana parishes, Alaska borough/census-area churn, Connecticut's 2022
  planning regions, Saint/St. and city-vs-county pairs. ~95–98% auto + patch →
  ~99%. Zero licensing risk, no heavy deps, tracks the CDE live. Ship as internal
  bundled data.
- **Place/municipal FIPS is inherently lossy** — the CDE lat/long is the agency
  *HQ point*, not its jurisdiction; a sheriff/state/campus HQ maps to the wrong
  place or to none (unincorporated → NA). Point-in-polygon against Census place
  shapefiles needs `sf` + `tigris` (heavy). Best-effort **enrichment only, behind
  `Suggests`, never a promised join key.** This is why place-level membership is a
  1.0 problem, not a v0.2 one.

## 9. Census join (v0.3+)

- Prefer **`censusapi`** (3 pure-R deps: `httr`, `jsonlite`, `rlang`) over
  `tidycensus` (drags in the whole `sf`/tidyverse stack). Reserve `tidycensus`
  for when a user explicitly wants `sf` geometry.
- Optional dependency: **`Suggests`**, guarded at runtime. Requires a free Census
  API key (env var). We return a FIPS-joinable frame (or optionally join ACS
  denominators), so users can compute their own rates/models.

## 9a. Optional reporting-gap imputation (phase v0.2b, between detail and aggregate)

**Motivation.** Coverage holes break time-series and trend analysis — the
canonical case is the 2021 California NIBRS-transition collapse where LA agencies
report 0 (a coverage hole, not a true zero; see §5). Users studying trends need to
fill these gaps, but imputation is *inference*, so it must be **opt-in,
transparent, and flagged** — never the default.

**Placement.** Operates on the Layer-1 detail series (per-agency time series),
sitting between detail (v0.2) and aggregate (v0.3): fill agency-month holes first,
then aggregation (v0.3) can optionally consume the imputed series. The raw v0.2
path stays pristine.

**Shape.** A standalone, composable transform rather than a hidden argument —
which makes "off by default" *structural* (you must call it):
`impute_reporting_gaps(detail, method = "interpolate")` takes a detail frame and
returns it with:
- gaps filled for agencies that report *some* periods but miss others,
- a new **`imputed`** logical column (never overwrites `reported`),
- **`impute_method`** metadata on each filled row,
- original values untouched for non-imputed rows.

**Methods (start simple and transparent).**
- **Within-agency temporal (default):** interpolate/seasonally fill an agency's
  missing months from its own surrounding periods, operating on rate × population
  so a drifting denominator is respected. Handles the 2021 case directly
  (agencies report 2019–2020, drop 2021, resume 2022 → interpolate 2021).
- **Fully non-reporting agencies** (never report in-window) are deliberately *out
  of scope here* — cross-sectional imputation (population-group models, the FBI's
  own approach) is far more opinionated and belongs with the aggregate/capstone.

**Relationship to the capstone.** This phase builds the "our transparent
imputation" leg. The capstone then compares three numbers — raw sum of reported
components, our-imputed sum, and the FBI's published (FBI-imputed) aggregate —
making visible how much of the official figure is statistically generated.

## 10. `data.table` removal (preparatory refactor)

Decision: **remove `data.table` entirely**, use base R. No object here exceeds a
few thousand rows and there are no complex grouped joins that would justify the
dependency. Surface:

- `melt`/`dcast` (utils.R) live only in **`srs_long_to_wide()` and `make_url()`,
  which are dead code** (never called; `make_url` targets the retired
  `api.usa.gov` endpoint) → **delete both functions.**
- `rbindlist` ×5 (ucr_participation ×3, lookups ×2, leoka ×1) → base
  `do.call(rbind, ...)`, with one small `rbind_fill()` helper for the single
  `fill = TRUE` site (ragged columns).
- `setorder` ×1 (lookups) → base `order()`.

Sequencing: **do this first, as its own PR**, so v0.2 geography code is written
against a clean base-R foundation and reuses the new `rbind_fill()` helper for its
fan-out stacking. Guarded by the existing test suite.

## 10a. `is_valid_ori()` fix (required prerequisite)

**Blocker discovered 2026-07-13.** The current regex `^[A-Z]{2}[0-9]{7}$` rejects
**1,805 of 18,459 bundled ORIs (9.8%)** — all exactly 9 characters, but with
*letters* in positions 3–9. The rejected set is **all state police, all tribal,
most university/college, and letter-bearing city ORIs** (e.g. West Hollywood
`CA0191H0X`). Because `is_valid_ori()` gates `get_agency_crime()` (`ucr_crime.R:62`,
and 9 other call sites), **`get_agency_crime()` currently throws for ~10% of real
agencies — exactly the `campus`/`state`/`tribal` attributable tier the membership
model targets, plus contract cities.** This is a hard blocker for the Layer-1
fan-out (the resolver would emit ORIs the fetch layer refuses).

Fix: relax to `^[A-Z]{2}[A-Z0-9]{7}$` (all bundled ORIs are exactly 9 chars; first
two are the state alpha, remaining seven are alphanumeric). Add a regression test
using `CA0191H0X`, `ARASP0000` (state), and a tribal ORI. Do this in the prep PR
alongside the `data.table` removal (both are foundation cleanups the geography
work depends on).

## 11. Testing discipline (house style)

Every data path gets an **offline fixture test** (mock `cde_request`, runs on CI)
**and** a `skip_if_no_fbi_api()`-guarded **live test**. Specifically:

- **Resolver (Layer 0):** pure offline tests against bundled data —
  classification correctness across all six `agency_class` values, VA/LA/AK edge
  counties, `state`/`tribal` flagged, `default_member` correct.
- **Detail (Layer 1):** offline fixture for a *small multi-agency county* —
  asserts comparison-row stripping, `agency_class`/`default_only` filtering,
  partial-failure handling and drop-reporting, and correct per-agency `rate` +
  population columns. Plus a live test.
- **`data.table` removal:** existing tests must stay green through the swap; add
  a targeted `rbind_fill()` unit test.

R CMD check must stay clean; CI is `.gitea/workflows/R-CMD-check.yaml`.

## 12. Phasing

| Phase | Contents |
|---|---|
| **v0.2a (prep)** | Remove `data.table`; delete dead reshape helpers; add `rbind_fill()`. **Fix `is_valid_ori()`** (§10a) so `get_agency_crime()` accepts letter-bearing ORIs (state/tribal/campus/contract cities). Its own PR — both are foundation cleanups the fan-out depends on. |
| **v0.2 (headline)** | Layer 0 resolver + Layer 1 detail: classification, filtering, coverage columns, per-agency rate, fan-out infra. **County only. No summation, no Census, no place, no FIPS.** |
| **v0.2b** | Optional **reporting-gap imputation** (§9a): `impute_reporting_gaps()`, off by default, within-agency temporal fill, `imputed`/`impute_method` flags. For trend/time-series users. |
| **v0.3** | Layer 2 aggregate (denominator model, sheriff-vs-city double-count handling per FBI county-file convention; can consume v0.2b-imputed detail) + county FIPS crosswalk (derived + patch table) + optional Census join (`censusapi`, `Suggests`). |
| **v0.4 → 1.0** | Metro (= union of county memberships); **place/municipal membership** + attributable-tier modeling (campus/transit); place FIPS via `sf`/`tigris` (`Suggests`, best-effort, lossy). |
| **Capstone (post-1.0)** | Imputation gap: FBI published aggregate vs. sum of reported components, using the `reported`/coverage columns collected from v0.2 onward. |

## 13. Decisions with rationale

- **Membership is an attribution model, not spatial truth** — because FBI data is
  agency-reported, not geocoded. Everything downstream honors this.
- **Detail-first, aggregate-deferred** — the itemized unsummed view delivers value
  while sidestepping every opinionated trap (denominator choice, double-counting,
  suppression-aware summation). It is also the exact substrate the aggregate and
  capstone are built on.
- **Filter, don't decide (v0.2)** — `agency_class` filtering is pure row
  selection; it gives users control without the package taking an aggregation
  stance yet.
- **Conservative default membership** — Tier 1 only; `state`/`tribal`/`campus`/
  `special` opt-in. Protects the common case (small counties) from distortion.
- **Per-agency rates ship; county rates wait** — per-agency denominators are
  unambiguous; the county denominator is a modeling choice that must be explicit.
- **Derive county FIPS, don't ship LEAIC** — licensing (no redistribution), auth
  (no runtime fetch), and staleness (2012) all rule LEAIC out; we already hold
  `county_name + state` to derive it cleanly.
- **Remove `data.table`** — data volumes and join complexity don't justify the
  dependency; the only non-trivial usage is dead code.
- **Imputation is opt-in, flagged, and temporal-first** — filling coverage holes
  is inference, so it lives in a standalone transform you must call (structurally
  off by default), always marks `imputed` rows, and starts with the defensible
  within-agency temporal method; cross-sectional imputation waits.

## 14. Open questions (for later phases, not v0.2)

- **Sheriff population coding — RESOLVED (2026-07-13, live CDE pull).** The CDE
  codes a sheriff's `population` as the **unincorporated remainder it polices, not
  the whole county.** Evidence: LA County Sheriff (CA0190000) = ~0.92–1.01M across
  2018–2023 vs. a ~10M county; Orange County Sheriff (CA0300000) = ~0.11–0.14M vs.
  a ~3.2M county. Both track their unincorporated-remainder populations, not
  county totals. **Implication:** denominator (c) Σ jurisdiction populations is
  *coherent* (cities + sheriff-remainder ≈ county, no overlap) — the whole-county
  double-count scenario does not occur. Populations also drift year-to-year
  (annexations/contract churn), so denominators must be taken *per period*.
- **Contract policing — RESOLVED (2026-07-13, live CDE pull).** Contract cities
  **report under their own city ORI**, with their own city population and their
  own crime counts, even when a sheriff does the actual policing. Evidence:
  Compton PD (CA0191500) codes ~90–96k (= city population) and ~1,100 violent
  offenses/yr; West Hollywood PD (CA0191H0X) codes ~34–37k and ~300/yr — both
  LASD-policed, both reporting separately; LASD's ~1M *excludes* them.
  **Implication:** population and crime partition cleanly (unincorporated=sheriff +
  each incorporated city=own ORI = county), so the aggregate is
  `sum(city ORIs) + sheriff` with no contract-policing double-count. Contract
  policing is invisible at the data layer.
- **Aggregate default denominator:** (a) full Census, (b) Σ participated, or a
  coverage-explicit pair returned together? Lean toward returning both a
  coverage-consistent rate *and* the coverage fraction rather than picking one.
- **Metro definition source:** CBSA delineation vintage for metro = union of
  counties.
- **Place membership rule:** point-in-polygon vs. place-name matching vs. both;
  how to handle embedded campus/transit at the place level.
- **AIANNH/tribal attribution:** requires Census reservation boundaries; post-1.0.
