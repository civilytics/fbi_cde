# Design: Metro (CBSA) geography for `fbi` (v0.5)

**Status:** ratified design, not yet implemented
**Issue:** Gitea #45 (split out of #41)
**Predecessors:** `2026-07-13-geography-first-querying-design.md` (v0.2/v0.3),
`2026-07-24-place-membership-v0.4-design.md` (v0.4)

## 0. Scope

Metro-level agency membership and crime detail, where a metro is the **union of
its member counties' agency sets**. This is the third geographic level in the
package, after county (v0.2/v0.3) and place (v0.4).

Not in scope: place FIPS as a promised join key (#46), the capstone imputation
gap (#42), Combined Statistical Areas (CSAs) and Metropolitan Divisions — the
delineation file carries both, and the crosswalk will retain their columns, but
no resolver is built on them in v0.5.

## 1. Empirical findings (probed 2026-07-24)

Source: the Census/OMB **2023** delineation file,
`https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2023/delineation-files/list1_2023.xlsx`
(OMB Bulletin 23-01, the current vintage). Public domain — same licensing
posture as the county FIPS crosswalk, and unlike LEAIC there is no
redistribution restriction.

| measure | value |
|---|---|
| county↔CBSA rows | 1,915 |
| distinct CBSAs | 935 (393 Metropolitan, 542 Micropolitan) |
| counties per CBSA | median 1, mean 2.0, **max 40** |
| our counties in a CBSA | 2,280 / 3,733 (**61.1%**) |
| CBSAs spanning >1 state | 59 |
| **CBSA title collisions** | **0** |
| short-name (pre-comma) collisions | 70 |

Two findings drive the design.

**Titles are unique nationally, so the resolver needs no `state` argument.**
This is a genuine difference from `county_agencies(county, state)` and
`place_agencies(place, state)`: `"Albany, GA"`, `"Albany, OR"`, and
`"Albany-Schenectady-Troy, NY"` are distinct titles. Short names collide 70
times, so a short-name convenience path needs disambiguation, but the canonical
key does not.

**Fan-out cost is the real risk, and the issue did not anticipate it.**

| CBSA | agencies |
|---|---|
| New York-Newark-Jersey City, NY-NJ | **459** |
| Chicago-Naperville-Elgin, IL-IN | 356 |
| Philadelphia-Camden-Wilmington, PA-NJ-DE-MD | 353 |
| Pittsburgh, PA | 339 |
| *median CBSA* | *7* |

Every agency is one sequential `cde_request()` through the package's single
network seam. A metro detail call for New York is 459 requests — minutes of
wall clock, and 459 independent chances of partial failure. The median metro (7
agencies) is cheaper than a typical county. The distribution is what matters:
the expensive cases are exactly the metros users will reach for first.

61.1% coverage is not a defect. Rural counties belong to no CBSA by
construction; that is what "metropolitan or micropolitan statistical area"
means.

### Connecticut is unsupported in v0.5, and must say so

Added 2026-07-24 after probing against the corrected FIPS crosswalk (#50).

84 of the 1,915 county↔CBSA rows do not join to our county crosswalk, and **17
of 935 CBSAs resolve to zero counties**. They fall into two groups:

- **Connecticut (5 metros).** The 2023 delineation delineates Connecticut by
  **planning regions** (FIPS `09110`–`09190`), which replaced its counties in
  2022. The CDE still reports Connecticut agencies by **traditional county**
  (`09001`–`09015`). The two vocabularies do not join, so
  `Hartford-West Hartford-East Hartford, CT`, `New Haven, CT`,
  `Bridgeport-Stamford-Danbury, CT`, `Norwich-New London-Willimantic, CT`, and
  `Torrington, CT` map to nothing at all.

  Planning regions were redrawn from towns, not aggregated from counties, so
  there is no exact county mapping — any crosswalk would be an approximation.
  Shipping an approximation silently would violate this package's central
  discipline, so v0.5 **does not support Connecticut metros** and says so
  loudly. Proper support is its own research task and gets its own issue.
- **Puerto Rico (12 metros).** Academic here: the CDE has exactly **1** PR
  agency, so these CBSAs would be near-empty regardless.

**Design consequence.** A CBSA resolving to zero counties, or to fewer counties
than the delineation lists, must **never** return quietly. `metro_agencies()`
warns in both cases, naming how many of the metro's counties resolved. A silent
zero-row frame for "Hartford" would read as "no agencies report in Hartford",
which is false and materially misleading — the same class of error the
`reported` flag exists to prevent at the county level.

## 2. Architecture

Mirrors the shipped county and place layers:

```
metro_agencies()          Layer 0  pure resolver, no network, no new deps
  └─ get_metro_crime_detail()  Layer 1  guarded fan-out over member ORIs
list_metros()             discovery helper, pure
```

No Layer 2 aggregate. Summing across a metro raises the same denominator
question the county aggregate deferred until v0.3, and a metro's denominator is
strictly harder (multi-state, mixed coverage). Users who want it can pass metro
detail to `get_county_crime()`'s successor logic themselves; the package takes
no stance in v0.5.

## 3. Data asset — county→CBSA crosswalk

Built in `data-raw/cbsa_crosswalk.R` from the 2023 delineation file, shipped in
`R/sysdata.rda`. Columns:

```
cbsa_code, cbsa_title, cbsa_type, csa_code, csa_title,
md_code, md_title, county_fips, central_outlying
```

`cbsa_type` is normalized to `"metro"` / `"micro"` from the file's
`"Metropolitan Statistical Area"` / `"Micropolitan Statistical Area"`.
`county_fips` is `paste0(FIPS State Code, FIPS County Code)`, zero-padded to 5.

### The sysdata footgun — must be handled explicitly

`R/sysdata.rda` currently holds **exactly one** object, `crosswalk` (the county
FIPS table, 3,733 rows). `save()` overwrites the whole file, so a build script
that saves only the new object **silently destroys the county FIPS crosswalk**,
breaking `county_to_fips()` and every geography function above it.

The build script must therefore load the existing `R/sysdata.rda` into an
environment, add the new object, and re-save **all** objects together. The
script asserts both objects are present and non-empty before writing, and the
test suite asserts the same after — so this failure cannot land silently.

(`data-raw/county_fips_crosswalk.R` still writes to the retired
`data/county_fips.rda` path. It is stale, not wrong-in-effect, since the shipped
asset now lives in `sysdata.rda`. Bringing it in line is worth doing but is not
part of this spec.)

### Vintage

Pinned to **2023** and recorded in the crosswalk as an attribute, surfaced by
`list_metros()` and documented in the function reference. CBSA definitions are
revised periodically and counties move between metros, so a result that does not
say which delineation produced it is not reproducible. Only one vintage ships;
supporting multiple is deferred until someone needs it.

## 4. Layer 0 — `metro_agencies(metro, state = NULL)`

Pure resolver. No network, no new dependencies.

Resolution order:

1. **Exact match on `cbsa_title`** (case- and whitespace-insensitive). Unique by
   construction — 0 collisions.
2. **Short-name match** on the portion before the first comma or dash
   (`"Albany"` → the three Albany CBSAs). If exactly one matches, use it. If
   several do, `state` filters them; if `state` is absent or still leaves more
   than one, **error listing the candidate titles**, never guess. This mirrors
   the place resolver's ambiguity handling.
3. No match → **warning + zero-row typed frame**, matching
   `county_agencies()` and `place_agencies()`.

Returns the union of `county_agencies()` across member counties, with metro
columns added:

```
ori, agency_name, agency_type_name, agency_class, default_member,
county_name, state_abbr, county_fips, latitude, longitude,
cbsa_code, cbsa_title, cbsa_type, central_outlying
```

Reusing `county_agencies()` rather than re-querying the agency table means
`agency_class` and `default_member` keep exactly the semantics they already have
— including that sheriffs are `county_primary` and included by default, which is
correct here: a metro is a set of whole counties, so the unincorporated
remainder belongs to it.

`central_outlying` rides along from the delineation file. It is the natural
filter for "the metro core versus its commuter periphery" and costs nothing to
carry.

### Zero-row frames

Every empty return is built column-by-column with types matching a populated
result, per the fix in #48. Not from `matrix(nrow = 0, ...)`.

## 5. Layer 1 — `get_metro_crime_detail()`

```r
get_metro_crime_detail(metro, state = NULL, offense = "V",
                       from = "01-2015", to = "12-2020",
                       agency_class = NULL, default_only = TRUE,
                       max_agencies = 150, progress = TRUE)
```

Same unsummed per-agency-period shape as the county and place detail functions,
plus `cbsa_code`, `cbsa_title`, `cbsa_type`, `central_outlying`. Reuses
`parse_agency_detail()`, comparison-row stripping, and the drop→warn→
`attr(x, "dropped")` partial-failure idiom unchanged.

Two deliberate differences from its siblings:

- **`max_agencies = 150`.** If the resolved, filtered set exceeds this, the
  function **errors before issuing any request**, naming the metro, the agency
  count, and how to proceed (`max_agencies = Inf`, or a narrower
  `agency_class`). This makes the 459-request case a deliberate choice rather
  than an unexplained multi-minute hang. `Inf` disables the guard.
- **`progress = TRUE` by default** (the county version defaults `FALSE`). At
  metro scale, silence for minutes is indistinguishable from a hang.

The threshold is a usability guard, not a correctness one — hence a plain
argument rather than an option or an interactive prompt, which would not work in
scripts.

## 6. Discovery — `list_metros(type = NULL)`

Returns the distinct CBSAs in the crosswalk: `cbsa_code`, `cbsa_title`,
`cbsa_type`, `n_counties`. `type` filters to `"metro"` or `"micro"`. Pure,
offline, and the answer to "what am I allowed to pass to `metro_agencies()`?"
— the same role `counties_with_fips()` plays for counties. Carries the
delineation vintage as an attribute.

## 7. Testing

Per house discipline: offline tests that run on CI, plus
`skip_if_no_fbi_api()`-guarded live tests.

- **Crosswalk integrity** — asserts the bundled crosswalk has both expected
  objects in `sysdata.rda` (the footgun guard), the row/CBSA counts above, zero
  title collisions, and that `county_fips` is 5 characters throughout. Drift in
  the delineation asset then fails CI rather than silently changing results.
- **Resolver** — exact title, short name, ambiguous short name errors listing
  candidates, `state` disambiguation, unknown metro warns with a typed empty
  frame, and a known multi-county metro returns agencies from more than one
  county.
- **Guard** — a resolved set above `max_agencies` errors *before* any
  `cde_request()`; assert the mock was never called. This is the test that
  proves the guard actually guards.
- **Fan-out** — mocked `cde_request()`, asserting comparison-row stripping, the
  `reported` flag, and partial-failure `attr(x, "dropped")` with N ≥ 2 agencies.
- **Empty frames** — extend `test-empty-frames.R` to cover the two new frames,
  asserting empty and populated agree and `rbind()` is lossless.

No vacuous tests: anything that iterates a collection asserts it is non-empty.

## 8. Decisions with rationale

| decision | rationale |
|---|---|
| 2023 OMB delineation, pinned, recorded | Current vintage; definitions move, so an unlabelled result is not reproducible |
| Derive and bundle, don't fetch at runtime | Public domain, matches the county FIPS precedent, keeps the package dependency-free and offline |
| No `state` argument required | Titles are unique nationally (0 collisions) — inventing a state argument would be cargo-culting the county signature |
| Metro **and** micro, with `cbsa_type` | Surface the distinction, don't decide for the user; micro areas are single-county and carry no fan-out risk |
| `max_agencies = 150`, error before any request | 459 requests for New York; the median metro is 7. A guard that fires before the first request costs nothing and prevents an unexplained hang |
| `progress = TRUE` by default | At metro scale silence is indistinguishable from a hang |
| Build on `county_agencies()`, not the raw table | Keeps `agency_class`/`default_member` semantics identical across levels for free |
| No Layer 2 metro aggregate | Same denominator question the county aggregate deferred, and harder — multi-state, mixed coverage |
| Ambiguous short name errors with candidates | Consistent with `place_agencies()`; guessing between Albany GA and Albany NY is not recoverable by the user |

## 9. Open questions (deferred, non-blocking)

- **CSA and Metropolitan Division resolvers.** The columns ship in the
  crosswalk; no resolver is built. Cheap to add later if wanted.
- **Delineation updates.** When OMB issues a new bulletin, the crosswalk is
  rebuilt and the vintage attribute changes. Whether to support multiple
  vintages side by side is deferred until there is a concrete need.
- **`data-raw/county_fips_crosswalk.R` writes to the retired
  `data/county_fips.rda` path.** Worth reconciling, out of scope here.
