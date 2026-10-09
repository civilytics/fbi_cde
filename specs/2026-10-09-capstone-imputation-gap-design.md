# Design: the imputation-gap capstone (#42)

**Date:** 2026-10-09
**Status:** Proposed. Findings verified against the live API; the interface
below is a proposal for review, not yet planned or built.
**Issue:** Gitea #42, `[roadmap] capstone — imputation gap (published vs
summed)`. Roadmap context: `specs/2026-07-13-geography-first-querying-design.md`
§1, §6 and §9a.

## 1. Question

The roadmap's capstone compares three numbers for one geography and period:

1. the **sum of reported components** (what the agencies that reported sent),
2. **our transparent imputation** of the missing part, and
3. the **FBI's published aggregate**, which imputes for non-reporters,

so a user can see how much of an official figure is statistically generated.
The package already has (1) and the raw material for (2). Can the API supply
(3)?

## 2. Finding: the API publishes estimates, but only NIBRS estimates for 2021-2022

- **SRS estimates are not in the API.** `summarized/{state,national}` returns
  reported sums (what `get_estimated_crime()` returns despite its name). The
  FBI's SRS state and national estimates, 1979 onward, are a bulk XLSX only
  (`specs/2026-10-06-bulk-downloads-decision.md`).
- **NIBRS estimates are.** The CDE's "NIBRS Estimates" page reads
  `nibrs-estimation/...` (found in one of the web app's lazy-loaded
  JavaScript chunks):

  | path | geography |
  |---|---|
  | `nibrs-estimation/national/{indicator}` | the nation |
  | `nibrs-estimation/region/{R}/{indicator}` | Census region (`M`, `N`, `S`, `W`) |
  | `nibrs-estimation/state/{state_id}/{indicator}` | a state |
  | `nibrs-estimation/national/agency-type/T/{type}/{indicator}` | city or county agencies (`C`, `N`) |
  | `nibrs-estimation/national/size/S/{size}/{indicator}` | a population group (1-8) |
  | `nibrs-estimation/region/{agency-type,size}/...` | the same, within a region |
  | `nibrs-estimation/lookup/all` | the vocabularies below |

  Every data path takes `?year=YYYY` ("Bad request, year is required"
  without it). `nibrs-estimation/msa/{indicator}` exists in the bundle but
  answers 404.
- **Vocabularies are numeric and not FIPS.** `lookup/all` returns `Years`
  (only **2021 and 2022**), `States` (59 entries, alphabetical IDs: Alaska 1,
  Alabama 2, ..., Ohio 39, Texas 48; Federal 98, Other 99), `Regions`,
  `AgencyLocation`, `StatDescription` (population groups) and `Indicators`
  (offense IDs in four groups: Violent Crime 133, Property Crime 116,
  Robbery 121, Aggravated Assault 55, Burglary/B&E 61, Larceny/Theft 102,
  Motor Vehicle Theft 105, Arson 58, Murder 106, Rape 118, plus every NIBRS
  offense). A name instead of an ID is HTTP 400.
- **Response shape.** `[{"data": {"Offense": {...}, "Victim": {...},
  "Incident": {...}, "Arrest": {...}}}]`. Each table is a list of
  `{"<category>": n, "lower_bound": x, "upper_bound": y}` objects, a point
  estimate with its 95% confidence bounds. The total is
  `Offense$"Offense Count - Total"`. Lower bounds can be negative (American
  Indian victims in Ohio, 2022: 96 [17, 176]; arrestees: 30 [-4, 64]), and
  some cells are `0` with null bounds.

## 3. The comparison is meaningful

Violent crime (`summarized` offense `V` summed over the year, against NIBRS
estimate indicator 133), live, 2026-10-09:

| geography | year | reported sum | coverage | FBI estimate [95% CI] | estimated, not reported |
|---|---|---|---|---|---|
| Texas | 2022 | 131,220 | 98.8% | 131,533 [131,313, 131,755] | 0.2% |
| Ohio | 2022 | 34,929 | 93.1% | 36,801 [35,580, 38,023] | 5.1% |
| Ohio | 2021 | 36,366 | 90.6% | 38,014 [36,163, 39,866] | 4.3% |
| California | 2022 | 193,814 | 98.8% | 207,381 [183,101, 231,663] | 6.5% |
| California | 2021 | 36,439 | 26.2% | 217,539 [189,849, 245,229] | **83.2%** |
| United States | 2021 | 932,565 | 77.0% | 1,325,991 [1,179,556, 1,472,426] | **29.7%** |
| United States | 2022 | 1,271,206 | 94.8% | 1,328,033 [1,226,130, 1,429,937] | 4.3% |

Coverage is the mean monthly `participated_population / population`.

At near-full coverage the two agree (Texas 2022: within 0.3%), which is the
evidence that the offense definitions line up for violent crime. Where
coverage collapses, the published figure is mostly model: five-sixths of
California's 2021 violent crime, and three-tenths of the nation's, is
estimated. This is the exhibit the roadmap wanted, and 2021 (the NIBRS
transition year) is where it matters most.

## 4. Proposal

Two functions, in the package's usual layering.

### 4.1 `get_nibrs_estimate()`: a plain wrapper

```r
get_nibrs_estimate(state_abb = NULL, year, offense = "V",
                   table = "Offense")
```

- One request through `cde_request()` per call (`nibrs-estimation/national/`
  or `.../state/{id}/`); regions and agency-type/size groups can follow.
- `offense` takes the package's SRS summary codes where they map
  (`V` 133, `P` 116, `ROB` 121, `ASS` 55, `BUR` 61, `LAR` 102, `MVT` 105,
  `ARS` 58, `HOM` 106, `RPE` 118) or an indicator ID.
- Returns one row per category of the requested table: `geography`, `year`,
  `offense`, `table`, `variable` (e.g. `"Victim age"`), `category`
  (`"25-34"`), `estimate`, `lower_bound`, `upper_bound`.
- Bundled vocabularies (`nibrs_estimation_states`,
  `nibrs_estimation_indicators`, available years) built by
  `data-raw/api_vocabularies.R` from `lookup/all`, never by hand.
- A year outside the lookup's years is an error naming the available ones.

### 4.2 `imputation_gap()`: the capstone

```r
imputation_gap(state_abb = NULL, year, offense = "V")
```

One row per geography-year:

| column | source |
|---|---|
| `reported` | sum of `get_estimated_crime()` monthly counts |
| `coverage` | mean monthly `participated_population / population` |
| `coverage_scaled` | `reported / coverage`: the simplest transparent imputation |
| `published`, `published_lower`, `published_upper` | `get_nibrs_estimate()` total |
| `gap` | `published - reported` |
| `share_estimated` | `gap / published` |

Two requests per row. Nothing is fanned out per agency.

### 4.3 Deliberately out of scope

- **Counties, places and metros.** The FBI publishes no estimate below the
  state through the API (the MSA path is a 404), so the published leg does
  not exist there. Our own leg (`impute_reporting_gaps()` +
  `get_county_crime()`) already covers sub-state geographies.
- **Agency-level imputation for a state.** `impute_reporting_gaps()` over
  every agency in a state is a fan-out of hundreds to thousands of requests;
  `coverage_scaled` is the state-level stand-in. A per-agency version can
  come later behind a guard like `max_agencies`.
- **SRS estimates (pre-2021).** Bulk-only; would need the reader the #58
  decision deferred.

## 5. Testing

Per house rules: offline tests on recorded `nibrs-estimation` responses (Ohio
and the nation, 2022, indicator 133; the `lookup/all` response) and on the
existing `summarized-state-*` fixtures; live tests that the lookup still
lists 2021-2022, that the vocabularies match it, and that Texas 2022's gap
stays under 1%.

## 6. Open questions for review

1. **Naming.** `imputation_gap()` or `published_vs_reported()`? And is now
   the time to rename `get_estimated_crime()` (it returns reported sums),
   with a deprecated alias?
2. **`coverage_scaled`.** It assumes non-reporting agencies have the
   reporters' average rate, which is wrong where small and large agencies
   differ (the FBI imputes by population group). Ship it labelled as such,
   or leave our leg out of the state-level function?
3. **Comparability beyond violent crime.** NIBRS counts every offense in an
   incident; SRS applies the hierarchy rule. For violent crime the two agree
   at full coverage (§3); property crime and the individual offenses need
   the same check before they are offered.
4. **Years.** Only 2021-2022 exist. Worth building for two years, on the
   expectation that the FBI extends the series?
