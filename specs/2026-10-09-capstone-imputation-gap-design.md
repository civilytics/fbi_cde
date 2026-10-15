# Design: the imputation-gap capstone (#42)

**Date:** 2026-10-09
**Status:** Reviewed 2026-10-15; all four questions decided (§6). Not yet
planned or built.
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

At near-full coverage Texas 2022 agrees within 0.3%, but California 2022
does not: 6.5% apart at the same 98.8% coverage, because much of
California still reported in the old format, which the estimate models
rather than uses (§3.2). Where
coverage collapses, the published figure is mostly model: five-sixths of the
FBI's figure for California's 2021 violent crime, and three-tenths of the
nation's, is estimated. This is the exhibit the roadmap wanted, and 2021 (the
NIBRS transition year) is where it matters most.

### 3.1 Checked against California's own count (2026-10-15)

Re-queried live: the 2021 figures reproduce exactly; California's 2022
reported sum is now 193,824 (was 193,814 on 2026-10-09), so the FBI revises
these series and tests must not pin exact values.

The California DOJ collected 2021 data in both formats, converted the IBR
reports to summary format, and published **183,546** violent crimes
(*Crime in California 2021*, Table 3; 466.2 per 100,000;
<https://data-openjustice.doj.ca.gov/sites/default/files/2022-08/Crime%20In%20CA%202021_0.pdf>).
Two consequences:

- **The 2021 gap is missing reports, not missing crime.** California counted
  nearly all of it; the FBI's 2021 collection was NIBRS-only and most
  California agencies had not switched, so their data never reached it.
  `reported` means *reported to the FBI*, and `share_estimated` describes the
  FBI's published figure, not how much crime went uncounted. Column names
  and documentation must say so.
- **The FBI's estimate overshoots the state's count.** 217,539 against
  183,546 is about 34,000 (18.5%) higher, and the state's count is below the
  estimate's 95% lower bound (189,849). Counting rules (the hierarchy rule
  against NIBRS's every-offense count) explain little of this; see §3.2.

### 3.2 Why California 2022 disagrees at full coverage (2026-10-15)

Not counting rules. Texas and California have the same incident structure
in the 2022 estimates (offenses per violent incident: Texas 131,533 /
112,501 = 1.17; California 207,381 / 178,654 = 1.16), and Texas's estimate
matches its reported sum within 0.3%. A counting-rule offset would show in
both.

The cause is the reporting format. The FBI took both formats in 2022, and
`summarized` (our `reported`, and our `coverage`) counts agencies that
reported in either. The estimate is built from NIBRS reports alone and
models every agency outside NIBRS, including agencies that did report, in
the old summary format:

- LAPD (`CA0194200`) has no 2021 months in `summarized` and all 12 in 2022
  (31,772 violent crimes).
- The estimate's 95% interval is ±0.2% for Texas 2022 and ±12% for
  California 2022: a wide interval means a large modelled share, despite
  98.8% `coverage`.
- NIBRS-only counts (§3.3) put California 2022 at 71,625 of 193,824
  reported violent crimes; the other 122,199 arrived in the old format and
  were modelled by the estimate anyway.

So California 2022's gap is the model's estimate for old-format reporters
minus what they actually reported: a measure of model error, not of crime
nobody reported. In 2021 the FBI accepted only NIBRS, so `reported` and the
estimate's inputs coincide, and the 2021 gap is cleanly "modelled".

### 3.3 NIBRS-only totals: `nibrs/{level}/{code}?type=counts` (2026-10-15)

The API's own OpenAPI document (embedded in the CDE web app bundle) gives
`nibrs/{national,state/{ST},agency/{ORI}}/{offense}` a required `type`
query parameter, `counts` or `totals`. Without it, or with `totals`, the
endpoint returns breakdowns (victim age, location, ...). With
`type=counts` it returns monthly `actuals` in the same shape as
`summarized`, counting NIBRS submissions only. It takes NIBRS offense codes
(`09A`, `11A`, `11B`, `11C`, `120`, `13A`, ...); a summary code such as `V`
answers all zeros. Violent crime is the sum of those six codes, so six
requests per geography-year.

| geography | year | `summarized` (any format) | NIBRS only | FBI estimate | estimate − NIBRS |
|---|---|---|---|---|---|
| Texas | 2022 | 131,220 | 132,433 | 131,533 | −0.7% of estimate |
| California | 2021 | 36,439 | 36,943 | 217,539 | 83.0% |
| California | 2022 | 193,824 | 71,625 | 207,381 | 65.5% |
| United States | 2021 | 932,565 | 922,832 | 1,325,991 | 30.4% |
| United States | 2022 | 1,271,206 | 1,007,752 | 1,328,033 | 24.1% |

Where everything arrived as NIBRS (Texas 2022; every state in 2021, when
the FBI accepted nothing else) the two reported series agree within about
1.5%. That residual is the counting-rule difference, and it is small for
violent crime.

California 2022: the FBI modelled 135,756 violent crimes for agencies
outside NIBRS, which actually reported 122,199 in the old format and cover
all but about 1% of the state's population. The model ran roughly 10%
high, consistent with the 2021 estimate's overshoot of the state's own
count (§3.1). Nationally in 2022 the modelled share also covers true
non-reporters (about 5% of the population), so the same subtraction does
not isolate model error there.

The bundled `fbi_api_agencies` cannot split the formats: its `nibrs` flag
is `FALSE` and `nibrs_start_date` `NA` for every California agency. The
live directory (`agency/byStateAbbr/{ST}`) has both (`is_nibrs`;
LAPD's `nibrs_start_date` is 2024-05-01).

State-published counts are a natural third comparison but are PDFs in each
state's own format. Parked; not part of this proposal.

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
| `reported` | sum of `get_reported_crime()` monthly counts (either format) |
| `reported_nibrs` | sum of `nibrs/{level}/{code}?type=counts` over the offense's NIBRS codes (§3.3) |
| `reported_other` | `reported - reported_nibrs`: old-format reports, which the estimate models rather than uses |
| `coverage` | mean monthly `participated_population / population` |
| `published`, `published_lower`, `published_upper` | `get_nibrs_estimate()` total |
| `gap`, `gap_lower`, `gap_upper` | `published - reported_nibrs`, bounds from the published interval |
| `share_estimated`, `share_lower`, `share_upper` | `gap / published`, with bounds |

One `summarized` request, one estimate request and one `nibrs` request per
NIBRS code in the offense (six for violent crime) per row. Nothing is
fanned out per agency.

### 4.3 Deliberately out of scope

- **Counties, places and metros.** The FBI publishes no estimate below the
  state through the API (the MSA path is a 404), so the published leg does
  not exist there. Our own leg (`impute_reporting_gaps()` +
  `get_county_crime()`) already covers sub-state geographies.
- **Agency-level imputation for a state.** `impute_reporting_gaps()` over
  every agency in a state is a fan-out of hundreds to thousands of requests.
  A per-agency version can come later behind a guard like `max_agencies`.
- **Our own state-level imputation** (`coverage_scaled`, dropped in
  review): it assumes non-reporting agencies have the reporters' rate per
  resident, which is wrong where small and large agencies differ, and it
  would sit unbounded beside an estimate with a 95% interval. `coverage` is
  returned so a user can scale the counts themselves.
- **SRS estimates (pre-2021).** Bulk-only; would need the reader the #58
  decision deferred.

## 5. Testing

Per house rules: offline tests on recorded `nibrs-estimation` responses (Ohio
and the nation, 2022, indicator 133; the `lookup/all` response) and on the
existing `summarized-state-*` fixtures; live tests that the lookup still
lists 2021-2022, that the vocabularies match it, and that Texas 2022's gap
stays under 1%.

## 6. Decisions (2026-10-15)

1. **Naming.** The capstone is `imputation_gap()`.
   `get_estimated_crime()` becomes `get_reported_crime()` and
   `get_estimated_arson()` becomes `get_reported_arson()`; the old names stay
   as deprecated aliases. The rename lands first, as its own change.
   **The user-facing documentation defines three terms and uses them
   consistently** — reference pages, the README examples and the vignettes:
   - *reported*: the sum of what agencies submitted to the FBI;
   - *estimated*: the FBI's published figure, which models the agencies that
     did not report and carries a 95% interval;
   - *imputed*: a value filled in for a missing report, whether by the FBI's
     model or by `impute_reporting_gaps()`, and the documentation says which.
2. **`coverage_scaled`.** Dropped (§4.3). `coverage` stays, and `gap` and
   `share_estimated` carry bounds from the published interval (§4.2).
3. **Reporting format.** *Decided: split `reported`.* The full-coverage disagreement is a
   format split, not a counting rule (§3.2). The precise version splits
   `reported` into NIBRS-reported and old-format-reported, because the
   estimate models everything outside NIBRS: `published - reported_nibrs`
   is what the FBI modelled, and the old-format reports are a direct check
   on part of that model. The NIBRS-reported total is available cheaply
   (§3.3: one request per NIBRS code). Agreement at full coverage is not a
   gate; it measures the counting-rule residual, which the documentation
   reports per offense. `imputation_gap()` returns
   `reported`, `reported_nibrs`, `reported_other` (`reported -
   reported_nibrs`), and computes `gap` and `share_estimated` against
   `reported_nibrs`.
4. **Years.** Build it for 2021-2022. The documentation states that the API
   publishes estimates only for those years and only for states, regions
   and the nation, and that coverage widens only if the FBI publishes more.
