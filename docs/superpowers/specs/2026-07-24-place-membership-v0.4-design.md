# Design: Place/municipal membership for `fbi` (v0.4)

**Status:** ratified design, not yet implemented
**Issue:** decomposed out of #41 (`[roadmap] v0.4→1.0 — place/municipal membership, metro, place FIPS`)
**Predecessor:** `2026-07-13-geography-first-querying-design.md` (v0.2/v0.3, shipped)

## 0. Scope

This spec covers **place/municipal membership only**. The other two subsystems
bundled into #41 are deferred, each to its own spec → plan cycle:

- **Metro** (= union of member counties; needs a county→CBSA crosswalk and a
  delineation-vintage decision).
- **Place FIPS** as a promised join key.

Rationale for splitting: the three differ sharply in dependency weight and risk.
Place membership is zero-dependency and mirrors the shipped county architecture;
metro needs a new data asset; place FIPS is inherently lossy. Bundling them is
what stalled #41.

## 1. The empirical finding that reframes this phase

The predecessor design (§14) posed place membership as an open choice between
**point-in-polygon, place-name matching, or both**. A probe of the bundled
`fbi_api_agencies` (18,459 rows, 2026-07-24) settles it: for the municipal tier
these are not competing options, because **the agency *is* the place**.

CDE city agencies are named `<Place> Police Department`:

| measure | value |
|---|---|
| municipal-tier agencies (`City`, `Municipality`, `Borough`, `City and Borough`) | 11,635 |
| `City` agencies whose name sheds a recognized suffix | 11,356 / 11,607 (97.8%) |
| remainder — names that are *already* a bare place name (`"Concord"`, `"Wilson"`) | 251 |
| effective place-name derivation rate | ~100% |
| distinct `(state_abbr, county_name, place)` keys | 11,635 |
| **key collisions** | **0** |

So the municipal tier is a **name-identity** problem, not a spatial-containment
problem. No `sf`, no `tigris`, no point-in-polygon is needed to answer "which
agency reports crime for Lufkin, TX."

At the `(state, place)` level only two keys are ambiguous nationally — PA
`Foster Township` and PA `Jefferson Township` — and `county_name` separates both.

**The embedded tier is the opposite story.** Name-matching campus/transit/airport
agencies to a same-county place achieves only **25.6% recall** (596 / 2,324), with
systematic false positives: `"Norfolk Southern Railway"` is a multi-state railroad,
not the city of Norfolk; `"Jefferson State Community College"` is named for a
county. Name-matching is therefore **rejected** for this tier, and spatial
attribution is the only honest mechanism.

This split — name-identity for the primary, point-in-polygon for the embedded —
drives the whole architecture below.

## 2. Architecture

Three units, deliberately separated so the dependency-free core stays pure:

```
place_agencies()            Layer 0  pure resolver, no network, no new deps
  └─ get_place_crime_detail()  Layer 1  fan-out over member ORIs via cde_request()
add_place_spatial_members()  opt-in   sf + tigris (Suggests), network, best-effort
```

The spatial unit is a **separate transform, not an argument**, for the same
reason `impute_reporting_gaps()` is standalone: it is best-effort inference and
it needs heavy optional dependencies plus a network fetch. Making it a separate
call keeps `place_agencies()` offline, dependency-free, and deterministic, and
makes the inference structurally opt-in rather than a flag someone can flip by
accident.

## 3. Layer 0 — `place_agencies(place, state, county = NULL)`

Pure membership resolver. No network, no new dependencies.

### Data asset

A build-time index mapping `(state_abbr, county_name, place)` → ORI for all
11,635 municipal-tier agencies, generated in `data-raw/` and shipped in
`R/sysdata.rda` — the same pattern already used for the county FIPS crosswalk.
Deriving at build time (not at call time) keeps the resolver fast and makes the
derivation rules reviewable in one place.

Place name is derived by stripping a recognized trailing suffix from
`agency_name`:

```
" Police Department" | " Police Dept" | " Police Dept." | " Police"
" Department of Public Safety" | " Public Safety Department"
" Marshal's Office"
```

Names matching no suffix are used verbatim (the 251 bare-name cases). The suffix
list is a reviewable constant, and the build script asserts the derivation rate
stays at ~100% and that `(state, county, place)` remains collision-free — so a
CDE naming drift fails the build loudly instead of silently degrading.

### Return shape

```
ori, agency_name, agency_type_name, agency_class, place_name,
county_name, state_abbr, default_member, attribution
```

- **`agency_class`** — place-level classification:
  | class | source | default |
  |---|---|---|
  | `place_primary` | the place's own municipal agency (name identity) | **include** |
  | `campus` | University or College, spatially inside the place | exclude (opt-in) |
  | `special` | transit/airport/park/Other, spatially inside the place | exclude (opt-in) |

- **`attribution`** — how this row earned membership: `"name_identity"` or
  `"point_in_polygon"`. Provenance is never implicit. Rows from Layer 0 alone are
  always `"name_identity"`.
- **`default_member`** — `TRUE` for `place_primary` only.

### Who is *not* a place member

**Sheriffs, state police, and tribal agencies are never place members.** This is
not a fresh judgment call; it follows from findings already ratified in the
predecessor design §14:

- A sheriff's `population` is the **unincorporated remainder**, which is by
  definition outside every incorporated place.
- **Contract cities report under their own city ORI** (Compton, West Hollywood),
  even where a sheriff does the actual policing.

Together these mean a place's crime is carried entirely by its own ORI — no
double-count and no gap. Attributing a sheriff to a place would double-count;
omitting the contract city would lose it. Neither occurs.

### Errors and edges

- Unknown `(place, state)` → error naming the place and state, and suggesting
  `counties_with_fips()`-style discovery (a `places_in_state()` helper is a
  reasonable follow-on, not required here).
- Ambiguous key without `county` → error listing the candidate counties, rather
  than silently picking one. Only two such keys exist, but the resolver must not
  guess.
- An incorporated place with no CDE agency at all is a **legitimate empty
  result**, not an error condition to paper over: it means no agency reports for
  that place. Document the distinction explicitly.

## 4. Layer 1 — `get_place_crime_detail()`

```r
get_place_crime_detail(place, state, county = NULL, offense = "V",
                       from = "01-2015", to = "12-2020",
                       agency_class = NULL, default_only = TRUE)
```

Fans out one `cde_request()` per member ORI and returns **unsummed**
per-agency-period rows. This reuses the county fan-out machinery wholesale —
`parse_agency_detail()`, comparison-row stripping, and partial-failure handling
(drop → warn → record in `attr(x, "dropped")`). No new HTTP paths; the single
`cde_request()` seam is untouched.

Return shape is the county detail shape plus `place_name` and `attribution`:

```
ori, agency_name, agency_type_name, agency_class, default_member,
place_name, county_name, state_abbr, attribution, offense, period,
count, population, participated_population, rate, reported
```

In the common case a place has exactly one member ORI, so this degenerates to a
by-name wrapper over `get_agency_crime()`. That is the point rather than a
weakness: **ORI discovery is the package's primary friction** — a 9-character
code with letters in positions 3–9 that users have no way to guess. Querying by
`("Lufkin", "TX")` removes it.

`reported`, `population`, and `participated_population` ride along unchanged, so
place detail composes with `impute_reporting_gaps()` for free.

**No place-level aggregate in v0.4.** With one member ORI in the default case
there is nothing to sum, and once the embedded tier is opted in, summing campus
crime into a city total is exactly the opinionated modeling the county Layer 2
deferred until the denominator was settled. Place aggregation waits for the same
treatment.

## 5. Opt-in spatial tier — `add_place_spatial_members()`

```r
add_place_spatial_members(x, vintage = NULL, places_fun = NULL)
```

Takes a `place_agencies()` result and appends spatially-attributed embedded
agencies. Requires `sf` and `tigris` (**`Suggests`**, guarded at runtime).

### Mechanism

Point-in-polygon of the 2,324 embedded-tier agencies' HQ `latitude`/`longitude`
against `tigris::places()` polygons for the state.

**Only `University or College`, `Other`, and `Other State Agency` are PIP
candidates.** Sheriffs and state police are excluded outright: their HQ point
carries no information about jurisdiction (predecessor §14 caveat). A university
police department's HQ, by contrast, genuinely sits on its campus inside its
city — this tier is precisely where an HQ point is meaningful, which is what
makes the spatial approach defensible here and nowhere else.

### Output

Appended rows carry `attribution = "point_in_polygon"` and
`default_member = FALSE`, plus two columns added to the frame:

- **`place_type`** — `"incorporated"` or `"cdp"`. Matching runs against both
  incorporated places and Census Designated Places, so an embedded agency in an
  unincorporated area lands in a CDP instead of vanishing; the column lets users
  filter to real municipal governments when that is what they mean.
- **`place_fips`** — the matched polygon's GEOID. Free, since PIP already
  computed it. **Best-effort enrichment, explicitly not a promised join key**;
  it is `NA` on `name_identity` rows, and the full place-FIPS deliverable
  remains its own deferred cycle. Documented as such in the function reference,
  not just in a NEWS entry.

### Degradation

Without `sf`/`tigris`, the function emits a clear installation message and
returns its input unchanged — never a partial or silently-empty result. This
mirrors `join_census_pop()`'s behavior when `censusapi` is absent.

## 6. Testing

Per house discipline: every data path gets an offline test that runs on CI, plus
a `skip_if_no_fbi_api()`-guarded live test.

- **Resolver** — pure offline fixture tests, no network and no mocking needed.
  Assert: known place resolves to the expected ORI; the ambiguous PA township
  keys error and list candidate counties; unknown place errors; a place with no
  agency returns an empty frame with correct columns; `attribution` is
  `"name_identity"` throughout.
- **Build-script guards** — the `data-raw/` script asserts ~100% derivation and
  zero `(state, county, place)` collisions, so CDE naming drift breaks the build
  rather than the semantics.
- **Layer 1** — `local_fbi_fixture()` mocking of `cde_request()`, exactly as the
  county detail tests do. Assert comparison-row stripping, the `reported` flag,
  and partial-failure `attr(x, "dropped")`.
- **Spatial tier** — a `places_fun` seam replaces `tigris::places()` with
  fixture polygons, mirroring `cde_request(get_fun=)` and
  `join_census_pop(census_fun=)`. This removes network and `tigris` from the test
  path entirely. The PIP arithmetic itself needs `sf`, so that assertion is
  guarded by `skip_if_not_installed("sf")`; the **degradation path** (deps absent
  → message, input returned unchanged) is tested unconditionally, since that is
  the branch most users on a bare install will hit.

Tests must not be vacuous. A guard that silently iterates zero items passes while
checking nothing — assert non-emptiness of whatever the test enumerates.

## 7. Non-goals

- Metro / CBSA (own cycle).
- Place FIPS as a promised join key (own cycle); only the best-effort byproduct
  above.
- Place-level Layer 2 aggregate (see §4).
- AIANNH/tribal attribution — still post-1.0, still needs Census reservation
  boundaries.
- Any change to `cde_request()` or the existing county surface.

## 8. Decisions with rationale

| decision | rationale |
|---|---|
| Name-identity for the municipal tier | ~100% derivation, 0 key collisions; spatial machinery would add dependencies to solve a problem the names already solve |
| Name-matching **rejected** for the embedded tier | 25.6% recall with systematic false positives; below the package's honesty bar |
| PIP for the embedded tier only | A campus PD's HQ is genuinely inside its place; a sheriff's is not — the tier restriction is what makes PIP defensible |
| Spatial work as a separate transform | Heavy `Suggests` + network + inference; keeps the resolver pure and makes opt-in structural (the `impute_reporting_gaps()` precedent) |
| Sheriff/state/tribal excluded from places | Follows from the ratified unincorporated-remainder and contract-city findings; inclusion would double-count |
| `attribution` column on every row | Provenance must be explicit when two mechanisms with very different reliability feed one frame |
| CDPs included, flagged via `place_type` | Prevents silent NA for unincorporated embedded agencies while letting users filter |
| `place_fips` surfaced but unpromised | Zero marginal cost from PIP; claiming it as a key would overstate coverage |
| No place aggregate in v0.4 | Summation is the opinionated step; same deferral logic the county Layer 2 used |

## 9. Open questions (deferred, not blocking)

- **`tigris` vintage.** Which place-boundary year to default to, and whether to
  pin or follow `tigris`'s default. Boundaries change (annexations, new
  incorporations). Decide during implementation; must be recorded in output
  metadata either way.
- **`places_in_state()` discovery helper.** Likely wanted once users hit the
  "unknown place" error, but not required for this phase.
- **Multi-place special districts.** A transit agency spanning several places
  will PIP into exactly one (its HQ). Currently accepted and flagged via
  `attribution`; a true many-to-many membership model is out of scope.
