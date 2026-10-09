# Design: place FIPS as a promised join key (#46)

**Status:** implemented
**Issue:** Gitea #46, `[roadmap] Place FIPS as a promised join key (sf/tigris, lossy)`
**Predecessors:** `2026-07-24-place-membership-v0.4-design.md` (place
membership; shipped `place_fips` only as a best-effort by-product of
`add_place_spatial_members()`), `2026-07-13-geography-first-querying-design.md` §8.

## 1. Goal

Give every municipal agency returned by `place_agencies()` the Census code of
the unit it polices, so place results can be joined to Census data. "Promised"
means:

- **deterministic and offline**: bundled, no network, no `sf`;
- **stated coverage**, with unresolved agencies `NA`, never guessed;
- **stated vintage** (`PLACE_VINTAGE`).

The point-in-polygon `place_fips` on rows added by
`add_place_spatial_members()` stays best-effort; it describes where a campus
agency's headquarters sits, not a jurisdiction.

## 2. What the probe found (bundled agency table, 11,646 municipal agencies)

1. **Name matching works; coordinates do not.** Matching each agency's derived
   place name to the 2023 Gazetteer within its state resolved 87%. But agency
   coordinates are too unreliable to arbitrate: correct matches sat up to
   1,035 km from the agency's point (Unalaska AK, Springville UT, Corona CA),
   and a unique name match could still be wrong ("Lakeview PD", Harris County
   TX, matched a Lakeview hundreds of km away).
2. **The county is the reliable arbiter.** Same-named places are common within
   a state but almost never within a county, and every agency carries its
   county attribution. The Census 2020 reference file
   `national_place_by_county2020.txt` links each place to the counties it
   touches.
3. **Most unmatched agencies are not places at all.** 641 were townships and
   most of the rest New England and New York towns -- minor civil divisions
   (MCDs), which are governments but have county-subdivision codes, not place
   codes. `national_cousub2020.txt` lists them with their county.
4. **146 agency names carried a county disambiguator** that
   `derive_place_name()` did not strip ("Clay Township Police Department,
   Montgomery County"; "Hamilton Township, Mercer County Police Department"),
   which also made those places unreachable by name in `place_agencies()`.
5. Normalisation matters: "Du Bois"/"DuBois", "La Salle"/"LaSalle",
   "Espanola"/"Española", and Massachusetts cities named "<Name> Town city".
   Consolidated cities (DC, Indianapolis, Athens-Clarke) carry a non-active
   FUNCSTAT, so incorporation is read from the file's TYPE column.

## 3. Decision

A bundled ORI-keyed crosswalk, `place_crosswalk` in `R/sysdata.rda`, built by
`data-raw/place_fips_crosswalk.R` from the Census 2020 reference code files
(public domain, one vintage for both files).

Matching, per municipal agency (the package's own `.municipal_agencies()`,
after its county attributions):

1. Candidates: Census units in the agency's state whose normalised name equals
   the agency's place name, with or without the Census descriptor ("Lufkin
   city", "Manheim township"), and which lie in one of the agency's counties.
   An agency whose name ends in a type word also matches a subdivision of
   that same type by its bare name: "Bloomfield Township" matches "Bloomfield
   charter township", but "Linn Township" does not match "Linn town".
2. Tiers: exactly one **incorporated place** -> `place_fips`, `place_type =
   "incorporated"`; else exactly one functioning **county subdivision** ->
   `cousub_fips`, `place_type = "county_subdivision"`; else exactly one
   **CDP** -> `place_fips`, `place_type = "cdp"`. A police department belongs
   to a government, so a functioning subdivision beats a same-named CDP.
3. More than one candidate in the deciding tier, or none: `NA`.
4. An agency with no county (a handful the CDE leaves `"N/A"`) is matched only
   to an incorporated place unique in its state.

Output columns, on `place_agencies()` and `get_place_crime_detail()`:
`place_type`, `place_fips` (7 digits, state + place), `cousub_fips` (10 digits,
state + county + subdivision). Exactly one code is set per resolved row.

`derive_place_name()` now strips the county disambiguator in either position.
`(state, county, place)` keys stay unique (0 collisions).

## 4. Coverage (2020 vintage)

| outcome | agencies |
|---|---|
| incorporated place | 9,894 |
| county subdivision | 1,580 |
| CDP | 19 |
| unresolved (`NA`) | 153 |
| **resolved** | **98.7%** |

The unresolved are mostly regional and multi-municipality departments
("Northern Berks Regional", "New Paltz Town and Village"), a few joint
city-county governments, and two names matching two units in one county
(Superior WI, city and village; Rangeley ME, town and plantation).

## 5. Rejected alternatives

- **Point-in-polygon for the municipal tier** (`sf`/`tigris`): heavy, network
  bound at run time, and the HQ point is unreliable (§2.1).
- **Coordinates as tie-breaker**: same reason; the county check needs none.
- **2023 Gazetteer**: newer names, but no place-to-county link; mixing it with
  the 2020 relationship file would mix vintages.
- **LEAIC**: licensing and staleness, as for the county crosswalk.

## 6. Invariants (tested)

Unique ORIs; one code per resolved row, matching its type; code formats; every
code's state prefix is the agency's state; every subdivision lies in one of
the agency's counties; resolved share >= 98%; `attr(place_crosswalk,
"vintage") == PLACE_VINTAGE`.

## 7. Not in scope

A Census population join at place level (`join_census_pop()` is county-only);
codes for county-level results; refreshing the agency snapshot.
