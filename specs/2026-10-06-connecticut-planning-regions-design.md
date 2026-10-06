# Design: Connecticut metros through planning regions (#52)

**Status:** implemented
**Issue:** Gitea #52, `Connecticut metros unsupported: delineation uses
planning regions, CDE uses traditional counties`
**Predecessor:** `2026-07-24-metro-cbsa-v0.5-design.md`, "Connecticut is
unsupported in v0.5, and must say so".

## 1. Problem

Connecticut replaced its eight counties with nine planning regions as county
equivalents in 2022 (FIPS 09110-09190). The 2023 OMB delineation builds the
state's seven CBSAs from planning regions; the bundled agency table (a 2019
snapshot) attributes Connecticut agencies to traditional counties
(09001-09015). The two did not join, so every Connecticut metro resolved to
nothing (with a warning). Planning regions were drawn from towns and cross
county lines, so v0.5 rightly refused to approximate a county mapping.

## 2. Finding

**The CDE has moved to planning regions.** Its live agency directory
(`agency/byStateAbbr/CT`, checked 2026-10-06) gives each Connecticut agency's
planning region as its county ("CAPITOL PLANNING REGION", ...). The
attribution the package needs is therefore the CDE's own; no approximation is
involved.

Of the 107 Connecticut agencies in the bundled table:

| | agencies |
|---|---|
| a planning region in the live directory | 101 |
| "NOT SPECIFIED" / "UNMAPPED COUNTY" (state police, DMV, 2 tribal) | 4 |
| not in the live directory (Yale, UConn Health; listed under other ORIs) | 2 |

## 3. Decision

- `ct_planning_regions` (ORI -> region), in `R/sysdata.rda`, built by
  `data-raw/ct_planning_regions.R` from the live directory for the bundled
  agencies, with the retrieval date as an attribute.
- `agencies_table()` **appends** the region to the agency's county list
  (`"HARTFORD; CAPITOL PLANNING REGION"`). Traditional-county queries keep
  working; `county_agencies("Capitol Planning Region", "CT")` works; metros
  dedupe by ORI as for any multi-county agency. The two systems cover the
  same agencies, so summing across them double counts; documented.
- The nine regions join the county FIPS crosswalk (codes from
  `tigris::fips_codes`), so CBSA county FIPS map back to them.
- The old patch entries that sent a bare region name to one traditional
  county ("NAUGATUCK VALLEY" -> Litchfield) now resolve to the region's own
  FIPS, state-scoped.
- The Connecticut-specific "not supported" warning is gone; the general
  partial-coverage warning remains (Puerto Rico).

## 4. Result

All seven Connecticut CBSAs resolve without warning: Hartford 35 agencies,
Bridgeport-Stamford-Danbury 23, New Haven 15, Waterbury-Shelton 13,
Norwich-New London-Willimantic 11, Putnam 2, Torrington 2: the 101 attributed
agencies, each in exactly one metro. CBSA coverage rises to 1,843 of 3,143
counties and county equivalents; 10 CBSAs (all Puerto Rico) still resolve to
none.

## 5. Known gaps

- Yale and UConn Health campus police are missing from their metros until
  the agency snapshot is refreshed (their live records use other ORIs).
- `join_census_pop()` for a traditional Connecticut county needs ACS vintages
  before 2022; planning regions need 2022 or later.
- Place codes (#46) for Connecticut towns use the 2020 county-based
  subdivision codes; ACS 2022+ codes Connecticut subdivisions under planning
  regions.
