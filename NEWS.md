# fbi 0.1.0.9000 (development version)

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

## Architecture

- `cde_request()` established as the single network seam for all API calls
- `cde_base_url()`, `cde_path()`, and `cde_query()` provide composable URL building
- No API key required for the current CDE host (`cde.ucr.cjis.gov`)
- All date parameters use MM-YYYY format except police employment (YYYY)
