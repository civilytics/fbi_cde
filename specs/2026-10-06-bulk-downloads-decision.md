# Decision: FBI bulk downloads (#58)

**Status:** decided (2026-10-06)
**Issue:** Gitea #58, `Investigate FBI bulk download files to fill gaps the
CDE API no longer exposes`

## 1. Question

The package documented four things the CDE API could not supply. Could the
FBI's bulk downloads fill them, and should the package read those files?

1. Arrests by offense and age (juvenile arrests for one offense).
2. State and national *estimated* crime.
3. Assaults on officers (LEOKA).
4. Police employment for states and the nation.

## 2. Finding: two of the four gaps were package bugs

Verified against the live API before acting:

- **Arrests by offense** are served by numeric code,
  `arrest/<level>/<code>`. An offense *name* is an HTTP 400, which the
  package had read as "no longer addressable". Codes give the monthly
  series (`type=counts`) and the age/sex/race breakdown (`type=totals`):
  Ohio's 2023 larceny arrests are 19,500, 1,666 of them under 18. Fixed in
  this branch (`ucr_arrest_offense_codes`; `get_arrest_count()` and
  `get_arrest_demographics()` take any offense).
- **Police employment** is served at `pe` (nation) and `pe/{ST}` (state).
  The package requested `pe/national` and `pe/state/{ST}`, which return
  every value null. Fixed in this branch.

Found along the way: the API's all-offense demographics omit arrests filed
under the five "(Unspecified)" offense codes (Ohio 2023: 883 of 188,836),
exactly, in Ohio, Texas and California. Documented; a live test watches it.

## 3. The real gaps, and the files that fill them

The CDE web app's Downloads page lists files in static catalogues
(`/LATEST/webapp/assets/JSON/downloads/{downloads,masters,cius,leoka,...}.json`)
and fetches each through `GET /LATEST/s3/signedurl?key=<key>`, which returns
`{"<key>": "<presigned URL>"}` (an unknown key returns `{}`). The URL points
at `cde-prd-data.s3.us-gov-east-1.amazonaws.com` and expires in 900 seconds.
The signed-URL call works through `cde_request()` unchanged.

| gap | file (key) | notes |
|---|---|---|
| 2. Estimated crime | `additional-datasets/srs/estimated_crimes_1979_2025.csv` | Nation, states and DC, 1979-2025. **An XLSX despite its name and `text/csv` type** (verified: ZIP signature). The previous year's key was a real but messy CSV (thousands separators, padded names). |
| 3. LEOKA assaults | `master_files/pe/pe-{YYYY}.zip` | Per-agency, fixed-width (7,689-byte records), monthly assault fields, 1985-2025. Reported ~2.5 MB zipped, ~200 MB unzipped per year; layout in `master_files/pe/pe-help.zip`. State totals reported to match the published LEOKA tables where reporting is complete. |
| 3 (rejected) | `additional-datasets/leoka/LEOKA_1995_2025.zip` | Reported unreliable: contents swapped relative to file names, no ORI, totals far from published figures. |

Verified here: the signed-URL call through `cde_request()`, and the
estimates file (226,863 bytes, a valid XLSX workbook). The LEOKA file sizes
and reconciliation are from the investigation and were not re-measured (the
presigned URLs refuse HEAD).

Also unwrapped but available through the API itself, for a future cycle:
`nibrs-estimation/...` (estimates with confidence bounds, 2021-2022 only) and
`hate-crime/...`.

## 4. Decision

- **No bulk-file reader for now.** The two gaps that mattered most (offense
  by age; state staffing) are closed through the API. What remains is a
  yearly spreadsheet with a moving key (estimates) and a 200 MB-per-year
  fixed-width file (LEOKA assaults), each a poor fit for an API wrapper with
  a single network seam and lean dependencies.
- **Document the recipe** (below) so users can fetch either file themselves.
- **`get_estimated_crime()` keeps its name** for compatibility; its docs and
  the Getting Started vignette already say it returns reported sums, not
  estimates. Renaming it is a separate decision.

## 5. If a reader is wanted later

Limit it to the estimates file. It would need:

- a second, mockable network seam beside `cde_request()`, e.g.
  `cde_download(key)`: get the signed URL through `cde_request()`, then
  stream the file with the same retry/backoff rules, tested with a recorded
  file;
- key discovery from `downloads.json`, since keys carry the end year
  (`..._1979_2025`) and change every year;
- `readxl` in Suggests for the current XLSX, plus number cleaning for older
  CSV vintages.

A LEOKA-assault reader is heavier still: a fixed-width layout table and
~200 MB unzipped per year.

## 6. Recipe

```r
key <- "additional-datasets/srs/estimated_crimes_1979_2025.csv"
url <- fbiCDE:::cde_request("s3/signedurl", list(key = key))[[key]]
path <- tempfile(fileext = ".xlsx")   # the file is an XLSX despite its name
utils::download.file(url, path, mode = "wb")   # within 15 minutes
estimates <- readxl::read_excel(path)
```

The download step was run on 2026-10-06; the `readxl` step was not (not
installed in that environment).

Current keys are listed in
`https://cde.ucr.cjis.gov/LATEST/webapp/assets/JSON/downloads/downloads.json`.
These are US government works (17 U.S.C. 105).
