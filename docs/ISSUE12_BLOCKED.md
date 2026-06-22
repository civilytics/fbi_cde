# Issue #12 — Estimates and LEOKA Endpoints

**Status: BLOCKED**

The following CDE API endpoints return 404 Not Found:

- `estimates/{level}/{offense}?from=MM-YYYY&to=MM-YYYY&type=counts`
- `leo/officer/killed?from=MM-YYYY&to=MM-YYYY&type=counts`
- `leo/officer/assault?from=MM-YYYY&to=MM-YYYY&type=counts`

These endpoints were available in the legacy SAPI (`api.data.gov/crime/fbi/sapi/`) but have been removed from the current CDE API at `cde.ucr.cjis.gov/LATEST/`.

No implementation is possible until the FBI restores these endpoints or provides alternative paths.

**Tested:** 2026-06-22
