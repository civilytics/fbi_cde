# Test fixtures

This directory holds saved FBI CDE API responses (raw JSON) used to test the
package **offline** — no network or API key required. This is what lets the test
suite run on CI.

## How the offline tests work

All network access in the package goes through a single internal seam,
`cde_request()` (defined in the HTTP-core module). It performs the HTTP GET and
returns the parsed JSON body. Tests stub that one function to return a saved
fixture instead, using the helper in `../helper-fbi.R`:

```r
test_that("get_estimated_crime parses a national summary", {
  local_fbi_fixture("summarized-national-V.json")   # stubs cde_request()
  out <- get_estimated_crime()
  expect_s3_class(out, "data.frame")
  expect_true("year" %in% names(out))
})
```

Because the fixture is the *raw* API body, the package's own parsing/reshaping
code is exercised by the test — only the HTTP call is mocked away.

## Recording a fixture

Fixtures are recorded from a live response, then committed. To (re)record:

```r
# locally, with FBI_API_KEY set in the environment
resp <- httr::GET(
  "https://cde.ucr.cjis.gov/LATEST/summarized/national/V?from=01-2015&to=12-2020&type=counts"
)
writeLines(rawToChar(resp$content),
           testthat::test_path("fixtures", "summarized-national-V.json"))
```

Guidelines:

- Name fixtures after the endpoint + key parameters, e.g.
  `summarized-national-V.json`, `arrest-agency-CA0010900-all-counts.json`.
- Keep ranges small (a few years) so fixtures stay readable.
- The CDE host (`https://cde.ucr.cjis.gov/LATEST/`) does **not** require an API
  key, so most fixtures can be recorded without one. Strip any key/token from
  the saved content if present.
- Pair every offline fixture test with a `skip_if_no_fbi_api()`-guarded live
  test that asserts the real endpoint still returns the expected shape, so drift
  in the upstream API is caught.
