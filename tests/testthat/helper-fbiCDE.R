# Test helpers for the fbiCDE package.
#
# Two testing modes are supported:
#
# 1. OFFLINE (default, runs everywhere incl. CI): tests mock the package's
#    single HTTP seam so no network is touched. See `local_fbi_fixture()`.
#
# 2. LIVE (opt-in): integration tests that hit the real FBI CDE API. These are
#    guarded by `skip_if_no_fbi_api()` and only run locally when FBI_API_KEY is
#    set. They are skipped on CI and CRAN. Use them to (re)record fixtures.

# ---- Live-API guard -------------------------------------------------------

fbi_has_key <- function() {
  nzchar(Sys.getenv("FBI_API_KEY"))
}

# Skip a test that requires the live FBI CDE API (network + key).
skip_if_no_fbi_api <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  if (!fbi_has_key()) {
    testthat::skip("FBI_API_KEY not set; skipping live API test")
  }
}

# ---- Offline fixtures -----------------------------------------------------

# Read a saved API response fixture from tests/testthat/fixtures/.
# Store the *raw* JSON exactly as the API returns it, and parse it exactly as
# cde_request() does (simplifyVector = FALSE, nested lists), so tests exercise
# the shape the parsers see in production. Fixtures used to be simplified to
# data.frames here, which let a parser that only worked on that shape pass.
read_fixture <- function(name) {
  path <- testthat::test_path("fixtures", name)
  if (!file.exists(path)) {
    stop("Missing fixture: ", path,
         "\nRecord it from a live response (see tests/testthat/fixtures/README.md).")
  }
  jsonlite::fromJSON(readLines(path, warn = FALSE), simplifyVector = FALSE)
}

# Run a test with the package's HTTP layer stubbed to return a fixture instead
# of performing a real request.
#
# This relies on the convention that ALL network access in the package goes
# through a single internal seam, `cde_request()` (see the HTTP-core issue),
# which returns already-parsed JSON. Mocking that one function makes every
# data function testable offline.
#
#   test_that("get_estimated_crime parses national summary", {
#     local_fbi_fixture("summarized-national-V.json")
#     out <- get_estimated_crime()
#     expect_s3_class(out, "data.frame")
#   })
local_fbi_fixture <- function(fixture, env = parent.frame()) {
  data <- read_fixture(fixture)
  testthat::local_mocked_bindings(
    cde_request = function(...) data,
    .package = "fbiCDE",
    .env = env
  )
}
