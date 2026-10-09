# Tests for R/http.R: cde_base_url() and cde_request()

# ---- cde_base_url() --------------------------------------------------------

test_that("cde_base_url() returns the default URL", {
  withr::with_options(
    list(fbiCDE.cde.base_url = NULL),
    withr::with_envvar(
      list(FBI_CDE_BASE_URL = ""),
      expect_equal(cde_base_url(), "https://cde.ucr.cjis.gov/LATEST/")
    )
  )
})

test_that("cde_base_url() respects option override", {
  withr::with_options(
    list(fbiCDE.cde.base_url = "https://custom.example.com/PATH/"),
    withr::with_envvar(
      list(FBI_CDE_BASE_URL = "https://env.example.com/"),
      expect_equal(cde_base_url(), "https://custom.example.com/PATH/")
    )
  )
})

test_that("cde_base_url() falls back to env var when option is unset", {
  withr::with_options(
    list(fbiCDE.cde.base_url = NULL),
    withr::with_envvar(
      list(FBI_CDE_BASE_URL = "https://env.example.com/"),
      expect_equal(cde_base_url(), "https://env.example.com/")
    )
  )
})

# ---- cde_request() ---------------------------------------------------------

# Helper: build a fake httr response object
fake_response <- function(status_code, body_raw = raw()) {
  structure(
    list(
      url = "https://example.com/test",
      status_code = status_code,
      headers = structure(list(), class = "headers"),
      content = body_raw,
      type = "application/json",
      encoding = "bytes"
    ),
    class = "response"
  )
}

test_that("cde_request() stops on 404", {
  fake_resp <- fake_response(404L)
  expect_error(
    cde_request("summarized/national/V", get_fun = function(url, ...) fake_resp),
    "HTTP 404"
  )
})

test_that("cde_request() stops on empty body", {
  fake_resp <- fake_response(200L, raw())
  expect_error(
    cde_request("summarized/national/V", get_fun = function(url, ...) fake_resp),
    "Empty"
  )
})

test_that("cde_request() includes API message in error", {
  api_body <- jsonlite::toJSON(list(message = "Invalid date range"),
                               auto_unbox = TRUE,
                               simplifyVector = TRUE)
  fake_resp <- fake_response(400L, charToRaw(api_body))
  err <- expect_error(
    cde_request("summarized/national/V", get_fun = function(url, ...) fake_resp),
    "Invalid date range"
  )
  expect_match(conditionMessage(err), "HTTP 400")
})

test_that("cde_request() reports the CDE's plain-text error body", {
  # The CDE's 400s are plain text served as application/json; they do not
  # parse, and used to be dropped, leaving a bare "HTTP 400".
  body <- "From year and month date is not valid, expected format MM-YYYY."
  fake_resp <- fake_response(400L, charToRaw(body))
  err <- expect_error(
    cde_request("summarized/national/V", get_fun = function(url, ...) fake_resp),
    "expected format MM-YYYY", fixed = TRUE
  )
  expect_match(conditionMessage(err), "HTTP 400")
})

test_that("cde_request() leaves an HTML error page out of the message", {
  page <- paste0("<!DOCTYPE html><html><head><title>CDE</title></head>",
                 "<body><h1>Not Found</h1></body></html>")
  fake_resp <- fake_response(404L, charToRaw(page))
  err <- expect_error(
    cde_request("no/such/path", get_fun = function(url, ...) fake_resp),
    "HTTP 404"
  )
  expect_no_match(conditionMessage(err), "DOCTYPE")
})

test_that("cde_request() keeps the status when the error body is unreadable", {
  # Embedded NULs make rawToChar() fail.
  fake_resp <- fake_response(500L, as.raw(c(0x41, 0x00, 0x42)))
  withr::local_options(fbiCDE.max_retries = 0)
  expect_error(
    cde_request("summarized/national/V", get_fun = function(url, ...) fake_resp),
    "HTTP 500 for [^ ]+$"
  )
})

test_that("cde_request() survives a JSON scalar error body", {
  fake_resp <- fake_response(400L, charToRaw('"Bad request"'))
  expect_error(
    cde_request("summarized/national/V", get_fun = function(url, ...) fake_resp),
    "HTTP 400 .* - Bad request"
  )
})

test_that("cde_request() returns parsed JSON on success", {
  api_body <- jsonlite::toJSON(list(offenses = list(V = 100)),
                               auto_unbox = TRUE,
                               simplifyVector = TRUE)
  fake_resp <- fake_response(200L, charToRaw(api_body))
  result <- cde_request("summarized/national/V",
                        get_fun = function(url, ...) fake_resp)
  expect_type(result, "list")
  expect_equal(result$offenses$V, 100)
})

# ---- cde_request() is the only httr::GET caller ----------------------------

test_that("cde_request() is the only httr request caller in the package", {
  # Inspect the loaded namespace rather than scanning source text: the R/*.R
  # files are not shipped with an installed package, so a file-based scan finds
  # nothing and silently passes. Every HTTP verb must funnel through
  # cde_request() (see the single-network-seam rule in CLAUDE.md).
  ns <- asNamespace("fbiCDE")
  verbs <- c("GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "RETRY", "VERB")
  pattern <- paste0("httr::(", paste(verbs, collapse = "|"), ")\\b")

  obj_names <- ls(ns, all.names = TRUE)
  fn_names <- Filter(
    function(nm) is.function(get(nm, envir = ns)),
    obj_names
  )
  # Guard against the inspection itself going vacuous.
  expect_true(length(fn_names) > 20L)
  expect_true("cde_request" %in% fn_names)

  offenders <- Filter(
    function(nm) {
      if (identical(nm, "cde_request")) return(FALSE)
      fn <- get(nm, envir = ns)
      code <- c(deparse(body(fn)), unlist(lapply(formals(fn), deparse)))
      any(grepl(pattern, code))
    },
    fn_names
  )

  expect_equal(offenders, character(0))
})

test_that("the httr-seam guard detects a violation", {
  # Meta-test: prove the check above can actually fail. Mirrors its logic
  # against a deliberately non-compliant function.
  bad <- function(url) httr::GET(url)
  code <- c(deparse(body(bad)), unlist(lapply(formals(bad), deparse)))
  expect_true(any(grepl("httr::(GET|POST)\\b", code)))
})

# ---- cde_path() ------------------------------------------------------------

test_that("cde_path() builds national path without offense", {
  expect_equal(cde_path("summarized", "national"), "summarized/national")
  expect_equal(cde_path("shr", "national"), "shr/national")
  expect_equal(cde_path("pe", "national"), "pe/national")
})

test_that("cde_path() builds national path with offense", {
  expect_equal(cde_path("summarized", "national", "V"), "summarized/national/V")
  expect_equal(cde_path("arrest", "national", "LARC"), "arrest/national/LARC")
  expect_equal(cde_path("nibrs", "national", "BUR"), "nibrs/national/BUR")
})

test_that("cde_path() builds state path without offense", {
  expect_equal(cde_path("shr", "state/CA"), "shr/state/CA")
  expect_equal(cde_path("pe", "state/NY"), "pe/state/NY")
})

test_that("cde_path() builds state path with offense", {
  expect_equal(cde_path("summarized", "state/CA", "V"), "summarized/state/CA/V")
  expect_equal(cde_path("arrest", "state/TX", "MUR"), "arrest/state/TX/MUR")
})

test_that("cde_path() builds agency path without offense", {
  expect_equal(cde_path("shr", "agency/CA0010900"), "shr/agency/CA0010900")
})

test_that("cde_path() builds agency path with offense", {
  expect_equal(cde_path("summarized", "agency/CA0010900", "V"),
               "summarized/agency/CA0010900/V")
})

# ---- cde_query() -----------------------------------------------------------

test_that("cde_query() formats years as MM-YYYY by default", {
  result <- cde_query("01-2015", "12-2020")
  expect_equal(result$from, "01-2015")
  expect_equal(result$to, "12-2020")
  expect_null(result$type)
})

test_that("cde_query() includes type when provided", {
  result <- cde_query("01-2015", "12-2020", type = "counts")
  expect_equal(result$from, "01-2015")
  expect_equal(result$to, "12-2020")
  expect_equal(result$type, "counts")
})

test_that("cde_query() formats years as YYYY when four_digit_year = TRUE", {
  result <- cde_query(2015, 2020, four_digit_year = TRUE)
  expect_equal(result$from, "2015")
  expect_equal(result$to, "2020")
})

test_that("cde_query() formats YYYY strings as YYYY when four_digit_year = TRUE", {
  result <- cde_query("2015", "2020", four_digit_year = TRUE)
  expect_equal(result$from, "2015")
  expect_equal(result$to, "2020")
})

test_that("cde_query() includes type with four_digit_year", {
  result <- cde_query(2015, 2020, type = "counts", four_digit_year = TRUE)
  expect_equal(result$from, "2015")
  expect_equal(result$to, "2020")
  expect_equal(result$type, "counts")
})

test_that("cde_query() extracts year from MM-YYYY when four_digit_year = TRUE", {
  result <- cde_query("01-2015", "12-2020", four_digit_year = TRUE)
  expect_equal(result$from, "2015")
  expect_equal(result$to, "2020")
})

# ---- cde_request() retries ---------------------------------------------------

# A get_fun that returns (or raises) each scripted outcome in turn, recording
# how often it was called and with what arguments.
scripted_get <- function(...) {
  outcomes <- list(...)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$args <- list()
  fun <- function(url, ...) {
    calls$n <- calls$n + 1L
    calls$args[[calls$n]] <- list(...)
    out <- outcomes[[min(calls$n, length(outcomes))]]
    if (inherits(out, "error")) stop(out)
    out
  }
  list(fun = fun, calls = calls)
}

ok_response <- function() {
  fake_response(200L, charToRaw('{"ok": true}'))
}

# Record requested sleeps instead of sleeping, and pin the retry/timeout
# options to their defaults so a caller's settings cannot change the result.
local_no_sleep <- function(env = parent.frame()) {
  withr::local_options(fbiCDE.max_retries = NULL, fbiCDE.timeout = NULL,
                       .local_envir = env)
  slept <- new.env(parent = emptyenv())
  slept$s <- numeric(0)
  testthat::local_mocked_bindings(
    .cde_sleep = function(seconds) slept$s <- c(slept$s, seconds),
    .package = "fbiCDE",
    .env = env
  )
  slept
}

test_that("cde_request() retries a transient status, backing off, then succeeds", {
  slept <- local_no_sleep()
  g <- scripted_get(fake_response(503L), fake_response(502L), ok_response())

  out <- cde_request("summarized/national/V", get_fun = g$fun)

  expect_equal(out, list(ok = TRUE))
  expect_equal(g$calls$n, 3L)
  expect_equal(slept$s, c(1, 2))
})

test_that("cde_request() gives up after fbiCDE.max_retries and says so", {
  slept <- local_no_sleep()
  g <- scripted_get(fake_response(503L))

  expect_error(
    cde_request("summarized/national/V", get_fun = g$fun),
    "HTTP 503 .*after 4 attempts"
  )
  expect_equal(g$calls$n, 4L)
  expect_equal(slept$s, c(1, 2, 4))
})

test_that("cde_request() does not retry a 404", {
  slept <- local_no_sleep()
  g <- scripted_get(fake_response(404L))

  expect_error(cde_request("nope", get_fun = g$fun), "HTTP 404")
  expect_equal(g$calls$n, 1L)
  expect_length(slept$s, 0L)
})

test_that("cde_request() retries a network error and reports a persistent one", {
  slept <- local_no_sleep()
  g <- scripted_get(simpleError("Timeout was reached"), ok_response())
  expect_equal(cde_request("x", get_fun = g$fun), list(ok = TRUE))
  expect_equal(g$calls$n, 2L)

  g2 <- scripted_get(simpleError("Could not resolve host"))
  expect_error(
    cde_request("x", get_fun = g2$fun),
    "Request failed for .*after 4 attempts.*Could not resolve host"
  )
})

test_that("cde_request() honours Retry-After on a 429", {
  slept <- local_no_sleep()
  limited <- fake_response(429L)
  limited$headers <- list("retry-after" = "7")
  g <- scripted_get(limited, ok_response())

  cde_request("x", get_fun = g$fun)
  expect_equal(slept$s, 7)
})

test_that("fbiCDE.max_retries = 0 disables retries", {
  slept <- local_no_sleep()
  g <- scripted_get(fake_response(503L), ok_response())

  withr::local_options(fbiCDE.max_retries = 0)
  expect_error(cde_request("x", get_fun = g$fun), "HTTP 503")
  expect_equal(g$calls$n, 1L)
})

test_that("cde_request() passes a per-attempt timeout to get_fun", {
  withr::local_options(fbiCDE.max_retries = NULL, fbiCDE.timeout = NULL)
  g <- scripted_get(ok_response())
  cde_request("x", get_fun = g$fun)
  has_timeout <- function(args, ms) {
    any(vapply(args, function(a) {
      inherits(a, "request") && identical(a$options$timeout_ms, ms)
    }, logical(1)))
  }
  expect_true(has_timeout(g$calls$args[[1]], 60000))

  withr::local_options(fbiCDE.timeout = 5)
  cde_request("x", get_fun = g$fun)
  expect_true(has_timeout(g$calls$args[[2]], 5000))
})

test_that("invalid retry and timeout options fail clearly", {
  g <- scripted_get(ok_response())
  withr::local_options(fbiCDE.max_retries = -1, fbiCDE.timeout = NULL)
  expect_error(cde_request("x", get_fun = g$fun), "fbiCDE.max_retries")
  withr::local_options(fbiCDE.max_retries = 3, fbiCDE.timeout = "soon")
  expect_error(cde_request("x", get_fun = g$fun), "fbiCDE.timeout")
})

# ---- ORI case ----------------------------------------------------------------

test_that("a lower-case ORI is requested upper-cased", {
  # The CDE matches ORIs case-sensitively: agency/ca0010900 is an unknown
  # agency, answered with HTTP 200 and no counts, so a lower-case ORI that
  # passed validation returned zero rows.
  seen <- character(0)
  fixture <- read_fixture("summarized-agency-CA0010900-V.json")
  testthat::local_mocked_bindings(
    cde_request = function(path, ...) {
      seen <<- c(seen, path)
      fixture
    },
    .package = "fbiCDE"
  )

  out <- get_agency_crime("ca0010900", from = "01-2019", to = "03-2019")
  expect_equal(seen[1], "summarized/agency/CA0010900/V")
  expect_equal(unique(out$geography), "CA0010900")

  # The other agency-level wrappers build their paths the same way.
  # Their parsers reject this fixture; only the requested path matters here.
  quietly <- function(expr) {
    try(suppressWarnings(suppressMessages(expr)), silent = TRUE)
  }
  quietly(get_shr(ori = "ca0010900"))
  quietly(get_arrest_count(ori = "ca0010900"))
  quietly(get_arrest_demographics(ori = "ca0010900"))
  quietly(get_nibrs_victim(ori = "ca0010900"))
  quietly(get_police_employment(ori = "ca0010900"))
  agency_paths <- seen[grepl("CA0010900", seen, ignore.case = TRUE)]
  expect_length(agency_paths, 6L)
  expect_false(any(grepl("ca0010900", agency_paths, fixed = TRUE)))
})

test_that("an ORI argument must be a single valid code", {
  expect_error(get_agency_crime(c("CA0010900", "CA0010100")),
               "Invalid ORI code")
  expect_error(get_shr(ori = character(0)), "Invalid ORI code")
})
