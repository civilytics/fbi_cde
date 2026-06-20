# Tests for R/http.R: cde_base_url() and cde_request()

# ---- cde_base_url() --------------------------------------------------------

test_that("cde_base_url() returns the default URL", {
  withr::with_options(
    list(fbi.cde.base_url = NULL),
    withr::with_envvar(
      list(FBI_CDE_BASE_URL = ""),
      expect_equal(cde_base_url(), "https://cde.ucr.cjis.gov/LATEST/")
    )
  )
})

test_that("cde_base_url() respects option override", {
  withr::with_options(
    list(fbi.cde.base_url = "https://custom.example.com/PATH/"),
    withr::with_envvar(
      list(FBI_CDE_BASE_URL = "https://env.example.com/"),
      expect_equal(cde_base_url(), "https://custom.example.com/PATH/")
    )
  )
})

test_that("cde_base_url() falls back to env var when option is unset", {
  withr::with_options(
    list(fbi.cde.base_url = NULL),
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

test_that("cde_request() is the only httr::GET caller in the package", {
  r_files <- list.files(system.file("R", package = "fbi"),
                        full.names = TRUE,
                        pattern = "\\.R$")
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    # Allow httr::GET only inside cde_request()
    in_cde_request <- FALSE
    for (line in lines) {
      trimmed <- trimws(line)
      if (grepl("^cde_request\\s*<-\\s*function", trimmed)) {
        in_cde_request <- TRUE
      }
      if (in_cde_request && trimmed == "}") {
        in_cde_request <- FALSE
      }
      if (!in_cde_request && grepl("httr::GET\\s*\\(", trimmed)) {
        fail(paste0("httr::GET found in ", f, ": ", trimmed))
      }
    }
  }
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
