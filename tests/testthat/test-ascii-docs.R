# Documentation must survive the PDF manual build.
#
# CRAN builds a PDF manual with LaTeX, and LaTeX rejects most non-ASCII
# characters outright. A Sigma (U+03A3) and an almost-equal (U+2248) in
# get_county_crime()'s roxygen produced "1 ERROR" under `R CMD check --as-cran`.
# CI runs with `--no-manual`, so it never surfaced there.
#
# A few characters -- typographic dashes and curly quotes -- are handled by R's
# LaTeX preamble and are allowed. Mathematical and other symbols are not.
# Rather than enumerate what LaTeX supports, this asserts the safe subset.

test_that("Rd files contain no LaTeX-hostile non-ASCII characters", {
  man_dir <- testthat::test_path("..", "..", "man")
  skip_if_not(dir.exists(man_dir), "man/ not available from this test location")

  rd <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)

  # Guard against the guard: an empty man/ would make this pass trivially.
  expect_gt(length(rd), 10L)

  # Dashes and curly quotes round-trip through LaTeX; anything else is suspect.
  allowed <- c("–", "—", "‘", "’", "“", "”")
  pattern <- paste0("[^-", paste(allowed, collapse = ""), "]")

  offenders <- character(0)
  for (f in rd) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep(pattern, lines, useBytes = FALSE)
    if (length(hits) > 0) {
      offenders <- c(offenders, paste0(basename(f), ":", hits))
    }
  }

  expect_equal(
    offenders, character(0),
    info = paste0(
      "non-ASCII characters that break the LaTeX PDF manual: ",
      paste(offenders, collapse = ", ")
    )
  )
})
