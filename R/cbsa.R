# County -> CBSA crosswalk accessors and metro discovery.
#
# The crosswalk is derived at build time from the public-domain OMB/Census
# delineation file and shipped in R/sysdata.rda. See data-raw/cbsa_crosswalk.R.

utils::globalVariables("cbsa_crosswalk")

#' The OMB delineation vintage the bundled CBSA crosswalk was built from
#'
#' CBSA definitions are revised periodically and counties move between metros,
#' so results are only reproducible against a stated vintage.
#'
#' @format An integer scalar.
#' @export
CBSA_VINTAGE <- 2023L

# Internal accessor so the crosswalk source is swappable in tests.
.cbsa_table <- function() {
  cbsa_crosswalk
}

#' List the metropolitan and micropolitan statistical areas
#'
#' Returns every CBSA in the bundled crosswalk, with how many counties each
#' contains. This is the discovery counterpart to [metro_agencies()]: it answers
#' "what may I pass as `metro`?"
#'
#' @param type Optionally filter to `"metro"` (Metropolitan Statistical Areas)
#'   or `"micro"` (Micropolitan Statistical Areas). `NULL` (default) returns
#'   both.
#' @return A data.frame with columns `cbsa_code`, `cbsa_title`, `cbsa_type`, and
#'   `n_counties`, carrying the delineation vintage in `attr(x, "vintage")`.
#' @seealso [metro_agencies()], [counties_with_fips()] for the county analogue.
#' @export
#' @examples
#' head(list_metros(type = "metro"))
list_metros <- function(type = NULL) {
  if (!is.null(type) && !type %in% c("metro", "micro")) {
    stop("'type' must be \"metro\", \"micro\", or NULL", call. = FALSE)
  }

  cw <- .cbsa_table()
  if (!is.null(type)) {
    cw <- cw[cw$cbsa_type == type, , drop = FALSE]
  }

  counts <- table(cw$cbsa_code)
  keys <- !duplicated(cw$cbsa_code)

  out <- data.frame(
    cbsa_code = cw$cbsa_code[keys],
    cbsa_title = cw$cbsa_title[keys],
    cbsa_type = cw$cbsa_type[keys],
    n_counties = as.integer(counts[cw$cbsa_code[keys]]),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$cbsa_title), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "vintage") <- CBSA_VINTAGE
  out
}
