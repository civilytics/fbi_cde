#' Deprecated functions
#'
#' `get_estimated_crime()` and `get_estimated_arson()` were renamed
#' [get_reported_crime()] and [get_reported_arson()]: they return the crime
#' agencies reported to the FBI, not the FBI's estimates. The old names
#' still work and warn.
#'
#' @param ... Arguments passed to the new function.
#' @return The new function's result.
#' @name fbiCDE-deprecated
#' @keywords internal
NULL

#' @rdname fbiCDE-deprecated
#' @export
get_estimated_crime <- function(...) {
  .Deprecated("get_reported_crime", package = "fbiCDE")
  get_reported_crime(...)
}

#' @rdname fbiCDE-deprecated
#' @export
get_estimated_arson <- function(...) {
  .Deprecated("get_reported_arson", package = "fbiCDE")
  get_reported_arson(...)
}
