#' Returns the FBI's Crime Data Explorer API key
#'
#' Returns the FBI's Crime Data Explorer API key if one is set. For the
#' current CDE host (cde.ucr.cjis.gov), no API key is required, so this
#' function returns NULL when no key is configured rather than erroring.
#'
#' @return
#' A string with the FBI's Crime Data Explorer API key, or NULL if not set.
#' @export
#'
#' @examples
#' \dontrun{
#' get_api_key()
#' }
get_api_key <- function() {
  env <- Sys.getenv("FBI_API_KEY")
  if (env != "") {
    return(env)
  }
  NULL
}

#' Sets the FBI's Crime Data Explorer API key
#'
#' Writes the key to .Renviron. The key is never echoed in messages.
#' Note: the current CDE host (cde.ucr.cjis.gov) does not require an API key.
#'
#' @param key Character string with the API key.
#'
#' @return
#' The key that was set (invisible).
#' @export
#'
#' @examples
#' \dontrun{
#' set_api_key("abc123")
#' }
set_api_key <- function(key) {
  if (is.null(key) || length(key) != 1 || !is.character(key)) {
    stop("key must be a single character string", call. = FALSE)
  }
  Sys.setenv(FBI_API_KEY = key)
  invisible(key)
}
