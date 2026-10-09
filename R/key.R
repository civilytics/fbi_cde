#' Returns the FBI's Crime Data Explorer API key
#'
#' @description
#' **Deprecated.** The current CDE API (`cde.ucr.cjis.gov/LATEST/`) does not
#' require an API key. This function is retained only for backward compatibility
#' with code that previously used the legacy `api.data.gov`-hosted API.
#'
#' @return
#' `NULL`. A deprecation message is printed on every call.
#' @export
#'
#' @examples
#' \dontrun{
#' get_api_key()
#' }
get_api_key <- function() {
  message("get_api_key() is deprecated. The current CDE API (cde.ucr.cjis.gov) does not require an API key.")
  NULL
}

#' Sets the FBI's Crime Data Explorer API key
#'
#' @description
#' **Deprecated.** The current CDE API (`cde.ucr.cjis.gov/LATEST/`) does not
#' require an API key. This function is retained only for backward compatibility
#' with code that previously used the legacy `api.data.gov`-hosted API.
#'
#' @param key A character string with the API key (ignored).
#'
#' @return
#' `NULL`. A deprecation message is printed on every call.
#' @export
#'
#' @examples
#' \dontrun{
#' set_api_key("abc123")
#' }
set_api_key <- function(key) {
  message("set_api_key() is deprecated. The current CDE API (cde.ucr.cjis.gov) does not require an API key.")
  NULL
}
