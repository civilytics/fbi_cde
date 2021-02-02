#' Returns the FBI's Crime Data Explorer API key
#'
#' Returns the FBI's Crime Data Explorer API is one is set. If the key is not
#' set then it will ask you to enter the key in the console and set the key
#' for you.
#'
#' @return
#' A string with the FBI's Crime Data Explorer API key
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

  if (!interactive()) {
    stop("Please set the environment variable FBI_API_KEY to your FBI API key. Keys can be acquired at https://api.data.gov/signup/",
         call. = FALSE)
  }
  message("Please enter your FBI API key and press enter:")
  key <- readline(": ")
  fbi::set_api_key(key)
  return(key)
}

#' Sets the FBI's Crime Data Explorer API key
#'
#' @inheritParams get_agency_crime
#'
#' @return
#' A string with the FBI's Crime Data Explorer API key
#' @export
#'
#' @examples
#' set_api_key("abc123")
set_api_key <- function(key) {
  if (key %in% c("", NA, NULL) || length(key) != 1 || !is.character(key)) {
    message("Incorrect key. Please enter the API key you received from https://api.data.gov/signup/ and press enter:")
    key <- readline(": ")
  }
  Sys.setenv(FBI_API_KEY = key)
  message(paste0("FBI API key set to: '", key, "'."))
  return(key)
}
