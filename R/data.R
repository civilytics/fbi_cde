# Documentation for the bundled datasets in data/. The datasets themselves are
# built by scripts in data-raw/ (api_vocabularies.R rebuilds the NIBRS and
# arrest vocabularies from the live API). This file used to also define copies
# of several of them as package objects, a second, stale source of truth that
# unqualified references silently picked up.

#' All available variables for NIBRS offender data.
#'
#' The values `get_nibrs_offender()` accepts for `variable`: the keys of a
#' NIBRS response's offender section. Built by `data-raw/api_vocabularies.R`.
#'
#' @format A character vector with 4 elements.
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"nibrs_offender_variables"

#' All available variables for NIBRS offense data.
#'
#' The values `get_nibrs_offense()` accepts for `variable`: the keys of a
#' NIBRS response's offense section. Built by `data-raw/api_vocabularies.R`.
#'
#' @format A character vector with 2 elements.
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"nibrs_offense_variables"

#' Offense codes the NIBRS functions accept.
#'
#' The summary groups (`"V"`, `"P"`, `"ROB"`, `"BUR"`, ...) and the NIBRS
#' offense codes from the CDE's offense lookup. The API also accepts NIBRS
#' codes missing from the lookup, such as `"13A"` or `"120"`. Built by
#' `data-raw/api_vocabularies.R`.
#'
#' @format A data frame with 73 rows and 2 variables:
#' \describe{
#'   \item{code}{Offense code, as passed to `offense`}
#'   \item{label}{Offense name}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"nibrs_offenses"

#' All available variables for NIBRS victim data.
#'
#' The values `get_nibrs_victim()` accepts for `variable`: the keys of a
#' NIBRS response's victim section. Built by `data-raw/api_vocabularies.R`.
#'
#' @format A character vector with 6 elements.
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"nibrs_victim_variables"

#' Regions available to get estimated UCR and NIBRS data for.
#'
#' Some functions let you get regional estimated data and require a
#' string input for which region you want data from. This is a vector
#' of strings for the six regions available.
#'
#' @format A vector with 6 elements:
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"regions"

#' All offenses available to get UCR arrest counts for.
#'
#' The offense names `get_arrest_count()` accepts: every key of the three maps
#' (offense name, category, breakdown) in the CDE's arrest totals response.
#' Built by `data-raw/api_vocabularies.R`.
#'
#' @format A character vector with 80 elements.
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"ucr_arrest_offenses"


#' All agencies included in the FBI's Crime Data Explorer API.
#'
#' A dataset containing information about the 18,459 agencies included in the
#' FBI's API. It is a snapshot: NIBRS start dates run to September 2019, so the
#' NIBRS columns predate the large 2020-2021 transition. Use
#' [get_agency_participation()] for an agency's current NIBRS status.
#'
#' @format A data frame with 18459 rows and 13 variables:
#' \describe{
#'   \item{ori}{9-character unique ID for the agency (ORI)}
#'   \item{agency_name}{Agency name}
#'   \item{agency_type_name}{Type of department, e.g. City, County,
#'   University or College}
#'   \item{state_name}{State name}
#'   \item{state_abbr}{State abbreviation}
#'   \item{division_name}{Census division name}
#'   \item{region_name}{Census region name}
#'   \item{region_desc}{Census region code}
#'   \item{county_name}{County name, uppercase. Semicolon-separated for an
#'   agency that polices several counties; \code{"N/A"} when the agency has
#'   no county}
#'   \item{nibrs}{Logical: whether the agency reported to NIBRS as of the
#'   snapshot}
#'   \item{latitude}{Numeric agency latitude; \code{NA} when unknown}
#'   \item{longitude}{Numeric agency longitude; \code{NA} when unknown}
#'   \item{nibrs_start_date}{Date the agency started reporting to NIBRS;
#'   \code{NA} if it had not}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"fbi_api_agencies"
