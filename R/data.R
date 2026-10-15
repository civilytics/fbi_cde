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
#' in the CDE's arrest totals response, with the map it comes from. A name can
#' appear at more than one level. Use [list_ucr_arrest_offenses()] to list them.
#' Built by `data-raw/api_vocabularies.R`.
#'
#' @format A data frame with 112 rows and 2 variables:
#' \describe{
#'   \item{offense}{Offense name, as passed to `get_arrest_count(offense = )`}
#'   \item{level}{`"name"` (34), `"category"` (29) or `"breakdown"` (49)}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"ucr_arrest_offenses"

#' Arrest offense codes
#'
#' The CDE's arrest endpoint addresses one offense by numeric code
#' (`arrest/<level>/<code>`); an offense name is rejected. Each code is one
#' breakdown, and reports its arrests under one offense name and one category.
#' [get_arrest_count()] and [get_arrest_demographics()] resolve a name at any
#' level to its codes and sum them. Built by `data-raw/api_vocabularies.R`
#' from the `lookup/offenses?type=arrest` codes (plus Suspicion, 320, which
#' the lookup omits), placing each by the names its own response reports.
#' Three names in [ucr_arrest_offenses], `"Rape"`, `"Rape - Not Specified"`
#' and `"Runaway"`, have no code and no arrests.
#'
#' @format A data frame with 47 rows and 5 variables:
#' \describe{
#'   \item{code}{The code, as a string}
#'   \item{label}{The CDE lookup's label for it}
#'   \item{name}{The offense name it reports under}
#'   \item{category}{The category it reports under}
#'   \item{breakdown}{The breakdown it reports under}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"ucr_arrest_offense_codes"


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
#'   University or College. Louisiana's and Alaska's types are the CDE's
#'   live directory's: the snapshot had typed every Louisiana agency
#'   \code{"Parish"} and every Alaska agency by its borough or census area.
#'   \code{NA} for 18 of those agencies that the directory no longer lists}
#'   \item{state_name}{State name}
#'   \item{state_abbr}{State abbreviation}
#'   \item{division_name}{Census division name}
#'   \item{region_name}{Census region name}
#'   \item{region_desc}{Census region code}
#'   \item{county_name}{County name, uppercase. Semicolon-separated for an
#'   agency that polices several counties; \code{"N/A"} when the agency has
#'   no county. This is the CDE's own value: the geography functions also
#'   attribute three \code{"N/A"} agencies (the NYPD, DC's Metropolitan
#'   Police and the Baltimore City Sheriff) to their counties, which this
#'   table does not show; see \code{\link{county_agencies}}}
#'   \item{nibrs}{Logical: whether the agency reported to NIBRS as of the
#'   snapshot}
#'   \item{latitude}{Numeric agency latitude; \code{NA} when unknown}
#'   \item{longitude}{Numeric agency longitude; \code{NA} when unknown}
#'   \item{nibrs_start_date}{Date the agency started reporting to NIBRS;
#'   \code{NA} if it had not}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/}
"fbi_api_agencies"

#' States the FBI's NIBRS estimates cover, with their estimate IDs.
#'
#' The estimates API identifies a state by its own number, in roughly
#' alphabetical order of the postal code (Alaska 1, Alabama 2, California
#' 6), not by FIPS. Built from `nibrs-estimation/lookup/all` by
#' `data-raw/api_vocabularies.R`. The lookup's "Federal" and "Other" entries
#' have no state abbreviation and are left out.
#'
#' @format A data frame with 57 rows and 3 variables:
#' \describe{
#'   \item{state_abbr}{Postal abbreviation, as passed to `state_abb`}
#'   \item{state_name}{Name as the lookup gives it}
#'   \item{id}{The estimates API's state ID}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all}
"nibrs_estimation_states"

#' Offenses ("indicators") the FBI's NIBRS estimates cover.
#'
#' Built from `nibrs-estimation/lookup/all` by `data-raw/api_vocabularies.R`.
#' [get_nibrs_estimate()] accepts an `id` from this table, or one of the
#' summary codes `"V"`, `"P"`, `"ROB"`, `"ASS"`, `"BUR"`, `"LAR"`, `"MVT"`,
#' `"ARS"`, `"HOM"` and `"RPE"`.
#'
#' @format A data frame with 71 rows and 3 variables:
#' \describe{
#'   \item{group}{The lookup's group: `"Person"`, `"Property"`, `"Society"`
#'     or `"GroupB"`}
#'   \item{indicator}{Offense name, e.g. `"Violent Crime"`}
#'   \item{id}{The estimates API's indicator ID, e.g. 133}
#' }
#' @source \url{https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all}
"nibrs_estimation_indicators"

#' Years the FBI's NIBRS estimates cover in the API.
#'
#' Built from `nibrs-estimation/lookup/all` by `data-raw/api_vocabularies.R`.
#' The API publishes estimates for 2021 and 2022 only; a live test fails when
#' the FBI adds a year, so the package can be rebuilt.
#'
#' @format An integer vector.
#' @source \url{https://cde.ucr.cjis.gov/LATEST/nibrs-estimation/lookup/all}
"nibrs_estimation_years"
