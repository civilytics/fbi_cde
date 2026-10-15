# The CDE's NIBRS estimates: nibrs-estimation/{national,state/{id}}/{indicator}
# with a required ?year=YYYY. The path is used by the CDE web app's "NIBRS
# Estimates" page and is absent from the CDE's public API documentation. State
# and indicator IDs are the lookup's own numbers, not FIPS (Alaska is 1,
# California 6). See specs/2026-10-09-capstone-imputation-gap-design.md.

# Names the estimates lookup uses that datasets::state.name does not.
.ESTIMATE_STATE_EXTRAS <- c(
  "District of Columbia" = "DC", "Puerto Rico" = "PR", "Guam" = "GU",
  "U.S. Virgin Islands" = "VI", "American Samoa" = "AS",
  "Canal Zone" = "CZ", "Mariana Islands" = "MP"
)

# Parse nibrs-estimation/lookup/all into the three bundled vocabularies.
# "Federal" and "Other" have no state abbreviation and are left out.
.parse_estimation_lookup <- function(response) {
  data <- response[[1]]$data

  abbr <- datasets::state.abb
  names(abbr) <- datasets::state.name
  abbr <- c(abbr, .ESTIMATE_STATE_EXTRAS)

  st <- data$States
  states <- data.frame(
    state_abbr = unname(abbr[names(st)]),
    state_name = names(st),
    id = as.integer(unlist(st, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  states <- states[!is.na(states$state_abbr), , drop = FALSE]
  states <- states[order(states$id), , drop = FALSE]
  rownames(states) <- NULL

  ind <- data$Indicators
  indicators <- do.call(rbind, lapply(names(ind), function(group) {
    data.frame(
      group = group,
      indicator = names(ind[[group]]),
      id = as.integer(unlist(ind[[group]], use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  }))
  indicators <- indicators[order(indicators$group, indicators$id,
                                 method = "radix"), , drop = FALSE]
  rownames(indicators) <- NULL

  years <- sort(as.integer(unlist(data$Years, use.names = FALSE)))

  list(states = states, indicators = indicators, years = years)
}

# Summary offense codes the estimates and imputation_gap() support: the
# estimate indicator each maps to (by its name in nibrs_estimation_indicators)
# and the NIBRS offense codes whose counts make up the same offense. Summary
# property crime is burglary, larceny and motor vehicle theft (arson is
# separate); summary rape is the revised definition (11A, 11B, 11C). Each
# code was checked live against summarized/ on 2026-10-15 (spec 3.3).
.ESTIMATE_OFFENSES <- list(
  V   = list(indicator = "Violent Crime",
             nibrs = c("09A", "11A", "11B", "11C", "120", "13A")),
  P   = list(indicator = "Property Crime",
             nibrs = c("220", "23A", "23B", "23C", "23D", "23E", "23F",
                       "23G", "23H", "240")),
  ROB = list(indicator = "Robbery", nibrs = "120"),
  ASS = list(indicator = "Aggravated Assault", nibrs = "13A"),
  BUR = list(indicator = "Burglary/B&E", nibrs = "220"),
  LAR = list(indicator = "Larceny/Theft Offenses",
             nibrs = c("23A", "23B", "23C", "23D", "23E", "23F", "23G",
                       "23H")),
  MVT = list(indicator = "Motor Vehicle Theft", nibrs = "240"),
  ARS = list(indicator = "Arson", nibrs = "200"),
  HOM = list(indicator = "Murder and Non-negligent Manslaughter",
             nibrs = "09A"),
  RPE = list(indicator = "Rape", nibrs = c("11A", "11B", "11C"))
)
