# ============================================================================
# data-raw/api_vocabularies.R
# ============================================================================
# Rebuild the bundled vocabularies the NIBRS and arrest functions document and
# validate against, from the live CDE API:
#
#   nibrs_offenses            offense codes the nibrs/ endpoint accepts
#   nibrs_victim_variables    keys of a response's `victim` section
#   nibrs_offender_variables  keys of a response's `offender` section
#   nibrs_offense_variables   keys of a response's `offense` section
#   ucr_arrest_offenses       every name in the arrest totals' three maps,
#                             with its level (name, category, breakdown)
#   ucr_arrest_offense_codes  the numeric codes the arrest/ endpoint takes for
#                             one offense, each with the name, category and
#                             breakdown it reports under
#
# The previous NIBRS vocabularies did not match the API: offenses were long
# names ("robbery", "burglary-breaking-and-entering") for which the endpoint
# returns no data, and several variables ("count", "bias", ...) do not exist.
# Verified by live probing on 2026-10-05: the endpoint accepts the crime-trend
# lookup codes (ROB, BUR, 13B, 35A, ...) plus V and P, and also other NIBRS
# offense codes not in that lookup (13A, 120, 220, ...).
#
# Run from the package root with: Rscript data-raw/api_vocabularies.R
# Needs network access to cde.ucr.cjis.gov.
# ============================================================================

pkgload::load_all(quiet = TRUE)

# ---- NIBRS offenses -------------------------------------------------------
codes <- get_offense_codes("crime-trend")
# "23*" (Not Specified) is in the lookup but returns no data.
codes <- codes[codes$code != "23*", , drop = FALSE]
nibrs_offenses <- rbind(
  data.frame(code = c("V", "P"),
             label = c("Violent Crime", "Property Crime"),
             stringsAsFactors = FALSE),
  codes
)
rownames(nibrs_offenses) <- NULL

# ---- NIBRS variables ------------------------------------------------------
# Any populated response carries every section and variable.
resp <- cde_request(cde_path("nibrs", "state/OH", "ROB"),
                    cde_query("01-2023", "12-2023", type = "totals"))
nibrs_victim_variables <- sort(names(resp$victim))
nibrs_offender_variables <- sort(names(resp$offender))
nibrs_offense_variables <- sort(names(resp$offense))

# ---- Arrest offenses ------------------------------------------------------
# The totals response always carries all three maps with every key, even
# for an agency with no arrests.
arr <- cde_request(cde_path("arrest", "state/OH", "all"),
                   cde_query("01-2023", "12-2023", type = "totals"))
levels <- c(name = "Offense Name", category = "Offense Category",
            breakdown = "Offense Breakdown")
ucr_arrest_offenses <- do.call(rbind, lapply(names(levels), function(lv) {
  data.frame(offense = names(arr[[levels[[lv]]]]), level = lv,
             stringsAsFactors = FALSE)
}))
ucr_arrest_offenses <- ucr_arrest_offenses[
  order(match(ucr_arrest_offenses$level, names(levels)),
        ucr_arrest_offenses$offense), ]
rownames(ucr_arrest_offenses) <- NULL

# ---- Arrest offense codes ---------------------------------------------------
# arrest/{level}/{code} takes a numeric code (an offense name is an HTTP 400)
# and answers with that offense's own monthly series (type=counts) and
# demographics (type=totals). The codes are in lookup/offenses?type=arrest,
# except Suspicion (320), which the lookup omits but the endpoint serves.
#
# Each code's totals response reports its arrests under exactly one name, one
# category and one breakdown of the three maps; that is how a code is placed.
# A national ten-year range makes every code with any arrests non-zero. A code
# with none (23, "Rape - Not Specified") is left out: Rape arrests are all
# reported under "Rape (Legacy)" (20). Rape and Runaway therefore have no
# code, and the API reports no arrests under them.
groups <- cde_request("lookup/offenses", list(type = "arrest"))$crimeGroups
lookup <- do.call(rbind, lapply(groups, function(grp) {
  do.call(rbind, lapply(grp$crimes, function(cr) {
    data.frame(code = as.character(cr$value), label = cr$label,
               stringsAsFactors = FALSE)
  }))
}))
lookup <- lookup[lookup$code != "all", , drop = FALSE]
lookup <- rbind(lookup, data.frame(code = "320", label = "Suspicion",
                                   stringsAsFactors = FALSE))

nonzero <- function(map) {
  v <- unlist(map)
  names(v)[!is.na(v) & v > 0]
}
placed <- lapply(seq_len(nrow(lookup)), function(i) {
  r <- cde_request(cde_path("arrest", "national", lookup$code[i]),
                   list(from = "01-2015", to = "12-2024", type = "totals"))
  hit <- lapply(levels, function(map) nonzero(r[[map]]))
  if (all(lengths(hit) == 0)) {
    return(NULL)
  }
  # One name per map, or the code cannot be placed.
  stopifnot(all(lengths(hit) == 1))
  data.frame(code = lookup$code[i], label = lookup$label[i],
             name = hit$name, category = hit$category,
             breakdown = hit$breakdown, stringsAsFactors = FALSE)
})
ucr_arrest_offense_codes <- do.call(rbind, placed)
ucr_arrest_offense_codes <- ucr_arrest_offense_codes[
  order(as.integer(ucr_arrest_offense_codes$code)), ]
rownames(ucr_arrest_offense_codes) <- NULL
stopifnot(!anyDuplicated(ucr_arrest_offense_codes$code))

for (obj in c("nibrs_offenses", "nibrs_victim_variables",
              "nibrs_offender_variables", "nibrs_offense_variables",
              "ucr_arrest_offenses", "ucr_arrest_offense_codes")) {
  save(list = obj, file = file.path("data", paste0(obj, ".rda")),
       compress = "bzip2", version = 2)
}
