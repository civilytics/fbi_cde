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

for (obj in c("nibrs_offenses", "nibrs_victim_variables",
              "nibrs_offender_variables", "nibrs_offense_variables",
              "ucr_arrest_offenses")) {
  save(list = obj, file = file.path("data", paste0(obj, ".rda")),
       compress = "bzip2", version = 2)
}
