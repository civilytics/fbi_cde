# ============================================================================
# data-raw/fix_county_fips_state_prefix.R
# ============================================================================
# Repair county FIPS codes whose state prefix disagrees with the row's own
# state (Gitea #50).
#
# Eight rows in the shipped crosswalk pointed at Connecticut: MA/NJ/VA
# MIDDLESEX -> 09007, OH/SC FAIRFIELD -> 09001, VT WINDHAM -> 09015.
# Connecticut retains historical county names that also exist in other states,
# and something in the original derivation let CT's codes win for all of them.
#
# The consequence was not cosmetic: join_census_pop() keys on county FIPS, so
# Massachusetts Middlesex (~1.63M residents) was silently receiving Connecticut
# Middlesex's (~164k) population — a ~10x denominator error with no warning.
#
# Corrections are derived from `tigris::fips_codes`, a bundled public-domain
# state/county FIPS table (no download required), rather than hand-typed.
#
# Run with: Rscript data-raw/fix_county_fips_state_prefix.R
#
# Output: R/sysdata.rda
#
# WARNING: R/sysdata.rda may hold several internal objects and save() overwrites
# the whole file. This script loads every existing object and re-saves them all.
# ============================================================================

stopifnot(requireNamespace("tigris", quietly = TRUE))

state_abbr_to_fips <- c(
  AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08",
  CT = "09", DE = "10", DC = "11", FL = "12", GA = "13", HI = "15",
  ID = "16", IL = "17", IN = "18", IA = "19", KS = "20", KY = "21",
  LA = "22", ME = "23", MD = "24", MA = "25", MI = "26", MN = "27",
  MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33",
  NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38", OH = "39",
  OK = "40", OR = "41", PA = "42", RI = "44", SC = "45", SD = "46",
  TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53",
  WV = "54", WI = "55", WY = "56",
  PR = "72", GU = "66", VI = "78", AS = "60", GM = "69"
)

# ---- 1. Load every existing internal object -------------------------------
existing <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = existing)
objs <- ls(existing)
cat("existing sysdata objects:", paste(objs, collapse = ", "), "\n")
stopifnot("crosswalk" %in% objs)

crosswalk <- get("crosswalk", envir = existing)
stopifnot(is.data.frame(crosswalk), nrow(crosswalk) > 3000)

# ---- 2. Find rows violating the state-prefix invariant --------------------
has_fips <- !is.na(crosswalk$county_fips)
expected_prefix <- unname(state_abbr_to_fips[crosswalk$state_abbr])
bad <- has_fips & substr(crosswalk$county_fips, 1L, 2L) != expected_prefix
bad[is.na(bad)] <- FALSE

cat("rows violating the state-prefix invariant:", sum(bad), "\n")
print(crosswalk[bad, ])

# ---- 3. Derive corrections from tigris::fips_codes ------------------------
fc <- tigris::fips_codes
# Strip the county-type suffix so names match the CDE's bare uppercase form.
fc$bare <- toupper(sub(
  " (County|Parish|Borough|Census Area|City and Borough|Municipality|city|City)$",
  "", fc$county
))
fc$fips <- paste0(fc$state_code, fc$county_code)
fc_key <- paste(fc$state, fc$bare, sep = "|")

# A multi-county county_name ("FAIRFIELD; LICKING") resolves on its first part,
# matching county_to_fips()'s own behaviour.
primary_name <- toupper(trimws(vapply(
  strsplit(crosswalk$county_name, ";", fixed = TRUE),
  function(z) z[1], character(1)
)))
lookup_key <- paste(crosswalk$state_abbr, primary_name, sep = "|")

corrected <- fc$fips[match(lookup_key[bad], fc_key)]
cat("\ncorrections derived:\n")
print(data.frame(
  state = crosswalk$state_abbr[bad],
  county = crosswalk$county_name[bad],
  was = crosswalk$county_fips[bad],
  now = corrected,
  stringsAsFactors = FALSE
))

if (any(is.na(corrected))) {
  stop("could not derive a correction for every offending row; refusing to write")
}

crosswalk$county_fips[bad] <- corrected

# ---- 4. Re-assert the invariant before writing ----------------------------
has_fips <- !is.na(crosswalk$county_fips)
expected_prefix <- unname(state_abbr_to_fips[crosswalk$state_abbr])
still_bad <- has_fips & substr(crosswalk$county_fips, 1L, 2L) != expected_prefix
still_bad[is.na(still_bad)] <- FALSE
stopifnot(sum(still_bad) == 0)

# Connecticut's own entries must survive untouched.
ct <- crosswalk[crosswalk$state_abbr == "CT" & has_fips, ]
stopifnot(nrow(ct) > 0, all(substr(ct$county_fips, 1L, 2L) == "09"))

cat("\ninvariant holds for all", sum(has_fips), "rows with a FIPS code\n")

# ---- 5. Re-save ALL objects (see the WARNING above) -----------------------
assign("crosswalk", crosswalk, envir = existing)
save(list = objs, envir = existing,
     file = "R/sysdata.rda", compress = "bzip2", version = 2)

cat("wrote R/sysdata.rda with objects:", paste(objs, collapse = ", "), "\n")
