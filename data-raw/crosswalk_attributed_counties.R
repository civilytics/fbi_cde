# ============================================================================
# data-raw/crosswalk_attributed_counties.R
# ============================================================================
# Add to the county FIPS crosswalk the counties that only the package's own
# agency attributions use (.AGENCY_COUNTY_ATTRIBUTIONS in R/geography.R).
#
# The crosswalk is keyed by the county names agencies carry. Queens and
# Richmond (Staten Island) counties, NY, and the District of Columbia have no
# agency of their own in the CDE -- the NYPD and DC's Metropolitan Police have
# no county there -- so they were missing, and the metro resolver, which maps
# a CBSA's county FIPS back through this crosswalk, could not reach them.
#
# FIPS codes come from `tigris::fips_codes`, a bundled public-domain table (no
# download), not typed by hand. Rows already present are left alone, so the
# script is safe to run again after editing the attribution table.
#
# Run from the package root with: Rscript data-raw/crosswalk_attributed_counties.R
#
# Output: R/sysdata.rda
#
# WARNING: R/sysdata.rda holds several internal objects and save() overwrites
# the whole file. This script loads every existing object and re-saves them all.
# ============================================================================

stopifnot(requireNamespace("tigris", quietly = TRUE),
          requireNamespace("pkgload", quietly = TRUE))

pkgload::load_all(".", quiet = TRUE)
ns <- asNamespace("fbiCDE")
attributions <- get(".AGENCY_COUNTY_ATTRIBUTIONS", envir = ns)

# ---- 1. Load every existing internal object -------------------------------
existing <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = existing)
objs <- ls(existing)
cat("existing sysdata objects:", paste(objs, collapse = ", "), "\n")
stopifnot("crosswalk" %in% objs)

crosswalk <- get("crosswalk", envir = existing)
stopifnot(is.data.frame(crosswalk), nrow(crosswalk) > 3000)

# ---- 2. The (state, county) pairs the attributions name -------------------
load("data/fbi_api_agencies.rda")
state_of <- fbi_api_agencies$state_abbr[match(names(attributions),
                                              fbi_api_agencies$ori)]
stopifnot(!anyNA(state_of))

pairs <- do.call(rbind, lapply(seq_along(attributions), function(i) {
  counties <- trimws(strsplit(attributions[[i]], ";", fixed = TRUE)[[1]])
  data.frame(state_abbr = state_of[i], county_name = counties,
             stringsAsFactors = FALSE)
}))

have <- paste(crosswalk$state_abbr, crosswalk$county_name, sep = "|")
missing <- pairs[!paste(pairs$state_abbr, pairs$county_name, sep = "|") %in% have,
                 , drop = FALSE]
cat("attributed counties not yet in the crosswalk:", nrow(missing), "\n")

if (nrow(missing) == 0) {
  cat("nothing to do\n")
} else {
  # ---- 3. Derive FIPS from tigris::fips_codes -----------------------------
  fc <- tigris::fips_codes
  fc$bare <- toupper(sub(" County$", "", fc$county))
  fc_key <- paste(fc$state, fc$bare, sep = "|")
  hit <- match(paste(missing$state_abbr, missing$county_name, sep = "|"), fc_key)
  if (anyNA(hit)) {
    print(missing[is.na(hit), ])
    stop("no FIPS for the counties above; refusing to write")
  }
  missing$county_fips <- paste0(fc$state_code[hit], fc$county_code[hit])
  print(missing)

  crosswalk <- rbind(crosswalk, missing[, names(crosswalk)])
  rownames(crosswalk) <- NULL

  # ---- 4. Invariants before writing ---------------------------------------
  keys <- paste(crosswalk$state_abbr, crosswalk$county_name, sep = "|")
  stopifnot(!anyDuplicated(keys))
  single <- crosswalk[!is.na(crosswalk$county_fips) &
                        !grepl(";", crosswalk$county_name, fixed = TRUE), ]
  stopifnot(!anyDuplicated(single$county_fips))

  # ---- 5. Re-save ALL objects (see the WARNING above) ---------------------
  assign("crosswalk", crosswalk, envir = existing)
  save(list = objs, envir = existing,
       file = "R/sysdata.rda", compress = "bzip2", version = 2)
  cat("wrote R/sysdata.rda with objects:", paste(objs, collapse = ", "), "\n")
}
