# ============================================================================
# data-raw/ct_planning_regions.R
# ============================================================================
# Attribute Connecticut agencies to their planning regions (Gitea #52).
#
# Connecticut replaced its eight counties with nine planning regions as county
# equivalents in 2022 (FIPS 09110-09190). The 2023 OMB delineation builds the
# state's CBSAs from planning regions, so with agencies attributed only to
# traditional counties (as the bundled 2019 agency table has them) every
# Connecticut metro resolved to nothing.
#
# The CDE has made the same move: its live agency directory
# (agency/byStateAbbr/CT) now gives each Connecticut agency's planning region
# as its county. This script records that attribution for the agencies in the
# bundled table, so it is the CDE's, not an approximation of ours. Planning
# regions were drawn from towns, not counties, so there is no county-level
# mapping to derive one from.
#
# It also adds the nine planning regions to the county FIPS crosswalk, with
# codes from tigris::fips_codes (bundled, no download).
#
# Run from the package root with: Rscript data-raw/ct_planning_regions.R
# Needs network access to cde.ucr.cjis.gov.
#
# Output: R/sysdata.rda, adding or replacing `ct_planning_regions` and adding
# planning-region rows to `crosswalk`.
#
# WARNING: R/sysdata.rda holds several internal objects and save() overwrites
# the whole file. This script loads every existing object and re-saves them all.
# ============================================================================

stopifnot(requireNamespace("pkgload", quietly = TRUE),
          requireNamespace("tigris", quietly = TRUE))
pkgload::load_all(".", quiet = TRUE)
ns <- asNamespace("fbiCDE")

# ---- 1. The CDE's own attribution ------------------------------------------
directory <- get(".flatten_agency_directory", envir = ns)(
  cde_request(cde_path("agency", "byStateAbbr", "CT"))
)
stopifnot(all(c("ori", "counties") %in% names(directory)))

load("data/fbi_api_agencies.rda")
bundled <- fbi_api_agencies$ori[fbi_api_agencies$state_abbr == "CT"]

in_region <- grepl(" PLANNING REGION$", directory$counties) &
  directory$ori %in% bundled
# One region per agency; a list would need the multi-county handling.
stopifnot(!any(grepl("[;,]", directory$counties[in_region])))

ct_planning_regions <- data.frame(
  ori = directory$ori[in_region],
  planning_region = toupper(trimws(directory$counties[in_region])),
  stringsAsFactors = FALSE
)
ct_planning_regions <- ct_planning_regions[order(ct_planning_regions$ori), ]
rownames(ct_planning_regions) <- NULL
attr(ct_planning_regions, "retrieved") <- Sys.Date()

cat("bundled CT agencies:", length(bundled), "\n")
cat("with a planning region in the live directory:", nrow(ct_planning_regions), "\n")
print(table(ct_planning_regions$planning_region))
unattributed <- setdiff(bundled, ct_planning_regions$ori)
cat("without one:", length(unattributed), "\n")
print(fbi_api_agencies[fbi_api_agencies$ori %in% unattributed,
                       c("ori", "agency_name", "agency_type_name")])

# ---- 2. Planning regions in the FIPS crosswalk ------------------------------
fc <- tigris::fips_codes
fc <- fc[fc$state == "CT" & as.integer(fc$county_code) >= 110, ]
regions <- data.frame(
  state_abbr = "CT",
  county_name = paste(toupper(fc$county), "PLANNING REGION"),
  county_fips = paste0(fc$state_code, fc$county_code),
  stringsAsFactors = FALSE
)
stopifnot(nrow(regions) == 9,
          all(ct_planning_regions$planning_region %in% regions$county_name))

existing <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = existing)
crosswalk <- get("crosswalk", envir = existing)
have <- paste(crosswalk$state_abbr, crosswalk$county_name)
add <- regions[!paste(regions$state_abbr, regions$county_name) %in% have, ]
crosswalk <- rbind(crosswalk, add)
rownames(crosswalk) <- NULL
cat("planning regions added to the crosswalk:", nrow(add), "\n")

single <- crosswalk[!is.na(crosswalk$county_fips) &
                      !grepl(";", crosswalk$county_name, fixed = TRUE), ]
stopifnot(!anyDuplicated(paste(crosswalk$state_abbr, crosswalk$county_name)),
          !anyDuplicated(single$county_fips))

# ---- 3. Re-save ALL objects (see the WARNING above) -------------------------
assign("crosswalk", crosswalk, envir = existing)
assign("ct_planning_regions", ct_planning_regions, envir = existing)
objs <- ls(existing)
stopifnot(all(c("crosswalk", "cbsa_crosswalk", "place_crosswalk",
                "ct_planning_regions") %in% objs))
save(list = objs, envir = existing,
     file = "R/sysdata.rda", compress = "bzip2", version = 2)
cat("wrote R/sysdata.rda with objects:", paste(objs, collapse = ", "), "\n")
