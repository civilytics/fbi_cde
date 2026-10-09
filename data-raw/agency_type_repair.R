# ============================================================================
# data-raw/agency_type_repair.R
# ============================================================================
# Replace the placeholder agency types in the bundled snapshot for Louisiana
# and Alaska with the types the CDE's live directory gives.
#
# The snapshot behind fbi_api_agencies gave every Louisiana agency the type
# "Parish" and every Alaska agency its county equivalent ("Borough", "Census
# Area", "City and Borough", "Municipality"), whatever the agency was: New
# Orleans PD, Delgado Community College and the Alaska State Troopers alike.
# Membership is decided by agency type alone, so every Louisiana agency classed
# as a sheriff (no Louisiana city resolved as a place, and a parish's "own"
# series could be a college's), and the Troopers classed as municipal.
#
# The live directory (agency/byStateAbbr/{LA,AK}) has real types: New Orleans
# PD is "City", Delgado "University or College", the Troopers "State Police".
# This script copies them in by ORI. An agency the directory no longer lists
# (16 Louisiana and 2 Alaska city departments in October 2026) gets NA, an
# unknown type, rather than a guess from its name; classify_agency() treats NA
# as "special", so such an agency is queried only on request.
#
# Every other state's types already match the directory, apart from three
# agencies the CDE has since reclassified (in CA, RI and WV); those are left
# as the snapshot has them.
#
# Run from the package root with: Rscript data-raw/agency_type_repair.R
# Needs pkgload and network access to cde.ucr.cjis.gov. It refuses to run
# twice. Afterwards re-run data-raw/place_fips_crosswalk.R, which codes only
# the municipal agencies and so had skipped every Louisiana city.
# ============================================================================

stopifnot(requireNamespace("pkgload", quietly = TRUE))
pkgload::load_all(".", quiet = TRUE)
ns <- asNamespace("fbiCDE")

PLACEHOLDER_TYPES <- c("Parish", "Borough", "Census Area", "City and Borough",
                       "Municipality")
STATES <- c("LA", "AK")

load("data/fbi_api_agencies.rda")
a <- fbi_api_agencies

target <- which(a$state_abbr %in% STATES &
                  a$agency_type_name %in% PLACEHOLDER_TYPES)
if (length(target) == 0) {
  stop("No placeholder agency types left in fbi_api_agencies; nothing to do.")
}
# The placeholders occur nowhere else.
stopifnot(!any(a$agency_type_name[-target] %in% PLACEHOLDER_TYPES))

directory <- do.call(rbind, lapply(STATES, function(st) {
  d <- get(".flatten_agency_directory", envir = ns)(
    cde_request(cde_path("agency", "byStateAbbr", st))
  )
  d[, c("ori", "agency_type_name")]
}))
stopifnot(!anyDuplicated(directory$ori),
          !any(directory$agency_type_name %in% PLACEHOLDER_TYPES))

live_type <- directory$agency_type_name[match(a$ori[target], directory$ori)]

cat("placeholder types:", length(target), "\n")
cat("replaced from the directory:", sum(!is.na(live_type)), "\n")
cat("not in the directory (set to NA):", sum(is.na(live_type)), "\n")
print(table(old = a$agency_type_name[target],
            new = ifelse(is.na(live_type), "<NA>", live_type)))

a$agency_type_name[target] <- live_type

fbi_api_agencies <- a
save(fbi_api_agencies, file = "data/fbi_api_agencies.rda",
     compress = "xz", version = 2)
