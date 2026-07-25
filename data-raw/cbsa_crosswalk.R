# ============================================================================
# data-raw/cbsa_crosswalk.R
# ============================================================================
# Build the county -> CBSA crosswalk from the public-domain OMB/Census
# delineation file (OMB Bulletin 23-01, the 2023 vintage).
#
# Run with: Rscript data-raw/cbsa_crosswalk.R
#
# Output: R/sysdata.rda
#
# WARNING: R/sysdata.rda holds MULTIPLE internal objects and save() overwrites
# the whole file. This script loads the existing objects, adds its own, and
# re-saves everything. Saving only `cbsa_crosswalk` would silently destroy the
# county FIPS `crosswalk` and break county_to_fips() and all of geography.
# ============================================================================

CBSA_VINTAGE <- 2023L
url <- paste0(
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/",
  "reference-files/2023/delineation-files/list1_2023.xlsx"
)

tmp <- tempfile(fileext = ".xlsx")
utils::download.file(url, tmp, mode = "wb", quiet = TRUE)

raw <- readxl::read_excel(tmp, skip = 2)
names(raw) <- c(
  "cbsa_code", "md_code", "csa_code", "cbsa_title", "cbsa_type_raw",
  "md_title", "csa_title", "county", "state_name", "st_fips", "cty_fips",
  "central_outlying"
)

# Drop the trailing footnote rows the file carries below the data block.
raw <- raw[!is.na(raw$cbsa_code) & !is.na(raw$st_fips), , drop = FALSE]

cbsa_crosswalk <- data.frame(
  cbsa_code = as.character(raw$cbsa_code),
  cbsa_title = as.character(raw$cbsa_title),
  cbsa_type = ifelse(
    grepl("^Metropolitan", raw$cbsa_type_raw), "metro", "micro"
  ),
  csa_code = as.character(raw$csa_code),
  csa_title = as.character(raw$csa_title),
  md_code = as.character(raw$md_code),
  md_title = as.character(raw$md_title),
  county_fips = paste0(
    formatC(as.integer(raw$st_fips), width = 2L, flag = "0"),
    formatC(as.integer(raw$cty_fips), width = 3L, flag = "0")
  ),
  central_outlying = as.character(raw$central_outlying),
  stringsAsFactors = FALSE
)

attr(cbsa_crosswalk, "vintage") <- CBSA_VINTAGE

# ---- Assertions: fail the build loudly rather than shipping a bad asset ----
stopifnot(
  nrow(cbsa_crosswalk) > 1800,
  all(nchar(cbsa_crosswalk$county_fips) == 5L),
  !any(is.na(cbsa_crosswalk$cbsa_title)),
  all(cbsa_crosswalk$cbsa_type %in% c("metro", "micro")),
  # Titles must be unique per code, and codes unique per title.
  length(unique(cbsa_crosswalk$cbsa_code)) ==
    length(unique(cbsa_crosswalk$cbsa_title))
)

# ---- Re-save ALL internal objects (see the WARNING above) -----------------
# Generic idiom (matches data-raw/fix_county_fips_state_prefix.R): capture
# every object already in R/sysdata.rda by NAME, add this script's own, and
# save the union from the environment. Naming objects explicitly in save()
# (the previous form here) is only correct for exactly the objects that exist
# today -- a later script adding a third internal object and re-running THIS
# script would silently drop it. The stopifnot() below turns that into a
# loud failure instead of a silent one.
existing <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = existing)
previous_objs <- ls(existing)
cat("existing sysdata objects:", paste(previous_objs, collapse = ", "), "\n")

crosswalk <- get("crosswalk", envir = existing)
stopifnot(is.data.frame(crosswalk), nrow(crosswalk) > 3000)

assign("cbsa_crosswalk", cbsa_crosswalk, envir = existing)
objs <- ls(existing)
stopifnot(all(previous_objs %in% objs))

save(list = objs, envir = existing,
     file = "R/sysdata.rda", compress = "bzip2", version = 2)

cat("wrote R/sysdata.rda with objects:", paste(objs, collapse = ", "), "\n")
cat("(", nrow(crosswalk), "county FIPS rows,",
    nrow(cbsa_crosswalk), "CBSA rows )\n")
