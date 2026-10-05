# ============================================================================
# data-raw/fbi_api_agencies_types.R
# ============================================================================
# Give the bundled agency table real column types.
#
# fbi_api_agencies was stored with every column as character, including
# placeholder strings from the source JSON:
#   - nibrs:            "TRUE"/"FALSE"                 -> logical
#   - latitude/longitude: numbers as text, 545 "NULL"  -> numeric, NA
#   - nibrs_start_date: "MM/DD/YYYY", 9,914 "NULL"     -> Date, NA
# One agency (KY0710900, South Central Kentucky Drug Task Force) carries the
# placeholder coordinates -9, -9; those become NA too.
#
# county_name keeps its "N/A" for agencies with no county (state police HQs,
# tribal agencies, DC): the geography code and county_to_fips() treat that
# value explicitly.
#
# Run from the package root with: Rscript data-raw/fbi_api_agencies_types.R
# It refuses to run twice.
# ============================================================================

load("data/fbi_api_agencies.rda")
a <- fbi_api_agencies

if (!is.character(a$nibrs)) {
  stop("fbi_api_agencies already has typed columns; nothing to do.")
}

null_to_na <- function(x) {
  x[x %in% "NULL"] <- NA_character_
  x
}

stopifnot(all(a$nibrs %in% c("TRUE", "FALSE")))
a$nibrs <- a$nibrs == "TRUE"

a$latitude <- as.numeric(null_to_na(a$latitude))
a$longitude <- as.numeric(null_to_na(a$longitude))
placeholder <- which(a$latitude == -9 & a$longitude == -9)
a$latitude[placeholder] <- NA_real_
a$longitude[placeholder] <- NA_real_

dates <- null_to_na(a$nibrs_start_date)
a$nibrs_start_date <- as.Date(dates, format = "%m/%d/%Y")
stopifnot(identical(is.na(a$nibrs_start_date), is.na(dates)))

fbi_api_agencies <- a
save(fbi_api_agencies, file = "data/fbi_api_agencies.rda",
     compress = "xz", version = 2)
