# ============================================================================
# data-raw/county_fips_crosswalk.R
# ============================================================================
# Build script for county_fips crosswalk (Issue #36)
#
# Derives state+county_name → 5-digit county_fips from tigris::counties()
# (public-domain Census Tiger/Line data), plus a patch table for known
# mismatches between fbi_api_agencies county_name values and Census names.
#
# Run with: Rscript data-raw/county_fips_crosswalk.R
#
# Output: data/county_fips.rda  (data.frame: state_abbr, county_name, county_fips)
# ============================================================================

library(tigris)
options(tigris_use_cache = TRUE)

# ---- 0. State abbreviation → FIPS prefix mapping (built-in standard) ----
state_fips_map <- c(
  AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08",
  CT = "09", DE = "10", FL = "12", GA = "13", HI = "15", ID = "16",
  IL = "17", IN = "18", IA = "19", KS = "20", KY = "21", LA = "22",
  ME = "23", MD = "24", MA = "25", MI = "26", MN = "27", MS = "28",
  MO = "29", MT = "30", NE = "31", NV = "32", NH = "33", NJ = "34",
  NM = "35", NY = "36", NC = "37", ND = "38", OH = "39", OK = "40",
  OR = "41", PA = "42", RI = "44", SC = "45", SD = "46", TN = "47",
  TX = "48", UT = "49", VT = "50", VA = "51", WA = "53", WV = "54",
  WI = "55", WY = "56", DC = "11", PR = "72", GU = "66", AS = "60",
  VI = "78", MP = "69"
)

# ---- 1. Load fbi_api_agencies to get the target universe ----
load("data/fbi_api_agencies.rda")

# Unique (state_abbr, county_name) pairs from the agency table
fbi_pairs <- unique(fbi_api_agencies[, c("state_abbr", "county_name")])
fbi_pairs$state_fips <- state_fips_map[fbi_pairs$state_abbr]

# ---- 2. Fetch all counties from tigris (Census Tiger/Line) ----
cat("Fetching all counties from tigris...\n")
all_counties <- counties(class = "data.frame")
cat("Total counties from tigris:", nrow(all_counties), "\n")

# Build lookup: "STATEFP|COUNTYNAME" → GEOID (5-digit FIPS)
# For duplicate names (e.g., Baltimore city vs county), also build a type-aware lookup.
# The NAMELSAD column distinguishes "Baltimore city" from "Baltimore County".
tigris_lookup <- with(all_counties, {
  # Determine type from NAMELSAD
  type <- ifelse(grepl(" city$", NAMELSAD, ignore.case = TRUE), "city",
                 ifelse(grepl(" County$", NAMELSAD, ignore.case = TRUE), "county", "other"))
  # 3-part key for type-aware lookup (handles duplicates)
  key3 <- paste(STATEFP, toupper(NAME), type, sep = "|")
  # 2-part key for general lookup (may be ambiguous for duplicates)
  key2 <- paste(STATEFP, toupper(NAME), sep = "|")
  # Build combined lookup: prefer 3-part, fall back to 2-part
  lookup <- setNames(GEOID, key3)
  # Add 2-part keys (only if not already present from 3-part)
  missing2 <- !key2 %in% names(lookup)
  lookup[c(key2[missing2])] <- GEOID[missing2]
  lookup
})

# ---- 3. Patch table: fbi county_name → tigris county_name ----
#
# Each entry explains WHY the patch is needed.
# Keys are the UPPERCASE fbi county_name; values are the tigris NAME.
#
# For multi-county entries (e.g. "SHELBY; JEFFERSON"), the patch maps
# to the *first* county — the one the agency is primarily attributed to.
# ============================================================================
patch_table <- c(
  # ---- Virginia independent cities ----
  # VA has 38 independent cities that are county-equivalents in Census.
  # fbi stores them as "RICHMOND CITY", tigris stores as "Richmond".
  "ALEXANDRIA CITY"      = "Alexandria",
  "BRISTOL CITY"         = "Bristol",
  "BUENA VISTA CITY"     = "Buena Vista",
  "CHARLOTTESVILLE CITY" = "Charlottesville",
  "CHESAPEAKE CITY"      = "Chesapeake",
  "COLONIAL HEIGHTS CITY"= "Colonial Heights",
  "COVINGTON CITY"       = "Covington",
  "DANVILLE CITY"        = "Danville",
  "EMPORIA CITY"         = "Emporia",
  "FAIRFAX CITY"         = "Fairfax",
  "FALLS CHURCH CITY"    = "Falls Church",
  "FRANKLIN CITY"        = "Franklin",
  "FREDERICKSBURG CITY"  = "Fredericksburg",
  "GALAX CITY"           = "Galax",
  "HAMPTON CITY"         = "Hampton",
  "HARRISONBURG CITY"    = "Harrisonburg",
  "HOPEWELL CITY"        = "Hopewell",
  "LEXINGTON CITY"       = "Lexington",
  "LYNCHBURG CITY"       = "Lynchburg",
  "MANASSAS CITY"        = "Manassas",
  "MANASSAS PARK CITY"   = "Manassas Park",
  "MARTINSVILLE CITY"    = "Martinsville",
  "NEWPORT NEWS CITY"    = "Newport News",
  "NORFOLK CITY"         = "Norfolk",
  "NORTON CITY"          = "Norton",
  "PETERSBURG CITY"      = "Petersburg",
  "POQUOSON CITY"        = "Poquoson",
  "PORTSMOUTH CITY"      = "Portsmouth",
  "RADFORD CITY"         = "Radford",
  "RICHMOND CITY"        = "Richmond",
  "ROANOKE CITY"         = "Roanoke",
  "SALEM CITY"           = "Salem",
  "STAUNTON CITY"        = "Staunton",
  "SUFFOLK CITY"         = "Suffolk",
  "VIRGINIA BEACH CITY"  = "Virginia Beach",
  "WAYNESBORO CITY"      = "Waynesboro",
  "WILLIAMSBURG CITY"    = "Williamsburg",
  "WINCHESTER CITY"      = "Winchester",
  "CHARLES CITY"         = "Charles City",  # VA county (not a city)

  # ---- Missouri independent city ----
  # (handled via state-specific patch below)

  # ---- Maryland independent city ----
  # (handled via state-specific patch below)

  # ---- "ST." vs "ST" name variants ----
  # Census uses "St." (with period); fbi stores "ST" (no period).
  "ST CLAIR"             = "St. Clair",
  "ST CLAIR; JEFFERSON"  = "St. Clair",
  "ST CLAIR; CLINTON"    = "St. Clair",
  "ST CLAIR; MADISON"    = "St. Clair",
  "ST CLAIR; MONROE"     = "St. Clair",
  "ST CLAIR; MACOMB"     = "St. Clair",
  "ST FRANCIS"           = "St. Francis",
  "ST JOHNS"             = "St. Johns",
  "ST LUCIE"             = "St. Lucie",
  "ST JOSEPH"            = "St. Joseph",
  "ST MARTIN"            = "St. Martin",
  "ST MARTIN; LAFAYETTE" = "St. Martin",
  "ST BERNARD"           = "St. Bernard",
  "ST CHARLES"           = "St. Charles",
  "ST HELENA"            = "St. Helena",
  "ST JAMES"             = "St. James",
  "ST JOHN THE BAPTIST"  = "St. John the Baptist",
  "ST LANDRY"            = "St. Landry",
  "ST MARY"              = "St. Mary",
  "ST MARY'S"            = "St. Mary's",
  "ST TAMMANY"           = "St. Tammany",
  "ST FRANCOIS"          = "St. Francois",
  "ST LAWRENCE"          = "St. Lawrence",
  "ST CROIX"             = "St. Croix",
  "ST CROIX; PIERCE"     = "St. Croix",
  # "ST LOUIS" handled via state-specific patch (29__ST LOUIS → 29189)
  "ST LOUIS; FRANKLIN"   = "29189",     # MO multi-county → St. Louis County

  # ---- Missouri "Ste." spelling ----
  "STE GENEVIEVE"        = "Ste. Genevieve",

  # ---- Alaska borough/census area name fixes ----
  # "VALDEZ-CORDOVA" was dissolved in 2015; split into Chugach & Copper River.
  # No single FIPS exists — leave as NA (handled below).
  # "ALEUTIANS" → "Aleutians" (Census dropped the extra 'e' in 2020+ data)
  "ALEUTIANS WEST"       = "Aleutians West",
  "ALEUTIANS EAST"       = "Aleutians East",
  "BRISTOL BAY"          = "Bristol Bay",
  "PRINCE OF WALES-HYDER"= "Prince of Wales-Hyder",

  # ---- Connecticut planning regions → traditional counties ----
  # tigris returns CT planning regions (not counties). Map to traditional names.
  "CAPITOL"              = "Hartford",
  "GREATER BRIDGEPORT"   = "Fairfield",
  "LOWER CONNECTICUT RIVER VALLEY" = "New London",
  "NAUGATUCK VALLEY"     = "Litchfield",
  "NORTHEASTERN CONNECTICUT" = "Windham",
  "NORTHWEST HILLS"      = "Litchfield",
  "SOUTH CENTRAL CONNECTICUT" = "New Haven",
  "SOUTHEASTERN CONNECTICUT"  = "New London",
  "WESTERN CONNECTICUT"  = "Fairfield",

  # ---- CT traditional county names (fbi uses these, tigris has planning regions) ----
  # Direct FIPS mapping since tigris doesn't have traditional CT county names.
  "NEW HAVEN"            = "09009",
  "HARTFORD"             = "09003",
  "FAIRFIELD"            = "09001",
  "MIDDLESEX"            = "09007",
  "TOLLAND"              = "09013",
  "NEW LONDON"           = "09011",
  "LITCHFIELD"           = "09005",
  "WINDHAM"              = "09015",

  # ---- Additional name variants ----
  "LA SALLE"             = "LaSalle",    # LA parish (no space, no period)
  "DEWITT"               = "De Witt",    # IL county (space, not capital W)
  "LA PORTE"             = "LaPorte",    # IN county (no space, no period)
  "DONA ANA"             = "Doña Ana", # NM county (with ñ)

  # ---- State-specific patches (key format: "STATEFP__COUNTY") ----
  # These override the general patch table when the state matches.
  "48__DEWITT"           = "DeWitt",     # TX county (capital W, no space)
  "48__LA SALLE"         = "La Salle",   # TX county (space, period)
  # Independent city patches (key format: "STATEFP__COUNTY__CITY")
  # These are only applied when the input has " CITY" suffix.
  # Note: Use "St. Louis" (with period) to match tigris NAME format.
  # For Baltimore, use "Baltimore City" to match crosswalk county_name format.
  "29__ST LOUIS__CITY"   = "St. Louis",  # MO independent city
  "24__BALTIMORE__CITY"  = "Baltimore",  # MD independent city (tigris NAME is "Baltimore")
  # MO "ST LOUIS" without CITY suffix → St. Louis County (direct FIPS)
  "29__ST LOUIS"         = "29189",      # MO county (direct FIPS mapping)
  # MN "ST LOUIS" → St. Louis County (use tigris NAME format)
  "27__ST LOUIS"         = "St. Louis"   # MN county
)

# ---- 4. CT planning region → FIPS direct mapping ----
# tigris returns CT planning regions (not counties), so we map directly.
# Source: Census Bureau traditional county FIPS codes.
ct_region_fips <- c(
  "CAPITOL"              = "09003",   # Hartford
  "GREATER BRIDGEPORT"   = "09001",   # Fairfield
  "LOWER CONNECTICUT RIVER VALLEY" = "09011", # New London
  "NAUGATUCK VALLEY"     = "09005",   # Litchfield
  "NORTHEASTERN CONNECTICUT" = "09015", # Windham
  "NORTHWEST HILLS"      = "09005",   # Litchfield
  "SOUTH CENTRAL CONNECTICUT" = "09009", # New Haven
  "SOUTHEASTERN CONNECTICUT"  = "09011", # New London
  "WESTERN CONNECTICUT"  = "09001"    # Fairfield
)

# ---- 5. Matching function ----
match_county <- function(state_fips, county_name) {
  # Handle "N/A" → NA
  if (is.na(county_name) || toupper(trimws(county_name)) == "N/A") {
    return(NA_character_)
  }

  # Handle VALDEZ-CORDOVA (dissolved 2015)
  if (toupper(trimws(county_name)) == "VALDEZ-CORDOVA") {
    return(NA_character_)
  }

  # Strip multi-county: take the first county before ";"
  parts <- strsplit(county_name, ";")[[1]]
  primary <- trimws(parts[1])
  upper_name <- toupper(primary)

  # Strip " CITY" suffix for independent cities (VA, MD, MO)
  # This is needed for state-specific patch lookups
  upper_name_clean <- sub(" CITY$", "", upper_name, ignore.case = TRUE)

  # Special case: CT planning regions → direct FIPS
  if (state_fips == "09" && upper_name %in% names(ct_region_fips)) {
    return(ct_region_fips[upper_name])
  }

  # State-specific patch lookup (e.g., "TX__DEWITT" → "DeWitt")
  # Only apply state-specific patches when the input has " CITY" suffix
  # (for Baltimore City, St. Louis City) or when the cleaned name matches
  # (for DeWitt, La Salle in TX).
  is_city_input <- grepl(" CITY$", county_name, ignore.case = TRUE)
  state_key <- paste(state_fips, upper_name_clean, sep = "__")
  if (is_city_input) {
    # Try independent city patch first (e.g., "29__ST LOUIS__CITY" → "St. Louis City")
    city_key <- paste(state_key, "CITY", sep = "__")
    if (city_key %in% names(patch_table)) {
      primary <- patch_table[city_key]
    } else if (state_key %in% names(patch_table)) {
      # Fall back to general state-specific patch
      primary <- patch_table[state_key]
    } else if (upper_name %in% names(patch_table)) {
      # Fall back to general patch table
      primary <- patch_table[upper_name]
    }
  } else if (state_key %in% names(patch_table)) {
    # State-specific patch for name variants (e.g., "48__DEWITT" → "DeWitt")
    primary <- patch_table[state_key]
  } else if (upper_name %in% names(patch_table)) {
    # Fall back to general patch table (use original upper_name)
    primary <- patch_table[upper_name]
  }

  # If the patch value is a 5-digit FIPS code, return it directly
  if (grepl("^[0-9]{5}$", primary)) {
    return(primary)
  }

  # Determine if this is a city (for type-aware lookup)
  is_city <- grepl(" CITY$", county_name, ignore.case = TRUE)

  # Build lookup key with type awareness for duplicates
  # e.g., "24|BALTIMORE|city" → "24510" (Baltimore city, not county)
  type <- if (is_city) "city" else "other"
  key <- paste(state_fips, toupper(primary), type, sep = "|")
  if (key %in% names(tigris_lookup)) {
    return(tigris_lookup[key])
  }

  # Fallback: try without type
  key2 <- paste(state_fips, toupper(primary), sep = "|")
  if (key2 %in% names(tigris_lookup)) {
    return(tigris_lookup[key2])
  }

  # Fallback: try without patch (in case patch didn't apply)
  key3 <- paste(state_fips, upper_name, type, sep = "|")
  if (key3 %in% names(tigris_lookup)) {
    return(tigris_lookup[key3])
  }

  NA_character_
}

# ---- 5. Run the match ----
cat("Matching", nrow(fbi_pairs), "unique (state, county) pairs...\n")
fbi_pairs$county_fips <- mapply(match_county, fbi_pairs$state_fips, fbi_pairs$county_name)

matched <- sum(!is.na(fbi_pairs$county_fips))
total <- nrow(fbi_pairs)
cat("Matched:", matched, "/", total, sprintf("(%.1f%%)\n", 100 * matched / total))

# Report unmatched
unmatched <- fbi_pairs[is.na(fbi_pairs$county_fips), ]
if (nrow(unmatched) > 0) {
  cat("\nUnmatched pairs:\n")
  print(unmatched)
}

# ---- 6. Build final crosswalk ----
crosswalk <- fbi_pairs[, c("state_abbr", "county_name", "county_fips")]
rownames(crosswalk) <- NULL

# Sort for reproducibility
crosswalk <- crosswalk[order(crosswalk$state_abbr, crosswalk$county_name), ]

# ---- 7. Save ----
# Save to data/
save(crosswalk, file = "data/county_fips.rda")
cat("\nSaved data/county_fips.rda (", nrow(crosswalk), " rows)\n")

# ---- 8. Summary stats ----
cat("\n=== Summary ===\n")
cat("Total unique (state, county) pairs in fbi_api_agencies:", total, "\n")
cat("Resolved to FIPS:", matched, sprintf("(%.1f%%)\n", 100 * matched / total))
cat("Unresolved:", total - matched, "\n")
cat("  - N/A county:", sum(is.na(fbi_pairs$county_name) | toupper(fbi_pairs$county_name) == "N/A"), "\n")
cat("  - VALDEZ-CORDOVA:", sum(fbi_pairs$county_name == "VALDEZ-CORDOVA"), "\n")
cat("  - Other unmatched:", sum(!is.na(fbi_pairs$county_fips) == FALSE &
                                 !is.na(fbi_pairs$county_name) &
                                 toupper(fbi_pairs$county_name) != "N/A" &
                                 fbi_pairs$county_name != "VALDEZ-CORDOVA"), "\n")
