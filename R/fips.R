#' County FIPS Code Lookup
#'
#' Map a state + county name to a 5-digit county FIPS code using the FBI's
#' internal agency-to-county mapping and a tigris-derived crosswalk.
#'
#' @param state A two-letter state abbreviation (e.g. `"CA"`, `"TX"`) or a
#'   two-digit state FIPS code (e.g. `"06"`, `"48"`). Case-insensitive.
#' @param county A county name as stored in `fbi_api_agencies` (typically
#'   uppercase, may include `" CITY"` suffix for independent cities, or
#'   `";"` for multi-county agencies).
#'
#' @return A character string with the 5-digit county FIPS code (e.g. `"06037"`)
#'   or `NA_character_` when the county cannot be resolved (e.g. `"N/A"` county,
#'   dissolved boroughs).
#'
#' @details
#' The crosswalk is built at package build time from `tigris::counties()` and
#' covers 98.6% of unique (state, county) pairs in `fbi_api_agencies`.
#'
#' Unresolvable cases:
#' - `county = "N/A"` (agency has no county assigned — state police, tribal, etc.)
#' - `VALDEZ-CORDOVA` (dissolved AK borough; split into Chugach & Copper River
#'   census areas in 2015, no single FIPS exists)
#'
#' @examples
#' county_to_fips("CA", "LOS ANGELES")
#' #> "06037"
#'
#' county_to_fips("TX", "DEWITT")
#' #> "48123"
#'
#' county_to_fips("MD", "BALTIMORE CITY")
#' #> "24510"
#'
#' county_to_fips("MO", "ST LOUIS CITY")
#' #> "29510"
#'
#' county_to_fips("AK", "N/A")
#' #> NA
#'
#' @export
county_to_fips <- function(state, county) {
  if (is.na(county) || toupper(trimws(county)) == "N/A") {
    return(NA_character_)
  }

  # Handle VALDEZ-CORDOVA (dissolved AK borough)
  if (toupper(trimws(county)) == "VALDEZ-CORDOVA") {
    return(NA_character_)
  }

  # Normalize state: abbreviation → FIPS
  state_fips <- .state_abbr_to_fips(state)
  if (is.na(state_fips)) {
    return(NA_character_)
  }

  # Strip multi-county: take the first county before ";"
  parts <- strsplit(county, ";")[[1]]
  primary <- trimws(parts[1])
  upper_name <- toupper(primary)

  # Strip " CITY" suffix for independent cities (VA, MD, MO)
  upper_name_clean <- sub(" CITY$", "", upper_name, ignore.case = TRUE)

  # State-specific patch lookup (e.g., "48__DEWITT" → "DeWitt")
  # Only apply state-specific patches when the input has " CITY" suffix
  # (for Baltimore City, St. Louis City) or when the cleaned name matches
  # (for DeWitt, La Salle in TX).
  is_city_input <- grepl(" CITY$", county, ignore.case = TRUE)
  state_key <- paste(state_fips, upper_name_clean, sep = "__")
  if (is_city_input) {
    # Try independent city patch first (e.g., "24__BALTIMORE__CITY" → "Baltimore City")
    city_key <- paste(state_key, "CITY", sep = "__")
    if (city_key %in% names(.fips_patch_table)) {
      primary <- .fips_patch_table[[city_key]]
    } else if (state_key %in% names(.fips_patch_table)) {
      # Fall back to general state-specific patch
      primary <- .fips_patch_table[[state_key]]
    } else if (upper_name %in% names(.fips_patch_table)) {
      # Fall back to general patch table
      primary <- .fips_patch_table[[upper_name]]
    }
  } else if (state_key %in% names(.fips_patch_table)) {
    # State-specific patch for name variants (e.g., "48__DEWITT" → "DeWitt")
    primary <- .fips_patch_table[[state_key]]
  } else if (upper_name %in% names(.fips_patch_table)) {
    # Fall back to general patch table (use original upper_name)
    primary <- .fips_patch_table[[upper_name]]
  }

  # If the patch value is a 5-digit FIPS code, return it directly
  if (grepl("^[0-9]{5}$", primary)) {
    return(primary)
  }

  # Build lookup key: state_abbr|county_name → county_fips
  # Use the original state_abbr (not FIPS) for the lookup
  state_abbr <- .fips_state_to_abbr(state_fips)
  key <- paste(state_abbr, primary, sep = "|")
  if (key %in% names(.fips_tigris_lookup)) {
    return(.fips_tigris_lookup[[key]])
  }

  # Fallback: try with uppercase county_name
  key2 <- paste(state_abbr, toupper(primary), sep = "|")
  if (key2 %in% names(.fips_tigris_lookup)) {
    return(.fips_tigris_lookup[[key2]])
  }

  # Fallback: try without patch (in case patch didn't apply)
  key3 <- paste(state_abbr, upper_name, sep = "|")
  if (key3 %in% names(.fips_tigris_lookup)) {
    return(.fips_tigris_lookup[[key3]])
  }

  NA_character_
}

#' @rdname county_to_fips
#' @export
counties_with_fips <- function() {
  # crosswalk is internal data (R/sysdata.rda); return a copy so users can
  # modify the result without affecting package state.
  crosswalk[NULL, , drop = FALSE]
}

# ---- Internal helpers ----

.fips_state_to_abbr <- function(fips) {
  # FIPS → abbreviation mapping
  fips_to_abbr <- c(
    "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA", "08" = "CO",
    "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL", "13" = "GA", "15" = "HI",
    "16" = "ID", "17" = "IL", "18" = "IN", "19" = "IA", "20" = "KS", "21" = "KY",
    "22" = "LA", "23" = "ME", "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN",
    "28" = "MS", "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
    "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND", "39" = "OH",
    "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI", "45" = "SC", "46" = "SD",
    "47" = "TN", "48" = "TX", "49" = "UT", "50" = "VT", "51" = "VA", "53" = "WA",
    "54" = "WV", "55" = "WI", "56" = "WY",
    "60" = "AS", "66" = "GU", "69" = "GM", "72" = "PR", "78" = "VI"
  )
  fips_to_abbr[fips]
}

.state_abbr_to_fips <- function(abbr) {
  abbr <- toupper(trimws(abbr))
  # Handle vectors by applying element-wise
  sapply(abbr, function(a) {
    if (grepl("^[0-9]{2}$", a)) {
      return(a)
    }
    # Abbreviation → FIPS mapping
    abbr_to_fips <- c(
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
    fips <- abbr_to_fips[a]
    if (is.na(fips)) {
      warning("Unknown state abbreviation: ", a)
      return(NA_character_)
    }
    fips
  }, USE.NAMES = FALSE)
}

# ---- Build lookup tables (called once at package load) ----

.fips_patch_table <- NULL
.fips_tigris_lookup <- NULL

.build_fips_lookups <- function() {
  # crosswalk is loaded from R/sysdata.rda (internal package data)

  # Build simple lookup from crosswalk: state_abbr|county_name → county_fips
  # The crosswalk already has the final FIPS codes, so we can use a direct lookup.
  # This handles all cases including duplicates (Baltimore city/county, St. Louis city/county).
  keys <- paste(crosswalk$state_abbr, crosswalk$county_name, sep = "|")
  .fips_tigris_lookup <<- base::`names<-`(crosswalk$county_fips, keys)

  # Patch table (same as in data-raw script)
  .fips_patch_table <<- c(
    # ---- Virginia independent cities ----
    "ALEXANDRIA CITY"        = "Alexandria",
    "BEDFORD CITY"           = "Bedford",
    "BLUFFTON CITY"          = "Bluffton",
    "CHARLOTTESVILLE CITY"   = "Charlottesville",
    "CHESAPEAKE CITY"        = "Chesapeake",
    "COLLINSVILLE CITY"      = "Collinsville",
    "CORINTH CITY"           = "Corinth",
    "CULPEPER CITY"          = "Culpeper",
    "CUTLER CITY"            = "Cutler",
    "DANVILLE CITY"          = "Danville",
    "EMPORIA CITY"           = "Emporia",
    "FAIRFAX CITY"           = "Fairfax",
    "FALLS CHURCH CITY"      = "Falls Church",
    "FRANKLIN CITY"          = "Franklin",
    "FREDERICK CITY"         = "Frederick",
    "GALAX CITY"             = "Galax",
    "GARRISONVILLE CITY"     = "Garrisonville",
    "GEORGETOWN CITY"        = "Georgetown",
    "GLEN ALLEN CITY"        = "Glen Allen",
    "GLOUCESTER CITY"        = "Gloucester",
    "GLOUCESTER POINT CITY"  = "Gloucester Point",
    "GRAYSON CITY"           = "Grayson",
    "GREENEVILLE CITY"       = "Greeneville",
    "HALIFAX CITY"           = "Halifax",
    "HARRISONBURG CITY"      = "Harrisonburg",
    "HERNDON CITY"           = "Herndon",
    "HOPKINTON CITY"         = "Hopkinton",
    "HUNTINGTON CITY"        = "Huntington",
    "HYDE PARK CITY"         = "Hyde Park",
    "ILEX CITY"              = "Ilex",
    "JACKSONVILLE CITY"      = "Jacksonville",
    "JAMES CITY"            = "James City",
    "JONESVILLE CITY"        = "Jonesville",
    "KING AND QUEEN CITY"    = "King and Queen",
    "KING WILLIAM CITY"      = "King William",
    "LANCASTER CITY"         = "Lancaster",
    "LEBANON CITY"           = "Lebanon",
    "LEXINGTON CITY"         = "Lexington",
    "LONG BEACH CITY"        = "Long Beach",
    "LOUISVILLE CITY"        = "Louisville",
    "LYNCHBURG CITY"         = "Lynchburg",
    "MANASSAS CITY"          = "Manassas",
    "MANASSAS PARK CITY"     = "Manassas Park",
    "MARTINSVILLE CITY"      = "Martinsville",
    "MECHANICSVILLE CITY"    = "Mechanicsville",
    "MIDDLETOWN CITY"        = "Middletown",
    "MINERAL CITY"           = "Mineral",
    "MONROE CITY"            = "Monroe",
    "MONTROSE CITY"          = "Montrose",
    "NANSEMOND CITY"         = "Nansemond",
    "NEW CASTLE CITY"        = "New Castle",
    "NEWPORT NEWS CITY"      = "Newport News",
    "NORFOLK CITY"           = "Norfolk",
    "NORTH AMPTON CITY"      = "Northampton",
    "NORTH CHARLESTON CITY"  = "North Charleston",
    "NORTH FORT MYERS CITY"  = "North Fort Myers",
    "NORTH LITTLE ROCK CITY" = "North Little Rock",
    "NORTH PORT CITY"        = "North Port",
    "NORTH SYRACUSE CITY"    = "North Syracuse",
    "NORTH TONAWANDA CITY"   = "North Tonawanda",
    "NORTH WILKESBORO CITY"  = "North Wilkesboro",
    "NORWOOD CITY"           = "Norwood",
    "OXFORD CITY"            = "Oxford",
    "PETERSBURG CITY"        = "Petersburg",
    "POQUOSON CITY"          = "Poquoson",
    "PORTSMOUTH CITY"        = "Portsmouth",
    "RADFORD CITY"           = "Radford",
    "RICHMOND CITY"          = "Richmond",
    "ROANOKE CITY"           = "Roanoke",
    "SALEM CITY"             = "Salem",
    "STAUNTON CITY"          = "Staunton",
    "SUFFOLK CITY"           = "Suffolk",
    "VIRGINIA BEACH CITY"    = "Virginia Beach",
    "WAYNESBORO CITY"        = "Waynesboro",
    "WILLIAMSBURG CITY"      = "Williamsburg",
    "WINCHESTER CITY"        = "Winchester",
    "CHARLES CITY"           = "Charles City",  # VA county (not a city)

    # ---- "ST." vs "ST" name variants ----
    # Census uses "St." (with period); fbi stores "ST" (no period).
    "ST CLAIR"               = "St. Clair",
    "ST CLAIR; JEFFERSON"    = "St. Clair",
    "ST CLAIR; CLINTON"      = "St. Clair",
    "ST CLAIR; MADISON"      = "St. Clair",
    "ST CLAIR; MONROE"       = "St. Clair",
    "ST CLAIR; MACOMB"       = "St. Clair",
    "ST FRANCIS"             = "St. Francis",
    "ST JOHNS"               = "St. Johns",
    "ST LUCIE"               = "St. Lucie",
    "ST JOSEPH"              = "St. Joseph",
    "ST MARTIN"              = "St. Martin",
    "ST MARTIN; LAFAYETTE"   = "St. Martin",
    "ST BERNARD"             = "St. Bernard",
    "ST CHARLES"             = "St. Charles",
    "ST HELENA"              = "St. Helena",
    "ST JAMES"               = "St. James",
    "ST JOHN THE BAPTIST"    = "St. John the Baptist",
    "ST LANDRY"              = "St. Landry",
    "ST MARY"                = "St. Mary",
    "ST MARY'S"              = "St. Mary's",
    "ST TAMMANY"             = "St. Tammany",
    "ST FRANCOIS"            = "St. Francois",
    "ST LAWRENCE"            = "St. Lawrence",
    "ST CROIX"               = "St. Croix",
    "ST CROIX; PIERCE"       = "St. Croix",
    # "ST LOUIS" handled via state-specific patch (29__ST LOUIS → 29189)
    "ST LOUIS; FRANKLIN"     = "29189",     # MO multi-county → St. Louis County

    # ---- Missouri "Ste." spelling ----
    "STE GENEVIEVE"          = "Ste. Genevieve",

    # ---- Alaska borough/census area name fixes ----
    # "VALDEZ-CORDOVA" was dissolved in 2015; split into Chugach & Copper River.
    # No single FIPS exists — leave as NA (handled below).
    # "ALEUTIANS" → "Aleutians" (Census dropped the extra 'e' in 2020+ data)
    "ALEUTIANS WEST"         = "Aleutians West",
    "ALEUTIANS EAST"         = "Aleutians East",
    "BRISTOL BAY"            = "Bristol Bay",
    "PRINCE OF WALES-HYDER"  = "Prince of Wales-Hyder",

    # ---- Connecticut planning regions → traditional counties ----
    # tigris returns CT planning regions (not counties). Map to traditional names.
    "CAPITOL"                = "Hartford",
    "GREATER BRIDGEPORT"     = "Fairfield",
    "LOWER CONNECTICUT RIVER VALLEY" = "New London",
    "NAUGATUCK VALLEY"       = "Litchfield",
    "NORTHEASTERN CONNECTICUT" = "Windham",
    "NORTHWEST HILLS"        = "Litchfield",
    "SOUTH CENTRAL CONNECTICUT" = "New Haven",
    "SOUTHEASTERN CONNECTICUT"  = "New London",
    "WESTERN CONNECTICUT"    = "Fairfield",

    # ---- CT traditional county names (fbi uses these, tigris has planning regions) ----
    # Direct FIPS mapping since tigris doesn't have traditional CT county names.
    #
    # These MUST be state-scoped ("09__NAME"). As unscoped, name-only keys they
    # applied to every state, so MA/NJ/VA "MIDDLESEX", OH/SC "FAIRFIELD" and VT
    # "WINDHAM" all resolved to Connecticut's FIPS — silently, and with real
    # consequences: join_census_pop() keys on county FIPS, so Massachusetts
    # Middlesex (~1.63M) was receiving Connecticut Middlesex's (~164k)
    # population. See Gitea #50. county_to_fips() checks the state-scoped key
    # before the bare one, so scoping confines these to CT.
    "09__NEW HAVEN"          = "09009",
    "09__HARTFORD"           = "09003",
    "09__FAIRFIELD"          = "09001",
    "09__MIDDLESEX"          = "09007",
    "09__TOLLAND"            = "09013",
    "09__NEW LONDON"         = "09011",
    "09__LITCHFIELD"         = "09005",
    "09__WINDHAM"            = "09015",

    # ---- Additional name variants ----
    "LA SALLE"               = "LaSalle",    # LA parish (no space, no period)
    "DEWITT"                 = "De Witt",    # IL county (space, not capital W)
    "LA PORTE"               = "LaPorte",    # IN county (no space, no period)
    "DONA ANA"               = "Do\u00f1a Ana",   # NM county (with \u00f1)

    # ---- State-specific patches (key format: "STATEFP__COUNTY") ----
    # These override the general patch table when the state matches.
    "48__DEWITT"             = "DeWitt",     # TX county (capital W, no space)
    "48__LA SALLE"           = "La Salle",   # TX county (space, period)
    # Independent city patches (key format: "STATEFP__COUNTY__CITY")
    # These are only applied when the input has " CITY" suffix.
    # Note: Use "St. Louis" (with period) to match tigris NAME format.
    # For Baltimore, use "Baltimore City" to match crosswalk county_name format.
    "29__ST LOUIS__CITY"     = "St. Louis",       # MO independent city
    "24__BALTIMORE__CITY"    = "Baltimore City",  # MD independent city (crosswalk has "BALTIMORE CITY" as county_name)
    # MO "ST LOUIS" without CITY suffix → St. Louis County (direct FIPS)
    "29__ST LOUIS"           = "29189",           # MO county (direct FIPS mapping)
    # MN "ST LOUIS" → St. Louis County (use tigris NAME format)
    "27__ST LOUIS"           = "St. Louis"        # MN county
  )
}

# Build lookups on package load
.onLoad <- function(libname, pkgname) {
  .build_fips_lookups()

  # CBSA_VINTAGE (R/cbsa.R) must reflect the shipped crosswalk's own
  # "vintage" attribute, not an independent literal that can drift from it.
  # sysdata.rda is only guaranteed to be available once the namespace is
  # loading, so the read happens here rather than at cbsa.R's top level.
  cbsa_vintage <- attr(cbsa_crosswalk, "vintage")
  if (!is.null(cbsa_vintage)) {
    CBSA_VINTAGE <<- cbsa_vintage
  }
}
