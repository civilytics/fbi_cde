# ============================================================================
# data-raw/place_fips_crosswalk.R
# ============================================================================
# Build the ORI -> Census place / county subdivision crosswalk behind the
# place_fips and cousub_fips columns of place_agencies() (Gitea #46). Design:
# specs/2026-10-06-place-fips-design.md.
#
# A municipal agency is matched to the Census unit it polices by NAME, within
# its state, and only among units that lie in one of the agency's own counties.
# The county check is what makes a name match trustworthy: same-named places
# are common within a state, but almost never within a county. Agency
# coordinates are not used -- they are too unreliable (matches that are right
# by name and county sit up to 1,000 km from the agency's point).
#
# Two tiers, in order:
#   1. Places. An incorporated place wins; a census-designated place (CDP) is
#      used only when nothing incorporated or no functioning county subdivision
#      matches, since a CDP has no government to run a police department.
#   2. County subdivisions (minor civil divisions): townships, and New England
#      and New York towns, which are governments but not Census places.
# More than one candidate in a tier, or none at all, leaves the agency
# unmatched: its codes are NA rather than guessed.
#
# Sources: the Census Bureau's 2020 reference code files (public domain),
#   national_place_by_county2020.txt  place -> county, with TYPE and FUNCSTAT
#   national_cousub2020.txt           county subdivisions, with FUNCSTAT
# from https://www2.census.gov/geo/docs/reference/codes2020/
#
# Run from the package root with: Rscript data-raw/place_fips_crosswalk.R
# (needs pkgload; downloads ~5 MB).
#
# Output: R/sysdata.rda, adding or replacing `place_crosswalk`.
#
# WARNING: R/sysdata.rda holds several internal objects and save() overwrites
# the whole file. This script loads every existing object and re-saves them all.
# ============================================================================

stopifnot(requireNamespace("pkgload", quietly = TRUE))
pkgload::load_all(".", quiet = TRUE)
ns <- asNamespace("fbiCDE")

VINTAGE <- 2020L
BASE_URL <- "https://www2.census.gov/geo/docs/reference/codes2020/"

fetch <- function(file) {
  path <- file.path(tempdir(), file)
  if (!file.exists(path)) {
    utils::download.file(paste0(BASE_URL, file), path, quiet = TRUE,
                         mode = "wb")
  }
  utils::read.delim(path, sep = "|", colClasses = "character", quote = "",
                    encoding = "UTF-8")
}

places <- fetch("national_place_by_county2020.txt")
cousubs <- fetch("national_cousub2020.txt")
stopifnot(
  all(c("STATE", "STATEFP", "COUNTYFP", "PLACEFP", "PLACENAME", "TYPE")
      %in% names(places)),
  all(c("STATE", "STATEFP", "COUNTYFP", "COUSUBFP", "COUSUBNAME", "FUNCSTAT")
      %in% names(cousubs))
)

# ---- Name normalisation ----------------------------------------------------
# Census names carry a legal/statistical descriptor ("Lufkin city", "Manheim
# township", "Indianapolis city (balance)"); agency names may carry it too
# ("Bloomingdale Village"). Each side is compared with and without it.
DESCRIPTOR <- paste0(
  " (city and borough|(unified|consolidated|metro|metropolitan) government",
  "( \\(balance\\))?|city \\(balance\\)|town \\(balance\\)|urban county|",
  "zona urbana|comunidad|corporation|municipality|charter township|township|",
  "borough|village|town|city|CDP|CCD|plantation|gore|grant|location|purchase|",
  "unorganized territory|UT)$"
)

# Transliterate, unify a few spellings, then keep letters and digits only, so
# "Du Bois" = "DuBois", "La Salle" = "LaSalle" and "Espanola" = "Española".
# Dropping everything but [A-Z0-9] after transliteration also absorbs the
# "~" or "'" some iconv implementations leave behind.
norm <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("\\bSAINT\\b", "ST", x, perl = TRUE)
  x <- sub("^(CITY|TOWN|VILLAGE|BOROUGH|TOWNSHIP) OF ", "", x)
  gsub("[^A-Z0-9]", "", x)
}
bare <- function(x) sub(DESCRIPTOR, "", x, perl = TRUE)
descriptor <- function(x) toupper(sub(paste0("^.*?", DESCRIPTOR), "\\1", x, perl = TRUE))

places$county_fips <- paste0(places$STATEFP, places$COUNTYFP)
places$fips <- paste0(places$STATEFP, places$PLACEFP)
places$k_full <- norm(places$PLACENAME)
places$k_bare <- norm(bare(places$PLACENAME))
# Massachusetts names some cities "<Name> Town city" ("Franklin Town city");
# their police departments are just "<Name> Police Department".
places$k_bare2 <- norm(sub(" Town$", "", bare(places$PLACENAME)))
places$incorporated <- places$TYPE == "INCORPORATED PLACE"

cousubs$county_fips <- paste0(cousubs$STATEFP, cousubs$COUNTYFP)
cousubs$fips <- paste0(cousubs$STATEFP, cousubs$COUNTYFP, cousubs$COUSUBFP)
cousubs$k_full <- norm(cousubs$COUSUBNAME)
cousubs$k_bare <- norm(bare(cousubs$COUSUBNAME))
cousubs$desc <- descriptor(cousubs$COUSUBNAME)
# Only functioning governments can run a police department.
cousubs <- cousubs[cousubs$FUNCSTAT %in% c("A", "B", "C"), , drop = FALSE]

# ---- The agencies ----------------------------------------------------------
# The package's own municipal tier and place names, after its county
# attributions (agencies_table()), so the crosswalk keys exactly what
# place_agencies() returns.
mun <- get(".municipal_agencies", envir = ns)()
mun$k <- norm(mun$place_name)
words <- toupper(trimws(mun$place_name))
type_word <- sub("^.* ", "", words)
has_type <- type_word %in% c("TOWNSHIP", "TOWN", "VILLAGE", "CITY", "BOROUGH")
mun$k_strip <- ifelse(has_type, norm(sub(" [^ ]+$", "", trimws(mun$place_name))), NA)
mun$type_word <- ifelse(has_type, type_word, NA)

agency_counties <- lapply(seq_len(nrow(mun)), function(i) {
  parts <- trimws(strsplit(mun$county_name[i], ";", fixed = TRUE)[[1]])
  parts <- parts[!is.na(parts) & parts != "N/A"]
  fips <- vapply(parts, function(p) county_to_fips(mun$state_abbr[i], p), "")
  unique(fips[!is.na(fips)])
})

places_by_state <- split(places, places$STATE)
cousubs_by_state <- split(cousubs, cousubs$STATE)

none <- function(ori) {
  data.frame(ori = ori, place_type = NA_character_, place_fips = NA_character_,
             cousub_fips = NA_character_, census_name = NA_character_,
             stringsAsFactors = FALSE)
}

match_one <- function(i) {
  st <- mun$state_abbr[i]
  k <- mun$k[i]
  counties <- agency_counties[[i]]

  ps <- places_by_state[[st]]
  cs <- cousubs_by_state[[st]]
  if (is.null(ps)) ps <- places[0, , drop = FALSE]
  if (is.null(cs)) cs <- cousubs[0, , drop = FALSE]
  p <- ps[ps$k_full == k | ps$k_bare == k | ps$k_bare2 == k, , drop = FALSE]
  c_ <- cs[cs$k_full == k | cs$k_bare == k |
             (!is.na(mun$k_strip[i]) & cs$k_bare == mun$k_strip[i] &
                grepl(mun$type_word[i], cs$desc, fixed = TRUE)), , drop = FALSE]

  if (length(counties) > 0) {
    p <- p[p$county_fips %in% counties, , drop = FALSE]
    c_ <- c_[c_$county_fips %in% counties, , drop = FALSE]
  } else {
    # No county to check against (a handful of agencies the CDE leaves
    # without one): only an incorporated place unique in the state is
    # accepted, and never a county subdivision, whose code needs the county.
    c_ <- c_[0, , drop = FALSE]
  }
  p <- p[!duplicated(p$fips), , drop = FALSE]
  c_ <- c_[!duplicated(c_$fips), , drop = FALSE]

  inc <- p[p$incorporated, , drop = FALSE]
  out <- none(mun$ori[i])
  if (nrow(inc) == 1) {
    out$place_type <- "incorporated"
    out$place_fips <- inc$fips
    out$census_name <- inc$PLACENAME
  } else if (nrow(inc) > 1) {
    return(out)
  } else if (nrow(c_) == 1) {
    out$place_type <- "county_subdivision"
    out$cousub_fips <- c_$fips
    out$census_name <- c_$COUSUBNAME
  } else if (nrow(c_) > 1) {
    return(out)
  } else if (nrow(p) == 1 && length(counties) > 0) {
    out$place_type <- "cdp"
    out$place_fips <- p$fips
    out$census_name <- p$PLACENAME
  }
  out
}

all_rows <- do.call(rbind, lapply(seq_len(nrow(mun)), match_one))
place_crosswalk <- all_rows[!is.na(all_rows$place_type), , drop = FALSE]
place_crosswalk <- place_crosswalk[order(place_crosswalk$ori), , drop = FALSE]
rownames(place_crosswalk) <- NULL
attr(place_crosswalk, "vintage") <- VINTAGE
attr(place_crosswalk, "source") <- BASE_URL

# ---- Invariants ------------------------------------------------------------
stopifnot(
  !anyDuplicated(place_crosswalk$ori),
  all(is.na(place_crosswalk$place_fips) | grepl("^[0-9]{7}$", place_crosswalk$place_fips)),
  all(is.na(place_crosswalk$cousub_fips) | grepl("^[0-9]{10}$", place_crosswalk$cousub_fips)),
  # Exactly one code per row, matching its type.
  all(xor(is.na(place_crosswalk$place_fips), is.na(place_crosswalk$cousub_fips))),
  all((place_crosswalk$place_type == "county_subdivision") ==
        !is.na(place_crosswalk$cousub_fips))
)
st_fips <- unique(places[, c("STATE", "STATEFP")])
ag_state <- mun$state_abbr[match(place_crosswalk$ori, mun$ori)]
code <- ifelse(is.na(place_crosswalk$place_fips), place_crosswalk$cousub_fips,
               place_crosswalk$place_fips)
stopifnot(all(substr(code, 1L, 2L) == st_fips$STATEFP[match(ag_state, st_fips$STATE)]))

cat("municipal agencies:", nrow(mun), "\n")
print(table(place_type = all_rows$place_type, useNA = "ifany"))
cat(sprintf("resolved: %.1f%%\n", 100 * nrow(place_crosswalk) / nrow(mun)))

# ---- Re-save ALL objects (see the WARNING above) ---------------------------
existing <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = existing)
assign("place_crosswalk", place_crosswalk, envir = existing)
objs <- ls(existing)
stopifnot(all(c("crosswalk", "cbsa_crosswalk", "place_crosswalk") %in% objs))
save(list = objs, envir = existing,
     file = "R/sysdata.rda", compress = "bzip2", version = 2)
cat("wrote R/sysdata.rda with objects:", paste(objs, collapse = ", "), "\n")
