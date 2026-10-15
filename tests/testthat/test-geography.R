test_that("classify_agency maps raw agency types to agency_class", {
  expect_equal(classify_agency("County"), "county_primary")
  expect_equal(classify_agency("Parish"), "county_primary")
  expect_equal(classify_agency("City"), "municipal")
  expect_equal(classify_agency("Municipality"), "municipal")
  expect_equal(classify_agency("Borough"), "municipal")
  expect_equal(classify_agency("City and Borough"), "municipal")
  expect_equal(classify_agency("University or College"), "campus")
  expect_equal(classify_agency("State Police"), "state")
  expect_equal(classify_agency("Tribal"), "tribal")
  expect_equal(classify_agency("Other"), "special")
  expect_equal(classify_agency("Other State Agency"), "state")
  # The CDE's "Census Area" agencies are Alaska city police departments.
  expect_equal(classify_agency("Census Area"), "municipal")
  # Unknown and NA are conservative (excluded by default).
  expect_equal(classify_agency("Something New"), "special")
  expect_equal(classify_agency(NA_character_), "special")
  # Vectorized.
  expect_equal(classify_agency(c("City", "County", "Tribal")),
               c("municipal", "county_primary", "tribal"))
})

test_that("county_agencies resolves and classifies a county (Alameda, CA)", {
  out <- county_agencies("Alameda", "CA")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 21L)
  expect_true(all(c("agency_class", "county_fips") %in% names(out)))
  expect_false("default_member" %in% names(out))

  cls <- setNames(out$agency_class, out$ori)
  expect_equal(cls[["CA0010000"]], "county_primary")  # Alameda County Sheriff
  expect_equal(cls[["CA0010900"]], "municipal")        # Oakland PD
  expect_equal(cls[["CA001300X"]], "municipal")        # Dublin PD (letter ORI)
  expect_equal(cls[["CA0012100"]], "special")          # BART
  expect_equal(cls[["CA0019900"]], "state")            # Highway Patrol
  expect_equal(cls[["CA0019700"]], "campus")           # UC Berkeley

  # 1 sheriff, 14 city police and 2 campus agencies are queried by default;
  # 3 special-purpose and 1 state agency only on request.
  counts <- table(out$agency_class)
  expect_equal(
    as.vector(counts[c("county_primary", "municipal", "campus", "special",
                       "state")]),
    c(1, 14, 2, 3, 1)
  )
  expect_equal(nrow(.filter_agency_members(out)), 17L)
  expect_equal(nrow(.filter_agency_members(out, include_statewide = TRUE)), 18L)
})

test_that("county_agencies includes county_fips column (Alameda, CA)", {
  out <- county_agencies("Alameda", "CA")
  # county_fips is a character column (preserves leading zero)
  expect_true(is.character(out$county_fips))
  # All rows share the same county FIPS
  expect_true(all(out$county_fips == "06001"))
  # Column order: county_fips appears right after state_abbr
  expect_equal(names(out),
               c("ori", "agency_name", "agency_type_name", "agency_class",
                 "county_name", "state_abbr", "county_fips",
                 "latitude", "longitude", "agency_county_names"))
})

test_that("county_agencies zero-row branch includes county_fips", {
  expect_warning(res <- county_agencies("Nowhere", "CA"), "No agencies")
  expect_equal(nrow(res), 0L)
  expect_true("county_fips" %in% names(res))
  expect_true(is.character(res$county_fips))
  expect_equal(length(res$county_fips), 0L)
})

test_that("county_agencies is case-insensitive on county and state", {
  a <- county_agencies("Alameda", "CA")
  b <- county_agencies("alameda", "ca")
  expect_equal(nrow(a), nrow(b))
  expect_setequal(a$ori, b$ori)
})

test_that("county_agencies validates state and warns on unknown county", {
  expect_error(county_agencies("Alameda", "ZZ"), "Invalid state")
  expect_warning(res <- county_agencies("Nowhere", "CA"), "No agencies")
  expect_equal(nrow(res), 0L)
})

# ---- Multi-county agencies (#56) -------------------------------------------
#
# The CDE stores a multi-county agency's county_name as a semicolon-separated
# list ("DELAWARE; FAIRFIELD; FRANKLIN"). Exact string matching never fired for
# those 617 agencies, so major city departments were silently missing from
# their own county and metro results.

test_that("county_agencies finds an agency whose county_name lists several counties", {
  # Columbus PD polices parts of Delaware, Fairfield and Franklin counties.
  for (cty in c("Franklin", "Delaware", "Fairfield")) {
    out <- county_agencies(cty, "OH")
    expect_true("OHCOP0000" %in% out$ori, info = paste("county:", cty))
  }
})

test_that("county_agencies still excludes agencies from unrelated counties", {
  # Columbus PD is not in Hocking, another Columbus-CBSA county.
  expect_false("OHCOP0000" %in% county_agencies("Hocking", "OH")$ori)
})

test_that("county matching is not substring-based", {
  # "YORK" must not match "NEW YORK": a substring test would wrongly cross-match.
  # county_name is the queried county on every row, so check the CDE's own
  # list: each agency must name NEW YORK itself, not just contain "YORK".
  ny <- county_agencies("New York", "NY")
  expect_gt(nrow(ny), 0L)
  expect_true(all(vapply(
    strsplit(ny$agency_county_names, ";", fixed = TRUE),
    function(p) "NEW YORK" %in% trimws(p), logical(1)
  )))
  york <- county_agencies("York", "PA")
  expect_false(any(grepl("NEW YORK", york$agency_county_names)))
})

test_that("a single-county agency matches only its own county", {
  alameda <- county_agencies("Alameda", "CA")
  expect_gt(nrow(alameda), 0L)
  expect_true(all(vapply(
    strsplit(toupper(alameda$agency_county_names), ";", fixed = TRUE),
    function(p) "ALAMEDA" %in% trimws(p), logical(1)
  )))
})

test_that("county_agencies attributes every row to the queried county", {
  # Columbus PD keeps its full county list, but in Franklin's result it is a
  # Franklin row.
  fr <- county_agencies("Franklin", "OH")
  cop <- fr[fr$ori == "OHCOP0000", ]
  expect_equal(cop$county_name, "FRANKLIN")
  expect_equal(cop$agency_county_names, "DELAWARE; FAIRFIELD; FRANKLIN")
  expect_equal(unique(fr$county_name), "FRANKLIN")
  expect_equal(unique(fr$county_fips), "39049")
})

test_that("county_fips comes from the queried county, not the first agency", {
  # The first Licking agency in table order is multi-county and lists
  # Fairfield first; county_fips used to be read off that row (39045).
  li <- county_agencies("Licking", "OH")
  expect_true(grepl(";", li$agency_county_names[1], fixed = TRUE))
  expect_equal(unique(li$county_fips), "39089")
  expect_equal(unique(li$county_name), "LICKING")
})

test_that("the bundled agency table has real column types", {
  ag <- fbi_api_agencies
  expect_type(ag$nibrs, "logical")
  expect_type(ag$latitude, "double")
  expect_type(ag$longitude, "double")
  expect_s3_class(ag$nibrs_start_date, "Date")
  # The source's placeholder strings are gone.
  chr <- vapply(ag, is.character, logical(1))
  expect_false(any(vapply(ag[chr], function(x) any(x %in% "NULL"),
                          logical(1))))
  # Coordinates are plausible where present (the one -9/-9 placeholder is NA).
  expect_false(any(ag$latitude == -9, na.rm = TRUE))
})

test_that("Louisiana and Alaska agencies carry real types, not placeholders", {
  # The snapshot typed every Louisiana agency "Parish" and every Alaska agency
  # by its borough or census area, so all of Louisiana classed as sheriffs.
  # data-raw/agency_type_repair.R takes the live directory's types instead.
  ag <- fbi_api_agencies
  placeholders <- c("Parish", "Borough", "Census Area", "City and Borough",
                    "Municipality")
  expect_false(any(ag$agency_type_name %in% placeholders))
  type_of <- function(ori) ag$agency_type_name[ag$ori == ori]
  expect_equal(type_of("LANPD0000"), "City")                   # New Orleans PD
  expect_equal(type_of("LA0360200"), "University or College")  # Delgado CC
  expect_equal(type_of("AKAST0100"), "State Police")           # AK Troopers
  # An agency the live directory no longer lists has an unknown type.
  expect_true(is.na(type_of("LA0070900")))                     # Ringgold PD
})

test_that("a Louisiana city resolves as a place and not as its parish's own", {
  nola <- place_agencies("New Orleans", "LA")
  expect_equal(nola$ori, "LANPD0000")
  expect_equal(nola$place_fips, "2255000")

  orleans <- county_agencies("Orleans", "LA")
  expect_equal(orleans$agency_class[orleans$ori == "LANPD0000"], "municipal")
  expect_equal(orleans$agency_class[orleans$ori == "LA0360200"], "campus")
  expect_false(any(orleans$agency_class == "county_primary"))

  ebr <- county_agencies("East Baton Rouge", "LA")
  expect_equal(sum(ebr$agency_class == "county_primary"), 1L)
})

# ---- Agencies the CDE leaves without a county --------------------------------
#
# The NYPD, DC's Metropolitan Police and the Baltimore City Sheriff carry
# county_name "N/A" in the CDE, so their counties and metros silently lost them.

test_that("the attribution table fills only agencies the CDE left as N/A", {
  ag <- data.frame(
    ori = c("NY0303000", "DCMPD0000", "OHCOP0000"),
    county_name = c("N/A", "SOMEWHERE", "DELAWARE; FAIRFIELD; FRANKLIN"),
    stringsAsFactors = FALSE
  )
  out <- .apply_county_attributions(ag)
  expect_equal(out$county_name[1], "BRONX; KINGS; NEW YORK; QUEENS; RICHMOND")
  # A county the CDE supplies wins over the table.
  expect_equal(out$county_name[2], "SOMEWHERE")
  expect_equal(out$county_name[3], "DELAWARE; FAIRFIELD; FRANKLIN")
})

test_that("every attributed agency exists and is N/A in the bundled table", {
  # If a snapshot refresh gives one of these a county, the entry is dead and
  # should be removed rather than silently ignored.
  ag <- fbi_api_agencies
  ori <- names(.AGENCY_COUNTY_ATTRIBUTIONS)
  expect_true(all(ori %in% ag$ori))
  expect_true(all(ag$county_name[match(ori, ag$ori)] == "N/A"))
})

test_that("every attributed county resolves to a FIPS code", {
  ag <- fbi_api_agencies
  for (ori in names(.AGENCY_COUNTY_ATTRIBUTIONS)) {
    st <- ag$state_abbr[ag$ori == ori]
    counties <- trimws(strsplit(.AGENCY_COUNTY_ATTRIBUTIONS[[ori]], ";",
                                fixed = TRUE)[[1]])
    for (cty in counties) {
      expect_false(is.na(county_to_fips(st, cty)), info = paste(ori, cty))
    }
  }
})

test_that("the NYPD is attributed in full to each of the five boroughs", {
  boroughs <- c(Bronx = "36005", Kings = "36047", `New York` = "36061",
                Queens = "36081", Richmond = "36085")
  for (b in names(boroughs)) {
    out <- county_agencies(b, "NY")
    nypd <- out[out$ori == "NY0303000", ]
    expect_equal(nrow(nypd), 1L, info = b)
    expect_equal(nypd$county_name, toupper(b), info = b)
    expect_equal(nypd$county_fips, boroughs[[b]], info = b)
    expect_equal(nypd$agency_class, "municipal", info = b)
    expect_equal(nypd$agency_county_names,
                 "BRONX; KINGS; NEW YORK; QUEENS; RICHMOND", info = b)
  }
})

test_that("the District of Columbia resolves to its police department", {
  dc <- expect_no_warning(county_agencies("District of Columbia", "DC"))
  expect_true("DCMPD0000" %in% dc$ori)
  expect_equal(unique(dc$county_fips), "11001")
})

test_that("Baltimore city includes its sheriff alongside its police", {
  bc <- county_agencies("Baltimore City", "MD")
  expect_true(all(c("MD0040600", "MDBPD0000") %in% bc$ori))
  expect_equal(unique(bc$county_fips), "24510")
})

# ---- Connecticut planning regions (#52) -------------------------------------

test_that("planning regions are appended to a Connecticut agency's counties", {
  ag <- data.frame(
    ori = c("CT0000400", "CTCSP0000", "OHCOP0000"),
    county_name = c("HARTFORD", "N/A", "DELAWARE; FAIRFIELD; FRANKLIN"),
    stringsAsFactors = FALSE
  )
  regions <- data.frame(ori = c("CT0000400", "CTCSP0000"),
                        planning_region = c("CAPITOL PLANNING REGION",
                                            "CAPITOL PLANNING REGION"),
                        stringsAsFactors = FALSE)
  out <- .apply_planning_regions(ag, regions)
  expect_equal(out$county_name[1], "HARTFORD; CAPITOL PLANNING REGION")
  # An agency with no county gets the region alone.
  expect_equal(out$county_name[2], "CAPITOL PLANNING REGION")
  expect_equal(out$county_name[3], "DELAWARE; FAIRFIELD; FRANKLIN")
})

test_that("a Connecticut agency is reachable by county and by planning region", {
  region <- county_agencies("Capitol Planning Region", "CT")
  expect_gt(nrow(region), 0L)
  expect_equal(unique(region$county_fips), "09110")
  expect_equal(unique(region$county_name), "CAPITOL PLANNING REGION")
  expect_true("CT0000400" %in% region$ori)   # Avon PD

  hartford <- county_agencies("Hartford", "CT")
  expect_equal(unique(hartford$county_fips), "09003")
  expect_true("CT0000400" %in% hartford$ori)
})

test_that("the planning-region table covers Connecticut agencies only", {
  r <- ct_planning_regions
  ag <- fbi_api_agencies
  expect_false(anyDuplicated(r$ori) > 0)
  expect_true(all(ag$state_abbr[match(r$ori, ag$ori)] == "CT"))
  expect_equal(length(unique(r$planning_region)), 9L)
  expect_true(all(grepl(" PLANNING REGION$", r$planning_region)))
  # Every region resolves to its FIPS code.
  fips <- vapply(unique(r$planning_region), function(x) county_to_fips("CT", x), "")
  expect_setequal(unname(fips), sprintf("09%d", seq(110, 190, by = 10)))
})
