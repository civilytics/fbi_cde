# Null-coalescing helper: return `x` unless it is NULL/empty, else `y`.
# Defined locally because the package targets R (>= 3.5.0); base `%||%`
# only exists from R 4.4.0 and the package does not import rlang.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Stack a list of data.frames with differing columns (base-R replacement for
# data.table::rbindlist(fill = TRUE)). NULL entries are dropped; the union of
# all columns is used, with missing cells filled NA and rows kept in order.
rbind_fill <- function(dfs) {
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0) {
    return(data.frame())
  }
  all_cols <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(df) {
    for (col in setdiff(all_cols, names(df))) {
      df[[col]] <- rep(NA, nrow(df))
    }
    df[all_cols]
  })
  do.call(rbind, dfs)
}

make_state <- function(state_abb) {
  state <- datasets::state.name[match(tolower(state_abb),
                                      tolower(datasets::state.abb))]
  state[tolower(state_abb) == "CZ"] <- "canal zone"
  state[tolower(state_abb) == "DC"] <- "district of columbia"
  state[tolower(state_abb) == "GU"] <- "guam"
  state[tolower(state_abb) == "PR"] <- "puerto rico"
  return(state)
}

make_year <- function() {
  as.numeric(format(Sys.Date(), "%Y"))
}

read.csv_system_file <- function(file) {
  data <- utils::read.csv(system.file("testdata",
                                      file,
                                      package = "fbi"))
  data$ori <- as.character(data$ori)
  rownames(data) <- 1:nrow(data)
  return(data)
}

clean_column_names <- function(.data) {
  names(.data) <- tolower(names(.data))
  names(.data) <- gsub("-", "_", names(.data))
  names(.data) <- gsub("^data_year$", "year", names(.data))


  # Fix arrest column names
  names(.data) <- gsub("^disorderly$", "disorderly_conduct", names(.data))
  names(.data) <- gsub("^driving$", "dui", names(.data))
  names(.data) <- gsub("^drug_abuse_gt$", "drug_grand_total", names(.data))
  names(.data) <- gsub("^drug_poss_m$", "drug_poss_marijuana", names(.data))
  names(.data) <- gsub("^drug_sales_m$", "drug_sales_marijuana", names(.data))
  names(.data) <- gsub("^g_all$", "gambling_all_others", names(.data))
  names(.data) <- gsub("^g_b$", "gambling_bookmaking", names(.data))
  names(.data) <- gsub("^g_n$", "gambling_numbers", names(.data))
  names(.data) <- gsub("^g_t$", "gambling_total", names(.data))
  names(.data) <- gsub("^ht_c_s_a$", "human_trafficking_commercial_sex_traffic", names(.data))
  names(.data) <- gsub("^ht_i_s$", "human_trafficking_servitude", names(.data))
  names(.data) <- gsub("^liquor$", "liquor_laws", names(.data))
  names(.data) <- gsub("^mvt$", "motor_vehicle_theft", names(.data))
  names(.data) <- gsub("^offense_family$", "offense_against_family", names(.data))
  names(.data) <- gsub("^prostitution$", "prostitution_total", names(.data))
  names(.data) <- gsub("^prostitution_a_p_p$", "prostitution_assisting", names(.data))
  names(.data) <- gsub("^prostitution_p$", "prostitution_performing", names(.data))
  names(.data) <- gsub("^prostitution_p_p$", "prostitution_purchasing", names(.data))
  names(.data) <- gsub("^sex_offense$", "other_sex_offenses", names(.data))



  .data$csv_header <- NULL
  return(.data)
}


combine_url_section <- function(data_type, ori, region_name, state_abb) {
  url_section <- paste0(data_type, "/national")
  if (!is.null(ori)) {
    url_section <- paste0(data_type, "/agencies/")
    url_section <- paste0(url_section, ori)
  } else if (!is.null(region_name)) {
    url_section <- paste0(data_type, "/regions/")
    url_section <- paste0(url_section, region_name)
  } else if (!is.null(state_abb)) {
    url_section <- paste0(data_type, "/states/")
    url_section <- paste0(url_section, state_abb)
  }
  return(url_section)
}


make_url <- function(url_section,
                     start_year,
                     end_year = NULL,
                     key = NULL) {

  url <- paste0("https://api.usa.gov/crime/fbi/sapi/api/",
                url_section,
                "/",
                start_year,
                "/",
                end_year,
                "?API_KEY=",
                key)
  return(url)
}


srs_long_to_wide <- function(.data) {
  .data <- data.table::melt(.data, id = c("ori",
                                          "state_abbr",
                                          "data_year",
                                          "offense"))
  .data <- data.table::dcast(.data,
                             formula = ori + state_abbr + data_year ~ offense + variable,
                             measure.var = c("value"),
                             fun.aggregate = mean,
                             fill = NA)
  return(.data)
}
