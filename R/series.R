# Shared parsing for the CDE's "series" payloads: the counts (`actuals`) and
# `rates` maps returned by summarized/{level}/{offense} and
# arrest/{level}/all?type=counts.
#
# Each map is keyed by a series label, "<name> <Measure>": "Oakland Police
# Department Offenses", "California Clearances", "United States Arrests".
# Agency- and state-level responses carry the queried geography's own series
# plus comparison series (its state, the nation) that have a rate but no count.
# A comparison series appears only under `rates`, never with counts, so the
# labels present in the counts map identify the geography's own series without
# matching on agency or state names.

# State and territory names the CDE uses as comparison-series names. Only
# consulted as a fallback when a response carries no counts at all.
.SERIES_STATE_NAMES <- c(
  datasets::state.name, "District of Columbia", "Puerto Rico", "Guam",
  "U.S. Virgin Islands", "American Samoa", "Northern Mariana Islands"
)

.SERIES_COLS <- c("geography", "offense", "measure", "period", "count", "rate")
.SERIES_COLS_COMPARISON <- c("geography", "series", "series_name", "offense",
                             "measure", "period", "count", "rate")

.empty_series_frame <- function(comparison = FALSE) {
  out <- data.frame(
    geography = character(0), series = character(0),
    series_name = character(0), offense = character(0),
    measure = character(0), period = character(0), count = numeric(0),
    rate = numeric(0), stringsAsFactors = FALSE
  )
  out[, if (comparison) .SERIES_COLS_COMPARISON else .SERIES_COLS,
      drop = FALSE]
}

# Split "<name> <Measure>" labels. A label without a recognised measure keeps
# its whole text as the name and gets measure NA, rather than being dropped.
.split_series_label <- function(label) {
  pattern <- "^(.*) (Offenses|Clearances|Arrests)$"
  has_measure <- grepl(pattern, label)
  list(
    name = ifelse(has_measure, sub(pattern, "\\1", label), label),
    measure = ifelse(has_measure, tolower(sub(pattern, "\\2", label)),
                     NA_character_)
  )
}

# "MM-YYYY" -> a sortable month index.
.period_key <- function(period) {
  year <- suppressWarnings(as.integer(sub("^\\d{2}-", "", period)))
  month <- suppressWarnings(as.integer(sub("-\\d{4}$", "", period)))
  year * 12L + month
}

# Tidy one series payload.
#
# @param counts_obj,rates_obj The `actuals` (or legacy `counts`) and `rates`
#   maps, label -> period -> value.
# @param geography Value for the geography column (ORI, state abbr, "US").
# @param offense The offense the caller requested, recorded on every row.
# @param level The queried level: "agency", "state" or "national".
# @param comparison Keep the comparison series, labelled by `series`?
.parse_series <- function(counts_obj, rates_obj, geography, offense, level,
                          comparison = FALSE) {
  counts_df <- flatten_cde_json(counts_obj)
  names(counts_df) <- c("label", "period", "count")
  rates_df <- flatten_cde_json(rates_obj)
  names(rates_df) <- c("label", "period", "rate")

  df <- merge(counts_df, rates_df, by = c("label", "period"), all = TRUE)
  if (nrow(df) == 0) {
    return(.empty_series_frame(comparison))
  }

  parts <- .split_series_label(df$label)
  own_labels <- unique(counts_df$label)
  is_own <- if (length(own_labels) > 0) {
    df$label %in% own_labels
  } else {
    # No counts at all (e.g. suppressed): fall back on the series name.
    parts$name != "United States" &
      (level != "agency" | !parts$name %in% .SERIES_STATE_NAMES)
  }
  series <- ifelse(is_own, level,
                   ifelse(parts$name == "United States", "national", "state"))

  out <- data.frame(
    geography = geography,
    series = series,
    series_name = parts$name,
    offense = offense,
    measure = parts$measure,
    period = df$period,
    count = df$count,
    rate = df$rate,
    stringsAsFactors = FALSE
  )

  if (!comparison) {
    out <- out[out$series == level, , drop = FALSE]
  }

  series_rank <- match(out$series, unique(c(level, "state", "national")))
  measure_rank <- match(out$measure, c("offenses", "clearances", "arrests"))
  out <- out[order(series_rank, out$series_name, measure_rank,
                   .period_key(out$period)), , drop = FALSE]
  rownames(out) <- NULL
  out[, if (comparison) .SERIES_COLS_COMPARISON else .SERIES_COLS,
      drop = FALSE]
}
