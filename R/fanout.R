# Shared agency fan-out machinery for the geography layers.
#
# get_county_crime_detail(), get_place_crime_detail(), and
# get_metro_crime_detail() all do the same thing once their members are
# resolved: filter by class, issue one cde_request() per ORI, tolerate partial
# failure, and stack the results into a fixed column contract.
#
# That loop lived in three near-identical copies and drifted five ways -- one of
# them a real bug, where the metro copy returned a silent zero-row frame that
# its siblings warned about (Gitea #54). The column contracts genuinely differ
# between levels, but that difference is *data* (which metadata columns to copy
# across), not control flow, so one loop parameterised by a column vector serves
# all three.
#
# What stays with each caller: its own signature, defaults, validation, resolver
# call, warning wording, and any level-specific guard (metro's max_agencies).

# Apply the agency_class / default_only filter. Pure -- warnings about an
# emptied set belong to the caller, whose message names its own geography.
.filter_agency_members <- function(agencies, agency_class = NULL,
                                   default_only = FALSE) {
  if (!is.null(agency_class)) {
    return(agencies[agencies$agency_class %in% agency_class, , drop = FALSE])
  }
  if (isTRUE(default_only)) {
    return(agencies[agencies$default_member, , drop = FALSE])
  }
  agencies
}

# Describe which filter emptied the set, for the caller's warning message.
.filter_desc <- function(agency_class = NULL) {
  if (!is.null(agency_class)) {
    paste0("agency_class = ", paste(agency_class, collapse = ", "))
  } else {
    "default_only = TRUE"
  }
}

# Fan out one cde_request() per member ORI and stack the per-agency-period rows.
#
# @param agencies Resolved, already-filtered membership frame. Must be non-empty
#   and carry `ori` plus every name in `meta_cols`.
# @param meta_cols Columns copied from `agencies` onto each agency's rows. This
#   is the only thing that differs between geographic levels.
# @param cols The output column contract, in order.
# @param empty_fn Zero-argument constructor for the level's typed empty frame.
# @param progress Print a progress line per agency.
# @return A data.frame with `cols`. Agencies whose request or parse fails are
#   dropped with a warning and recorded in `attr(x, "dropped")`.
.fanout_agency_detail <- function(agencies, meta_cols, cols, empty_fn,
                                  offense, from, to, progress = FALSE) {
  dropped <- character(0)
  parts <- vector("list", nrow(agencies))

  for (i in seq_len(nrow(agencies))) {
    ori <- agencies$ori[i]
    if (isTRUE(progress)) {
      message(sprintf("[%d/%d] %s", i, nrow(agencies), ori))
    }
    path <- cde_path("summarized", paste0("agency/", ori), offense)
    query <- list(from = from, to = to, type = "counts")

    res <- tryCatch(
      parse_agency_detail(cde_request(path, query), ori, offense, from, to),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      dropped <- c(dropped, ori)
      next
    }
    for (col in meta_cols) {
      res[[col]] <- agencies[[col]][i]
    }
    parts[[i]] <- res
  }

  # rbind_fill() drops the NULL entries left by failed agencies, and returns a
  # 0-column data.frame when every entry failed.
  out <- rbind_fill(parts)
  if (is.null(out) || nrow(out) == 0 || ncol(out) == 0) {
    out <- empty_fn()
  } else {
    out <- out[, intersect(cols, names(out)), drop = FALSE]
    rownames(out) <- NULL
  }

  if (length(dropped) > 0) {
    warning("Dropped ", length(dropped),
            " agenc", if (length(dropped) == 1) "y" else "ies",
            " that returned no data: ", paste(dropped, collapse = ", "),
            call. = FALSE)
    attr(out, "dropped") <- dropped
  }
  out
}
