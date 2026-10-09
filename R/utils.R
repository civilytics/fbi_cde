# Null-coalescing helper: return `x` unless it is NULL/empty, else `y`.
# Defined locally because the package targets R (>= 3.5.0); base `%||%`
# only exists from R 4.4.0 and the package does not import rlang.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Stack a list of data.frames with differing columns (base-R rbind that fills
# missing columns, rather than requiring identical columns). NULL entries are
# dropped; the union of all columns is used, with missing cells filled NA and
# rows kept in order.
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
