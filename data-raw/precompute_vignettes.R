# ============================================================================
# data-raw/precompute_vignettes.R
# ============================================================================
# Knit the *.Rmd.orig vignette sources into the *.Rmd files that ship with the
# package, baking in results and figures.
#
# Why: every vignette here queries the live CDE API. Building or checking the
# package must never depend on the network, so the shipped .Rmd contains the
# rendered output rather than the code that produced it. The .Rmd.orig files
# are the real sources -- edit those, then re-run this.
#
# Some vignettes also use ggplot2, sf and tigris for maps. Those are needed
# only HERE, at precompute time, and are deliberately not package
# dependencies: the shipped .Rmd references saved figures.
#
# Run with: Rscript data-raw/precompute_vignettes.R
#
# Note this takes several minutes and issues a few hundred API requests.
# ============================================================================

needed <- c("knitr", "pkgload", "ggplot2", "sf", "tigris", "scales")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("precompute needs these packages: ", paste(missing, collapse = ", "))
}

pkgload::load_all(quiet = TRUE)

old <- setwd("vignettes")
on.exit(setwd(old), add = TRUE)

sources <- list.files(".", pattern = "\\.Rmd\\.orig$")
if (length(sources) == 0) {
  stop("no .Rmd.orig sources found in vignettes/")
}

for (src in sources) {
  out <- sub("\\.orig$", "", src)
  message("knitting ", src, " -> ", out)
  knitr::knit(src, output = out)
}

message("done: ", paste(sub("\\.orig$", "", sources), collapse = ", "))
