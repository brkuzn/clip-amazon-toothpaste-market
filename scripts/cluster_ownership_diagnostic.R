# ============================================================================
# cluster_ownership_diagnostic.R
#
# Per-CLIP-cluster diagnostic: how many distinct owning firms each nest spans,
# which brand dominates it, and what each of the three models implies for the
# negative-marginal-cost rate inside it.
#
# The point of the table is the M2/M3 comparison sitting side by side. Brand
# nests never cross an ownership boundary, so their Bertrand markups (and hence
# negative-MC rate) are identical to plain logit -- the Appendix D identity.
# CLIP nests do cross ownership, which is where the repair comes from.
#
# Reads:  output/model{1,2,3}_asin_results.csv
# Writes: output/cluster_ownership_diagnostic.csv
#
# Standalone: modifies nothing else, and is not part of the cached
# `computation` chunk in the thesis.
#
# Usage (from the repo root):
#   Rscript scripts/cluster_ownership_diagnostic.R
# ============================================================================
suppressPackageStartupMessages(library(data.table))

repo_dir <- if (nzchar(Sys.getenv("REPO_DIR"))) Sys.getenv("REPO_DIR") else getwd()
out_dir  <- file.path(repo_dir, "output")
stopifnot(dir.exists(out_dir))

m1 <- fread(file.path(out_dir, "model1_asin_results.csv"))
m2 <- fread(file.path(out_dir, "model2_asin_results.csv"))
m3 <- fread(file.path(out_dir, "model3_asin_results.csv"))

# M3 carries the CLIP nest assignment; M1/M2 contribute their own marginal costs.
d <- merge(m3[, .(asin, quarter, brand_grouped_50, nest_id, mc_M3 = mc)],
           m1[, .(asin, quarter, mc_M1 = mc)], by = c("asin", "quarter"))
d <- merge(d, m2[, .(asin, quarter, mc_M2 = mc)], by = c("asin", "quarter"))

# ── Ownership, mirroring scripts/blp_three_models.R lines 155-163 ────────────
# GSK's three brands are one firm; Tom's of Maine belongs to Colgate. hello is
# treated as independent here: it only joins Colgate post-merger, and this table
# describes the pre-merger ownership structure the nests sit inside.
d[, firm := brand_grouped_50]
d[brand_grouped_50 %in% c("Sensodyne", "SENSODYNE PRONAMEL", "Parodontax"),
  firm := "GSK"]
d[brand_grouped_50 == "Tom's of Maine", firm := "Colgate"]

modal <- function(x) names(sort(table(x), decreasing = TRUE))[1]

res <- d[, {
  dom <- modal(brand_grouped_50)
  .(n_obs                  = .N,
    n_asins                = uniqueN(asin),
    n_firms                = uniqueN(firm),
    dom_brand              = dom,
    dom_share_pct          = round(100 * sum(brand_grouped_50 == dom) / .N, 2),
    negmc_M1_pct           = round(100 * mean(mc_M1 < 0), 2),
    negmc_M2_pct           = round(100 * mean(mc_M2 < 0), 2),
    negmc_M3_pct           = round(100 * mean(mc_M3 < 0), 2),
    max_abs_mc_change_M1_M3 = round(max(abs(mc_M1 - mc_M3)), 4),
    n_repaired             = sum(mc_M1 < 0 & mc_M3 >= 0))
}, by = nest_id][order(nest_id)]

fwrite(res, file.path(out_dir, "cluster_ownership_diagnostic.csv"))

cat("\nPer-cluster ownership and negative-MC diagnostic\n")
cat(strrep("=", 108), "\n")
print(res, row.names = FALSE)
cat(strrep("-", 108), "\n")
cat(sprintf("Mean distinct firms per CLIP nest: %.2f  (brand nests: 1.00 by construction)\n",
            mean(res$n_firms)))
cat(sprintf("M1 = M2 negative-MC in every cluster: %s  (Appendix D identity)\n",
            all(abs(res$negmc_M1_pct - res$negmc_M2_pct) < 1e-9)))
cat(sprintf("Total observations repaired by CLIP nesting (mc<0 under M1, mc>=0 under M3): %d\n",
            sum(res$n_repaired)))
cat("\nWritten: output/cluster_ownership_diagnostic.csv\n")
