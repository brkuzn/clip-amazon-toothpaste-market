# ============================================================================
# brand_orthogonal_nesting.R
#
# Experiment: strip brand out of the embedding space entirely, then cluster.
# If the CLIP result were really "brand-independent", nests built on the
# brand-residual PCs should reproduce it. They do not, and the way they fail is
# the informative part.
#
# Procedure
#   1. one row per ASIN from data/asin_joint_pcs_complete.csv
#   2. residualise joint_pc1..5 on a full set of brand_grouped_50 dummies
#   3. k-means K=5 on the residuals (seed 42, nstart 50, iter.max 500);
#      adjusted Rand index against the brand grouping should be ~0
#   4. run the usual pipeline on those nests: profile rho, invert marginal
#      costs, simulate the merger
#   5. report fit, cost side, merger effects, hello->Colgate diversion, and the
#      average number of distinct firms per nest
#
# Self-contained: reads the shared engine out of robustness_checks.R without
# executing its PART A/B/C side effects, edits no existing script, and writes
# only into output/brand_orthogonal/.
#
# Usage (from the repo root):
#   Rscript scripts/brand_orthogonal_nesting.R
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(lubridate)})

repo_dir <- if (nzchar(Sys.getenv("REPO_DIR"))) Sys.getenv("REPO_DIR") else getwd()
data_dir <- file.path(repo_dir, "data")
out_dir  <- file.path(repo_dir, "output")
res_dir  <- file.path(out_dir, "brand_orthogonal")
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

# ── Shared engine ────────────────────────────────────────────────────────────
# Lines 1-247 of robustness_checks.R build panel0 and define nl_shares,
# build_delta_mat, nash_bertrand and run_scenario. Everything after 247 is that
# script's own output-writing, which must not run here.
eng_all <- readLines(file.path(repo_dir, "scripts", "robustness_checks.R"))
cut <- grep("^# ── PART A: SCENARIOS", eng_all)
stopifnot(length(cut) == 1L)
eval(parse(text = eng_all[1:(cut - 1L)]))

# ── 1-2. Residualise the joint PCs on brand dummies (ASIN level) ─────────────
pcs <- fread(file.path(data_dir, "asin_joint_pcs_complete.csv"))
pcs <- pcs[!duplicated(asin)]
# The PC file already carries brand_grouped_50, so restrict to the estimation
# panel rather than merging the column in a second time.
pcs <- pcs[asin %in% unique(panel0$asin)]
pcs[, brand_grouped_50 := factor(brand_grouped_50)]
clipmap <- unique(panel0[, .(asin, clip_nest)])
pcs <- merge(pcs, clipmap, by = "asin", all.x = TRUE)

r2 <- numeric(length(PC_COLS)); names(r2) <- PC_COLS
resid_mat <- matrix(NA_real_, nrow(pcs), length(PC_COLS),
                    dimnames = list(NULL, PC_COLS))
for (v in PC_COLS) {
  fit <- lm(as.formula(paste(v, "~ brand_grouped_50")), data = pcs)
  r2[v] <- summary(fit)$r.squared
  resid_mat[, v] <- residuals(fit)
}
tot_var  <- sum(vapply(PC_COLS, function(v) var(pcs[[v]]), numeric(1)))
expl_var <- sum(vapply(PC_COLS, function(v) var(pcs[[v]]) * r2[v], numeric(1)))

cat("\n=== 1-2. Brand dummies on the joint PCs (ASIN level) ===\n")
for (v in PC_COLS) cat(sprintf("  R^2(%s) = %.4f\n", v, r2[v]))
cat(sprintf("  variance-weighted R^2 across the five PCs = %.4f  (residual space retains %.1f%%)\n",
            expl_var / tot_var, 100 * (1 - expl_var / tot_var)))

# ── 3. k-means on the residual space + ARI against brand ─────────────────────
set.seed(42)
km_res <- kmeans(resid_mat, centers = K_CLIP, nstart = 50, iter.max = 500)
pcs[, bo_nest := km_res$cluster]

adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  cs  <- function(n) sum(choose(n, 2))
  idx <- cs(as.vector(tab))
  ea  <- cs(rowSums(tab)); eb <- cs(colSums(tab)); n <- cs(sum(tab))
  exp <- ea * eb / n
  (idx - exp) / (0.5 * (ea + eb) - exp)
}
ari_bo   <- adjusted_rand(pcs$bo_nest,   pcs$brand_grouped_50)
ari_clip <- adjusted_rand(pcs$clip_nest, pcs$brand_grouped_50)  # from the engine
cat("\n=== 3. Clustering on the brand-residual space ===\n")
cat(sprintf("  ARI(brand-orthogonal nests, brand groups) = %+.4f\n", ari_bo))
cat(sprintf("  ARI(CLIP nests,             brand groups) = %+.4f  (for contrast)\n", ari_clip))

# ── 4. Full pipeline on these nests ──────────────────────────────────────────
bomap <- setNames(pcs$bo_nest, pcs$asin)
panel0[, bo_nest := bomap[as.character(asin)]]
panel0[is.na(bo_nest), bo_nest := K_CLIP + 1L]

cat("\n=== 4. Pipeline (rho profile -> MC inversion -> Nash merger) ===\n")
ref  <- run_scenario(LAMBDA_BASE, "clip_nest", "logit", "avg", "CLIP (reference)")
stopifnot(abs(ref$hello_eff - 0.404) < 0.05)          # engine sanity gate
bo   <- run_scenario(LAMBDA_BASE, "bo_nest",   "logit", "avg", "Brand-orthogonal")

# ── 5. Diversion hello -> Colgate, and firms per nest ────────────────────────
# DR_{j->k} = -(ds_k/dp_j)/(ds_j/dp_j), mirroring PART B of robustness_checks.R:
# share-weighted over hello ASINs, averaged over pre-merger quarters.
diversion_hello_to_colgate <- function(nest_col, rho) {
  pan <- copy(panel0)
  mkt <- pan[, .(total_q = sum(q_jt)), by = quarter]
  mkt[, M_t := (1 + LAMBDA_BASE) * total_q]
  pan <- merge(pan, mkt[, .(quarter, M_t, total_q)], by = "quarter", all.x = TRUE)
  pan[, share := pmax(1e-6, pmin(1 - 1e-6, q_jt / M_t))]
  pan[, nestv := get(nest_col)]
  pan[, s_nest := sum(share), by = .(quarter, nestv)]
  pan[, s_jg   := share / s_nest]
  alpha <- EPS_TARGET / median(pan$price_mean * (1 - pan$share))
  pre_q <- sort(unique(pan$quarter)); pre_q <- pre_q[pre_q < MERGER_START]
  vals <- rbindlist(lapply(pre_q, function(qtr) {
    dt <- pan[quarter == qtr][order(asin)]
    D  <- build_delta_mat(alpha, rho, dt$share, dt$s_jg, dt$nestv)
    hj <- which(dt$brand_grouped_50 == "hello")
    ck <- which(dt$brand_grouped_50 == "Colgate")
    if (!length(hj) || !length(ck)) return(NULL)
    data.table(w = dt$share[hj],
               dr = vapply(hj, function(j) sum(-D[ck, j] / D[j, j]), numeric(1)))
  }))
  100 * sum(vals$w * vals$dr) / sum(vals$w)
}
div_bo   <- diversion_hello_to_colgate("bo_nest",   bo$rho_star)
div_clip <- diversion_hello_to_colgate("clip_nest", ref$rho_star)

firms_per_nest <- function(nest_col) {
  u <- unique(panel0[, .(asin, brand_grouped_50, nest = get(nest_col))])
  u[, firm := brand_grouped_50]
  u[brand_grouped_50 %in% c("Sensodyne","SENSODYNE PRONAMEL","Parodontax"), firm := "GSK"]
  u[brand_grouped_50 == "Tom's of Maine", firm := "Colgate"]
  mean(u[, uniqueN(firm), by = nest]$V1)
}
fp_bo   <- firms_per_nest("bo_nest")
fp_clip <- firms_per_nest("clip_nest")
hello_spread <- uniqueN(unique(panel0[brand_grouped_50 == "hello",
                                      .(asin, bo_nest)])$bo_nest)

summary_dt <- data.table(
  metric = c("rho*", "RSS drop (%)", "neg-MC (%)", "hello effect (%)",
             "Colgate effect (%)", "diversion hello->Colgate (%)",
             "mean firms per nest", "ARI vs brand"),
  brand_orthogonal = c(bo$rho_star, bo$rss_drop, bo$neg_mc, bo$hello_eff,
                       bo$colgate_eff, round(div_bo, 2), round(fp_bo, 2),
                       round(ari_bo, 4)),
  CLIP = c(ref$rho_star, ref$rss_drop, ref$neg_mc, ref$hello_eff,
           ref$colgate_eff, round(div_clip, 2), round(fp_clip, 2),
           round(ari_clip, 4)))

fwrite(summary_dt, file.path(res_dir, "brand_orthogonal_summary.csv"))
fwrite(data.table(pc = PC_COLS, brand_r2 = round(r2, 4)),
       file.path(res_dir, "brand_r2_by_pc.csv"))
fwrite(unique(pcs[, .(asin, brand_grouped_50, bo_nest, clip_nest)]),
       file.path(res_dir, "brand_orthogonal_assignments.csv"))

cat("\n=== 5. Results ===\n")
print(summary_dt, row.names = FALSE)
cat(sprintf("\n  hello ASINs are spread across %d of the %d brand-orthogonal nests\n",
            hello_spread, K_CLIP))
cat(sprintf("  Written to %s\n", res_dir))
