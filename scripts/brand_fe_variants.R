# ============================================================================
# brand_fe_variants.R
#
# Three specifications that add brand fixed effects to the existing pipeline:
#   1. brand FE, no nesting (rho = 0)
#   2. brand FE + brand nesting
#   3. brand FE + CLIP nesting
#
# The question is whether the CLIP principal components are merely proxying for
# brand identity. If they were, absorbing brand with dummies would kill them.
#
# Brand dummies replace `is_other`, which is a function of brand and therefore
# collinear with the dummy set.
#
# Self-contained: reads the shared engine out of robustness_checks.R without
# running its output sections, edits no existing script, writes only into
# output/brand_fe/.
#
# Usage (from the repo root):
#   Rscript scripts/brand_fe_variants.R
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(lubridate)})

repo_dir <- if (nzchar(Sys.getenv("REPO_DIR"))) Sys.getenv("REPO_DIR") else getwd()
out_dir  <- file.path(repo_dir, "output")
res_dir  <- file.path(out_dir, "brand_fe")
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

eng_all <- readLines(file.path(repo_dir, "scripts", "robustness_checks.R"))
cut <- grep("^# ── PART A: SCENARIOS", eng_all)
stopifnot(length(cut) == 1L)
eval(parse(text = eng_all[1:(cut - 1L)]))

# ── Brand dummies (drop one as base), is_other removed ───────────────────────
brands <- sort(unique(panel0$brand_grouped_50))
base_brand <- brands[1]
dummy_names <- character(0)
for (b in brands[-1]) {
  nm <- paste0("bd_", gsub("[^A-Za-z0-9]", "", b))
  panel0[, (nm) := as.numeric(brand_grouped_50 == b)]
  dummy_names <- c(dummy_names, nm)
}
cat(sprintf("Brand dummies added: %d (base = %s); is_other dropped as collinear\n",
            length(dummy_names), base_brand))

X_VARS_FE <- c(setdiff(X_VARS, "is_other"), dummy_names)

# ── Scenario runner with the FE design matrix ────────────────────────────────
# Mirrors run_scenario from the engine; the only changes are X_VARS_FE in place
# of X_VARS and the extra t statistics on the joint PCs.
run_fe <- function(nest_col, label, rho_fixed = NA) {
  pan <- copy(panel0)
  mkt <- pan[, .(total_q = sum(q_jt)), by = quarter]
  mkt[, M_t := (1 + LAMBDA_BASE) * total_q]
  pan <- merge(pan, mkt[, .(quarter, M_t, total_q)], by = "quarter", all.x = TRUE)
  pan[, share         := pmax(1e-6, pmin(1 - 1e-6, q_jt / M_t))]
  pan[, outside_share := 1 - total_q / M_t]
  pan[, log_odds      := log(share) - log(outside_share)]
  pan[, nestv  := get(nest_col)]
  pan[, s_nest := sum(share), by = .(quarter, nestv)]
  pan[, s_jg   := share / s_nest]
  pan[, log_sjg := log(pmax(s_jg, 1e-6))]

  dm_cols <- c("log_odds", "price_mean", "log_sjg", X_VARS_FE)
  pan[, (paste0(dm_cols, "_dm")) := lapply(.SD, function(x) x - mean(x, na.rm = TRUE)),
      .SDcols = dm_cols, by = quarter]
  x_mat <- as.matrix(pan[, paste0(X_VARS_FE, "_dm"), with = FALSE])
  keep  <- apply(x_mat, 2, function(c) sd(c) > 1e-10)   # drop degenerate columns
  x_mat <- x_mat[, keep, drop = FALSE]
  XtX_inv <- solve(crossprod(x_mat))

  alpha <- EPS_TARGET / median(pan$price_mean * (1 - pan$share))
  base_y <- pan$log_odds_dm - alpha * pan$price_mean_dm
  prof <- function() {
    rss <- vapply(RHO_GRID, function(rho) {
      ydm <- base_y - rho * pan$log_sjg_dm
      bh  <- as.numeric(XtX_inv %*% crossprod(x_mat, ydm))
      sum((ydm - x_mat %*% bh)^2)
    }, numeric(1))
    list(rho = RHO_GRID[which.min(rss)], drop = 100 * (rss[1] - min(rss)) / rss[1])
  }
  if (is.na(rho_fixed)) { pr <- prof(); rho <- pr$rho; rss_drop <- pr$drop }
  else { rho <- rho_fixed; rss_drop <- 0 }

  ydm  <- base_y - rho * pan$log_sjg_dm
  bhat <- as.numeric(XtX_inv %*% crossprod(x_mat, ydm))
  resid <- as.numeric(ydm - x_mat %*% bhat)
  dfree <- length(ydm) - ncol(x_mat)
  se    <- sqrt(diag(XtX_inv) * sum(resid^2) / dfree)
  tstat <- setNames(bhat / se, colnames(x_mat))
  betas <- setNames(bhat, colnames(x_mat))

  pan[, delta := log_odds - rho * log_sjg]
  qtrs_l <- sort(unique(pan$quarter))
  mc_rows <- rbindlist(lapply(qtrs_l, function(qtr) {
    dt <- pan[quarter == qtr][order(asin)]
    D  <- build_delta_mat(alpha, rho, dt$share, dt$s_jg, dt$nestv)
    O  <- outer(dt$firm_id_pre, dt$firm_id_pre, "==") * 1
    mc <- tryCatch(as.numeric(dt$price_mean + solve(O * D, dt$share)),
                   error = function(e) rep(NA_real_, nrow(dt)))
    data.table(asin = dt$asin, quarter = qtr, mc = mc)
  }))
  pan <- merge(pan, mc_rows, by = c("asin", "quarter"), all.x = TRUE)
  neg_mc <- 100 * mean(pan$mc < 0, na.rm = TRUE)

  pre_q <- qtrs_l[qtrs_l < MERGER_START]
  mcf <- pan[quarter %in% pre_q, .(mc_fixed = mean(mc, na.rm = TRUE)), by = asin]
  pan <- merge(pan, mcf, by = "asin", all.x = TRUE)
  pan[is.na(mc_fixed), mc_fixed := mc]

  eff <- rbindlist(lapply(qtrs_l[qtrs_l >= MERGER_START], function(qtr) {
    dt <- pan[quarter == qtr][order(asin)]
    p0 <- nash_bertrand(dt$delta, dt$price_mean, dt$mc_fixed, dt$firm_id_pre,  alpha, rho, dt$nestv)
    p1 <- nash_bertrand(dt$delta, dt$price_mean, dt$mc_fixed, dt$firm_id_post, alpha, rho, dt$nestv)
    data.table(brand = dt$brand_grouped_50, eff = (p1 - p0) / p0 * 100)
  }))

  cat(sprintf("  %-28s rho*=%.2f  rssD=%5.2f%%  negMC=%5.2f%%  hello=%+.3f%%  colg=%+.3f%%\n",
              label, rho, rss_drop, neg_mc,
              eff[brand == "hello", mean(eff)], eff[brand == "Colgate", mean(eff)]))

  PCD <- paste0(PC_COLS, "_dm")   # design-matrix columns carry the demeaning suffix
  list(row = data.table(specification = label, rho_star = rho,
                        rss_drop = round(rss_drop, 2), neg_mc = round(neg_mc, 2),
                        hello_eff = round(eff[brand == "hello", mean(eff)], 3),
                        colgate_eff = round(eff[brand == "Colgate", mean(eff)], 3)),
       t = tstat[PCD], b = betas[PCD])
}

cat("\n=== Brand fixed-effects variants ===\n")
r1 <- run_fe("market_ids",   "brand FE, no nesting", rho_fixed = 0)
stopifnot(abs(r1$row$hello_eff - 1.233) < 0.01)   # brand FE, no nesting == committed M1
r2 <- run_fe("brand_nest_id","brand FE + brand nesting")
r3 <- run_fe("clip_nest",    "brand FE + CLIP nesting")

summary_dt <- rbindlist(list(r1$row, r2$row, r3$row))
tstats <- data.table(pc = PC_COLS,
                     t_noNest   = round(as.numeric(r1$t), 3),
                     t_brandNest= round(as.numeric(r2$t), 3),
                     t_clipNest = round(as.numeric(r3$t), 3),
                     b_clipNest = round(as.numeric(r3$b), 4))

fwrite(summary_dt, file.path(res_dir, "brand_fe_summary.csv"))
fwrite(tstats,     file.path(res_dir, "brand_fe_pc_tstats.csv"))

cat("\n--- summary ---\n"); print(summary_dt, row.names = FALSE)
cat("\n--- joint-PC t statistics under brand FE ---\n"); print(tstats, row.names = FALSE)
cat(sprintf("\n  PCs significant at 5%% under brand FE + CLIP nesting: %d of %d\n",
            sum(abs(as.numeric(r3$t)) > 1.96), length(PC_COLS)))
cat(sprintf("  Written to %s\n", res_dir))
