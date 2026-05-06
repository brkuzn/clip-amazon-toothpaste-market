# ============================================================================
# clip_k_sensitivity.R
# Tests K = 2..10 for CLIP k-means nesting.
# For each K reports:
#   - Cluster quality : WCSS, avg silhouette
#   - Demand fit      : rho*, RSS drop vs rho=0
#   - Economics       : neg-MC rate, hello/Colgate merger effect
#
# Exactly mirrors blp_three_models.R M3 logic (all demeaning and XtX_inv
# computed inside run_for_k from pan after merge, matching row order).
# K=6 must reproduce: rho*=0.60, RSS drop=6.7%, hello=+0.591%
# ============================================================================
library(data.table); library(lubridate); library(cluster)

# ── PATH CONFIG ───────────────────────────────────────────────────────────────
if (!exists("BASE_DIR")) BASE_DIR <- "/Users/brkuzn"
if (!exists("out_dir"))  out_dir  <- file.path(BASE_DIR, "analysis_2704")
if (!exists("data_dir")) data_dir <- file.path(BASE_DIR, "clip-amazon-toothpaste-market", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# ─────────────────────────────────────────────────────────────────────────────

# ── CONSTANTS (identical to blp_three_models.R) ──────────────────────────────
LAMBDA        <- 13.93
TIME_CUTOFF   <- as.Date("2022-07-01")
MIN_QUARTERS  <- 3
MERGER_START  <- "2020Q1"
ALPHA         <- -0.31005
PC_COLS       <- paste0("joint_pc", 1:5)
X_VARS        <- c("pkg_size","joint_pc1","joint_pc2","joint_pc3",
                   "joint_pc4","joint_pc5","is_other","import_freight_shock")
RHO_GRID      <- seq(0, 0.90, by=0.01)
NASH_TOL      <- 1e-8
NASH_MAXITER  <- 2000
NASH_DAMP     <- 0.4
K_TEST        <- 2:10
import_brands       <- c("Sensodyne","SENSODYNE PRONAMEL","Parodontax","APAGARD")
freight_crisis_qtrs <- c("2021Q3","2021Q4","2022Q1","2022Q2")

# ── BUILD PANEL (once) ───────────────────────────────────────────────────────
cat("Loading data...\n")
panel <- fread(file.path(data_dir, "asin_quarter_panel.csv"))
panel[, month_date_dummy := as.Date(paste0(
  sub("Q.*","",quarter),"-",
  sprintf("%02d",(as.integer(sub(".*Q","",quarter))-1)*3+1),"-01"))]
panel <- panel[month_date_dummy <= TIME_CUTOFF][, month_date_dummy := NULL]

asin_chars <- fread(file.path(data_dir, "asin_characteristics.csv"))
panel <- merge(panel, asin_chars[, .(asin, pkg_size, is_import)], by="asin", all.x=TRUE)
panel[is.na(pkg_size),  pkg_size  := median(panel$pkg_size,  na.rm=TRUE)]
panel[is.na(is_import), is_import := 0]
keep <- panel[, .(n=uniqueN(quarter)), by=asin][n>=MIN_QUARTERS, asin]
panel <- panel[asin %in% keep]
mkt   <- panel[, .(total_q=sum(q_jt)), by=quarter]
mkt[, M_t := (1+LAMBDA)*total_q]
panel <- merge(panel, mkt[,.(quarter,M_t,total_q)], by="quarter", all.x=TRUE)
panel[, share         := pmax(1e-6, pmin(1-1e-6, q_jt/M_t))]
panel[, outside_share := 1 - total_q/M_t]
panel[, is_freight_crisis    := as.numeric(quarter %in% freight_crisis_qtrs)]
panel[, import_freight_shock := is_import * is_freight_crisis]
panel[, is_other             := as.numeric(brand_grouped_50 == "Other")]
panel[, log_odds             := log(share) - log(outside_share)]

# Firm IDs (pre/post merger)
brand_firm <- data.table(brand_grouped_50=sort(unique(panel$brand_grouped_50)))
brand_firm[, firm_id_pre := seq_len(.N)]
panel <- merge(panel, brand_firm, by="brand_grouped_50", all.x=TRUE)
panel[brand_grouped_50 %in% c("Sensodyne","SENSODYNE PRONAMEL","Parodontax"),
      firm_id_pre := 9999L]
colgate_fid <- panel[brand_grouped_50=="Colgate", unique(firm_id_pre)][1]
panel[brand_grouped_50=="Tom's of Maine", firm_id_pre := colgate_fid]
panel[, firm_id_post := firm_id_pre]
panel[brand_grouped_50=="hello" & quarter >= MERGER_START,
      firm_id_post := colgate_fid]

qtrs <- sort(unique(panel$quarter))
panel[, market_ids := match(quarter, qtrs)]

# CLIP PC matrix (for clustering)
pcs_raw <- fread(file.path(data_dir, "asin_joint_pcs_complete.csv"))
pcs_raw <- pcs_raw[asin %in% panel$asin]
pc_mat  <- as.matrix(pcs_raw[, PC_COLS, with=FALSE])

# ── SHARED FUNCTIONS ─────────────────────────────────────────────────────────
nl_shares <- function(V, nests, rho) {
  J <- length(V); s <- numeric(J)
  if (rho == 0) {
    e <- exp(V); s[] <- e/(1+sum(e))
  } else {
    e_adj <- exp(V/(1-rho))
    un    <- sort(unique(nests))
    E_g   <- vapply(un, function(g) sum(e_adj[nests==g]), numeric(1))
    names(E_g) <- as.character(un)
    D_g <- E_g^(1-rho); D <- sum(D_g)
    for (j in seq_len(J)) {
      g <- as.character(nests[j])
      s[j] <- (e_adj[j]/E_g[g]) * (D_g[g]/(1+D))
    }
  }
  s
}

nash_bertrand <- function(delta_pre, p_pre, mc, firms_post,
                           alpha, rho, nests) {
  J          <- length(p_pre)
  x_fixed    <- delta_pre - alpha*p_pre
  p          <- p_pre
  Omega_post <- outer(firms_post, firms_post, "==")*1.0
  for (iter in seq_len(NASH_MAXITER)) {
    V  <- x_fixed + alpha*p
    s  <- nl_shares(V, nests, rho)
    if (rho > 0) {
      e_adj <- exp(V/(1-rho))
      un    <- sort(unique(nests))
      E_g   <- vapply(un, function(g) sum(e_adj[nests==g]), numeric(1))
      names(E_g) <- as.character(un)
      sjg   <- e_adj / E_g[as.character(nests)]
    } else { sjg <- rep(1, J) }
    Delta <- matrix(0, J, J)
    for (j in seq_len(J)) for (k in seq_len(J)) {
      sn <- (nests[j]==nests[k])
      if (j==k)
        Delta[j,j] <- alpha*s[j]*(1/(1-rho) - rho/(1-rho)*sjg[j] - s[j])
      else if (sn && rho>0)
        Delta[j,k] <- alpha*s[j]*(-rho/(1-rho)*sjg[k] - s[k])
      else
        Delta[j,k] <- -alpha*s[j]*s[k]
    }
    OD    <- Omega_post * Delta
    p_new <- tryCatch(mc - solve(OD, s), error=function(e) p)
    p_upd <- NASH_DAMP*p_new + (1-NASH_DAMP)*p
    if (max(abs(p_upd-p)) < NASH_TOL) { p <- p_upd; break }
    p <- p_upd
  }
  p
}

run_for_k <- function(K) {
  cat(sprintf("\n── K=%d ──────────────────────────────────\n", K))

  # 1. K-means
  set.seed(42)
  km    <- kmeans(pc_mat, centers=K, nstart=50, iter.max=500)
  pcs_k <- copy(pcs_raw); pcs_k[, clip_nest := km$cluster]

  # Merge clip_nest into panel — use setkey to preserve row order in panel
  # by doing a left join that keeps panel's original row order
  panel_copy <- copy(panel)
  panel_copy[, clip_nest := pcs_k$clip_nest[match(asin, pcs_k$asin)]]
  panel_copy[is.na(clip_nest), clip_nest := K+1L]
  pan <- panel_copy

  # Cluster quality
  wcss <- km$tot.withinss
  sil  <- mean(silhouette(km$cluster, dist(pc_mat))[,3])
  cat(sprintf("  WCSS=%.1f  avg silhouette=%.3f\n", wcss, sil))

  # 2. RSS profile → rho*, RSS drop
  #    Mirror blp_three_models.R exactly:
  #    - compute log_sjg_tmp (observed within-nest shares)
  #    - demean log_odds, price_mean, log_sjg_tmp, and all X_VARS together by quarter
  #    - build x_mat and XtX_inv from pan (same row order as ydm)
  #    - base_y = log_odds_dm - ALPHA * price_mean_dm
  pan[, s_nest_t    := sum(share), by=.(quarter, clip_nest)]
  pan[, s_jg        := share / s_nest_t]
  pan[, log_sjg_tmp := log(pmax(s_jg, 1e-6))]

  # Demean all relevant columns together (exactly as blp_three_models.R)
  dm_cols <- c("log_odds", "price_mean", "log_sjg_tmp", X_VARS)
  pan[, (paste0(dm_cols,"_dm")) := lapply(.SD, function(x) x - mean(x, na.rm=TRUE)),
      .SDcols = dm_cols, by = quarter]

  x_mat   <- as.matrix(pan[, paste0(X_VARS,"_dm"), with=FALSE])
  XtX_inv <- solve(crossprod(x_mat))
  base_y  <- pan$log_odds_dm - ALPHA * pan$price_mean_dm

  rss_tbl <- rbindlist(lapply(RHO_GRID, function(rho) {
    ydm <- base_y - rho * pan$log_sjg_tmp_dm
    bh  <- as.numeric(XtX_inv %*% crossprod(x_mat, ydm))
    res <- ydm - x_mat %*% bh
    data.table(rho=rho, rss=as.numeric(crossprod(res)))
  }))

  rho_star <- rss_tbl[which.min(rss), rho]
  rss0     <- rss_tbl[rho==0, rss]
  rss_opt  <- min(rss_tbl$rss)
  rss_drop <- 100*(rss0 - rss_opt)/rss0
  cat(sprintf("  rho*=%.2f  RSS(0)=%.2f  RSS(opt)=%.2f  drop=%.1f%%\n",
              rho_star, rss0, rss_opt, rss_drop))

  # 3. Berry inversion: delta_jt = log(s/s0) - rho* * log(s_{j|g})
  #    (alpha*p is inside delta, matches blp_three_models.R)
  pan[, delta := log_odds - rho_star * log_sjg_tmp]

  # MC inversion over ALL quarters (exactly as blp_three_models.R line 388)
  # Then average over pre-merger only for mc_fixed; fallback = own quarter mc
  mc_rows <- rbindlist(lapply(qtrs, function(qtr) {
    dt_q    <- pan[quarter == qtr][order(asin)]
    J       <- nrow(dt_q)
    p_q     <- dt_q$price_mean
    s_q     <- dt_q$share
    sjg_q   <- dt_q$s_jg          # observed within-nest shares (blp_three_models.R: sv <- dt_q$s_jg)
    nests_q <- dt_q$clip_nest
    firms_q <- dt_q$firm_id_pre
    rho     <- rho_star

    Delta <- matrix(0, J, J)
    for (j in seq_len(J)) for (k in seq_len(J)) {
      sn <- (nests_q[j] == nests_q[k])
      if (j == k)
        Delta[j,j] <- ALPHA*s_q[j]*(1/(1-rho) - rho/(1-rho)*sjg_q[j] - s_q[j])
      else if (sn && rho > 0)
        Delta[j,k] <- ALPHA*s_q[j]*(-rho/(1-rho)*sjg_q[k] - s_q[k])
      else
        Delta[j,k] <- -ALPHA*s_q[j]*s_q[k]
    }
    Omega <- outer(firms_q, firms_q, "==")*1.0
    OD    <- Omega * Delta
    mc_q  <- tryCatch(as.numeric(p_q + solve(OD, s_q)), error=function(e) rep(NA, J))
    data.table(asin=dt_q$asin, quarter=qtr, mc=mc_q)
  }))
  # Merge all-quarter MCs back into pan (needed for fallback)
  pan <- merge(pan, mc_rows, by=c("asin","quarter"), all.x=TRUE)

  # Neg MC over all quarters (matches blp_three_models.R line 397-399)
  neg_mc_rate <- 100 * mean(pan$mc < 0, na.rm=TRUE)
  cat(sprintf("  Neg MC rate=%.1f%%\n", neg_mc_rate))

  # Average pre-merger MCs per ASIN; fallback = own quarter mc (not global median)
  pre_qtrs <- qtrs[qtrs < MERGER_START]
  mc_avg   <- pan[quarter %in% pre_qtrs, .(mc_fixed=mean(mc, na.rm=TRUE)), by=asin]
  pan      <- merge(pan, mc_avg, by="asin", all.x=TRUE)
  pan[is.na(mc_fixed), mc_fixed := mc]   # blp_three_models.R line 453

  post_qtrs   <- qtrs[qtrs >= MERGER_START]
  merger_rows <- rbindlist(lapply(post_qtrs, function(qtr) {
    dt_q       <- pan[quarter == qtr]
    nests_q    <- dt_q$clip_nest
    firms_pre  <- dt_q$firm_id_pre
    firms_post <- dt_q$firm_id_post

    p_no <- nash_bertrand(dt_q$delta, dt_q$price_mean, dt_q$mc_fixed,
                          firms_pre,  ALPHA, rho_star, nests_q)
    p_mg <- nash_bertrand(dt_q$delta, dt_q$price_mean, dt_q$mc_fixed,
                          firms_post, ALPHA, rho_star, nests_q)
    data.table(asin=dt_q$asin, brand=dt_q$brand_grouped_50,
               quarter=qtr, p_no=p_no, p_mg=p_mg)
  }))

  effects <- merger_rows[brand %in% c("hello","Colgate"),
    .(effect_pct = mean((p_mg-p_no)/p_no*100, na.rm=TRUE)),
    by=brand]

  hello_eff   <- effects[brand=="hello",   effect_pct]
  colgate_eff <- effects[brand=="Colgate", effect_pct]
  cat(sprintf("  Merger effect: hello=+%.3f%%  Colgate=+%.3f%%\n",
              hello_eff, colgate_eff))

  list(K=K, wcss=wcss, sil=sil, rho_star=rho_star,
       rss_drop=rss_drop, neg_mc=neg_mc_rate,
       hello_eff=hello_eff, colgate_eff=colgate_eff)
}

# ── RUN ALL K VALUES ─────────────────────────────────────────────────────────
cat("=== CLIP K SENSITIVITY ANALYSIS ===\n")
results <- lapply(K_TEST, run_for_k)

# ── SUMMARY TABLE ────────────────────────────────────────────────────────────
res_dt <- rbindlist(lapply(results, as.data.table))
res_dt[, K := as.integer(K)]

cat("\n\n")
cat(strrep("=", 80), "\n")
cat(sprintf("  %-4s  %-8s  %-8s  %-7s  %-9s  %-8s  %-12s  %-12s\n",
    "K", "WCSS", "Sil.", "rho*", "RSS drop", "Neg MC%",
    "hello eff%", "Colgate eff%"))
cat(strrep("-", 80), "\n")
for (i in seq_len(nrow(res_dt))) {
  r <- res_dt[i]
  marker <- if (r$K == 6) " ◄ current" else ""
  cat(sprintf("  %-4d  %-8.1f  %-8.3f  %-7.2f  %-9.1f  %-8.1f  %-12.3f  %-12.3f%s\n",
      r$K, r$wcss, r$sil, r$rho_star, r$rss_drop, r$neg_mc,
      r$hello_eff, r$colgate_eff, marker))
}
cat(strrep("=", 80), "\n")

fwrite(res_dt, file.path(out_dir, "clip_k_sensitivity.csv"))
cat("\nSaved: clip_k_sensitivity.csv\n")
