# ============================================================================
# blp_three_models.R
# Three BLP demand models compared at ASIN×quarter level
#
# MODEL 1: Plain logit            (ρ = 0,    no nesting)
# MODEL 2: Brand-nested logit     (ρ = ρ*,   nests = brand groups)
# MODEL 3: CLIP-nested logit      (ρ = ρ*,   nests = K=5 CLIP clusters)
#
# ALL MODELS:
#   α  = calibrated to ε_target = −2.62         (professor step 3, fixed)
#   β  = concentrated OLS at fixed (α, ρ)        (professor step 4)
#   ρ* = argmin RSS over grid [0, 0.90]           (profile for nested models)
#
# DEMAND:   Berry (1994) linearisation
#   δ_jt = log(s_jt/s_0t) − ρ·log(s_{j|g,t})
#   δ_jt = α·p_jt + β'x_jt + FE_t + ξ_jt
#
# COSTS:    Bertrand inversion (Berry 1994)
#   mc = p + (Ω_pre ∘ Δ)^{−1} s
#
# NASH MERGER SIMULATION — Bertrand contraction mapping
#   Same algorithm as PyBLP compute_prices(), implemented directly so that
#   calibrated α = −0.31005 (not OLS α_OLS ≈ −0.012) drives the markup.
#   (PyBLP compute_prices() cannot be used here because PyBLP's OLS on
#    the raw prices recovers α_OLS ≈ −0.012, not the calibrated value;
#    the price-scaling trick preserves OLS coefficient labels but inflates
#    Nash markups by 1/k ≈ 28× when unscaled.)
#
#   Contraction:  p_{t+1} = mc − (Ω_post ∘ Δ(p_t))^{−1} s(p_t)
#   Mean utility: V_j(p) = δ_j_pre + α·(p_j − p_pre_j)
#   Shares from nested-logit formula at each iteration
#
# OUTPUT (all in output/):
#   three_models_formulas.txt
#   model{1,2,3}_coefficients.csv
#   model{1,2,3}_asin_results.csv    ← ASIN×quarter: share, mc, elas, pre/post price
#   three_models_merger_summary.csv  ← brand-level comparison across models
# ============================================================================

library(data.table); library(lubridate)

# ── PATH CONFIG ───────────────────────────────────────────────────────────────
# When sourced from blp_thesis.Rmd, repo_dir / out_dir / data_dir are already
# defined.  When run standalone, auto-detect from this script's own location —
# works on Windows, Mac, and Linux regardless of repo folder name.
if (!exists("repo_dir")) {
  .sf <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/"),   # via source()
    error = function(e) tryCatch(
      normalizePath(rstudioapi::getSourceEditorContext()$path, winslash = "/"),
      error = function(e) ""
    )
  )
  if (nzchar(.sf)) {
    repo_dir <- dirname(dirname(.sf))           # scripts/ -> repo root
  } else {
    .cands <- c("clip-amazon-toothpaste-market",
                "clip-amazon-toothpaste-market-main")
    .found <- Filter(function(d) dir.exists(file.path(getwd(), d)), .cands)
    repo_dir <- normalizePath(
      if (length(.found)) file.path(getwd(), .found[1]) else getwd(),
      winslash = "/"
    )
  }
}
if (!exists("data_dir")) data_dir <- file.path(repo_dir, "data")
if (!exists("out_dir"))  out_dir  <- file.path(repo_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# ─────────────────────────────────────────────────────────────────────────────

# ── PARAMETERS ────────────────────────────────────────────────────────────────
LAMBDA        <- 13.93
TIME_CUTOFF   <- as.Date("2022-07-01")
MIN_QUARTERS  <- 3
EPS_TARGET    <- -2.62
K_CLIP        <- 5
RHO_GRID      <- seq(0, 0.90, by=0.01)
MERGER_START  <- "2020Q1"

import_brands       <- c("Sensodyne","SENSODYNE PRONAMEL","Parodontax","APAGARD")
freight_crisis_qtrs <- c("2021Q3","2021Q4","2022Q1","2022Q2")
PC_COLS             <- paste0("joint_pc", 1:5)
X_VARS              <- c("pkg_size", PC_COLS, "is_other", "import_freight_shock")

# Nash solver settings
NASH_TOL      <- 1e-8
NASH_MAXITER  <- 2000
NASH_DAMP     <- 0.4   # dampening in [0,1]; lower = more stable

cat(strrep("=",72),"\n")
cat("  THREE BLP MODELS — ASIN×quarter, ε_target=−2.62\n")
cat("  M1: Logit  |  M2: Brand-Nested  |  M3: CLIP-Nested\n")
cat(strrep("=",72),"\n\n")

# ── SECTION 1: DATA LOADING ───────────────────────────────────────────────────
panel <- fread(file.path(data_dir, "asin_quarter_panel.csv"))
panel[, month_date_dummy := as.Date(paste0(
  sub("Q.*","",quarter),"-",
  sprintf("%02d",(as.integer(sub(".*Q","",quarter))-1)*3+1),"-01"))]
panel <- panel[month_date_dummy <= TIME_CUTOFF][, month_date_dummy := NULL]

# Pre-computed ASIN characteristics (pkg_size, is_import) — replaces the raw
# 148 MB choice_set_with_state_and_region.csv (not redistributed).
# Regenerate with: scripts/build_asin_characteristics.R
asin_chars <- fread(file.path(data_dir, "asin_characteristics.csv"))
panel <- merge(panel, asin_chars[, .(asin, pkg_size, is_import)], by="asin", all.x=TRUE)
panel[is.na(pkg_size),  pkg_size  := median(panel[["pkg_size"]],  na.rm=TRUE)]
panel[is.na(is_import), is_import := 0]

keep_asins <- panel[, .(n=uniqueN(quarter)), by=asin][n>=MIN_QUARTERS, asin]
panel      <- panel[asin %in% keep_asins]

mkt_totals <- panel[, .(total_q=sum(q_jt)), by=quarter]
mkt_totals[, M_t := (1+LAMBDA)*total_q]
panel      <- merge(panel, mkt_totals[,.(quarter,M_t,total_q)], by="quarter", all.x=TRUE)
panel[, share         := pmax(1e-6, pmin(1-1e-6, q_jt/M_t))]
panel[, outside_share := 1 - total_q/M_t]
panel[, log_odds      := log(share) - log(outside_share)]
panel[, is_other            := as.numeric(brand_grouped_50=="Other")]
panel[, is_freight_crisis   := as.numeric(quarter %in% freight_crisis_qtrs)]
panel[, import_freight_shock := is_import * is_freight_crisis]

qtrs <- sort(unique(panel$quarter))
panel[, market_ids := match(quarter, qtrs)]

cat(sprintf("Panel: %d obs | %d ASINs | %d quarters\n\n",
    nrow(panel), uniqueN(panel$asin), uniqueN(panel$quarter)))

# ── SECTION 2: CLIP CLUSTERS (K=5) ───────────────────────────────────────────
pcs_raw <- fread(file.path(data_dir, "asin_joint_pcs_complete.csv"))
pcs_raw <- pcs_raw[asin %in% panel$asin]
set.seed(42)
km6 <- kmeans(as.matrix(pcs_raw[, PC_COLS, with=FALSE]),
              centers=K_CLIP, nstart=50, iter.max=500)
pcs_raw[, clip_nest := km6$cluster]
panel  <- merge(panel, pcs_raw[, .(asin, clip_nest)], by="asin", all.x=TRUE)
panel[is.na(clip_nest), clip_nest := K_CLIP + 1L]
pct_var <- (1 - km6$tot.withinss / sum(scale(
  as.matrix(pcs_raw[, PC_COLS, with=FALSE]), scale=FALSE)^2)) * 100
cat(sprintf("CLIP clusters (K=%d): %.1f%% variance explained\n\n", K_CLIP, pct_var))

# ── SECTION 3: CALIBRATE α ────────────────────────────────────────────────────
med_scale <- median(panel$price_mean * (1 - panel$share))
ALPHA     <- EPS_TARGET / med_scale

panel[, price_dm := price_mean - mean(price_mean), by=quarter]
panel[, y_dm     := log_odds   - mean(log_odds),   by=quarter]
alpha_ols <- sum(panel$price_dm * panel$y_dm) / sum(panel$price_dm^2)

cat(sprintf("α calibration: ε_target=%.1f / median(p·(1-s))=$%.4f → α=%.5f\n",
    EPS_TARGET, med_scale, ALPHA))
cat(sprintf("OLS α (for reference): %.5f  (not used in demand or Nash)\n\n", alpha_ols))

# ── SECTION 4: FIRM IDs (PRE AND POST MERGER) ─────────────────────────────────
brand_firm <- data.table(brand_grouped_50=sort(unique(panel$brand_grouped_50)))
brand_firm[, firm_id_pre := seq_len(.N)]
panel <- merge(panel, brand_firm, by="brand_grouped_50", all.x=TRUE)
# GSK brands share one firm_id
panel[brand_grouped_50 %in% c("Sensodyne","SENSODYNE PRONAMEL","Parodontax"),
      firm_id_pre := 9999L]
# Tom's of Maine → Colgate pre-merger
colgate_fid <- panel[brand_grouped_50=="Colgate", unique(firm_id_pre)][1]
panel[brand_grouped_50=="Tom's of Maine", firm_id_pre := colgate_fid]
# Post-merger: hello → Colgate from MERGER_START
panel[, firm_id_post := firm_id_pre]
panel[brand_grouped_50=="hello" & quarter >= MERGER_START,
      firm_id_post := colgate_fid]

# Brand-integer nest ID for brand-nested logit
brand_nest_dt <- data.table(brand_grouped_50=sort(unique(panel$brand_grouped_50)))
brand_nest_dt[, brand_nest_id := seq_len(.N)]
panel <- merge(panel, brand_nest_dt, by="brand_grouped_50", all.x=TRUE)

# ── SECTION 5: BERTRAND MC INVERSION ──────────────────────────────────────────
compute_mc_quarter <- function(dt_q, alpha, rho, nest_vec, sjg_vec) {
  J     <- nrow(dt_q)
  s     <- dt_q$share
  p     <- dt_q$price_mean
  firms <- dt_q$firm_id_pre
  Delta <- matrix(0, J, J)
  for (j in 1:J) for (k in 1:J) {
    sn <- (nest_vec[j] == nest_vec[k])
    if (j==k) {
      Delta[j,j] <- alpha*s[j]*(1/(1-rho) - rho/(1-rho)*sjg_vec[j] - s[j])
    } else if (sn && rho>0) {
      Delta[j,k] <- alpha*s[j]*(-rho/(1-rho)*sjg_vec[k] - s[k])
    } else {
      Delta[j,k] <- -alpha*s[j]*s[k]
    }
  }
  Omega <- outer(firms, firms, "==") * 1.0
  OD    <- Omega * Delta
  tryCatch(as.numeric(p + solve(OD, s)), error=function(e) rep(NA_real_, J))
}

# ── SECTION 6: OWN-PRICE ELASTICITY ───────────────────────────────────────────
own_elas <- function(alpha, rho, price, share, sjg) {
  alpha * price * (1/(1-rho) - rho/(1-rho)*sjg - share)
}

# ── SECTION 7: NESTED-LOGIT SHARES FROM MEAN UTILITIES ────────────────────────
# V     : vector of mean utilities (length J)
# nests : integer vector of nest assignments (length J)
# rho   : scalar nesting parameter
nl_shares <- function(V, nests, rho) {
  J   <- length(V)
  s   <- numeric(J)
  if (rho == 0) {
    e   <- exp(V)
    D   <- sum(e)
    s[] <- e / (1 + D)
  } else {
    e_adj  <- exp(V / (1 - rho))
    un     <- sort(unique(nests))
    E_g    <- vapply(un, function(g) sum(e_adj[nests==g]), numeric(1))
    names(E_g) <- as.character(un)
    D_g    <- E_g^(1 - rho)
    D      <- sum(D_g)
    for (j in seq_len(J)) {
      g     <- as.character(nests[j])
      s[j]  <- (e_adj[j] / E_g[g]) * (D_g[g] / (1 + D))
    }
  }
  s
}

# ── SECTION 8: BERTRAND-NASH CONTRACTION (same algorithm as PyBLP compute_prices)
# delta_pre : mean utility at pre-merger equilibrium (δ_j = log(s/s0) - ρ·log(s_{j|g}))
# p_pre     : pre-merger price vector
# mc        : marginal cost vector (from Bertrand inversion)
# firms_post: post-merger firm IDs
# alpha, rho, nests : demand parameters
nash_bertrand <- function(delta_pre, p_pre, mc, firms_post,
                           alpha, rho, nests,
                           tol=NASH_TOL, max_iter=NASH_MAXITER, damp=NASH_DAMP) {
  J       <- length(p_pre)
  x_fixed <- delta_pre - alpha * p_pre  # non-price mean utility (fixed during Nash)
  p       <- p_pre                       # starting prices

  Omega_post <- outer(firms_post, firms_post, "==") * 1.0

  for (iter in seq_len(max_iter)) {
    V <- x_fixed + alpha * p

    # ── shares ────────────────────────────────────────────────────────────────
    s <- nl_shares(V, nests, rho)

    # ── within-nest shares ────────────────────────────────────────────────────
    if (rho > 0) {
      e_adj  <- exp(V / (1 - rho))
      un     <- sort(unique(nests))
      E_g    <- vapply(un, function(g) sum(e_adj[nests==g]), numeric(1))
      names(E_g) <- as.character(un)
      sjg    <- e_adj / E_g[as.character(nests)]
    } else {
      sjg <- rep(1, J)
    }

    # ── demand Jacobian Δ (∂s_j / ∂p_k) ─────────────────────────────────────
    Delta <- matrix(0, J, J)
    for (j in seq_len(J)) {
      for (k in seq_len(J)) {
        sn <- (nests[j] == nests[k])
        if (j == k) {
          Delta[j,j] <- alpha*s[j]*(1/(1-rho) - rho/(1-rho)*sjg[j] - s[j])
        } else if (sn && rho > 0) {
          Delta[j,k] <- alpha*s[j]*(-rho/(1-rho)*sjg[k] - s[k])
        } else {
          Delta[j,k] <- -alpha*s[j]*s[k]
        }
      }
    }

    # ── Nash FOC ──────────────────────────────────────────────────────────────
    OD    <- Omega_post * Delta
    p_new <- tryCatch(mc - solve(OD, s), error=function(e) p)

    # ── damped update ─────────────────────────────────────────────────────────
    p_upd <- damp * p_new + (1 - damp) * p
    diff  <- max(abs(p_upd - p))
    p     <- p_upd

    if (diff < tol) break
  }
  p
}

# ── SECTION 9: RUN THREE MODELS ───────────────────────────────────────────────
model_specs <- list(
  list(id=1, name="Logit",        rho_fixed=0,   nest_col="market_ids",   nest_label="none"),
  list(id=2, name="Brand-Nested", rho_fixed=NA,  nest_col="brand_nest_id",nest_label="brand"),
  list(id=3, name="CLIP-Nested",  rho_fixed=NA,  nest_col="clip_nest",    nest_label="clip")
)

all_coef    <- list()
all_results <- list()
rho_stars   <- numeric(3)

for (m in model_specs) {
  cat(strrep("-",72),"\n")
  cat(sprintf("  MODEL %d: %s\n", m$id, m$name))
  cat(strrep("-",72),"\n")

  pan <- copy(panel)

  # ── 9a. Within-nest shares ────────────────────────────────────────────────
  if (m$nest_label == "none") {
    pan[, s_nest  := 1]
    pan[, s_jg    := 1]
  } else {
    pan[, s_nest := sum(share), by=c("quarter", m$nest_col)]
    pan[, s_jg   := share / s_nest]
  }
  pan[, log_sjg := log(pmax(s_jg, 1e-6))]

  # ── 9b. ρ profile ─────────────────────────────────────────────────────────
  if (!is.na(m$rho_fixed) && m$rho_fixed == 0) {
    rho_star <- 0
    dm_cols  <- c("log_odds","price_mean", X_VARS)
    pan[, (paste0(dm_cols,"_dm")) :=
          lapply(.SD, function(x) x - mean(x, na.rm=TRUE)),
        .SDcols=dm_cols, by=quarter]
    x_mat   <- as.matrix(pan[, paste0(X_VARS,"_dm"), with=FALSE])
    XtX_inv <- solve(crossprod(x_mat))
    base_y  <- pan$log_odds_dm - ALPHA * pan$price_mean_dm
    bhat    <- as.numeric(XtX_inv %*% crossprod(x_mat, base_y))
    res     <- base_y - x_mat %*% bhat
    sigma2  <- as.numeric(crossprod(res)) /
               (nrow(pan) - length(X_VARS) - uniqueN(pan$quarter))
    se_bhat <- sqrt(diag(XtX_inv) * sigma2)
    rss_0   <- as.numeric(crossprod(res))
    rss_star <- rss_0
    pan[, bx    := as.numeric(as.matrix(.SD) %*% bhat), .SDcols=X_VARS]
    pan[, delta := log_odds]
    pan[, xi_jt := delta - ALPHA*price_mean - bx]
    cat(sprintf("  ρ = 0 (plain logit, fixed)\n"))
  } else {
    # Re-compute s_jg using the nest column for this model
    pan[, sjg_col_tmp := get(m$nest_col)]
    pan[, s_nest2 := sum(share), by=c("quarter","sjg_col_tmp")]
    pan[, s_jg2   := share / s_nest2]
    pan[, log_sjg_tmp := log(pmax(s_jg2, 1e-6))]

    dm_cols <- c("log_odds","price_mean","log_sjg_tmp", X_VARS)
    pan[, (paste0(dm_cols,"_dm")) :=
          lapply(.SD, function(x) x - mean(x, na.rm=TRUE)),
        .SDcols=dm_cols, by=quarter]
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
    rss_0    <- rss_tbl[rho==0, rss]
    rss_star <- rss_tbl[which.min(rss), rss]

    ydm_star <- base_y - rho_star * pan$log_sjg_tmp_dm
    bhat     <- as.numeric(XtX_inv %*% crossprod(x_mat, ydm_star))
    res_star <- ydm_star - x_mat %*% bhat
    sigma2   <- as.numeric(crossprod(res_star)) /
                (nrow(pan) - length(X_VARS) - uniqueN(pan$quarter))
    se_bhat  <- sqrt(diag(XtX_inv) * sigma2)
    pan[, bx    := as.numeric(as.matrix(.SD) %*% bhat), .SDcols=X_VARS]
    pan[, delta := log_odds - rho_star * log_sjg_tmp]
    pan[, xi_jt := delta - ALPHA*price_mean - bx]
    cat(sprintf("  ρ* = %.2f (grid) | RSS drop: %.1f%%\n",
        rho_star, 100*(rss_0-rss_star)/rss_0))
  }
  rho_stars[m$id] <- rho_star

  # ── 9c. Print coefficients ────────────────────────────────────────────────
  tvals <- bhat / se_bhat
  stars <- ifelse(abs(tvals)>2.58,"***",ifelse(abs(tvals)>1.96,"**",
           ifelse(abs(tvals)>1.64,"*","")))
  cat(sprintf("  %-26s %10s %10s %7s\n","Variable","β","SE","t"))
  cat(strrep("-",57),"\n")
  cat(sprintf("  %-26s %10.5f  [calibrated to ε=%.1f]\n","prices (α)",ALPHA,EPS_TARGET))
  for (i in seq_along(X_VARS)) {
    cat(sprintf("  %-26s %+10.5f %10.5f %6.2f%s\n",
        X_VARS[i], bhat[i], se_bhat[i], tvals[i], stars[i]))
  }
  if (rho_star == 0) {
    cat(sprintf("  %-26s %10.5f  [fixed]\n","rho",0))
  } else {
    cat(sprintf("  %-26s %10.5f  [grid-selected]\n","rho*",rho_star))
  }
  cat("\n")

  coef_dt <- data.table(
    model    = m$name,
    variable = c("prices_alpha", X_VARS, "rho"),
    beta     = c(ALPHA, bhat, rho_star),
    se       = c(NA, se_bhat, NA),
    t_stat   = c(NA, tvals, NA),
    signif   = c("calibrated", stars, if(rho_star==0) "fixed" else "profiled")
  )
  all_coef[[m$id]] <- coef_dt
  fwrite(coef_dt, file.path(out_dir, sprintf("model%d_coefficients.csv", m$id)))

  # ── 9d. Marginal costs ────────────────────────────────────────────────────
  nest_vec_col <- if (m$nest_label=="none") "market_ids" else
                  if (m$nest_label=="brand") "brand_nest_id" else "clip_nest"

  mc_rows <- rbindlist(lapply(qtrs, function(qtr) {
    dt_q <- pan[quarter==qtr][order(asin)]
    nv   <- dt_q[[nest_vec_col]]
    sv   <- dt_q$s_jg
    data.table(asin=dt_q$asin, quarter=qtr,
               mc=compute_mc_quarter(dt_q, ALPHA, rho_star, nv, sv))
  }))
  pan <- merge(pan, mc_rows, by=c("asin","quarter"), all.x=TRUE)

  n_neg   <- sum(pan$mc < 0, na.rm=TRUE)
  n_total <- sum(!is.na(pan$mc))
  cat(sprintf("  Negative MCs: %d / %d (%.1f%%)\n", n_neg, n_total, 100*n_neg/n_total))

  # ── 9e. Own-price elasticities ────────────────────────────────────────────
  pan[, elas_own := own_elas(ALPHA, rho_star, price_mean, share, s_jg)]

  brand_mc_summary <- pan[, .(
    avg_price = round(mean(price_mean),2),
    avg_mc    = round(mean(mc, na.rm=TRUE),2),
    lerner    = round(mean((price_mean-mc)/price_mean, na.rm=TRUE)*100,1),
    pct_neg   = round(mean(mc<0,na.rm=TRUE)*100,1),
    med_elas  = round(median(elas_own),3)
  ), by=brand_grouped_50][order(-avg_price)]
  cat(sprintf("  %-24s %7s %7s %8s %7s %8s\n","Brand","p","MC","Lerner","neg%","ε_own"))
  cat(strrep("-",65),"\n")
  for (i in seq_len(nrow(brand_mc_summary))) {
    r <- brand_mc_summary[i]
    mk <- if(r$brand_grouped_50 %in% c("hello","Colgate","Tom's of Maine")) "★" else " "
    cat(sprintf("  %s %-23s %7.2f %7.2f %7.1f%% %6.1f%% %8.3f\n",
        mk, r$brand_grouped_50, r$avg_price, r$avg_mc,
        r$lerner, r$pct_neg, r$med_elas))
  }
  cat("\n")

  # ── 9f. Bertrand-Nash Counterfactual Simulation ───────────────────────────
  # WHAT WE'RE COMPUTING:
  #   The merger actually happened at 2020Q1. Observed prices in 2020Q1–2022Q3
  #   already reflect the merged firm's pricing. Our counterfactual asks:
  #   "What would prices have been if this merger had NEVER happened?"
  #
  # Strategy for post-merger quarters:
  #   (A) No-merger counterfactual:  Nash with firm_id_pre  (hello stays separate)
  #   (B) Merger simulation:         Nash with firm_id_post (hello→Colgate merged)
  #
  # CRITICAL: MCs must be estimated from PRE-MERGER quarters only and held fixed.
  #   If we invert MCs quarter-by-quarter (including post-merger quarters), the
  #   no-merger Nash trivially recovers observed prices by construction — the FOC
  #   p = mc + markup(Ω_pre) is the identity that defined mc in the first place.
  #   Using pre-merger MCs breaks this tautology and gives a genuine counterfactual.
  #
  # Merger price effect = price_merger − price_nomerger
  # We also keep price_observed (actual data) for comparison/validation.
  #
  # Contraction: p_{n+1} = λ·[mc − (Ω∘Δ(p_n))^{-1} s(p_n)] + (1−λ)·p_n
  #   λ = NASH_DAMP = 0.4
  cat("  Running no-merger counterfactual and merger simulation...\n")

  merger_qtrs  <- qtrs[qtrs >= MERGER_START]
  premerger_qtrs <- qtrs[qtrs <  MERGER_START]

  # Average MCs per ASIN across pre-merger quarters only
  mc_pre_asin <- pan[quarter %in% premerger_qtrs,
                     .(mc_fixed = mean(mc, na.rm=TRUE)), by=asin]
  pan <- merge(pan, mc_pre_asin, by="asin", all.x=TRUE)
  # Fallback for ASINs with no pre-merger data: use their own MC
  pan[is.na(mc_fixed), mc_fixed := mc]

  run_nash_qtrs <- function(firm_col) {
    rbindlist(lapply(merger_qtrs, function(qtr) {
      dt_q      <- pan[quarter==qtr][order(asin)]
      nv        <- dt_q[[nest_vec_col]]
      mc_q      <- dt_q$mc_fixed   # ← pre-merger MCs, fixed across quarters
      firms_use <- dt_q[[firm_col]]
      p_pre     <- dt_q$price_mean
      p_nash <- nash_bertrand(
        delta_pre  = dt_q$delta,
        p_pre      = p_pre,
        mc         = mc_q,
        firms_post = firms_use,
        alpha      = ALPHA,
        rho        = rho_star,
        nests      = nv
      )
      data.table(asin=dt_q$asin, quarter=qtr, p=p_nash)
    }))
  }

  nash_merger   <- run_nash_qtrs("firm_id_post")   # (B) with merger
  nash_nomerger <- run_nash_qtrs("firm_id_pre")    # (A) no-merger counterfactual

  setnames(nash_merger,   "p", "price_merger")
  setnames(nash_nomerger, "p", "price_nomerger")

  pan <- merge(pan, nash_merger,   by=c("asin","quarter"), all.x=TRUE)
  pan <- merge(pan, nash_nomerger, by=c("asin","quarter"), all.x=TRUE)

  # For pre-merger quarters: both Nash prices = observed (no counterfactual applicable)
  pan[is.na(price_merger),   price_merger   := price_mean]
  pan[is.na(price_nomerger), price_nomerger := price_mean]

  # Merger price effect (in post-merger quarters only)
  pan[, merger_effect_pct := fifelse(
    quarter %in% merger_qtrs,
    (price_merger - price_nomerger) / price_nomerger * 100,
    NA_real_
  )]
  # Model fit: how close is our merger Nash to observed?
  pan[, model_fit_pct := fifelse(
    quarter %in% merger_qtrs,
    (price_merger / price_mean - 1) * 100,
    NA_real_
  )]

  # ── Summary (merger quarters only) ───────────────────────────────────────
  focus_brands <- c("hello","Colgate","Tom's of Maine","Crest","Sensodyne")
  summ <- pan[quarter %in% merger_qtrs & brand_grouped_50 %in% focus_brands, .(
    obs       = round(mean(price_mean),3),
    mc        = round(mean(mc,na.rm=TRUE),3),
    nomerger  = round(mean(price_nomerger),3),
    merger    = round(mean(price_merger),3),
    effect_pct= round(mean(merger_effect_pct,na.rm=TRUE),3)
  ), by=brand_grouped_50]
  cat(sprintf("  %-22s %8s %8s %9s %9s %8s\n",
      "Brand","obs$","MC","nomrg$","mrg$","effect%"))
  cat(strrep("-",68),"\n")
  for (i in seq_len(nrow(summ))) {
    r <- summ[i]
    mk <- if(r$brand_grouped_50 %in% c("hello","Colgate")) "★" else " "
    cat(sprintf("  %s %-21s %8.3f %8.3f %9.3f %9.3f %7.3f%%\n",
        mk, r$brand_grouped_50,
        r$obs, r$mc, r$nomerger, r$merger, r$effect_pct))
  }
  cat("\n")

  # ── Save ASIN-level results ───────────────────────────────────────────────
  nest_col_save <- if (m$nest_label=="brand") "brand_nest_id" else
                   if (m$nest_label=="clip")  "clip_nest"     else "market_ids"
  asin_out <- pan[, .(
    asin, quarter, brand_grouped_50,
    nest_id        = get(nest_col_save),
    share, price_observed=price_mean, mc, elas_own,
    price_nomerger, price_merger, merger_effect_pct, model_fit_pct
  )]
  fwrite(asin_out, file.path(out_dir, sprintf("model%d_asin_results.csv", m$id)))
  cat(sprintf("  Saved: model%d_asin_results.csv (%d rows)\n\n", m$id, nrow(asin_out)))

  all_results[[m$id]] <- pan
}

# ── SECTION 10: FORMULA SUMMARY ───────────────────────────────────────────────
formula_txt <- c(
"============================================================",
"THREE BLP MODELS — FORMULAS",
"============================================================",
"",
"SHARED SETUP:",
sprintf("  α = ε_target / median(p_jt · (1 − s_jt))"),
sprintf("    = %.1f / $%.4f = %.5f  [calibrated, not estimated]", EPS_TARGET, med_scale, ALPHA),
"",
"  Berry (1994) linearisation:",
"    log(s_jt / s_0t) − ρ · log(s_{j|g,t}) = α·p_jt + β'x_jt + FE_t + ξ_jt",
"",
"  Covariates x_jt: pkg_size, joint_pc1-5, is_other, import_freight_shock",
"  [joint_pc1-5 = first 5 PCs of CLIP image+text joint embeddings]",
"",
"  Bertrand MC inversion (Berry 1994):",
"    mc = p + (Ω_pre ∘ Δ)^{−1} s",
"    Δ_{jj} = α·s_j·[1/(1−ρ) − ρ·s_{j|g}/(1−ρ) − s_j]",
"    Δ_{jk} = α·s_j·[−ρ·s_{k|g}/(1−ρ) − s_k]   [same nest]",
"    Δ_{jk} = −α·s_j·s_k                           [diff nest]",
"",
"  Own-price elasticity:",
"    ε_{jj} = α·p_j·[1/(1−ρ) − ρ·s_{j|g}/(1−ρ) − s_j]",
"",
"  Bertrand-Nash Merger Simulation:",
"    (Same iterative algorithm as PyBLP compute_prices())",
"    V_j(p) = δ_j_pre + α·(p_j − p_pre_j)   [non-price utility fixed]",
"    Contraction: p_{t+1} = mc − (Ω_post ∘ Δ(p_t))^{−1} s(p_t)",
"    hello → Colgate (firm_id_post) from 2020Q1",
"",
"------------------------------------------------------------",
"MODEL 1: PLAIN LOGIT",
"  ρ = 0  (no nesting)",
"  All ASINs compete symmetrically within each quarter-market",
"  Markup: μ_j ≈ −1 / α·(1−s_j) ≈ $2.82 (uniform across products)",
"  Diversion ratio: DR_{j→k} = s_k / (1 − s_j)  [share-proportional only]",
"",
"------------------------------------------------------------",
"MODEL 2: BRAND-NESTED LOGIT",
"  Nests = brand groups (all hello ASINs in one nest, etc.)",
paste0("  ρ* = ", rho_stars[2], "  [concentrated OLS profile]"),
"  Higher ρ → stronger within-brand substitution",
"  Diversion: cross-brand reduced, within-brand boosted by ρ",
"",
"------------------------------------------------------------",
"MODEL 3: CLIP-NESTED LOGIT  [THESIS CONTRIBUTION]",
"  Nests = K=5 k-means clusters on CLIP joint_pc1-5",
paste0("  ρ* = ", rho_stars[3], "  [concentrated OLS profile]"),
sprintf("  %.1f%% of PC1-5 variance explained by clusters", pct_var),
"  Clusters group embedding-similar ASINs (attribute-based nests)",
"  Diversion: products close in CLIP space substitute more,",
"             regardless of brand identity",
"============================================================"
)
writeLines(formula_txt, file.path(out_dir, "three_models_formulas.txt"))
cat(paste(formula_txt, collapse="\n"), "\n\n")

# ── SECTION 11: CROSS-MODEL BRAND SUMMARY ─────────────────────────────────────
merger_brands <- c("hello","Colgate","Tom's of Maine","Crest","Sensodyne",
                   "APAGARD","Orajel","JASON")
merger_qtrs   <- qtrs[qtrs >= MERGER_START]

summary_list <- rbindlist(lapply(seq_along(model_specs), function(mi) {
  if (is.null(all_results[[mi]])) return(NULL)
  pan_m <- all_results[[mi]]
  pan_m[quarter %in% merger_qtrs & brand_grouped_50 %in% merger_brands, .(
    model          = model_specs[[mi]]$name,
    brand          = brand_grouped_50,
    avg_price_obs  = round(mean(price_mean),2),
    avg_mc         = round(mean(mc,na.rm=TRUE),2),
    lerner_pct     = round(mean((price_mean-mc)/price_mean,na.rm=TRUE)*100,1),
    med_elas       = round(median(elas_own,na.rm=TRUE),3),
    price_nomerger = round(mean(price_nomerger,na.rm=TRUE),3),
    price_merger   = round(mean(price_merger,na.rm=TRUE),3),
    effect_pct     = round(mean(merger_effect_pct,na.rm=TRUE),3)
  ), by=.(brand_grouped_50)][, brand_grouped_50 := NULL]
}), fill=TRUE)

fwrite(summary_list, file.path(out_dir, "three_models_merger_summary.csv"))

# ── SECTION 12: RMSE TABLE ────────────────────────────────────────────────────
# Compare Nash Merger and Nash No-merger against observed prices.
# Post-merger quarters only (2020Q1+), where both simulations exist.
rmse_rows <- rbindlist(lapply(seq_along(model_specs), function(mi) {
  if (is.null(all_results[[mi]])) return(NULL)
  pan_m  <- all_results[[mi]][quarter %in% merger_qtrs & !is.na(price_merger)]
  brands <- c("All", "hello", "Colgate")
  rbindlist(lapply(brands, function(b) {
    sub <- if (b == "All") pan_m else pan_m[brand_grouped_50 == b]
    if (nrow(sub) == 0) return(NULL)
    data.table(
      model        = model_specs[[mi]]$name,
      brand        = b,
      n_obs        = nrow(sub),
      rmse_merger  = round(sqrt(mean((sub$price_merger  - sub$price_mean)^2)), 4),
      rmse_nomerger= round(sqrt(mean((sub$price_nomerger- sub$price_mean)^2)), 4),
      mae_merger   = round(mean(abs(sub$price_merger  - sub$price_mean)), 4),
      mae_nomerger = round(mean(abs(sub$price_nomerger- sub$price_mean)), 4)
    )
  }))
}))

fwrite(rmse_rows, file.path(out_dir, "three_models_rmse.csv"))

cat(strrep("=",72),"\n")
cat("  RMSE / MAE — Nash Merger & No-merger vs Observed (post-merger qtrs)\n")
cat(strrep("=",72),"\n")
cat(sprintf("  %-18s %-10s %6s  %12s %12s\n",
    "Model","Brand","N","RMSE(merger)","RMSE(nomrg)"))
cat(strrep("-",64),"\n")
for (i in seq_len(nrow(rmse_rows))) {
  r <- rmse_rows[i]
  cat(sprintf("  %-18s %-10s %6d  %12.4f %12.4f\n",
      r$model, r$brand, r$n_obs, r$rmse_merger, r$rmse_nomerger))
}
cat(strrep("=",72),"\n\n")

cat(strrep("=",72),"\n")
cat("  CROSS-MODEL MERGER SUMMARY (post-merger quarters, merger parties)\n")
cat(strrep("=",72),"\n")
cat(sprintf("  %-18s %-14s %8s %8s %7s %7s %8s\n",
    "Model","Brand","pre$","MC","Lerner","ε_own","Δpost%"))
cat(strrep("-",72),"\n")
for (i in seq_len(nrow(summary_list))) {
  r <- summary_list[i]
  if (r$brand %in% c("hello","Colgate")) {
    cat(sprintf("  ★ %-17s %-14s %8.2f %8.2f %6.1f%% %7.3f %7.3f%%\n",
        r$model, r$brand, r$avg_price_obs, r$avg_mc,
        r$lerner_pct, r$med_elas, r$effect_pct))
  }
}
cat(strrep("=",72),"\n")
cat("  Saved: three_models_merger_summary.csv\n")
cat("  Saved: three_models_formulas.txt\n")
cat("  Saved: model{1,2,3}_coefficients.csv\n")
cat("  Saved: model{1,2,3}_asin_results.csv\n")
