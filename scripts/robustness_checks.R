# ============================================================================
# robustness_checks.R
# Robustness scenarios + diversion ratios + welfare for the three-model paper.
#
# PART A — Robustness scenarios (writes output/robustness_checks.csv)
#   A1. Market-size sensitivity: lambda in {13.93, 2x, 5x, 10x} for M3
#       (alpha recalibrated to eps = -2.62 at rho = 0 within each lambda)
#   A2. Per-model alpha recalibration: keep the baseline profiled rho*,
#       rescale alpha so the model's own implied MEDIAN elasticity at that
#       rho* equals -2.62 (two-step; no re-profiling), for M2 and M3
#   A3. MC window: last pre-merger quarter (2019Q4) instead of the
#       pre-merger average, M3 at baseline calibration
#
# PART B — Diversion ratios (writes output/diversion_ratios.csv)
#   DR_{j->k} = -(ds_k/dp_j)/(ds_j/dp_j), aggregated from hello ASINs into
#   fixed reporting buckets (Colgate brand / rest of CLIP cluster C5 /
#   other inside goods / outside), pre-merger quarters, share-weighted.
#
# PART C — Welfare (writes output/welfare_effects.csv)
#   Nested-logit consumer surplus per consumer, merger vs no-merger Nash,
#   averaged over post-merger quarters; also total per quarter using M_t.
# ============================================================================
library(data.table); library(lubridate)

# ── PATH CONFIG (same auto-detect as the other scripts) ───────────────────────
if (!exists("repo_dir")) {
  .sf <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/"),
    error = function(e) tryCatch(
      normalizePath(rstudioapi::getSourceEditorContext()$path, winslash = "/"),
      error = function(e) ""
    )
  )
  if (nzchar(.sf)) {
    repo_dir <- dirname(dirname(.sf))
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

# ── CONSTANTS (identical to blp_three_models.R) ──────────────────────────────
LAMBDA_BASE   <- 13.93
TIME_CUTOFF   <- as.Date("2022-07-01")
MIN_QUARTERS  <- 3
EPS_TARGET    <- -2.62
K_CLIP        <- 5
RHO_GRID      <- seq(0, 0.90, by = 0.01)
MERGER_START  <- "2020Q1"
NASH_TOL      <- 1e-8
NASH_MAXITER  <- 2000
NASH_DAMP     <- 0.4
PC_COLS       <- paste0("joint_pc", 1:5)
X_VARS        <- c("pkg_size", PC_COLS, "is_other", "import_freight_shock")
freight_crisis_qtrs <- c("2021Q3","2021Q4","2022Q1","2022Q2")

# ── BASE DATA (lambda applied later, per scenario) ───────────────────────────
panel0 <- fread(file.path(data_dir, "asin_quarter_panel.csv"))
panel0[, month_date_dummy := as.Date(paste0(
  sub("Q.*","",quarter),"-",
  sprintf("%02d",(as.integer(sub(".*Q","",quarter))-1)*3+1),"-01"))]
panel0 <- panel0[month_date_dummy <= TIME_CUTOFF][, month_date_dummy := NULL]
asin_chars <- fread(file.path(data_dir, "asin_characteristics.csv"))
panel0 <- merge(panel0, asin_chars[, .(asin, pkg_size, is_import)], by="asin", all.x=TRUE)
panel0[is.na(pkg_size),  pkg_size  := median(panel0$pkg_size,  na.rm=TRUE)]
panel0[is.na(is_import), is_import := 0]
keep <- panel0[, .(n=uniqueN(quarter)), by=asin][n>=MIN_QUARTERS, asin]
panel0 <- panel0[asin %in% keep]
panel0[, is_other             := as.numeric(brand_grouped_50=="Other")]
panel0[, is_freight_crisis    := as.numeric(quarter %in% freight_crisis_qtrs)]
panel0[, import_freight_shock := is_import * is_freight_crisis]

brand_firm <- data.table(brand_grouped_50=sort(unique(panel0$brand_grouped_50)))
brand_firm[, firm_id_pre := seq_len(.N)]
panel0 <- merge(panel0, brand_firm, by="brand_grouped_50", all.x=TRUE)
panel0[brand_grouped_50 %in% c("Sensodyne","SENSODYNE PRONAMEL","Parodontax"),
       firm_id_pre := 9999L]
colgate_fid <- panel0[brand_grouped_50=="Colgate", unique(firm_id_pre)][1]
panel0[brand_grouped_50=="Tom's of Maine", firm_id_pre := colgate_fid]
panel0[, firm_id_post := firm_id_pre]
panel0[brand_grouped_50=="hello" & quarter >= MERGER_START,
       firm_id_post := colgate_fid]

pcs_raw <- fread(file.path(data_dir, "asin_joint_pcs_complete.csv"))
pcs_raw <- pcs_raw[asin %in% panel0$asin]
set.seed(42)
km <- kmeans(as.matrix(pcs_raw[, PC_COLS, with=FALSE]),
             centers=K_CLIP, nstart=50, iter.max=500)
pcs_raw[, clip_nest := km$cluster]
panel0 <- merge(panel0, pcs_raw[, .(asin, clip_nest)], by="asin", all.x=TRUE)
panel0[is.na(clip_nest), clip_nest := K_CLIP + 1L]
brand_nest_dt <- data.table(brand_grouped_50=sort(unique(panel0$brand_grouped_50)))
brand_nest_dt[, brand_nest_id := seq_len(.N)]
panel0 <- merge(panel0, brand_nest_dt, by="brand_grouped_50", all.x=TRUE)
qtrs <- sort(unique(panel0$quarter))
panel0[, market_ids := match(quarter, qtrs)]   # single all-market nest for M1 scenarios

# ── SHARED FUNCTIONS (identical logic to blp_three_models.R) ─────────────────
nl_shares <- function(V, nests, rho) {
  J <- length(V); s <- numeric(J)
  if (rho == 0) { e <- exp(V); s[] <- e/(1+sum(e)) } else {
    e_adj <- exp(V/(1-rho)); un <- sort(unique(nests))
    E_g <- vapply(un, function(g) sum(e_adj[nests==g]), numeric(1))
    names(E_g) <- as.character(un)
    D_g <- E_g^(1-rho); D <- sum(D_g)
    for (j in seq_len(J)) {
      g <- as.character(nests[j])
      s[j] <- (e_adj[j]/E_g[g]) * (D_g[g]/(1+D))
    }
  }
  s
}

build_delta_mat <- function(alpha, rho, s, sjg, nests) {
  J <- length(s)
  Delta <- matrix(0, J, J)
  for (j in seq_len(J)) for (k in seq_len(J)) {
    sn <- (nests[j]==nests[k])
    if (j==k)            Delta[j,j] <- alpha*s[j]*(1/(1-rho) - rho/(1-rho)*sjg[j] - s[j])
    else if (sn && rho>0) Delta[j,k] <- alpha*s[j]*(-rho/(1-rho)*sjg[k] - s[k])
    else                  Delta[j,k] <- -alpha*s[j]*s[k]
  }
  Delta
}

nash_bertrand <- function(delta_pre, p_pre, mc, firms_post, alpha, rho, nests) {
  J <- length(p_pre)
  x_fixed <- delta_pre - alpha*p_pre
  p <- p_pre
  Omega_post <- outer(firms_post, firms_post, "==")*1.0
  for (iter in seq_len(NASH_MAXITER)) {
    V <- x_fixed + alpha*p
    s <- nl_shares(V, nests, rho)
    if (rho > 0) {
      e_adj <- exp(V/(1-rho)); un <- sort(unique(nests))
      E_g <- vapply(un, function(g) sum(e_adj[nests==g]), numeric(1))
      names(E_g) <- as.character(un)
      sjg <- e_adj / E_g[as.character(nests)]
    } else sjg <- rep(1, J)
    Delta <- build_delta_mat(alpha, rho, s, sjg, nests)
    OD <- Omega_post * Delta
    p_new <- tryCatch(mc - solve(OD, s), error=function(e) p)
    p_upd <- NASH_DAMP*p_new + (1-NASH_DAMP)*p
    if (max(abs(p_upd-p)) < NASH_TOL) { p <- p_upd; break }
    p <- p_upd
  }
  p
}

# Full scenario runner: given lambda, nest column, alpha mode, MC window,
# returns summary stats. alpha_mode: "logit" (calibrate at rho=0, baseline)
# or "within" (fixed point so model's own median elasticity = target).
run_scenario <- function(lambda, nest_col, alpha_mode = "logit",
                         mc_window = "avg", label = "", rho_fixed = NA) {
  pan <- copy(panel0)
  mkt <- pan[, .(total_q = sum(q_jt)), by=quarter]
  mkt[, M_t := (1+lambda)*total_q]
  pan <- merge(pan, mkt[,.(quarter, M_t, total_q)], by="quarter", all.x=TRUE)
  pan[, share         := pmax(1e-6, pmin(1-1e-6, q_jt/M_t))]
  pan[, outside_share := 1 - total_q/M_t]
  pan[, log_odds      := log(share) - log(outside_share)]

  pan[, nestv  := get(nest_col)]
  pan[, s_nest := sum(share), by=.(quarter, nestv)]
  pan[, s_jg   := share / s_nest]
  pan[, log_sjg := log(pmax(s_jg, 1e-6))]

  dm_cols <- c("log_odds","price_mean","log_sjg", X_VARS)
  pan[, (paste0(dm_cols,"_dm")) := lapply(.SD, function(x) x - mean(x, na.rm=TRUE)),
      .SDcols=dm_cols, by=quarter]
  x_mat   <- as.matrix(pan[, paste0(X_VARS,"_dm"), with=FALSE])
  XtX_inv <- solve(crossprod(x_mat))

  profile_rho <- function(alpha) {
    base_y <- pan$log_odds_dm - alpha * pan$price_mean_dm
    rss <- vapply(RHO_GRID, function(rho) {
      ydm <- base_y - rho * pan$log_sjg_dm
      bh  <- as.numeric(XtX_inv %*% crossprod(x_mat, ydm))
      sum((ydm - x_mat %*% bh)^2)
    }, numeric(1))
    list(rho = RHO_GRID[which.min(rss)],
         rss_drop = 100*(rss[1]-min(rss))/rss[1])
  }

  # alpha calibration
  alpha <- EPS_TARGET / median(pan$price_mean * (1 - pan$share))
  if (!is.na(rho_fixed)) {
    rho <- rho_fixed
    pr  <- list(rho = rho, rss_drop = 0)
  } else {
    pr  <- profile_rho(alpha)
    rho <- pr$rho
  }
  if (alpha_mode == "within") {
    # two-step: keep the baseline profiled rho*, then rescale alpha once so
    # the model's own implied MEDIAN elasticity at that rho equals the target
    scale_med <- median(pan$price_mean *
      (1/(1-rho) - rho/(1-rho)*pan$s_jg - pan$share))
    alpha <- EPS_TARGET / scale_med
  }

  pan[, delta := log_odds - rho * log_sjg]

  # MC inversion, all quarters
  mc_rows <- rbindlist(lapply(qtrs, function(qtr) {
    dt_q <- pan[quarter==qtr][order(asin)]
    Delta <- build_delta_mat(alpha, rho, dt_q$share, dt_q$s_jg, dt_q$nestv)
    Omega <- outer(dt_q$firm_id_pre, dt_q$firm_id_pre, "==")*1.0
    mc_q  <- tryCatch(as.numeric(dt_q$price_mean + solve(Omega*Delta, dt_q$share)),
                      error=function(e) rep(NA_real_, nrow(dt_q)))
    data.table(asin=dt_q$asin, quarter=qtr, mc=mc_q)
  }))
  pan <- merge(pan, mc_rows, by=c("asin","quarter"), all.x=TRUE)
  neg_mc <- 100*mean(pan$mc < 0, na.rm=TRUE)

  # fixed MCs from chosen pre-merger window
  pre_qtrs <- if (mc_window == "avg") qtrs[qtrs < MERGER_START] else "2019Q4"
  mc_fix <- pan[quarter %in% pre_qtrs, .(mc_fixed=mean(mc, na.rm=TRUE)), by=asin]
  pan <- merge(pan, mc_fix, by="asin", all.x=TRUE)
  pan[is.na(mc_fixed), mc_fixed := mc]

  post_qtrs <- qtrs[qtrs >= MERGER_START]
  eff <- rbindlist(lapply(post_qtrs, function(qtr) {
    dt_q <- pan[quarter==qtr][order(asin)]
    p_no <- nash_bertrand(dt_q$delta, dt_q$price_mean, dt_q$mc_fixed,
                          dt_q$firm_id_pre,  alpha, rho, dt_q$nestv)
    p_mg <- nash_bertrand(dt_q$delta, dt_q$price_mean, dt_q$mc_fixed,
                          dt_q$firm_id_post, alpha, rho, dt_q$nestv)
    data.table(brand=dt_q$brand_grouped_50, eff=(p_mg-p_no)/p_no*100)
  }))
  hello_eff   <- eff[brand=="hello",   mean(eff)]
  colgate_eff <- eff[brand=="Colgate", mean(eff)]

  cat(sprintf("  %-34s lam=%6.2f a=%8.5f rho*=%.2f rssD=%4.1f%% negMC=%4.1f%% hello=+%.3f%% colg=+%.3f%%\n",
      label, lambda, alpha, rho, pr$rss_drop, neg_mc, hello_eff, colgate_eff))
  data.table(scenario=label, lambda=lambda, alpha=round(alpha,5), rho_star=rho,
             rss_drop=round(pr$rss_drop,1), neg_mc=round(neg_mc,1),
             hello_eff=round(hello_eff,3), colgate_eff=round(colgate_eff,3))
}

# ── PART A: SCENARIOS ────────────────────────────────────────────────────────
cat("=== PART A: robustness scenarios ===\n")
scen <- list(
  run_scenario(LAMBDA_BASE,    "market_ids",    "logit",  "avg",  "M1 baseline",         rho_fixed = 0),
  run_scenario(2*LAMBDA_BASE,  "market_ids",    "logit",  "avg",  "M1, market size x2",  rho_fixed = 0),
  run_scenario(5*LAMBDA_BASE,  "market_ids",    "logit",  "avg",  "M1, market size x5",  rho_fixed = 0),
  run_scenario(10*LAMBDA_BASE, "market_ids",    "logit",  "avg",  "M1, market size x10", rho_fixed = 0),
  run_scenario(LAMBDA_BASE,    "clip_nest",     "logit",  "avg",  "M3 baseline"),
  run_scenario(2*LAMBDA_BASE,  "clip_nest",     "logit",  "avg",  "M3, market size x2"),
  run_scenario(5*LAMBDA_BASE,  "clip_nest",     "logit",  "avg",  "M3, market size x5"),
  run_scenario(10*LAMBDA_BASE, "clip_nest",     "logit",  "avg",  "M3, market size x10"),
  run_scenario(LAMBDA_BASE,    "brand_nest_id", "within", "avg",  "M2, alpha recalibrated at rho*"),
  run_scenario(LAMBDA_BASE,    "clip_nest",     "within", "avg",  "M3, alpha recalibrated at rho*"),
  run_scenario(LAMBDA_BASE,    "clip_nest",     "logit",  "last", "M3, MC from 2019Q4 only")
)
rob <- rbindlist(scen)
fwrite(rob, file.path(out_dir, "robustness_checks.csv"))
cat("Saved: robustness_checks.csv\n\n")

# ── PART B: DIVERSION RATIOS (from main-run outputs) ─────────────────────────
cat("=== PART B: diversion ratios ===\n")
models <- list(
  list(name="M1: Logit",        file="model1_asin_results.csv", rho_file="model1_coefficients.csv"),
  list(name="M2: Brand-Nested", file="model2_asin_results.csv", rho_file="model2_coefficients.csv"),
  list(name="M3: CLIP-Nested",  file="model3_asin_results.csv", rho_file="model3_coefficients.csv")
)
ALPHA_MAIN <- fread(file.path(out_dir, "model1_coefficients.csv"))[variable=="prices_alpha", beta]
pre_qtrs   <- qtrs[qtrs < MERGER_START]
# fixed reporting buckets from CLIP clusters (hello's cluster = its modal clip_nest)
clipmap  <- unique(panel0[, .(asin, clip_nest, brand_grouped_50)])
hello_c  <- clipmap[brand_grouped_50=="hello", as.integer(names(sort(table(clip_nest), decreasing=TRUE))[1])]

div_rows <- rbindlist(lapply(models, function(m) {
  res <- fread(file.path(out_dir, m$file))
  rho <- fread(file.path(out_dir, m$rho_file))[variable=="rho", beta]
  qout <- rbindlist(lapply(pre_qtrs, function(qtr) {
    dq <- res[quarter==qtr][order(asin)]
    if (!nrow(dq[brand_grouped_50=="hello"])) return(NULL)
    s     <- dq$share
    nests <- dq$nest_id
    sjg   <- {sn <- ave(s, nests, FUN=sum); s/sn}
    Delta <- build_delta_mat(ALPHA_MAIN, rho, s, sjg, nests)
    dq2   <- merge(dq, clipmap[, .(asin, clip_nest)], by="asin", sort=FALSE)
    hj    <- which(dq$brand_grouped_50=="hello")
    out <- rbindlist(lapply(hj, function(j) {
      dr <- -Delta[, j] / Delta[j, j]     # DR_{j -> k} = -(ds_k/dp_j)/(ds_j/dp_j)
      dr[j] <- NA
      data.table(w = s[j],
        to_colgate = sum(dr[dq$brand_grouped_50=="Colgate"], na.rm=TRUE),
        to_cluster = sum(dr[dq2$clip_nest==hello_c & dq$brand_grouped_50!="hello"], na.rm=TRUE),
        to_inside  = sum(dr, na.rm=TRUE))
    }))
    out
  }))
  qout[, .(model = m$name,
           to_colgate = round(100*weighted.mean(to_colgate, w), 2),
           to_cluster = round(100*weighted.mean(to_cluster, w), 2),
           to_other   = round(100*weighted.mean(to_inside - to_colgate - to_cluster, w), 2),
           to_outside = round(100*weighted.mean(1 - to_inside, w), 2))]
}))
print(div_rows)
fwrite(div_rows, file.path(out_dir, "diversion_ratios.csv"))
cat("Saved: diversion_ratios.csv\n\n")

# ── PART C: WELFARE ──────────────────────────────────────────────────────────
cat("=== PART C: consumer welfare ===\n")
mkt0 <- panel0[, .(total_q = sum(q_jt)), by=quarter]
mkt0[, M_t := (1+LAMBDA_BASE)*total_q]
post_qtrs <- qtrs[qtrs >= MERGER_START]

cs_fun <- function(V, nests, rho) {
  if (rho == 0) return(log(1 + sum(exp(V))))
  un <- sort(unique(nests))
  D_g <- vapply(un, function(g) sum(exp(V[nests==g]/(1-rho)))^(1-rho), numeric(1))
  log(1 + sum(D_g))
}

wf <- rbindlist(lapply(models, function(m) {
  res <- fread(file.path(out_dir, m$file))
  rho <- fread(file.path(out_dir, m$rho_file))[variable=="rho", beta]
  per_q <- rbindlist(lapply(post_qtrs, function(qtr) {
    dq <- res[quarter==qtr][order(asin)]
    s  <- dq$share; nests <- dq$nest_id
    s0 <- 1 - sum(s)
    sjg <- {sn <- ave(s, nests, FUN=sum); s/sn}
    delta <- log(s/s0) - rho*log(pmax(sjg,1e-12))
    V_no <- delta + ALPHA_MAIN*(dq$price_nomerger - dq$price_observed)
    V_mg <- delta + ALPHA_MAIN*(dq$price_merger   - dq$price_observed)
    dCS  <- (cs_fun(V_mg, nests, rho) - cs_fun(V_no, nests, rho)) / abs(ALPHA_MAIN)
    data.table(quarter=qtr, dCS=dCS)
  }))
  per_q <- merge(per_q, mkt0[,.(quarter,M_t)], by="quarter")
  data.table(model = m$name,
             dcs_cents_per_consumer = round(100*mean(per_q$dCS), 4),
             dcs_total_per_quarter  = round(mean(per_q$dCS * per_q$M_t), 0))
}))
print(wf)
fwrite(wf, file.path(out_dir, "welfare_effects.csv"))
cat("Saved: welfare_effects.csv\nDone.\n")
