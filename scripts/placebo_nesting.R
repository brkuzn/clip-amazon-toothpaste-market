# ============================================================================
# placebo_nesting.R
# Placebo nesting tests: is the M3 gain from CLIP's SEMANTIC content, or from
# ANY cross-firm K=5 partition? Reuses the exact run_scenario() merger logic
# from blp_three_models.R / robustness_checks.R (reproduces hello=+0.404%),
# then swaps the nest assignment for placebos:
#   A) random uniform K=5 per ASIN   (cross-firm, no semantics)
#   B) permuted CLIP labels          (exact CLIP nest-size distribution)
# For each of N_REP draws records rho*, RSS drop, neg-MC%, hello & Colgate
# merger effects, and whether hello & Colgate land in the same nest.
#
# OUTPUT (in output/):
#   placebo_replications.csv   (per-draw, both placebos)
#   placebo_summary.csv        (CLIP reference + placebo medians / quantiles)
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

# ── PLACEBO LOOP (uses run_scenario VERBATIM above) ─────────────────────────
N_REP <- 200L

clip_row <- run_scenario(LAMBDA_BASE, "clip_nest", "logit", "avg", "CLIP")
stopifnot(abs(clip_row$hello_eff - 0.404) < 0.05)   # validity gate

ua            <- unique(panel0$asin)
hello_asin1   <- panel0[brand_grouped_50=="hello",   asin[1]]
colgate_asin1 <- panel0[brand_grouped_50=="Colgate", asin[1]]
clip_by_asin  <- panel0[!duplicated(asin), setNames(clip_nest, asin)]

set.seed(2024)
reps <- vector("list", N_REP)
for (i in seq_len(N_REP)) {
  # A: random uniform K=5 per ASIN (cross-firm, no semantics)
  rmapA <- setNames(sample.int(K_CLIP, length(ua), replace=TRUE), ua)
  panel0[, placebo_nest := rmapA[as.character(asin)]]
  rA <- run_scenario(LAMBDA_BASE, "placebo_nest", "logit", "avg", sprintf("A%03d", i))
  # B: permuted CLIP labels across ASINs (exact CLIP nest-size distribution)
  rmapB <- setNames(sample(clip_by_asin), names(clip_by_asin))
  panel0[, placebo_nest := rmapB[as.character(asin)]]
  rB <- run_scenario(LAMBDA_BASE, "placebo_nest", "logit", "avg", sprintf("B%03d", i))
  reps[[i]] <- data.table(
    rep       = i,
    rhoA      = rA$rho_star, dropA = rA$rss_drop, negA = rA$neg_mc,
    helloA    = rA$hello_eff, colgA = rA$colgate_eff,
    sameA     = as.integer(rmapA[[hello_asin1]] == rmapA[[colgate_asin1]]),
    rhoB      = rB$rho_star, dropB = rB$rss_drop, negB = rB$neg_mc,
    helloB    = rB$hello_eff, colgB = rB$colgate_eff)
}
reps <- rbindlist(reps)
fwrite(reps, file.path(out_dir, "placebo_replications.csv"))

qq <- function(x, p) as.numeric(quantile(x, p, na.rm=TRUE))
summ <- data.table(
  metric = c("rho_star","rss_drop","neg_mc","hello_eff","colgate_eff"),
  clip   = c(clip_row$rho_star, clip_row$rss_drop, clip_row$neg_mc,
             clip_row$hello_eff, clip_row$colgate_eff),
  A_med  = c(median(reps$rhoA), median(reps$dropA), median(reps$negA),
             median(reps$helloA), median(reps$colgA)),
  A_p05  = c(qq(reps$rhoA,.05), qq(reps$dropA,.05), qq(reps$negA,.05),
             qq(reps$helloA,.05), qq(reps$colgA,.05)),
  A_p95  = c(qq(reps$rhoA,.95), qq(reps$dropA,.95), qq(reps$negA,.95),
             qq(reps$helloA,.95), qq(reps$colgA,.95)),
  B_med  = c(median(reps$rhoB), median(reps$dropB), median(reps$negB),
             median(reps$helloB), median(reps$colgB)))
fwrite(summ, file.path(out_dir, "placebo_summary.csv"))

cat("\n=== PLACEBO NESTING TESTS (", N_REP, " draws each) ===\n", sep="")
print(summ)
cat(sprintf("\nP(random neg-MC <= CLIP %.2f%%) = %.1f%%   [neg-MC gain is generic]\n",
    clip_row$neg_mc, 100*mean(reps$negA <= clip_row$neg_mc)))
cat(sprintf("P(random RSS drop >= CLIP %.1f%%) = %.1f%%  [CLIP fits better]\n",
    clip_row$rss_drop, 100*mean(reps$dropA >= clip_row$rss_drop)))
cat(sprintf("P(random hello effect <= CLIP %.2f%%) = %.1f%%  [merger prediction unique to CLIP]\n",
    clip_row$hello_eff, 100*mean(reps$helloA <= clip_row$hello_eff)))
cat(sprintf("hello & Colgate same random nest: %.1f%%\n", 100*mean(reps$sameA)))

# ── HAND-CODED KEYWORD BASELINE ──────────────────────────────────────────────
# A human analyst's rule-based categories from the raw listing text (title +
# product_benefit) — no CLIP output used. Two priority orders, since most
# listings carry several benefit claims and the analyst must break ties:
#   benefit-first : kids > sensitivity > whitening > natural > standard
#   natural-first : natural > kids > sensitivity > whitening > standard
# (natural-first is the merger-aware order: it deliberately pulls every
# natural-positioned product, including hello, into one nest)
titles <- fread(file.path(repo_dir, "scripts/CLIP-Embeddings/asin_titles.csv"))
titles[, text := tolower(paste(title, product_benefit))]
KW_KID <- "kid|child|toddler|baby|junior"
KW_SEN <- "sensitiv|enamel|pronamel"
KW_WHI <- "whiten|charcoal|stain|bright"
KW_NAT <- "natural|herbal|organic|vegan|fluoride.free|sls.free|botanic|aloe|coconut"
titles[, hand_ben := fifelse(grepl(KW_KID, text), 1L, fifelse(grepl(KW_SEN, text), 2L,
                     fifelse(grepl(KW_WHI, text), 3L, fifelse(grepl(KW_NAT, text), 4L, 5L))))]
titles[, hand_nat := fifelse(grepl(KW_NAT, text), 1L, fifelse(grepl(KW_KID, text), 2L,
                     fifelse(grepl(KW_SEN, text), 3L, fifelse(grepl(KW_WHI, text), 4L, 5L))))]
for (v in c("hand_ben", "hand_nat")) {
  hm <- setNames(titles[[v]], titles$asin)
  panel0[, (v) := hm[as.character(asin)]]
  panel0[is.na(get(v)), (v) := 5L]
}
r_ben <- run_scenario(LAMBDA_BASE, "hand_ben", "logit", "avg", "HAND benefit-first")
r_nat <- run_scenario(LAMBDA_BASE, "hand_nat", "logit", "avg", "HAND natural-first")
hand <- rbind(cbind(variant = "benefit_first", r_ben[, .(rho_star, rss_drop, neg_mc, hello_eff, colgate_eff)]),
              cbind(variant = "natural_first", r_nat[, .(rho_star, rss_drop, neg_mc, hello_eff, colgate_eff)]))
fwrite(hand, file.path(out_dir, "handcoded_baseline.csv"))
cat("\n=== HAND-CODED KEYWORD BASELINE ===\n"); print(hand)
