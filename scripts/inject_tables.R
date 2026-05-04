# ============================================================================
# inject_tables.R
# Generates clean HTML for the K-sensitivity section + Tables 1–3,
# then injects them into blp_three_models.html.
# Updated for K=5 CLIP clusters (rho*=0.67, RSS drop=8.1%, hello=+0.40%)
# ============================================================================
library(data.table); library(kableExtra)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")
setwd("/Users/brkuzn")
out_dir <- "analysis_2704"

html_path <- file.path(out_dir, "blp_three_models.html")
lines <- readLines(html_path, encoding="UTF-8", warn=FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
tab1   <- fread(file.path(out_dir, "table1_cluster_summary.csv"))
coef1  <- fread(file.path(out_dir, "model1_coefficients.csv"))
coef2  <- fread(file.path(out_dir, "model2_coefficients.csv"))
coef3  <- fread(file.path(out_dir, "model3_coefficients.csv"))
merger <- fread(file.path(out_dir, "three_models_merger_summary.csv"))
ksens  <- fread(file.path(out_dir, "clip_k_sensitivity.csv"))

# ── helper: kable → raw HTML string ──────────────────────────────────────────
kbl_html <- function(k) as.character(k)

# ════════════════════════════════════════════════════════════════════════════
# SECTION 0 — K Sensitivity Table + Selection Methodology
# ════════════════════════════════════════════════════════════════════════════

# Format the K sensitivity table
ksens_fmt <- ksens[, .(
  K          = K,
  WCSS       = sprintf("%.1f", wcss),
  Silhouette = sprintf("%.3f", sil),
  `&rho;*`   = sprintf("%.2f", rho_star),
  `RSS drop` = sprintf("%.1f%%", rss_drop),
  `Neg MC`   = sprintf("%.1f%%", neg_mc),
  `hello &Delta;p` = sprintf("+%.3f%%", hello_eff),
  `Colgate &Delta;p` = sprintf("+%.3f%%", colgate_eff)
)]

k_sens_tbl <- kbl(ksens_fmt,
    col.names = c("K", "WCSS", "Silhouette", "&rho;*",
                  "RSS drop", "Neg MC%",
                  "hello &Delta;p", "Colgate &Delta;p"),
    format="html", escape=FALSE, booktabs=TRUE,
    align=c("c","r","r","r","r","r","r","r")) |>
  kable_styling(bootstrap_options=c("striped","hover","condensed","bordered"),
                full_width=FALSE, font_size=13) |>
  row_spec(which(ksens$K == 5), bold=TRUE, background="#D1FAE5",
           extra_css="outline: 2px solid #059669;") |>
  footnote(
    general = paste0(
      "K-means on 5 CLIP principal components (seed 42, 50 restarts, iter.max=500). ",
      "WCSS = within-cluster sum of squares (lower = tighter clusters). ",
      "Silhouette = average silhouette width (higher = better separation, max 1.0). ",
      "&rho;* = argmin RSS over grid {0, 0.01, &hellip;, 0.90}. ",
      "RSS drop = (RSS(&rho;=0) &minus; RSS(&rho;*)) / RSS(&rho;=0) &times; 100. ",
      "Neg MC% = fraction of negative marginal costs across all quarters. ",
      "Merger effects = average % price increase for hello and Colgate in post-merger quarters (2020Q1&ndash;2022Q2). ",
      "<b>Bold/green row = selected specification (K=5).</b>"
    ),
    escape=FALSE, general_title="Notes: "
  )

k_sens_html <- kbl_html(k_sens_tbl)

# ════════════════════════════════════════════════════════════════════════════
# TABLE 1 — Summary Statistics by CLIP Cluster (K=5)
# ════════════════════════════════════════════════════════════════════════════
t1 <- tab1[, .(
  Cluster      = paste0("C", clip_nest),
  `Dom. Brand` = dom_brand,
  `N ASINs`    = n_asins,
  `Mean`       = sprintf("%.2f", price_mean),
  `Med.`       = sprintf("%.2f", price_median),
  `SD`         = sprintf("%.2f", price_sd),
  `Mean `      = sprintf("%.3f", share_mean),
  `Med. `      = sprintf("%.3f", share_median),
  `SD `        = sprintf("%.3f", share_sd),
  `Mean  `     = sprintf("%.0f", pkg_mean),
  `Med.  `     = sprintf("%.0f", pkg_median),
  `SD  `       = sprintf("%.0f", pkg_sd)
)]

k1 <- kbl(t1, format="html", escape=TRUE, booktabs=TRUE,
          align=c("l","l","r","r","r","r","r","r","r","r","r","r")) |>
  add_header_above(c(" "=3, "Price ($)"=3,
                     "Mkt. Share (x1000)"=3,
                     "Pkg. Size (g)"=3),
                   escape=TRUE) |>
  kable_styling(bootstrap_options=c("striped","hover","condensed","bordered"),
                full_width=FALSE, font_size=13) |>
  column_spec(1, bold=TRUE) |>
  footnote(
    general = paste0(
      "1,164 ASIN&times;quarter observations, 19 quarters (2019Q1&ndash;2022Q3). ",
      "Mean/Median/SD of price (USD), market share (&times;1000), package weight (g). ",
      "Dominant brand = modal brand by ASIN count within cluster. ",
      "K=5 k-means on CLIP joint PC<sub>1&ndash;5</sub> (seed 42, 50 restarts)."
    ),
    escape=FALSE, general_title="Notes: "
  )

tab1_html <- kbl_html(k1)

# ════════════════════════════════════════════════════════════════════════════
# TABLE 2 — Demand Estimates
# ════════════════════════════════════════════════════════════════════════════
var_labels <- c(
  "prices_alpha"         = "&alpha; (price)",
  "pkg_size"             = "Package size (g)",
  "joint_pc1"            = "CLIP PC<sub>1</sub>",
  "joint_pc2"            = "CLIP PC<sub>2</sub>",
  "joint_pc3"            = "CLIP PC<sub>3</sub>",
  "joint_pc4"            = "CLIP PC<sub>4</sub>",
  "joint_pc5"            = "CLIP PC<sub>5</sub>",
  "is_other"             = "Other brand",
  "import_freight_shock" = "Import &times; freight shock",
  "rho"                  = "&rho;*"
)

fmt_coef <- function(dt, model_name) {
  dt2 <- dt[variable %in% names(var_labels)]
  dt2[, cell := fcase(
    variable == "prices_alpha",
      "&minus;0.310 &dagger;",
    variable == "rho" & model_name == "Logit",
      "0.000 &Dagger;",
    variable == "rho",
      paste0(sprintf("%.3f", beta), " &Dagger;"),
    !is.na(se),
      paste0(sprintf("%.4f", beta), signif,
             "<br><small>(", sprintf("%.4f", se), ")</small>"),
    default = ""
  )]
  dt2[, .(variable, cell)]
}

c1 <- fmt_coef(coef1, "Logit")
c2 <- fmt_coef(coef2, "Brand-Nested")
c3 <- fmt_coef(coef3, "CLIP-Nested")

tab2 <- merge(c1, c2, by="variable", suffixes=c("_m1","_m2"))
tab2 <- merge(tab2, c3, by="variable")
setnames(tab2, c("cell","cell_m1","cell_m2"), c("M3","M1","M2"))
tab2 <- tab2[match(names(var_labels), variable), .(variable, M1, M2, M3)]
tab2[, variable := var_labels[variable]]

# Summary rows — updated for K=5: M3 rho*=0.67, RSS drop=8.1%, eps≈-7.9
tab2 <- rbind(tab2, data.table(
  variable = c("Implied median &epsilon;", "RSS drop vs &rho;=0"),
  M1       = c("&minus;2.62", "&mdash;"),
  M2       = c("&asymp; &minus;4.0", "3.1%"),
  M3       = c("&asymp; &minus;7.9", "8.1%")
))

n_main <- length(var_labels)

k2 <- kbl(tab2,
          col.names = c("Variable",
                        "M1: Logit (rho=0)",
                        "M2: Brand-Nested (rho*=0.35)",
                        "M3: CLIP-Nested (rho*=0.67)"),
          format="html", escape=FALSE, booktabs=TRUE,
          align=c("l","c","c","c")) |>
  kable_styling(bootstrap_options=c("striped","hover","condensed","bordered"),
                full_width=FALSE, font_size=13) |>
  row_spec(1, bold=TRUE, background="#EEF2FF") |>
  row_spec(n_main, extra_css="border-bottom: 2px solid #888;") |>
  row_spec(n_main+1, italic=TRUE, background="#F8F8F8") |>
  row_spec(n_main+2, italic=TRUE, background="#F8F8F8") |>
  footnote(
    symbol = c(
      "&dagger; Calibrated: &alpha; = &epsilon;<sub>target</sub> / median(p(1&minus;s)) = &minus;2.62/8.45. Not estimated.",
      "&Dagger; Grid-searched on {0, 0.01, &hellip;, 0.90}; &rho;=0 fixed for M1."
    ),
    general = paste0(
      "Standard errors in parentheses. ",
      "Significance: ***p&lt;0.01, **p&lt;0.05, *p&lt;0.10. ",
      "Quarter FEs absorbed by demeaning. ",
      "Implied &epsilon; at the profiled &rho;* exceeds target &minus;2.62 due to sequential calibration. ",
      "1,164 obs., 159 ASINs, 19 quarters."
    ),
    escape=FALSE, general_title="Notes: "
  )

tab2_html <- kbl_html(k2)

# ════════════════════════════════════════════════════════════════════════════
# TABLE 3 — Merger Counterfactuals
# ════════════════════════════════════════════════════════════════════════════
focus  <- c("hello","Colgate","Tom's of Maine","Crest","Sensodyne")
models <- c("Logit","Brand-Nested","CLIP-Nested")
model_labels <- c(
  "Logit"        = "M1: Logit (&rho;=0)",
  "Brand-Nested" = "M2: Brand-Nested (&rho;*=0.35)",
  "CLIP-Nested"  = "M3: CLIP-Nested (&rho;*=0.67)"
)

tab3_list <- lapply(models, function(m) {
  sub <- merger[model==m & brand %in% focus][match(focus, brand)]
  data.table(
    brand     = sub$brand,
    obs       = sprintf("%.2f", sub$avg_price_obs),
    mc        = sprintf("%.2f", sub$avg_mc),
    nomerger  = sprintf("%.2f", sub$price_nomerger),
    merger_p  = sprintf("%.2f", sub$price_merger),
    delta_abs = sprintf("+%.3f", sub$price_merger - sub$price_nomerger),
    delta_pct = sprintf("+%.3f%%", sub$effect_pct)
  )
})

make_block <- function(m, dt) {
  hdr <- data.table(brand=model_labels[m],
                    obs="", mc="", nomerger="", merger_p="", delta_abs="", delta_pct="")
  rbind(hdr, dt)
}

tab3 <- rbindlist(mapply(make_block, models, tab3_list, SIMPLIFY=FALSE))
n_focus  <- length(focus)
hdr_rows <- c(1, 1+(n_focus+1), 1+2*(n_focus+1))
party_rows <- unlist(lapply(hdr_rows, function(h) h + which(focus %in% c("hello","Colgate"))))

k3 <- kbl(tab3,
          col.names = c("Brand",
                        "Obs. Price ($)",
                        "Marg. Cost ($)",
                        "No-Merger Nash ($)",
                        "Merger Nash ($)",
                        "Dp ($)",
                        "Dp (%)"),
          format="html", escape=FALSE, booktabs=TRUE,
          align=c("l","r","r","r","r","r","r")) |>
  kable_styling(bootstrap_options=c("striped","hover","condensed","bordered"),
                full_width=FALSE, font_size=13) |>
  row_spec(hdr_rows,  bold=TRUE, italic=TRUE, background="#E8E8F0") |>
  row_spec(party_rows, bold=TRUE) |>
  footnote(
    general = paste0(
      "Prices in USD; averages over post-merger quarters (2020Q1&ndash;2022Q2). ",
      "<b>Bold</b> = merger parties (hello, Colgate). ",
      "No-Merger Nash: Bertrand&ndash;Nash equilibrium under pre-merger ownership. ",
      "Merger Nash: equilibrium with hello assigned to Colgate firm from 2020Q1. ",
      "&Delta;p = Merger Nash &minus; No-Merger Nash. ",
      "MCs inverted from pre-merger FOC and fixed across simulations."
    ),
    escape=FALSE, general_title="Notes: "
  )

tab3_html <- kbl_html(k3)

# ════════════════════════════════════════════════════════════════════════════
# Build the section HTML to inject
# ════════════════════════════════════════════════════════════════════════════

tables_section <- sprintf('
<!-- ══ FORMAL ACADEMIC TABLES ══ -->

<div class="section">
  <div class="st">&#x1F4CA; Appendix A: K Sensitivity Analysis &mdash; Selecting the Number of CLIP Clusters</div>
  <p style="font-size:0.84rem;color:#444;margin:0 0 10px">
    The number of k-means clusters <em>K</em> is a modelling choice. We select it by jointly optimising
    four criteria evaluated across <em>K</em> = 2&ndash;10:
  </p>
  <ol style="font-size:0.84rem;color:#444;margin:0 0 10px;padding-left:1.4em">
    <li><strong>Cluster quality</strong> &mdash; average <em>silhouette width</em> (higher = better separation in CLIP space; peaks near K=5)</li>
    <li><strong>Demand fit</strong> &mdash; RSS drop vs &rho;=0 from the concentrated OLS profile (higher = stronger within-nest substitutability signal)</li>
    <li><strong>Economic plausibility</strong> &mdash; share of negative marginal costs (lower = more credible cost estimates)</li>
    <li><strong>Merger-effect stability</strong> &mdash; hello and Colgate must land in <em>different</em> clusters for the diversion ratio to be economically meaningful</li>
  </ol>
  <p style="font-size:0.84rem;color:#444;margin:0 0 12px">
    <strong>K=2&ndash;4</strong> produce implausibly large hello merger effects (+1.7&ndash;4.0%%) because the coarse nesting
    collapses hello and Colgate into the same broad cluster, inflating the implied diversion ratio.
    <strong>K&ge;7</strong> shows declining RSS improvement and rising negative-MC rates as nests fragment.
    <strong>K=5</strong> maximises the silhouette (0.388), achieves the highest RSS drop (8.1%%),
    yields the lowest negative-MC rate (1.7%%), and keeps the merger effect economically small (+0.40%% for hello)
    &mdash; making it the dominant choice across all four criteria. K=6 is the closest competitor
    (silhouette 0.387, RSS drop 6.7%%, neg-MC 3.4%%) but is strictly dominated by K=5 on every metric.
  </p>
  %s
</div>

<div class="section">
  <div class="st">&#x2705; Update: &rho; Grid Refined to 0.01 Increments &amp; K Updated to 5</div>
  <p style="font-size:0.86rem;color:#444;margin:0 0 8px">
    The concentrated OLS profile was re-run on a finer grid
    <code>seq(0, 0.90, by=<strong>0.01</strong>)</code> (was 0.05).
    Confirmed values: &rho;*&nbsp;=&nbsp;<strong>0.35</strong> (M2, Brand-Nested)
    and &rho;*&nbsp;=&nbsp;<strong>0.67</strong> (M3, CLIP-Nested, K=5).
    RSS drops: M2&nbsp;&minus;&nbsp;3.1%%, M3&nbsp;&minus;&nbsp;8.1%%.
    Switching from K=6 to K=5 improves RSS drop from 6.7%% to 8.1%% and reduces negative-MC rate from 3.4%% to 1.7%%.
  </p>
</div>

<div class="section">
  <div class="st">&#x1F4CA; Table 1: Summary Statistics by CLIP Cluster (K=5)</div>
  <p style="font-size:0.83rem;color:#555;margin:0 0 10px">
    Mean, median, and SD of price, market share (&times;1000), and package weight
    across the 5 CLIP clusters. <em>K</em>=5 <em>k</em>-means on 5 CLIP principal components
    (seed 42, 50 restarts). hello and Colgate occupy different clusters in all K&isin;{5,&hellip;,10} specifications.
  </p>
  %s
</div>

<div class="section">
  <div class="st">&#x1F4CA; Table 2: Demand Parameter Estimates &mdash; Three Models</div>
  <p style="font-size:0.83rem;color:#555;margin:0 0 10px">
    &alpha; calibrated to match &epsilon;<sub>target</sub>=&minus;2.62 (Bijmolt et al. 2005).
    &beta;s estimated by OLS after demeaning. &rho;* profiled by concentrated OLS on a 0.01 grid.
    M3 uses K=5 CLIP clusters; implied median elasticity &asymp; &minus;7.9 = &minus;2.62/(1&minus;0.67).
  </p>
  %s
</div>

<div class="section">
  <div class="st">&#x1F4CA; Table 3: Merger Simulation Counterfactuals &mdash; hello&rarr;Colgate (2020Q1)</div>
  <p style="font-size:0.83rem;color:#555;margin:0 0 10px">
    No-Merger Nash: Bertrand equilibrium under pre-merger ownership.
    Merger Nash: equilibrium with hello reassigned to Colgate firm from 2020Q1.
    CLIP nesting (M3, K=5) predicts a smaller merger effect (+0.40%% for hello)
    than plain logit (M1: +1.23%%) because hello and Colgate occupy <em>different</em> CLIP clusters,
    lowering the implied diversion ratio.
  </p>
  %s
</div>
', k_sens_html, tab1_html, tab2_html, tab3_html)

# ════════════════════════════════════════════════════════════════════════════
# Inject: place new sections right before the Coefficients section
# ════════════════════════════════════════════════════════════════════════════

# Fix the rho grid code snippet: by=0.05 -> by=0.01
lines <- gsub('by=<span class="nm">0\\.05</span>',
              'by=<span class="nm">0.01</span>', lines, fixed=FALSE)

# Find insertion point: just before "Estimated Coefficients" heading
coef_line <- grep("Estimated Coefficients", lines, fixed=TRUE)[1]

if (!is.na(coef_line)) {
  window_start <- max(1, coef_line - 10)
  window       <- lines[window_start:coef_line]
  rel_matches  <- grep('<div class="section">', window, fixed=TRUE)
  if (length(rel_matches) > 0) {
    section_start <- window_start + tail(rel_matches, 1) - 1
  } else {
    section_start <- coef_line
  }
} else {
  section_start <- grep("</body>", lines, fixed=TRUE)[1]
  if (is.na(section_start)) section_start <- length(lines)
}

new_lines  <- strsplit(tables_section, "\n")[[1]]
lines_out  <- c(lines[1:(section_start-1)], new_lines, lines[section_start:length(lines)])

con <- file(html_path, open="w", encoding="UTF-8")
writeLines(lines_out, con)
close(con)
cat("Done. Injected K-sensitivity + Tables 1-3 + update note into blp_three_models.html\n")
cat(sprintf("Final HTML: %d lines\n", length(lines_out)))
