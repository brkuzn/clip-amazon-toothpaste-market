# ============================================================
# ASIN-Level Joint PCA Builder
# Mirrors brand_qtr_joint_pca.R but skips weighted aggregation:
# runs joint PCA directly on static ASIN-level embeddings.
#
# Rationale: brand×quarter aggregation destroyed within-brand
# cross-sectional variation in CLIP embeddings (professor feedback).
# ASIN-level PCs restore that micro variation. Embeddings are
# treated as static (one vector per ASIN) — Amazon listings do
# not meaningfully change over the 2018–2022 window.
#
# Inputs:
#   asin_features_for_blp_clip.csv   (ASIN-level img+text PCs)
#   choice_set_with_state_and_region.csv  (purchase observations)
#
# Outputs:
#   asin_joint_pcs.csv               (5 joint PCs per ASIN, static)
#   asin_quarter_panel.csv           (ASIN×quarter panel with joint PCs merged)
#   asin_joint_pc_loadings.csv       (PCA rotation + modality shares)
#   asin_joint_pca_scaling_params.csv (scaling params for reproducibility)
# ============================================================

library(data.table)
library(lubridate)

# -------------------------------------------------------
# CONFIG
# -------------------------------------------------------
K            <- 5
ASIN_PC_FILE <- "asin_features_for_blp_clip.csv"
CHOICE_FILE  <- "choice_set_with_state_and_region.csv"

# NOTE: columns are interleaved in asin_features_for_blp_clip.csv:
#   asin_img_pc1, asin_text_pc1, asin_img_pc2, asin_text_pc2, ...
img_cols  <- paste0("asin_img_pc",  1:5)
text_cols <- paste0("asin_text_pc", 1:5)
pc_cols   <- c(img_cols, text_cols)   # 10 columns — same as brand-level script

# ============================================================
# PART A: Load and filter ASIN-level PC data
# (no weighted aggregation — use ASIN embeddings directly)
# ============================================================
cat("=== PART A: Loading ASIN-level embeddings ===\n")

asin_pcs <- fread(ASIN_PC_FILE)

cat("Raw ASIN rows:", nrow(asin_pcs), "\n")
cat("Brands:", paste(sort(unique(asin_pcs$brand_grouped_50)), collapse = ", "), "\n")

# Verify required columns
stopifnot(all(pc_cols %in% names(asin_pcs)))
stopifnot("has_img"  %in% names(asin_pcs))
stopifnot("has_text" %in% names(asin_pcs))

# Report embedding coverage
cat("\nEmbedding coverage:\n")
cat("  has_img  == 1:", sum(asin_pcs$has_img  == 1), "\n")
cat("  has_text == 1:", sum(asin_pcs$has_text == 1), "\n")
cat("  both     == 1:", sum(asin_pcs$has_img  == 1 & asin_pcs$has_text == 1), "\n")

# Keep only ASINs with both modalities present
asin_pcs_clean <- asin_pcs[has_img == 1 & has_text == 1]
n_dropped <- nrow(asin_pcs) - nrow(asin_pcs_clean)
if (n_dropped > 0) {
  cat("Dropped", n_dropped, "ASINs missing image or text embedding\n")
  print(asin_pcs[has_img != 1 | has_text != 1, .(asin, brand_grouped_50, has_img, has_text)])
}
cat("ASINs entering PCA:", nrow(asin_pcs_clean), "\n\n")

# ============================================================
# PART B: Joint PCA on ASIN-level img + text PCs
# (identical logic to brand_qtr_joint_pca.R Part B)
# ============================================================
cat("=== PART B: Joint PCA ===\n")

X_raw <- as.matrix(asin_pcs_clean[, pc_cols, with = FALSE])

# Guard against any residual NAs
complete_rows <- complete.cases(X_raw)
n_na <- sum(!complete_rows)
if (n_na > 0) {
  cat("WARNING: Dropping", n_na, "ASINs with NA PCs before PCA\n")
  print(asin_pcs_clean[!complete_rows, .(asin, brand_grouped_50)])
}
X_clean    <- X_raw[complete_rows, ]
meta_clean <- asin_pcs_clean[complete_rows]

# Standardize (z-score) — save params for reproducibility
col_means <- colMeans(X_clean)
col_sds   <- apply(X_clean, 2, sd)
zero_sd   <- col_sds == 0
if (any(zero_sd)) {
  cat("WARNING: Zero-variance columns (will not scale):",
      paste(pc_cols[zero_sd], collapse = ", "), "\n")
  col_sds[zero_sd] <- 1
}
X_scaled <- scale(X_clean, center = col_means, scale = col_sds)

# Fit PCA
pca_fit <- prcomp(X_scaled, center = FALSE, scale. = FALSE)

# Variance explained
var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
cat("\nVariance explained by first", K, "joint PCs:\n")
for (k in 1:K) {
  cat(sprintf("  joint_pc%d: %.1f%%  (cumulative: %.1f%%)\n",
              k,
              100 * var_explained[k],
              100 * sum(var_explained[1:k])))
}

# Extract joint PCs
joint_pc_mat <- as.data.table(pca_fit$x[, 1:K])
setnames(joint_pc_mat, paste0("joint_pc", 1:K))

asin_joint_pcs <- cbind(
  meta_clean[, .(asin, brand_grouped_50, txn_count, has_img, has_text)],
  joint_pc_mat
)

fwrite(asin_joint_pcs, "asin_joint_pcs.csv")
cat("\nSaved: asin_joint_pcs.csv  (", nrow(asin_joint_pcs), "ASINs ×",
    K, "joint PCs)\n")

# -------------------------------------------------------
# PCA Loadings + Modality Shares
# -------------------------------------------------------
loadings_mat <- pca_fit$rotation[, 1:K]
loadings_dt  <- as.data.table(loadings_mat, keep.rownames = "variable")
setnames(loadings_dt, c("variable", paste0("PC", 1:K)))

cat("\nModality share per joint PC (image vs text):\n")
for (k in 1:K) {
  pc_name    <- paste0("PC", k)
  l2         <- loadings_dt[[pc_name]]^2
  img_idx    <- loadings_dt$variable %in% img_cols
  text_idx   <- loadings_dt$variable %in% text_cols
  img_share  <- 100 * sum(l2[img_idx])  / sum(l2)
  text_share <- 100 * sum(l2[text_idx]) / sum(l2)
  cat(sprintf("  joint_pc%d — image: %.1f%%  text: %.1f%%\n",
              k, img_share, text_share))
  loadings_dt[, paste0("img_share_PC",  k) := img_share]
  loadings_dt[, paste0("text_share_PC", k) := text_share]
}

fwrite(loadings_dt, "asin_joint_pc_loadings.csv")
cat("Saved: asin_joint_pc_loadings.csv\n")

# Save scaling params
scaling_params <- data.table(variable = pc_cols, mean = col_means, sd = col_sds)
fwrite(scaling_params, "asin_joint_pca_scaling_params.csv")
cat("Saved: asin_joint_pca_scaling_params.csv\n")

# -------------------------------------------------------
# Choice-data availability check
# choice_set_with_state_and_region.csv (148 MB raw purchase data) is NOT
# redistributed. It is only needed to rebuild asin_quarter_panel.csv
# (Parts C-D). The joint PCs themselves (Parts B and E) need only
# asin_features_for_blp_clip.csv, so asin_joint_pcs_complete.csv is fully
# reproducible without it: the text-only ASIN set is identified directly
# from the CLIP feature file (has_img == 0), which is equivalent to the
# panel-based diagnosis in Part D (all 12 image-missing ASINs are in the
# estimation panel).
# -------------------------------------------------------
RUN_PANEL <- file.exists(CHOICE_FILE)
if (!RUN_PANEL) {
  cat("\nNOTE:", CHOICE_FILE, "not found — skipping Parts C-D (panel rebuild).\n")
  cat("Joint-PC reproduction continues from the CLIP feature file alone.\n")
  clip_all      <- fread(ASIN_PC_FILE)
  missing_asins <- clip_all[has_img == 0 | has_text == 0, .(asin, brand_grouped_50)]
}

# ============================================================
# PART C: Build ASIN×quarter panel and merge joint PCs
# ============================================================
cat("\n=== PART C: Building ASIN×quarter panel ===\n")

if (RUN_PANEL) {

choices <- fread(CHOICE_FILE)
choices[, month_date := as.Date(month_date)]
choices[, quarter := paste0(year(month_date), "Q", quarter(month_date))]

cat("Quarters found:", paste(sort(unique(choices$quarter)), collapse = ", "), "\n")

# Restrict to purchase observations
purchases <- choices[choice == 1]
cat("Purchase rows:", nrow(purchases), "\n")

# Aggregate to ASIN×quarter
asin_qtr <- purchases[, .(
  q_jt       = .N,                                      # purchase count (proxy for share numerator)
  price_mean = mean(price_deflated,  na.rm = TRUE),     # mean deflated price
  price_sd   = sd(price_deflated,    na.rm = TRUE),     # within-cell price SD
  n_hh       = uniqueN(hhid)                             # unique households
), by = .(asin, brand_grouped_50, quarter)]

setorder(asin_qtr, asin, quarter)
cat("ASIN×quarter rows (before PC merge):", nrow(asin_qtr), "\n")
cat("Unique ASINs in panel:              ", uniqueN(asin_qtr$asin), "\n")

# Coverage distribution (how many quarters each ASIN appears in)
asin_coverage <- asin_qtr[, .(n_quarters = uniqueN(quarter)), by = asin]
cat("\nASIN coverage distribution (quarters with purchases):\n")
print(asin_coverage[, .N, by = .(
  coverage = cut(n_quarters, breaks = c(0, 1, 3, 6, 9, 12, 18), include.lowest = TRUE)
)][order(coverage)])

# Merge joint PCs onto ASIN×quarter panel
asin_qtr_pcs <- merge(
  asin_qtr,
  asin_joint_pcs[, c("asin", paste0("joint_pc", 1:K)), with = FALSE],
  by    = "asin",
  all.x = TRUE
)

n_missing_pc <- asin_qtr_pcs[is.na(joint_pc1), .N]
cat("\nASIN×quarter rows missing joint PCs:", n_missing_pc,
    sprintf("(%.1f%% — ASINs with missing img/text embedding)\n",
            100 * n_missing_pc / nrow(asin_qtr_pcs)))

if (n_missing_pc > 0) {
  cat("Brands with missing PCs:\n")
  print(asin_qtr_pcs[is.na(joint_pc1), .(n = .N), by = brand_grouped_50][order(-n)])
}

setorder(asin_qtr_pcs, brand_grouped_50, asin, quarter)
fwrite(asin_qtr_pcs, "asin_quarter_panel.csv")
cat("\nSaved: asin_quarter_panel.csv  (", nrow(asin_qtr_pcs), "rows)\n")

}  # end if (RUN_PANEL) — Part C

# -------------------------------------------------------
# Summary: within-brand PC variation (key identification source)
# -------------------------------------------------------
cat("\n--- Within-brand cross-ASIN variation in joint_pc1 ---\n")
variation_check <- asin_joint_pcs[, .(
  n_asin  = .N,
  pc1_sd  = sd(joint_pc1),
  pc1_min = min(joint_pc1),
  pc1_max = max(joint_pc1)
), by = brand_grouped_50][order(-pc1_sd)]
print(variation_check)

# ============================================================
# PART D: Diagnose missing-PC ASINs
# ============================================================
cat("\n=== PART D: Missing PC diagnostics ===\n")

if (RUN_PANEL) {

# Unique ASINs in the panel that have no joint PCs
missing_asins <- unique(asin_qtr_pcs[is.na(joint_pc1), .(asin, brand_grouped_50)])
cat("Unique ASINs with missing PCs:", nrow(missing_asins), "\n\n")

# Cross-reference against asin_features_for_blp_clip.csv to find root cause
#   Case A — ASIN is in the file but was dropped (has_img=0 or has_text=0)
#   Case B — ASIN is completely absent from the file (never extracted)
clip_all <- fread(ASIN_PC_FILE)   # reload full file (before has_img/has_text filter)

missing_detail <- merge(
  missing_asins,
  clip_all[, .(asin, has_img, has_text, txn_count)],
  by    = "asin",
  all.x = TRUE
)

missing_detail[, root_cause := fcase(
  is.na(has_img),          "B: absent from CLIP file",
  has_img == 0 & has_text == 0, "A: no image AND no text",
  has_img == 0,             "A: no image embedding",
  has_text == 0,            "A: no text embedding",
  default                 = "unknown"
)]

cat("Root cause breakdown:\n")
print(missing_detail[, .N, by = root_cause][order(-N)])

cat("\nDetail by brand:\n")
print(missing_detail[, .(
  n_asin     = .N,
  absent     = sum(root_cause == "B: absent from CLIP file"),
  no_img     = sum(root_cause == "A: no image embedding"),
  no_text    = sum(root_cause == "A: no text embedding"),
  no_both    = sum(root_cause == "A: no image AND no text")
), by = brand_grouped_50][order(-n_asin)])

cat("\nFull missing-ASIN list:\n")
print(missing_detail[order(brand_grouped_50, asin)])

# Quarter coverage of missing ASINs (are they thin or recurring?)
missing_coverage <- asin_qtr_pcs[is.na(joint_pc1),
                                  .(n_quarters = uniqueN(quarter), q_jt_total = sum(q_jt)),
                                  by = .(asin, brand_grouped_50)][order(-n_quarters)]
cat("\nQuarter coverage of missing ASINs:\n")
print(missing_coverage)

cat("\nMissing ASIN coverage distribution:\n")
print(missing_coverage[, .N, by = .(
  coverage = cut(n_quarters, breaks = c(0, 1, 3, 6, 9, 12, 18, 21), include.lowest = TRUE)
)][order(coverage)])

cat("\n    (Part D: diagnostic only, no new files written)\n")

}  # end if (RUN_PANEL) — Part D

# ============================================================
# PART E: Text-only projection for 12 ASINs with has_img=0, has_text=1
#
# All 12 missing ASINs have valid text embeddings — only image
# scraping failed. Rather than brand-mean imputation, we project
# using the actual text PCs by setting the image dimensions to
# the cross-sectional mean (= 0 in standardised space) before
# applying the saved PCA rotation. A binary `text_only_embed`
# flag is added so estimation can absorb any attenuation bias.
# ============================================================
cat("\n=== PART E: Text-only projection for image-missing ASINs ===\n")

# Pull text PCs for the 12 ASINs from the full (unfiltered) CLIP file
text_only_raw <- clip_all[
  asin %in% missing_asins$asin & has_img == 0 & has_text == 1,
  c("asin", "brand_grouped_50", "txn_count", "has_img", "has_text", text_cols),
  with = FALSE
]
cat("ASINs entering text-only projection:", nrow(text_only_raw), "\n")

# Build 10-column input matrix
#   image cols → set to col_means[img] so they become exactly 0 after z-scoring
#   text cols  → use actual values
X_proj <- matrix(0, nrow = nrow(text_only_raw), ncol = length(pc_cols))
colnames(X_proj) <- pc_cols

for (col in img_cols)  X_proj[, col] <- col_means[col]   # → 0 after scaling
for (col in text_cols) X_proj[, col] <- text_only_raw[[col]]

# Apply the same z-score params used in Part B
X_proj_scaled <- scale(X_proj, center = col_means, scale = col_sds)
# sanity check: image columns should be exactly 0
stopifnot(all(abs(X_proj_scaled[, img_cols]) < 1e-10))

# Project into joint PC space using the saved rotation
joint_pc_proj <- X_proj_scaled %*% pca_fit$rotation[, 1:K]
joint_pc_proj_dt <- as.data.table(joint_pc_proj)
setnames(joint_pc_proj_dt, paste0("joint_pc", 1:K))

text_only_pcs <- cbind(
  text_only_raw[, .(asin, brand_grouped_50, txn_count, has_img, has_text)],
  joint_pc_proj_dt
)
text_only_pcs[, text_only_embed := 1L]

cat("\nProjected joint PCs for text-only ASINs:\n")
print(text_only_pcs[, c("asin", "brand_grouped_50", paste0("joint_pc", 1:K)), with = FALSE])

# -------------------------------------------------------
# Combine: full-embed ASINs (155) + text-only ASINs (12)
# -------------------------------------------------------
asin_joint_pcs[, text_only_embed := 0L]

asin_joint_pcs_complete <- rbindlist(
  list(asin_joint_pcs, text_only_pcs),
  use.names = TRUE, fill = TRUE
)
setorder(asin_joint_pcs_complete, brand_grouped_50, asin)

cat("\nCombined ASIN joint PCs: ", nrow(asin_joint_pcs_complete), "ASINs",
    "(", sum(asin_joint_pcs_complete$text_only_embed == 0), "full,",
    sum(asin_joint_pcs_complete$text_only_embed == 1), "text-only)\n")

fwrite(asin_joint_pcs_complete, "asin_joint_pcs_complete.csv")
cat("Saved: asin_joint_pcs_complete.csv\n")

# -------------------------------------------------------
# Re-merge complete PCs onto ASIN×quarter panel
# -------------------------------------------------------
if (RUN_PANEL) {

pc_cols_joint <- c(paste0("joint_pc", 1:K), "text_only_embed")

# Drop the old (incomplete) joint PC columns from panel
old_cols <- intersect(names(asin_qtr_pcs), pc_cols_joint)
if (length(old_cols) > 0) asin_qtr_pcs[, (old_cols) := NULL]

asin_qtr_complete <- merge(
  asin_qtr_pcs,
  asin_joint_pcs_complete[, c("asin", pc_cols_joint), with = FALSE],
  by    = "asin",
  all.x = TRUE
)

n_still_missing <- asin_qtr_complete[is.na(joint_pc1), .N]
cat("\nASIN×quarter rows still missing joint PCs after projection:", n_still_missing, "\n")
if (n_still_missing > 0) {
  cat("Remaining missing brands:\n")
  print(asin_qtr_complete[is.na(joint_pc1), .(n = .N), by = brand_grouped_50])
}

setorder(asin_qtr_complete, brand_grouped_50, asin, quarter)
fwrite(asin_qtr_complete, "asin_quarter_panel.csv")
cat("Updated: asin_quarter_panel.csv  (", nrow(asin_qtr_complete), "rows,",
    sum(asin_qtr_complete$text_only_embed == 1, na.rm = TRUE),
    "rows with text-only embed flag)\n")

}  # end if (RUN_PANEL) — panel re-merge

cat("\n=== Done. Final outputs:\n")
cat("    asin_joint_pcs.csv               (155 full-embed ASINs)\n")
cat("    asin_joint_pcs_complete.csv      (155 + 12 text-only projected ASINs)\n")
cat("    asin_quarter_panel.csv           (ASIN×quarter panel, complete PCs)\n")
cat("    asin_joint_pc_loadings.csv\n")
cat("    asin_joint_pca_scaling_params.csv\n")
