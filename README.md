# CLIP Embeddings for Market Definition

**Demand Estimation with Unstructured Product Data: Evidence from Amazon's
Toothpaste Market**  
  Burak Uzun, Cristian Huse · burak.uzun@uni-oldenburg.de · cristian.huse@uni-oldenburg.de
 
> This repo replicates the full work, using CLIP image+text embeddings to define product nesting in a nested-logit framework with Bertrand-Nash merger simulation
---

## Overview

Three demand models are estimated and compared:

| Model | Nesting | ρ* | Neg. MC% | hello Δp |
|---|---|---|---|---|
| M1: Plain Logit | none | 0 (fixed) | 8.6% | +1.23% |
| M2: Brand-Nested | brand identity | 0.35 | 8.6% | +1.28% |
| **M3: CLIP-Nested** | K=5 CLIP embedding clusters | **0.67** | **1.7%** | **+0.40%** |

**Key finding:** CLIP-based nesting places hello and Colgate in *different* clusters (C5 vs C2), implying a low diversion ratio and a small predicted merger price effect of +0.40% for hello — substantially smaller than the logit baseline of +1.23%.

---

## Read the thesis (no code required)

Two rendered PDFs sit in the repository root, both built from the same source, `blp_thesis_h.Rmd`:

| File | What it is |
|---|---|
| `Demand_Estimation_with_Unstructured_Product_Data_UzunBurak_2026.pdf` | Submission version, University of Oldenburg template |
| `Demand_Estimation_with_Unstructured_Product_Data_UzunBurak_2026_article.pdf` | Article version |

---

## Repository Structure

```
clip-amazon-toothpaste-market/
├── data/
│   ├── asin_quarter_panel.csv          # ASIN×quarter panel (raw; estimation sample = 159 ASINs, 19 quarters)
│   ├── asin_joint_pcs_complete.csv     # CLIP embedding PCs (5 components per ASIN)
│   └── asin_characteristics.csv       # Package size & import flag per ASIN
├── scripts/
│   ├── blp_three_models.R             # MAIN: demand estimation + merger simulation
│   ├── clip_k_sensitivity.R           # K=2..10 sensitivity analysis
│   ├── tables_and_figures.R           # Table 1, Figure 1, Figure 2
│   ├── build_thesis_pdf.R             # Builds both PDFs from blp_thesis_h.Rmd
│   ├── placebo_nesting.R              # 200 random + 200 shuffled placebo partitions
│   ├── robustness_checks.R            # Scenario grid, diversion ratios, welfare
│   ├── brand_orthogonal_nesting.R     # Clusters the brand-residual embedding space
│   ├── brand_fe_variants.R            # Brand fixed effects under all three nestings
│   ├── cluster_ownership_diagnostic.R # Firms per CLIP cluster
│   ├── CLIP-Embeddings/               # Upstream embedding pipeline (see its README.md)
│   │   ├── asin_master_for_embeddings.parquet   # Input: ASIN titles/benefits/image URLs
│   │   ├── clip_asin_embedding_extraction.ipynb # CLIP extraction (Colab/GPU)
│   │   ├── asin_features_for_blp_clip.csv       # Extraction output (committed for exact repro)
│   │   └── asin_joint_pca.R                     # Joint PCA → data/asin_joint_pcs_complete.csv
│   │                                            #   (verified byte-identical reproduction)
├── output/                            # Reference outputs (scripts regenerate these in place)
│   ├── figure1_clip_space.png         # CLIP PC1×PC2 scatter (K=5 clusters)
│   ├── figure2_price_trajectories.png # Nash merger vs no-merger price paths
│   ├── table1_cluster_summary.csv
│   ├── three_models_merger_summary.csv
│   ├── three_models_rmse.csv
│   ├── three_models_formulas.txt      # Plain-text formula reference for all models
│   ├── clip_k_sensitivity.csv
│   ├── model{1,2,3}_coefficients.csv
│   └── model{1,2,3}_asin_results.csv  # ASIN×quarter: share, mc, elasticity, Nash prices
├── thesis/                            # Oldenburg template assets (LaTeX template,
│                                      #   title page, university logos)
├── blp_thesis_h.Rmd                   # THE SOURCE. Self-contained: sources every
│                                      #   script, runs the pipeline, renders both PDFs
├── Demand_..._UzunBurak_2026.pdf         # Submission version (Oldenburg template)
└── Demand_..._UzunBurak_2026_article.pdf # Article version
```

Reference PDFs of the cited works are not redistributed here — they are
third-party copyrighted articles and nothing in the pipeline reads them.
Every source is fully cited in the thesis reference list.

---

## How to Reproduce

> **Option A and Option B are independent — pick one, not both.**  
> Option A (knit Rmd) runs all scripts internally. Option B runs scripts directly without the Rmd.

### Requirements

```r
install.packages(c("data.table", "lubridate", "cluster",
                   "ggplot2", "ggrepel", "patchwork",
                   "knitr", "rmarkdown", "kableExtra"))
```

R ≥ 4.2 and [TinyTeX](https://yihui.org/tinytex/) (or another LaTeX distribution) for PDF output only.

### Option A — Knit the thesis Rmd (recommended)

`blp_thesis_h.Rmd` is fully self-contained. It **automatically sources every analysis script**, runs the complete pipeline from raw data, and renders every table and figure. You do **not** need to run any scripts separately beforehand.

**No path configuration needed** — the Rmd auto-detects its own location on Windows, Mac, and Linux, regardless of what the repo folder is named (e.g. `clip-amazon-toothpaste-market-main/` from a GitHub ZIP download works fine).

Open `blp_thesis_h.Rmd` in RStudio and click **Knit** — the `knit:` hook in its YAML header builds *both* PDFs under their final names. Equivalently, from the repo root:

```r
# Both PDFs (requires TinyTeX or another LaTeX distribution)
source("scripts/build_thesis_pdf.R"); build_thesis()
```

```bash
# Same thing from a shell; --thesis or --article builds just one
Rscript scripts/build_thesis_pdf.R
```

On the first knit the computation chunk runs the analysis scripts (≈ 25–30 min total). Subsequent knits are fast — results are cached and only recompute if a script file changes.

### Option B — Run scripts directly (no Rmd)

Use this if you want to inspect or modify individual estimation steps without knitting a document. Scripts auto-detect their own location — **no `setwd()` needed**, works on any OS and folder name.

Run in this order (each step's output is required by the next):

```r
# 1. Demand estimation + merger simulation (≈ 5–10 min)
#    Writes: output/three_models_merger_summary.csv
#            output/model{1,2,3}_coefficients.csv
#            output/model{1,2,3}_asin_results.csv
source("/path/to/repo/scripts/blp_three_models.R")

# 2. K sensitivity analysis — K=2..10 (≈ 15–20 min)
#    Writes: output/clip_k_sensitivity.csv
source("/path/to/repo/scripts/clip_k_sensitivity.R")

# 3. Figures and Table 1 (< 1 min) — requires step 1 output
#    Writes: output/figure1_clip_space.png
#            output/figure2_price_trajectories.png
#            output/table1_cluster_summary.csv
source("/path/to/repo/scripts/tables_and_figures.R")
```

All scripts write directly into `output/`, overwriting the reference copies shipped with the repo. To compare your results against ours, diff against the committed versions (e.g. `git diff output/`) — all randomness is seed-pinned, so results should match to numerical precision (tiny last-digit floating-point differences across machines/BLAS libraries are normal).


---

## Data

| File | Rows | Description |
|---|---|---|
| `asin_quarter_panel.csv` | 1,278 | ASIN×quarter sales, prices, CLIP PCs (raw: 167 ASINs, 2018Q1–2023Q1) |
| `asin_joint_pcs_complete.csv` | 167 | 5 CLIP principal components per ASIN |
| `asin_characteristics.csv` | 159 | Package weight (g) and import flag per ASIN |

The estimation sample applies two filters (in `blp_three_models.R`): quarters through 2022Q3 only (`TIME_CUTOFF`), and ASINs present in ≥ 3 quarters. This yields the final panel of **1,164 observations, 159 ASINs, 19 quarters (2018Q1–2022Q3)** used in all models.

**Not included:** `choice_set_with_state_and_region.csv` (148 MB raw individual choice data, source: Amazon review + product metadata). `asin_characteristics.csv` was pre-computed from it and is the only output needed to replicate all results.

### CLIP Embeddings

Images and product text for each ASIN were encoded with OpenAI's `clip-vit-base-patch32` model. Joint image+text embeddings were projected to 5 principal components. K=5 k-means (seed 42, 50 restarts) assigns each ASIN to a CLIP cluster used as the nest in M3.

### K=5 Cluster Map

| Cluster | Label | Key Brands |
|---|---|---|
| C1 | GSK Sensitivity | Sensodyne, SENSODYNE PRONAMEL, Parodontax |
| C2 | Colgate | Colgate, Arm & Hammer |
| C3 | Crest | Crest |
| C4 | Tom's of Maine | Tom's of Maine |
| C5 | Natural / Specialty | **hello**, APAGARD, JASON, Orajel, Other |

hello (C5) and Colgate (C2) are in **different clusters** → lower diversion → smaller merger effect.

---

## Methodology

### Demand Model (Berry 1994)

$$\log(s_{jt}/s_{0t}) - \rho \cdot \log(s_{j|g,t}) = \alpha p_{jt} + \beta' x_{jt} + \xi_{jt}$$

- **α** calibrated to median own-price elasticity = −2.62 (Bijmolt et al. 2005 meta-analysis)
- **β** estimated by concentrated OLS with quarter fixed effects absorbed by demeaning
- **ρ*** found by grid search over {0, 0.01, …, 0.90} minimising RSS

### Merger Simulation (Bertrand-Nash)

Pre-merger marginal costs inverted from first-order conditions, averaged per ASIN, then fixed. Nash equilibrium prices computed under pre- and post-merger ownership via damped contraction mapping (λ = 0.4, tol = 1e-8). Validated against PyBLP's `compute_prices()` (Conlon & Gortmaker 2020); max deviation < 2 × 10⁻⁸.

---

## Results at a Glance

**M3 (CLIP-Nested, K=5, ρ\*=0.67):**
- RSS improvement over logit: **8.1%**
- Negative MC rate: **1.7%** (vs 8.6% for plain logit)
- hello merger price effect: **+0.40%**
- Colgate merger price effect: **+0.018%**

See the rendered PDFs in the repository root for all three models, price trajectories, the K sensitivity analysis, and full references.

---

## References

Berry, S.T. (1994). Estimating discrete-choice models of product differentiation. *RAND Journal of Economics*, 25(2), 242–262.

Bijmolt, T.H.A., van Heerde, H.J., & Pieters, R.G.M. (2005). New empirical generalizations on the determinants of price elasticity. *Journal of Marketing Research*, 42(2), 141–156.

Compiani, G., Morozov, I., & Seiler, S. (2026). Demand estimation with text and image data. *The RAND Journal of Economics*. https://doi.org/10.1111/1756-2171.70052

Conlon, C., & Gortmaker, J. (2020). Best practices for differentiated products demand estimation with PyBLP. *RAND Journal of Economics*, 51(4), 1108–1161.

Draganska, M., & Jain, D.C. (2006). Consumer preferences and product-line pricing strategies: An empirical analysis. *Marketing Science*, 25(2), 164–174.

Nevo, A. (2000). A practitioner's guide to estimation of random-coefficients logit models of demand. *Journal of Economics & Management Strategy*, 9(4), 513–548.

Nevo, A. (2001). Measuring market power in the ready-to-eat cereal industry. *Econometrica*, 69(2), 307–342.

Radford, A., Kim, J.W., Hallacy, C., Ramesh, A., Goh, G., Agarwal, S., Sastry, G., Askell, A., Mishkin, P., Clark, J., Krueger, G., & Sutskever, I. (2021). Learning transferable visual models from natural language supervision. *Proceedings of the 38th International Conference on Machine Learning (ICML)*, PMLR 139, 8748–8763.

---

## Citation

If you use this code or data, please cite:

```
Uzun, B., & Huse, C. (2026). Demand Estimation with Unstructured Product Data:
Evidence from Amazon's Toothpaste Market.
Master's Thesis, Carl von Ossietzky Universität Oldenburg.
https://github.com/brkuzn/clip-amazon-toothpaste-market
```
