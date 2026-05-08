# CLIP Embeddings for Market Definition

**Master's Thesis — Demand Estimation with Unstructured Product Data: Evidence from Amazon's
Toothpaste Market**  
  Burak Uzun · burak.uzun@uni-oldenburg.de · brkuznmail@gmail.com

> *Does the hello → Colgate merger (2020Q1) raise toothpaste prices on Amazon?*  
> This repo replicates the full BLP demand estimation and Bertrand-Nash merger simulation  
> using CLIP image+text embeddings to define product nesting in a nested-logit framework.

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

## Quick Browse (no code required)

Open **`blp_three_models.html`** directly in any browser for the full interactive dashboard: all three models, demand estimates, merger simulation, price trajectories, K sensitivity analysis, and a complete references list.

---

## Repository Structure

```
clip-amazon-toothpaste-market/
├── data/
│   ├── asin_quarter_panel.csv          # ASIN×quarter panel (159 ASINs, 19 quarters)
│   ├── asin_joint_pcs_complete.csv     # CLIP embedding PCs (5 components per ASIN)
│   └── asin_characteristics.csv       # Package size & import flag per ASIN
├── scripts/
│   ├── blp_three_models.R             # MAIN: demand estimation + merger simulation
│   ├── clip_k_sensitivity.R           # K=2..10 sensitivity analysis
│   ├── tables_and_figures.R           # Table 1, Figure 1, Figure 2
│   └── inject_tables.R                # DEV ONLY: injects kableExtra tables into HTML
│                                      #   (hardcoded local paths, not needed for replication)
├── output/                            # Pre-populated reference outputs for comparison
│   ├── figures/
│   │   ├── figure1_clip_space.png     # CLIP PC1×PC2 scatter (K=5 clusters)
│   │   └── figure2_price_trajectories.png  # Nash merger vs no-merger price paths
│   ├── tables/
│   │   ├── table1_cluster_summary.csv
│   │   ├── three_models_merger_summary.csv
│   │   └── clip_k_sensitivity.csv
│   └── coefficients/
│       ├── model1_coefficients.csv
│       ├── model2_coefficients.csv
│       └── model3_coefficients.csv
├── papers/                            # Reference PDFs for all cited works
│   ├── Berry1994.pdf
│   ├── JMR_2005_BijmoltVanHeerdePieters.pdf
│   ├── Consumer_Preferences_and_Product-Line_Pricing_Stra.pdf
│   ├── nevo2000.pdf
│   ├── nevo-2001-cereal.pdf
│   ├── pyblp_2020_conlon_gortmaker.pdf
│   ├── learning_transferable_visual_models.pdf
│   └── compiani_estimating_demand.pdf  # Related work (not yet cited)
├── blp_thesis.Rmd                     # Self-contained R Markdown → compiles full PDF + HTML
├── blp_thesis.pdf                   # 7-page thesis PDF
└── blp_thesis.html                   # 7-page thesis html
└── blp_three_models.html              # Interactive results dashboard (open in browser)
```

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

`blp_thesis.Rmd` is fully self-contained. It **automatically sources all three scripts**, runs the complete pipeline from raw data, and renders every table and figure. You do **not** need to run any scripts separately beforehand.

**No path configuration needed** — the Rmd auto-detects its own location on Windows, Mac, and Linux, regardless of what the repo folder is named (e.g. `clip-amazon-toothpaste-market-main/` from a GitHub ZIP download works fine).

1. Open `blp_thesis.Rmd` in RStudio and click **Knit**, or run:

```r
# Render both HTML and PDF in one call (recommended)
rmarkdown::render("blp_thesis.Rmd", output_format = "all")

# HTML only — no LaTeX needed, renders in minutes
rmarkdown::render("blp_thesis.Rmd", output_format = "html_document")

# PDF only — requires TinyTeX or another LaTeX distribution
rmarkdown::render("blp_thesis.Rmd", output_format = "pdf_document")
```

On the first knit the computation chunk runs all three scripts (≈ 25–30 min total). Subsequent knits are fast — results are cached and only recompute if a script file changes.

### Option B — Run scripts directly (no Rmd)

Use this if you want to inspect or modify individual estimation steps without knitting a document. Scripts auto-detect their own location — **no `setwd()` needed**, works on any OS and folder name.

Run in this order (each step's output is required by the next):

```r
# 1. Demand estimation + merger simulation (≈ 5–10 min)
#    Writes: output/tables/three_models_merger_summary.csv
#            output/coefficients/model{1,2,3}_coefficients.csv
source("/path/to/repo/scripts/blp_three_models.R")

# 2. K sensitivity analysis — K=2..10 (≈ 15–20 min)
#    Writes: output/tables/clip_k_sensitivity.csv
source("/path/to/repo/scripts/clip_k_sensitivity.R")

# 3. Figures and Table 1 (< 1 min) — requires step 1 output
#    Writes: output/figures/figure1_clip_space.png
#            output/figures/figure2_price_trajectories.png
#            output/tables/table1_cluster_summary.csv
source("/path/to/repo/scripts/tables_and_figures.R")
```

Your results can be compared against the pre-populated `output/` folder included in the repo.

> **Note:** `inject_tables.R` is a development utility used to rebuild `blp_three_models.html` from local intermediate files. It contains hardcoded paths specific to the author's machine and is **not needed** for replication — the HTML is already fully built and included in the repo.

---

## Data

| File | Rows | Description |
|---|---|---|
| `asin_quarter_panel.csv` | 3,492 | ASIN×quarter sales, prices, CLIP PCs (2019Q1–2022Q2) |
| `asin_joint_pcs_complete.csv` | 159 | 5 CLIP principal components per ASIN |
| `asin_characteristics.csv` | 159 | Package weight (g) and import flag per ASIN |

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

See `blp_three_models.html` for the interactive dashboard with all three models, price trajectories, K sensitivity analysis, and full references.

---

## References

Berry, S.T. (1994). Estimating discrete-choice models of product differentiation. *RAND Journal of Economics*, 25(2), 242–262.

Bijmolt, T.H.A., van Heerde, H.J., & Pieters, R.G.M. (2005). New empirical generalizations on the determinants of price elasticity. *Journal of Marketing Research*, 42(2), 141–156.

Conlon, C., & Gortmaker, J. (2020). Best practices for differentiated products demand estimation with PyBLP. *RAND Journal of Economics*, 51(4), 1108–1161.

Draganska, M., & Jain, D.C. (2006). Consumer preferences and product-line pricing strategies: An empirical analysis. *Marketing Science*, 25(2), 164–174.

Nevo, A. (2000). A practitioner's guide to estimation of random-coefficients logit models of demand. *Journal of Economics & Management Strategy*, 9(4), 513–548.

Nevo, A. (2001). Measuring market power in the ready-to-eat cereal industry. *Econometrica*, 69(2), 307–342.

Radford, A., Kim, J.W., Hallacy, C., Ramesh, A., Goh, G., Agarwal, S., Sastry, G., Askell, A., Mishkin, P., Clark, J., Krueger, G., & Sutskever, I. (2021). Learning transferable visual models from natural language supervision. *Proceedings of the 38th International Conference on Machine Learning (ICML)*, PMLR 139, 8748–8763.

---

## Citation

If you use this code or data, please cite:

```
Uzun, B. (2026). Demand Estimation with Unstructured Product Data: Evidence from Amazon's
Toothpaste Market
Master's Thesis, Carl von Ossietzky Universität Oldenburg.
https://github.com/brkuzn/clip-amazon-toothpaste-market
```
