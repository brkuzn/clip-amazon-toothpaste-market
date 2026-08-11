# CLIP Embedding Pipeline (upstream of `data/`)

This folder documents and reproduces the construction of the CLIP joint principal
components shipped in `data/asin_joint_pcs_complete.csv`.

## Pipeline

```
asin_master_for_embeddings.parquet        (ASIN titles, benefits, image URLs)
        │
        ▼  clip_asin_embedding_extraction.ipynb   [Colab/GPU, ~minutes]
asin_features_for_blp_clip.csv            (per-ASIN: 5 image PCs + 5 text PCs,
        │                                  has_img/has_text flags, APAGARD URL fix)
        ▼  asin_joint_pca.R                [CPU, seconds — fully deterministic]
asin_joint_pcs_complete.csv               (5 joint PCs per ASIN, 155 full + 12
                                           text-only projected = 167 ASINs)
```

## Exact reproduction of the joint PCs

```r
# working directory = this folder
source("asin_joint_pca.R")
```

The script detects that `choice_set_with_state_and_region.csv` (148 MB raw
purchase data, not redistributed) is absent and skips the panel-rebuild parts
(C–D), which are not needed for the joint PCs. **Verified: the output
`asin_joint_pcs_complete.csv` is byte-identical to the copy in `data/`.**

## Notes

- The notebook step (CLIP inference + image downloads) is *not* bit-reproducible —
  image URLs can rot and GPU inference varies slightly across hardware. Its output
  `asin_features_for_blp_clip.csv` is therefore committed here, making everything
  downstream exactly reproducible.
- `asin_joint_pca.R` Part E implements the text-only projection for the 12 ASINs
  with missing images (image dimensions set to the standardized cross-sectional
  mean before applying the saved PCA rotation) — see the thesis Data section.
