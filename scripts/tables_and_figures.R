# ============================================================================
# tables_and_figures.R
# Produces publication-quality tables and figures for the thesis.
#
# OUTPUT:
#   table1_cluster_summary.csv    ← summary stats by CLIP cluster
#   figure1_clip_space.png        ← CLIP scatter PC1 vs PC2 (ggplot2)
#   figure2_price_trajectories.png← Nash merger vs no-merger price paths
# ============================================================================

library(data.table); library(lubridate); library(ggplot2); library(ggrepel)
setwd("/Users/brkuzn")
out_dir <- "analysis_2704"

LAMBDA        <- 13.93
TIME_CUTOFF   <- as.Date("2022-07-01")
MIN_QUARTERS  <- 3
MERGER_START  <- "2020Q1"
K_CLIP        <- 5
PC_COLS       <- paste0("joint_pc", 1:5)
import_brands       <- c("Sensodyne","SENSODYNE PRONAMEL","Parodontax","APAGARD")
freight_crisis_qtrs <- c("2021Q3","2021Q4","2022Q1","2022Q2")

# ── DATA ─────────────────────────────────────────────────────────────────────
panel <- fread("asin_quarter_panel.csv")
panel[, month_date_dummy := as.Date(paste0(
  sub("Q.*","",quarter),"-",
  sprintf("%02d",(as.integer(sub(".*Q","",quarter))-1)*3+1),"-01"))]
panel <- panel[month_date_dummy <= TIME_CUTOFF][, month_date_dummy := NULL]

asin_chars <- fread("asin_characteristics.csv")
panel <- merge(panel, asin_chars[, .(asin, pkg_size, is_import)], by="asin", all.x=TRUE)
panel[is.na(pkg_size),  pkg_size  := median(panel$pkg_size,  na.rm=TRUE)]
panel[is.na(is_import), is_import := 0]

keep_asins <- panel[, .(n=uniqueN(quarter)), by=asin][n>=MIN_QUARTERS, asin]
panel      <- panel[asin %in% keep_asins]

mkt_totals <- panel[, .(total_q=sum(q_jt)), by=quarter]
mkt_totals[, M_t := (1+LAMBDA)*total_q]
panel      <- merge(panel, mkt_totals[,.(quarter,M_t,total_q)], by="quarter", all.x=TRUE)
panel[, share         := pmax(1e-6, pmin(1-1e-6, q_jt/M_t))]
panel[, outside_share := 1 - total_q/M_t]
panel[, is_freight_crisis    := as.numeric(quarter %in% freight_crisis_qtrs)]
panel[, import_freight_shock := is_import * is_freight_crisis]

# CLIP clusters
pcs_raw <- fread("asin_joint_pcs_complete.csv")
pcs_raw <- pcs_raw[asin %in% panel$asin]
set.seed(42)
km6 <- kmeans(as.matrix(pcs_raw[, PC_COLS, with=FALSE]),
              centers=K_CLIP, nstart=50, iter.max=500)
pcs_raw[, clip_nest := km6$cluster]
panel   <- merge(panel, pcs_raw[, .(asin, clip_nest)], by="asin", all.x=TRUE)
# joint_pc1, joint_pc2 already in asin_quarter_panel.csv

# Load M3 results for merger prices
m3 <- fread(file.path(out_dir, "model3_asin_results.csv"))
panel <- merge(panel, m3[, .(asin, quarter, mc, price_nomerger, price_merger)],
               by=c("asin","quarter"), all.x=TRUE)

cat("Data loaded:", nrow(panel), "obs\n")

# ─────────────────────────────────────────────────────────────────────────────
# TABLE 1: Summary Statistics by CLIP Cluster
# ─────────────────────────────────────────────────────────────────────────────
cat("\nBuilding Table 1...\n")

# Cluster labels — look at dominant brand per cluster
cluster_brands <- panel[, .(brand=names(which.max(table(brand_grouped_50)))), by=clip_nest]
cluster_labels <- c(
  "1" = "Cluster 1", "2" = "Cluster 2", "3" = "Cluster 3",
  "4" = "Cluster 4", "5" = "Cluster 5", "6" = "Cluster 6"
)

tab1 <- panel[!is.na(clip_nest), .(
  n_asins      = uniqueN(asin),
  n_obs        = .N,
  # Price
  price_mean   = round(mean(price_mean,    na.rm=TRUE), 2),
  price_median = round(median(price_mean,  na.rm=TRUE), 2),
  price_sd     = round(sd(price_mean,      na.rm=TRUE), 2),
  # Market share (×1000 for readability)
  share_mean   = round(mean(share,         na.rm=TRUE)*1000, 3),
  share_median = round(median(share,       na.rm=TRUE)*1000, 3),
  share_sd     = round(sd(share,           na.rm=TRUE)*1000, 3),
  # Package size
  pkg_mean     = round(mean(pkg_size,      na.rm=TRUE), 1),
  pkg_median   = round(median(pkg_size,    na.rm=TRUE), 1),
  pkg_sd       = round(sd(pkg_size,        na.rm=TRUE), 1),
  # Dominant brand
  dom_brand    = names(which.max(table(brand_grouped_50)))
), by=clip_nest][order(clip_nest)]

cat("\nTable 1: Summary Statistics by CLIP Cluster\n")
cat(sprintf("%-10s %7s %6s %8s %8s %8s %9s %9s %9s %8s %8s %8s  %s\n",
    "Cluster","N_asins","N_obs",
    "P_mean","P_med","P_sd",
    "Sh_mean","Sh_med","Sh_sd",
    "Pkg_mean","Pkg_med","Pkg_sd","Dom.Brand"))
for (i in seq_len(nrow(tab1))) {
  r <- tab1[i]
  cat(sprintf("%-10s %7d %6d %8.2f %8.2f %8.2f %9.3f %9.3f %9.3f %8.1f %8.1f %8.1f  %s\n",
      paste0("C",r$clip_nest), r$n_asins, r$n_obs,
      r$price_mean, r$price_median, r$price_sd,
      r$share_mean, r$share_median, r$share_sd,
      r$pkg_mean, r$pkg_median, r$pkg_sd,
      r$dom_brand))
}

fwrite(tab1, file.path(out_dir, "table1_cluster_summary.csv"))
cat("Saved: table1_cluster_summary.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 1: CLIP Space — PC1 vs PC2 scatter
# ─────────────────────────────────────────────────────────────────────────────
cat("\nBuilding Figure 1...\n")

# One row per ASIN (average PCs across quarters — PCs are ASIN-level constant)
asin_level <- panel[, .(
  pc1       = mean(joint_pc1, na.rm=TRUE),
  pc2       = mean(joint_pc2, na.rm=TRUE),
  clip_nest = clip_nest[1],
  brand     = brand_grouped_50[1]
), by=asin]

# Highlight hello and Colgate
asin_level[, highlight := fifelse(brand == "hello",   "hello",
                           fifelse(brand == "Colgate", "Colgate", "Other"))]

# Cluster centroids for labels
centroids <- asin_level[, .(pc1=mean(pc1), pc2=mean(pc2)), by=clip_nest]
centroids[, label := paste0("C", clip_nest)]

# Annotation data for hello and Colgate
annot <- asin_level[highlight != "Other", .(
  pc1  = mean(pc1),
  pc2  = mean(pc2),
  label = brand
), by=brand]

cluster_colours <- c(
  "1"="#4e79a7","2"="#f28e2b","3"="#e15759",
  "4"="#76b7b2","5"="#59a14f","6"="#edc948"
)

# Nudge centroid labels so they don't overlap with dense clusters
# C3 = Colgate cluster (right side) → push label above the cloud
centroids[, pc2_label := pc2]
centroids[label == "C3", pc2_label := pc2 + 0.55]
centroids[label == "C2", pc2_label := pc2 + 0.30]

fig1 <- ggplot(asin_level, aes(x=pc1, y=pc2)) +
  # background: non-highlighted points coloured by cluster
  geom_point(data=asin_level[highlight=="Other"],
             aes(colour=factor(clip_nest)), alpha=0.55, size=2.4, shape=16) +
  # cluster centroid labels — nudged to avoid overlap
  geom_label(data=centroids,
             aes(x=pc1, y=pc2_label, label=label),
             inherit.aes=FALSE,
             colour="grey20", fill="white", size=3.2, fontface="bold",
             label.padding=unit(0.22,"lines"), label.size=0.3) +
  # hello ASINs — red filled diamond, on top
  geom_point(data=asin_level[highlight=="hello"],
             colour="#c0392b", fill="#c0392b", size=4.5, shape=23, stroke=1.3) +
  # Colgate ASINs — dark blue filled triangle, on top
  geom_point(data=asin_level[highlight=="Colgate"],
             colour="#1a5276", fill="#1a5276", size=4.5, shape=24, stroke=1.3) +
  # Brand annotations with repel
  geom_label_repel(data=annot,
                   aes(x=pc1, y=pc2, label=label),
                   inherit.aes=FALSE,
                   colour="grey10", fill="white", size=3.2, fontface="bold",
                   nudge_y=0.35, nudge_x=0.15,
                   label.padding=unit(0.25,"lines"), label.size=0.35,
                   min.segment.length=0.15, segment.colour="grey50",
                   segment.size=0.4) +
  scale_colour_manual(
    values = cluster_colours,
    name   = "CLIP Cluster",
    labels = paste0("C", 1:6),
    breaks = as.character(1:6)
  ) +
  labs(
    title    = "Figure 1: Product Space from CLIP Embeddings",
    subtitle = "Each point = one ASIN  ·  PC1 & PC2 of CLIP joint image+text embeddings (5-PC basis, K=5 clusters)",
    x        = "PC 1",
    y        = "PC 2",
    caption  = "◆ hello (red)   ▲ Colgate (dark blue)   Other brands shown by CLIP cluster colour\nhello and Colgate occupy different clusters → lower diversion ratio → smaller predicted merger effect"
  ) +
  theme_minimal(base_size=11) +
  theme(
    plot.title      = element_text(face="bold", size=12),
    plot.subtitle   = element_text(size=9, colour="grey40"),
    plot.caption    = element_text(size=8.5, colour="grey50"),
    legend.position = "right",
    panel.grid.minor= element_blank(),
    panel.grid.major= element_line(colour="grey92")
  )

ggsave(file.path(out_dir, "figure1_clip_space.png"),
       fig1, width=7, height=5, dpi=300)
cat("Saved: figure1_clip_space.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 2: Price Trajectories — Observed, No-Merger, Merger Nash
# ─────────────────────────────────────────────────────────────────────────────
cat("\nBuilding Figure 2...\n")

focus_brands <- c("hello", "Colgate")

# Post-merger quarters only (the counterfactual window)
qtrs      <- sort(unique(panel$quarter))
post_qtrs <- qtrs[qtrs >= MERGER_START]

price_avg <- panel[brand_grouped_50 %in% focus_brands &
                   quarter %in% post_qtrs & !is.na(price_merger),
  .(
    obs      = mean(price_mean,     na.rm=TRUE),
    nomerger = mean(price_nomerger, na.rm=TRUE),
    merger   = mean(price_merger,   na.rm=TRUE)
  ), by=.(brand=brand_grouped_50, quarter)]

# Merger effect % panel
price_avg[, effect_pct := (merger - nomerger) / nomerger * 100]

# Long format — Nash lines only for top panel (observed too volatile to plot alongside)
price_long <- melt(price_avg[, .(brand, quarter, nomerger, merger)],
                   id.vars=c("brand","quarter"),
                   variable.name="series", value.name="price")
price_long[, series := fifelse(series=="nomerger", "No-Merger Nash (counterfactual)",
                                                    "Merger Nash (model)")]
price_long[, series := factor(series,
  levels=c("No-Merger Nash (counterfactual)","Merger Nash (model)"))]
price_long[, qtr_num := match(quarter, post_qtrs)]

# Panel A: Nash price levels only — y-axis tight to Nash range so divergence is visible
fig2a <- ggplot(price_long, aes(x=qtr_num, y=price,
                colour=series, linetype=series, group=series)) +
  geom_line(linewidth=1.0) +
  geom_point(size=2.2) +
  scale_colour_manual(values=c(
    "No-Merger Nash (counterfactual)"= "#2563eb",
    "Merger Nash (model)"            = "#e15759"
  ), name=NULL) +
  scale_linetype_manual(values=c(
    "No-Merger Nash (counterfactual)"= "dashed",
    "Merger Nash (model)"            = "solid"
  ), name=NULL) +
  scale_x_continuous(breaks=seq_along(post_qtrs), labels=post_qtrs) +
  facet_wrap(~brand, scales="free_y", ncol=2) +
  labs(y="Nash equilibrium price ($)", x=NULL,
       subtitle="Observed prices omitted from top panel — Nash range ($4–$8) is much narrower than\nobserved price swings ($4–$10), which would compress the counterfactual divergence") +
  theme_minimal(base_size=10) +
  theme(
    legend.position = "bottom",
    plot.subtitle   = element_text(size=8, colour="grey50", face="italic"),
    axis.text.x     = element_text(angle=45, hjust=1, size=7.5),
    panel.grid.minor= element_blank(),
    panel.grid.major= element_line(colour="grey92"),
    strip.text      = element_text(face="bold", size=11)
  )

# Panel B: merger effect %
price_avg[, brand := factor(brand, levels=c("Colgate","hello"))]
fig2b <- ggplot(price_avg, aes(x=match(quarter, post_qtrs),
                y=effect_pct, colour=brand, group=brand)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey70") +
  geom_line(linewidth=0.85) +
  geom_point(size=2.2) +
  scale_colour_manual(values=c("hello"="#e15759","Colgate"="#2563eb"), name=NULL) +
  scale_x_continuous(breaks=seq_along(post_qtrs), labels=post_qtrs) +
  labs(y="Merger price effect (%)", x=NULL) +
  theme_minimal(base_size=10) +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle=45, hjust=1, size=7.5),
    panel.grid.minor= element_blank(),
    panel.grid.major= element_line(colour="grey92")
  )

# Combine with patchwork
library(patchwork)
fig2 <- (fig2a / fig2b) +
  plot_annotation(
    title    = "Figure 2: Price Trajectories — Merger vs No-Merger Counterfactual",
    subtitle = "Model 3 (CLIP-Nested Logit, ρ*=0.60) · Post-merger window (2020Q1–2022Q3) · MCs fixed at pre-merger averages",
    caption  = "No-Merger Nash: Bertrand equilibrium under pre-merger ownership (Ω_pre)\nMerger Nash: Bertrand equilibrium under merged ownership (Ω_post)\nEffect = (Merger Nash − No-Merger Nash) / No-Merger Nash × 100%",
    theme    = theme(
      plot.title    = element_text(face="bold", size=12),
      plot.subtitle = element_text(size=9, colour="grey40"),
      plot.caption  = element_text(size=8, colour="grey50")
    )
  ) + plot_layout(heights=c(2,1))

ggsave(file.path(out_dir, "figure2_price_trajectories.png"),
       fig2, width=10, height=8, dpi=300)
cat("Saved: figure2_price_trajectories.png\n")

cat("\nDone.\n")
