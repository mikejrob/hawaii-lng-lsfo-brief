## fig_fuel_cost_comparison.R
## Fuel-cost comparison: LSFO vs. LNG (FSRU-inclusive) vs. solar+battery
## All costs in cents/kWh, plotted against Brent crude oil price $50–$150/bbl.
##
## LSFO  : model steady-state R3 curve (heco_lsfo_long_run_curve.csv)
##          converted using steam-turbine heat rate (11.0895 MMBtu/MWh)
## LNG   : Brent-indexed formula (HSEO/FGE Aug 2024): 0.118×Brent + $0.60/MMBtu
##          + regasification adder, four scenarios:
##            (a) 0.4 mtpa at full capacity  → +$3.93/MMBtu
##            (b) 1.0 mtpa at full capacity  → +$1.68/MMBtu
##            (c) IGP land-constrained avg utilization (~55% of 0.4 mtpa) → +$7.15/MMBtu
##            (d) IGP preferred avg utilization      (~25% of 0.4 mtpa) → +$15.72/MMBtu
##          FSRU fixed costs are largely independent of volume (vessel lease, crew,
##          maintenance), so the per-MMBtu adder scales inversely with throughput.
##          All LNG converted using CCGT heat rate (6.5 MMBtu/MWh).
## Solar : NREL ATB moderate 2030 (~$70/MWh mainland) + 62% Hawaii premium
##          → 9–11 ¢/kWh (Leidos/EIA 2016); shown as shaded band

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(scales)
})
# silence data.table/dplyr namespace conflicts
filter    <- dplyr::filter
transmute <- dplyr::transmute

base_dir <- "/Users/mike/Library/CloudStorage/GoogleDrive-michael@ae4cast.com/Other computers/My Mac (1)/EIA/lng_vs_oil"
out_dir  <- file.path(base_dir, "brief_figures")
dir.create(out_dir, showWarnings = FALSE)

# ── Shared theme (matches rest of brief) ──────────────────────────────────────
brief_theme <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = "grey90"),
    axis.title        = element_text(size = 10),
    legend.position   = "bottom",
    legend.key.width  = unit(1.4, "cm"),
    legend.text       = element_text(size = 9),
    strip.background  = element_blank()
  )

# ── Colors ────────────────────────────────────────────────────────────────────
COL_LSFO      <- "#333333"   # near-black
COL_LNG_FULL  <- "#27ae60"   # green  — full-capacity FSRU (0.4 / 1.0 mtpa)
COL_LNG_LC    <- "#2980b9"   # blue   — IGP land-constrained utilization
COL_LNG_PREF  <- "#8e44ad"   # purple — IGP preferred utilization
COL_SOLAR     <- "#f39c12"   # amber
COL_SOLAR_DK  <- "#b07300"   # darker amber for text
COL_VLINE     <- "#c0392b"   # red current-Brent marker

# ── Physical constants ────────────────────────────────────────────────────────
LSFO_MMBTU_PER_BBL <- 6.22        # residual fuel oil energy content
STEAM_HR           <- 11.0895     # MMBtu/MWh — existing HECO steam turbines
CCGT_HR            <-  6.5        # MMBtu/MWh — new combined-cycle gas turbine

# LNG commodity formula (HSEO/FGE Aug 2024 indicative):
LNG_SLOPE     <- 0.118            # $/MMBtu per $/bbl Brent
LNG_INTERCEPT <- 0.60             # $/MMBtu (commodity base)

# Regasification adders ($/MMBtu), all fixed-cost basis:
#   0.4 mtpa at 100% utilization (nameplate per FGE):      $3.93
#   1.0 mtpa at 100% utilization (nameplate per FGE):      $1.68
#   IGP land-constrained: ~55% of 0.4 mtpa → $3.93/0.55:  $7.15
#   IGP preferred:        ~25% of 0.4 mtpa → $3.93/0.25: $15.72
# Source: pv_savings_sketch.R — igp_pref = 207 MW × 8.76 h = 1,813 GWh/yr (~25% of
# 3,200 GWh full capacity); igp_lc = 308 MW × 8.76 h = 2,698 GWh/yr (~55% capacity)
REGAS_04_FULL <- 3.93
REGAS_10_FULL <- 1.68
REGAS_LC      <- REGAS_04_FULL / 0.55   # $7.145/MMBtu
REGAS_PREF    <- REGAS_04_FULL / 0.25   # $15.72/MMBtu

# Solar LCOE band (cents/kWh, all-in levelized)
SOLAR_LO  <-  9.0
SOLAR_HI  <- 11.0
SOLAR_MID <- 10.0

# Current Brent reference (March 31, 2026 front-month, Investing.com)
CURRENT_BRENT <- 112.85

# ── Load LSFO steady-state curve and compute all series ──────────────────────
raw <- fread(file.path(base_dir, "heco_lsfo_long_run_curve.csv"))
names(raw) <- trimws(names(raw))

# Helper: convert LNG $/MMBtu to ¢/kWh via CCGT
lng_ckwh <- function(brent, regas) (LNG_SLOPE * brent + LNG_INTERCEPT + regas) * CCGT_HR / 10

curve <- raw |>
  as_tibble() |>
  filter(p >= 50, p <= 150) |>
  transmute(
    brent         = p,
    lsfo_ckwh     = (heco_lsfo_bbl / LSFO_MMBTU_PER_BBL) * STEAM_HR / 10,
    lng_04_ckwh   = lng_ckwh(p, REGAS_04_FULL),
    lng_10_ckwh   = lng_ckwh(p, REGAS_10_FULL),
    lng_lc_ckwh   = lng_ckwh(p, REGAS_LC),
    lng_pref_ckwh = lng_ckwh(p, REGAS_PREF)
  )

# ── Pre-compute callout values at current Brent ───────────────────────────────
at_now <- function(col) approx(curve$brent, curve[[col]], xout = CURRENT_BRENT)$y
lsfo_now      <- at_now("lsfo_ckwh")
lng_04_now    <- at_now("lng_04_ckwh")
lng_lc_now    <- at_now("lng_lc_ckwh")
lng_pref_now  <- at_now("lng_pref_ckwh")

brent_range <- range(curve$brent)

# ── Legend key names ──────────────────────────────────────────────────────────
LBL_LSFO  <- "HECO LSFO (R3 contract, steam turbine, 11.1 MMBtu/MWh)"
LBL_04    <- "LNG full capacity — 0.4 mtpa HECO-only"
LBL_10    <- "LNG full capacity — 1.0 mtpa shared"
LBL_LC    <- "LNG at IGP land-constrained utilization (~55% of capacity)"
LBL_PREF  <- "LNG at IGP preferred utilization (~25% of capacity)"

# ── Build plot ────────────────────────────────────────────────────────────────
p <- ggplot(curve, aes(x = brent)) +

  # ── Solar+battery band ──────────────────────────────────────────────────────
  annotate("rect",
           xmin = brent_range[1], xmax = brent_range[2],
           ymin = SOLAR_LO, ymax = SOLAR_HI,
           fill = COL_SOLAR, alpha = 0.18) +
  annotate("segment",
           x = brent_range[1], xend = brent_range[2],
           y = SOLAR_MID, yend = SOLAR_MID,
           colour = COL_SOLAR, linewidth = 1.1) +
  annotate("text",
           x = 53, y = SOLAR_HI + 0.9,
           label = "Solar + 4-hr battery  (Hawaii LCOE, 9\u201311 \u00a2/kWh)",
           colour = COL_SOLAR_DK, size = 2.9, hjust = 0, fontface = "italic") +

  # ── LNG full-capacity ribbon (0.4 to 1.0 mtpa) ──────────────────────────────
  geom_ribbon(aes(ymin = lng_10_ckwh, ymax = lng_04_ckwh),
              fill = COL_LNG_FULL, alpha = 0.12) +

  # LNG 0.4 mtpa full capacity
  geom_line(aes(y = lng_04_ckwh, colour = LBL_04, linetype = LBL_04),
            linewidth = 0.9) +

  # LNG 1.0 mtpa full capacity
  geom_line(aes(y = lng_10_ckwh, colour = LBL_10, linetype = LBL_10),
            linewidth = 0.9) +

  # ── IGP utilization lines ────────────────────────────────────────────────────
  geom_line(aes(y = lng_lc_ckwh,   colour = LBL_LC,   linetype = LBL_LC),
            linewidth = 1.0) +
  geom_line(aes(y = lng_pref_ckwh, colour = LBL_PREF, linetype = LBL_PREF),
            linewidth = 1.0) +

  # ── LSFO (drawn last so it sits on top) ─────────────────────────────────────
  geom_line(aes(y = lsfo_ckwh, colour = LBL_LSFO, linetype = LBL_LSFO),
            linewidth = 1.3) +

  # ── Current Brent reference line ────────────────────────────────────────────
  geom_vline(xintercept = CURRENT_BRENT,
             colour = COL_VLINE, linewidth = 0.8, linetype = "solid") +
  annotate("text",
           x = CURRENT_BRENT - 1.5, y = 32,
           label = "Current spot\n(Mar 31 '26\n~$113/bbl)",
           size = 2.6, colour = COL_VLINE, hjust = 1, vjust = 1, fontface = "bold") +

  # ── Callout dots and labels at current Brent ─────────────────────────────────
  # LSFO
  annotate("point", x = CURRENT_BRENT, y = lsfo_now, colour = COL_LSFO, size = 2.5) +
  annotate("text",  x = CURRENT_BRENT + 1.5, y = lsfo_now,
           label = sprintf("%.1f \u00a2", lsfo_now),
           size = 2.7, colour = COL_LSFO, hjust = 0) +
  # LNG IGP preferred
  annotate("point", x = CURRENT_BRENT, y = lng_pref_now, colour = COL_LNG_PREF, size = 2.5) +
  annotate("text",  x = CURRENT_BRENT + 1.5, y = lng_pref_now,
           label = sprintf("%.1f \u00a2", lng_pref_now),
           size = 2.7, colour = COL_LNG_PREF, hjust = 0) +
  # LNG IGP land-constrained
  annotate("point", x = CURRENT_BRENT, y = lng_lc_now, colour = COL_LNG_LC, size = 2.5) +
  annotate("text",  x = CURRENT_BRENT + 1.5, y = lng_lc_now,
           label = sprintf("%.1f \u00a2", lng_lc_now),
           size = 2.7, colour = COL_LNG_LC, hjust = 0) +
  # LNG 0.4 mtpa full
  annotate("point", x = CURRENT_BRENT, y = lng_04_now, colour = COL_LNG_FULL, size = 2.5) +
  annotate("text",  x = CURRENT_BRENT + 1.5, y = lng_04_now,
           label = sprintf("%.1f \u00a2", lng_04_now),
           size = 2.7, colour = COL_LNG_FULL, hjust = 0) +

  # ── Scales ───────────────────────────────────────────────────────────────────
  scale_colour_manual(
    name   = NULL,
    values = setNames(
      c(COL_LSFO, COL_LNG_FULL, COL_LNG_FULL, COL_LNG_LC, COL_LNG_PREF),
      c(LBL_LSFO, LBL_04,      LBL_10,       LBL_LC,     LBL_PREF)
    )
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = setNames(
      c("solid", "dashed", "dotdash", "dashed", "solid"),
      c(LBL_LSFO, LBL_04,  LBL_10,   LBL_LC,   LBL_PREF)
    )
  ) +
  scale_x_continuous(
    labels = dollar_format(prefix = "$", suffix = "/bbl"),
    breaks = seq(50, 150, by = 10),
    expand = expansion(add = c(1, 3))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " \u00a2"),
    breaks = seq(4, 32, by = 2),
    limits = c(4, 33),
    expand = expansion(mult = 0.01)
  ) +
  labs(
    x = "Brent crude oil price ($/barrel)",
    y = "Fuel cost (\u00a2/kWh)",
    caption = paste0(
      "Notes: LSFO uses R3 contract steady-state model (UHERO) with steam-turbine heat rate (11.09 MMBtu/MWh). ",
      "LNG uses HSEO/FGE (Aug 2024)\nindicative formula: 0.118 \u00d7 Brent + $0.60/MMBtu commodity, ",
      "plus regasification with CCGT heat rate (6.5 MMBtu/MWh). ",
      "Full-capacity regas adder: $3.93/MMBtu (0.4 mtpa)\nand $1.68/MMBtu (1.0 mtpa). ",
      "FSRU fixed costs (vessel lease, crew, maintenance) are largely independent of throughput volume, ",
      "so per-MMBtu regas costs\nscale inversely with utilization: ",
      "IGP land-constrained (~55% of 0.4 mtpa capacity) \u2192 $7.15/MMBtu; ",
      "IGP preferred (~25%) \u2192 $15.72/MMBtu.\n",
      "Utilization rates from pv_savings_sketch.R (IGP preferred: 207 MW avg = 1,813 GWh/yr; ",
      "land-constrained: 308 MW = 2,698 GWh/yr; 0.4 mtpa capacity \u2248 3,200 GWh/yr at CCGT rates).\n",
      "Solar+battery: NREL ATB 2030 moderate (~$70/MWh mainland) adjusted +62% for Hawaii ",
      "(Leidos/EIA 2016) = 9\u201311 \u00a2/kWh; zero fuel-price risk. ",
      "Current Brent: $112.85/bbl (March 31, 2026)."
    )
  ) +
  brief_theme +
  theme(
    legend.position       = "bottom",
    legend.box            = "vertical",
    plot.caption          = element_text(size = 6.5, colour = "grey40",
                                         hjust = 0, margin = margin(t = 6)),
    plot.caption.position = "plot"
  ) +
  guides(
    colour   = guide_legend(ncol = 1, override.aes = list(linewidth = 1.1)),
    linetype = guide_legend(ncol = 1)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
quartz_pdf <- function(filename, width, height, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = "white")
}
ggsave(file.path(out_dir, "fig_fuel_cost_comparison.png"),
       p, width = 9, height = 7, dpi = 200, bg = "white")
ggsave(file.path(out_dir, "fig_fuel_cost_comparison.pdf"),
       p, width = 9, height = 7, device = quartz_pdf)
message("Saved: fig_fuel_cost_comparison (.png / .pdf)")
