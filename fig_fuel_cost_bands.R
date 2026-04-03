## fig_fuel_cost_bands.R
## Fuel-cost comparison — LNG (CT-to-CCGT bands) vs. LSFO vs. solar+battery
##
## Each LNG band spans:
##   upper edge — simple-cycle combustion turbine (CT)  : 9.5 MMBtu/MWh
##   lower edge — combined-cycle gas turbine (CCGT)     : 6.5 MMBtu/MWh
## LSFO uses existing HECO steam turbines               : 11.09 MMBtu/MWh
##
## Output:
##   fig_fuel_cost_opt1.png  — 4 LNG scenarios (0.4 mtpa full, 1.0 mtpa full,
##                              IGP land-constrained, IGP preferred)
##   fig_fuel_cost_opt2.png  — 3 LNG scenarios (drops 1.0 mtpa shared)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

base_dir <- "/Users/mike/Library/CloudStorage/GoogleDrive-michael@ae4cast.com/Other computers/My Mac (1)/EIA/lng_vs_oil"
out_dir  <- file.path(base_dir, "brief_figures")
dir.create(out_dir, showWarnings = FALSE)

# ── Theme ─────────────────────────────────────────────────────────────────────
brief_theme <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_line(colour = "grey90"),
    axis.title            = element_text(size = 10),
    legend.position       = "bottom",
    legend.key.size       = unit(0.65, "cm"),
    legend.text           = element_text(size = 9),
    legend.spacing.x      = unit(0.4, "cm"),
    strip.background      = element_blank(),
    plot.margin           = margin(t = 6, r = 85, b = 6, l = 6, unit = "pt"),
    plot.caption          = element_text(size = 6.5, colour = "grey40",
                                         hjust = 0, margin = margin(t = 6)),
    plot.caption.position = "plot"
  )

# ── Physical constants ────────────────────────────────────────────────────────
LSFO_BBL  <- 6.22        # MMBtu per barrel (residual fuel oil)
STEAM_HR  <- 11.0895     # MMBtu/MWh — existing HECO steam turbines
CC_HR     <-  6.5        # MMBtu/MWh — combined-cycle gas turbine (NREL ATB 2024)
CT_HR     <-  9.5        # MMBtu/MWh — simple-cycle combustion turbine (NREL ATB 2024)

LNG_SLOPE <- 0.118       # Brent-indexed commodity: slope (HSEO/FGE Aug 2024)
LNG_INT   <- 0.60        # Brent-indexed commodity: intercept ($/MMBtu)

# Regasification adders — all costs assumed fixed (vessel lease, crew, maintenance
# are largely independent of throughput volume), so per-MMBtu cost scales as
# nameplate_adder / utilization_fraction.
REGAS_04   <- 3.93                # $/MMBtu, 0.4 mtpa at full capacity (FGE Aug 2024)
REGAS_10   <- 1.68                # $/MMBtu, 1.0 mtpa at full capacity (FGE Aug 2024)
REGAS_LC   <- REGAS_04 / 0.55    # $/MMBtu, IGP land-constrained (~55% of 0.4 mtpa)
REGAS_PREF <- REGAS_04 / 0.25    # $/MMBtu, IGP preferred        (~25% of 0.4 mtpa)
# Utilization fractions from pv_savings_sketch.R:
#   igp_pref: 207 MW × 8.76 h = 1,813 GWh/yr  ≈ 25% of 3,200 GWh 0.4-mtpa capacity
#   igp_lc:   308 MW × 8.76 h = 2,698 GWh/yr  ≈ 55% of 3,200 GWh capacity

SOLAR_LO   <-  9.0  # cents/kWh, all-in levelized cost (Hawaii, Leidos/EIA 2016 low)
SOLAR_HI   <- 11.0  # cents/kWh (Hawaii high)
SOLAR_MID  <- 10.0  # midpoint for center line

CURRENT_BRENT <- 112.85   # March 31, 2026 front-month (Investing.com)

COL_LSFO     <- "#333333"
COL_SOLAR    <- "#f39c12"
COL_SOLAR_DK <- "#b07300"

# ── Load LSFO steady-state curve ─────────────────────────────────────────────
raw <- fread(file.path(base_dir, "heco_lsfo_long_run_curve.csv"))
names(raw) <- trimws(names(raw))

# Helper: LNG all-in fuel cost (cents/kWh)
lng_ckwh <- function(brent, regas, hr) (LNG_SLOPE * brent + LNG_INT + regas) * hr / 10

curve <- as_tibble(raw) |>
  dplyr::filter(p >= 50, p <= 150) |>
  dplyr::transmute(
    brent    = p,
    lsfo     = (heco_lsfo_bbl / LSFO_BBL) * STEAM_HR / 10,
    # Full capacity at nameplate throughput
    ct_04    = lng_ckwh(p, REGAS_04,   CT_HR),
    cc_04    = lng_ckwh(p, REGAS_04,   CC_HR),
    ct_10    = lng_ckwh(p, REGAS_10,   CT_HR),
    cc_10    = lng_ckwh(p, REGAS_10,   CC_HR),
    # IGP utilization scenarios
    ct_lc    = lng_ckwh(p, REGAS_LC,   CT_HR),
    cc_lc    = lng_ckwh(p, REGAS_LC,   CC_HR),
    ct_pref  = lng_ckwh(p, REGAS_PREF, CT_HR),
    cc_pref  = lng_ckwh(p, REGAS_PREF, CC_HR)
  )

# ── Scenario specifications ───────────────────────────────────────────────────
# label       — full legend text
# short       — right-margin label (2 lines max)
# col         — fill / edge colour
# ct / cc     — column names in `curve` for CT (top) and CC (bottom) band edges
# alp         — ribbon fill alpha (lower for "reference" scenarios)
SCENARIOS <- list(
  list(
    label = "LNG 0.4 mtpa — full capacity",
    short = "0.4 mtpa\nfull",
    col   = "#27ae60",
    ct    = "ct_04", cc = "cc_04",
    alp   = 0.18
  ),
  list(
    label = "LNG 1.0 mtpa — full capacity (shared)",
    short = "1.0 mtpa\nfull",
    col   = "#16a085",
    ct    = "ct_10", cc = "cc_10",
    alp   = 0.18
  ),
  list(
    label = "LNG — IGP land-constrained (~55% of capacity)",
    short = "IGP land-\nconstrained",
    col   = "#2980b9",
    ct    = "ct_lc", cc = "cc_lc",
    alp   = 0.28
  ),
  list(
    label = "LNG — IGP preferred (~25% of capacity)",
    short = "IGP\npreferred",
    col   = "#8e44ad",
    ct    = "ct_pref", cc = "cc_pref",
    alp   = 0.30
  )
)

# ── Plot-building function ────────────────────────────────────────────────────
# show_note: if TRUE, adds the lower-left CT/CC edge key.
#   TRUE  → standalone figures (PNG + PDF) shared independently of the document
#   FALSE → document-embedded version (key is explained in figure caption + text)
make_bands_plot <- function(scens, show_note = TRUE) {

  brent_rng <- range(curve$brent)
  lsfo_now  <- approx(curve$brent, curve$lsfo, xout = CURRENT_BRENT)$y
  lsfo_rhs  <- approx(curve$brent, curve$lsfo, xout = 150)$y

  # Build per-scenario data frames and right-margin label positions
  scen_dats <- lapply(scens, function(sc) {
    data.frame(
      brent = curve$brent,
      lo    = curve[[sc$cc]],
      hi    = curve[[sc$ct]],
      label = sc$label
    )
  })
  fill_vals <- setNames(sapply(scens, `[[`, "col"),
                        sapply(scens, `[[`, "label"))

  # Right-margin y positions (midpoint of band at Brent = 150)
  rhs_y <- sapply(scens, function(sc) {
    lo150 <- approx(curve$brent, curve[[sc$cc]], xout = 150)$y
    hi150 <- approx(curve$brent, curve[[sc$ct]], xout = 150)$y
    (lo150 + hi150) / 2
  })

  # ── Base plot: solar band ──────────────────────────────────────────────────
  p <- ggplot() +
    annotate("rect",
             xmin = brent_rng[1], xmax = brent_rng[2],
             ymin = SOLAR_LO, ymax = SOLAR_HI,
             fill = COL_SOLAR, alpha = 0.18) +
    annotate("segment",
             x = brent_rng[1], xend = brent_rng[2],
             y = SOLAR_MID, yend = SOLAR_MID,
             colour = COL_SOLAR, linewidth = 1.0) +
    annotate("text",
             x = 53, y = SOLAR_HI + 0.8,
             label = "Solar + 4-hr battery  (Hawaii, 9\u201311 \u00a2/kWh)",
             colour = COL_SOLAR_DK, size = 2.9, hjust = 0, fontface = "italic")

  # ── LNG scenario bands (drawn back-to-front so IGP layers sit on top) ─────
  for (i in seq_along(scens)) {
    sc  <- scens[[i]]
    dat <- scen_dats[[i]]
    p <- p +
      # Filled ribbon (fill mapped to label → enters legend)
      geom_ribbon(data = dat,
                  aes(x = brent, ymin = lo, ymax = hi, fill = label),
                  alpha = sc$alp) +
      # CT upper edge — solid line (fixed colour, no legend entry)
      geom_line(data = dat, aes(x = brent, y = hi),
                colour = sc$col, linewidth = 0.70, linetype = "solid") +
      # CC lower edge — dashed line (fixed colour, no legend entry)
      geom_line(data = dat, aes(x = brent, y = lo),
                colour = sc$col, linewidth = 0.65, linetype = "dashed")
  }

  # ── LSFO ──────────────────────────────────────────────────────────────────
  p <- p +
    geom_line(data = curve, aes(x = brent, y = lsfo),
              colour = COL_LSFO, linewidth = 1.4)

  # ── Current Brent reference ────────────────────────────────────────────────
  p <- p +
    geom_vline(xintercept = CURRENT_BRENT,
               colour = "#c0392b", linewidth = 0.8) +
    annotate("text",
             x = CURRENT_BRENT - 1.5, y = 35,
             label = "Current spot\n(Mar 31 '26\n~$113/bbl)",
             size = 2.6, colour = "#c0392b", hjust = 1, vjust = 1, fontface = "bold") +
    annotate("point",
             x = CURRENT_BRENT, y = lsfo_now,
             colour = COL_LSFO, size = 2.5) +
    annotate("text",
             x = CURRENT_BRENT + 1.5, y = lsfo_now,
             label = sprintf("LSFO:\n%.1f \u00a2", lsfo_now),
             colour = COL_LSFO, size = 2.6, hjust = 0, lineheight = 0.9)

  # ── CT/CC edge key (lower-left corner) — standalone use only ─────────────
  if (show_note) {
    p <- p +
      annotate("text",
               x = 51, y = 5.2,
               label = paste0(
                 "\u2014  upper edge = simple-cycle CT (9.5 MMBtu/MWh)\n",
                 "- -  lower edge = combined-cycle CCGT (6.5 MMBtu/MWh)\n",
                 "     HECO steam turbine: 11.1 MMBtu/MWh"
               ),
               colour = "grey40", size = 2.5, hjust = 0, vjust = 0,
               fontface = "italic", lineheight = 1.1)
  }

  # ── Right-margin band labels (outside clip area) ──────────────────────────
  rhs_layers <- lapply(seq_along(scens), function(i) {
    annotate("text",
             x = 152, y = rhs_y[i],
             label = scens[[i]]$short,
             colour = scens[[i]]$col,
             hjust = 0, vjust = 0.5, size = 2.6, lineheight = 0.9)
  })
  p <- p + rhs_layers

  # LSFO right-margin label
  p <- p +
    annotate("text",
             x = 152, y = lsfo_rhs,
             label = "HECO LSFO\n(steam, 11.1\nMMBtu/MWh)",
             colour = COL_LSFO, hjust = 0, vjust = 0.5, size = 2.6, lineheight = 0.9)

  # ── Scales, axes, labels ──────────────────────────────────────────────────
  caption_txt <- paste0(
    "LNG: HSEO/FGE (Aug 2024) Brent-indexed formula (0.118 \u00d7 Brent + $0.60/MMBtu) ",
    "plus regasification. Each band spans simple-cycle CT (9.5 MMBtu/MWh, upper edge) to ",
    "CCGT (6.5 MMBtu/MWh, lower edge).\n",
    "Regasification at full capacity: $3.93/MMBtu (0.4 mtpa), $1.68/MMBtu (1.0 mtpa). ",
    "FSRU costs are largely fixed (vessel lease, crew, maintenance), ",
    "so per-MMBtu cost scales inversely with utilization:\n",
    "IGP land-constrained (~55% avg, 308 MW = 2,698 GWh/yr) \u2192 $7.15/MMBtu; ",
    "IGP preferred (~25% avg, 207 MW = 1,813 GWh/yr) \u2192 $15.72/MMBtu. ",
    "LSFO: R3 contract steady-state model (UHERO), steam turbine (11.1 MMBtu/MWh).\n",
    "Solar+battery: NREL ATB 2030 moderate (~$70/MWh mainland) + 62% Hawaii premium ",
    "(Leidos/EIA 2016) = 9\u201311 \u00a2/kWh. Current Brent: $112.85/bbl (Mar 31, 2026)."
  )

  p +
    scale_fill_manual(name = NULL, values = fill_vals) +
    scale_x_continuous(
      labels = dollar_format(prefix = "$", suffix = "/bbl"),
      breaks = seq(50, 150, by = 10),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, " \u00a2"),
      breaks = seq(4, 34, by = 2),
      expand = expansion(mult = 0)
    ) +
    # Use coord_cartesian (not scale limits) to set visible window so that
    # out-of-range annotations (right-margin labels at x=152) are NOT dropped
    coord_cartesian(xlim = c(49, 151), ylim = c(4, 36), clip = "off") +
    labs(
      x       = "Brent crude oil price ($/barrel)",
      y       = "Fuel cost (\u00a2/kWh)",
      caption = caption_txt
    ) +
    brief_theme +
    guides(fill = guide_legend(
      nrow = 2,
      override.aes = list(alpha = 0.5, colour = NA, linetype = 0)
    ))
}

# ── Generate figures ──────────────────────────────────────────────────────────
# Each variant is produced in three formats:
#   *_standalone.pdf — with note, PDF quality (for sharing independent of doc)
#   *.png            — with note, PNG (existing; used in markdown and for sharing)
#   *_doc.pdf        — without note, PDF (for embedding in the published document)

quartz_pdf <- function(filename, width, height, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = "white")
}

save_bands <- function(p_standalone, p_doc, stem) {
  ggsave(file.path(out_dir, paste0(stem, ".png")),
         p_standalone, width = 9.5, height = 7, dpi = 200, bg = "white")
  ggsave(file.path(out_dir, paste0(stem, "_standalone.pdf")),
         p_standalone, width = 9.5, height = 7, device = quartz_pdf)
  ggsave(file.path(out_dir, paste0(stem, "_doc.pdf")),
         p_doc, width = 9.5, height = 7, device = quartz_pdf)
  message("Saved: ", stem, "  (.png / _standalone.pdf / _doc.pdf)")
}

# Option 1: all 4 LNG scenarios
save_bands(
  make_bands_plot(SCENARIOS,            show_note = TRUE),
  make_bands_plot(SCENARIOS,            show_note = FALSE),
  "fig_fuel_cost_opt1"
)

# Option 2: 3 scenarios — drop 1.0 mtpa shared (indices 1, 3, 4)
save_bands(
  make_bands_plot(SCENARIOS[c(1, 3, 4)], show_note = TRUE),
  make_bands_plot(SCENARIOS[c(1, 3, 4)], show_note = FALSE),
  "fig_fuel_cost_opt2"
)
