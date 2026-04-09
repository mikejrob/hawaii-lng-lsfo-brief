## fig_fuel_cost_bands.R
## Fuel-cost comparison — LNG vs. LSFO vs. solar+battery
##
## Two figure designs are generated:
##
## Design A — single-panel band style (opt1, opt2)
##   Each LNG band spans CT (upper, solid) to CC (lower, dashed).
##   LSFO shown at existing steam turbine heat rate (11.1 MMBtu/MWh).
##   opt1: 4 LNG scenarios  opt2: 3 scenarios (drops 1.0 mtpa shared)
##
## Design B — two-panel (twopanel) ** main figure **
##   Left panel : CCGT (6.5 MMBtu/MWh)   Right panel: CT (9.5 MMBtu/MWh)
##   Each panel shows:
##     LSFO — existing steam turbines (baseline, same in both panels)
##     LSFO — same technology as LNG  (grey dashed; isolates fuel-price difference)
##     LNG scenarios at the panel heat rate (colored lines)
##     Solar+battery band (amber)
##
## LNG scenario naming:
##   "HECO high solar" = IGP preferred         (~25% avg FSRU utilization)
##   "HECO low solar"  = IGP land-constrained  (~55% avg FSRU utilization)
##   Note in caption explains origin and land-constraint basis.

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

LNG_SLOPE <- 0.118       # Brent-indexed commodity slope  (HSEO/FGE Aug 2024)
LNG_INT   <- 0.60        # Brent-indexed commodity intercept ($/MMBtu)

# Regasification adders — fixed costs per MMBtu scale inversely with utilization
REGAS_04   <- 3.93                # $/MMBtu, 0.4 mtpa at full capacity (FGE Aug 2024)
REGAS_10   <- 1.68                # $/MMBtu, 1.0 mtpa at full capacity (FGE Aug 2024)
REGAS_LC   <- REGAS_04 / 0.55    # $/MMBtu, HECO low solar  (~55% of 0.4 mtpa)
REGAS_PREF <- REGAS_04 / 0.25    # $/MMBtu, HECO high solar (~25% of 0.4 mtpa)

SOLAR_LO   <-  9.0  # cents/kWh, all-in levelized cost (Hawaii, Leidos/EIA 2016 low)
SOLAR_HI   <- 11.0  # cents/kWh (Hawaii high)
SOLAR_MID  <- 10.0  # midpoint for centre line

CURRENT_BRENT <- 112.85   # March 31, 2026 front-month (Investing.com)

COL_LSFO     <- "#333333"
COL_LSFO_GRY <- "#888888"   # LSFO at matched technology
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
    brent      = p,
    lsfo_mmbtu = heco_lsfo_bbl / LSFO_BBL,          # $/MMBtu — used to compute at any HR
    lsfo       = lsfo_mmbtu * STEAM_HR / 10,          # ¢/kWh — existing steam
    lsfo_cc    = lsfo_mmbtu * CC_HR    / 10,          # ¢/kWh — CC heat rate
    lsfo_ct    = lsfo_mmbtu * CT_HR    / 10,          # ¢/kWh — CT heat rate
    # Full capacity at nameplate throughput
    ct_04    = lng_ckwh(p, REGAS_04,   CT_HR),
    cc_04    = lng_ckwh(p, REGAS_04,   CC_HR),
    ct_10    = lng_ckwh(p, REGAS_10,   CT_HR),
    cc_10    = lng_ckwh(p, REGAS_10,   CC_HR),
    # IGP utilization scenarios (renamed: low/high solar)
    ct_lc    = lng_ckwh(p, REGAS_LC,   CT_HR),
    cc_lc    = lng_ckwh(p, REGAS_LC,   CC_HR),
    ct_pref  = lng_ckwh(p, REGAS_PREF, CT_HR),
    cc_pref  = lng_ckwh(p, REGAS_PREF, CC_HR)
  )

# ── Scenario specifications ───────────────────────────────────────────────────
# key   — suffix used to look up ct_<key> and cc_<key> columns
# label — full legend text
# short — right-margin label for band plot (2 lines)
# col   — fill / edge colour
# ct/cc — column names in `curve` (derived from key)
# alp   — ribbon fill alpha for band plot
SCENARIOS <- list(
  list(
    key       = "04",
    label     = "LNG \u2014 best case (fully utilized)",
    rhs_label = "LNG: best case\n(fully utilized)",
    short     = "best case\n(full use)",
    col       = "#27ae60",
    ct        = "ct_04", cc = "cc_04",
    alp       = 0.18
  ),
  list(
    key       = "10",
    label     = "LNG 1.0 mtpa \u2014 full capacity (shared)",
    rhs_label = "LNG: 1.0 mtpa\n(shared)",
    short     = "1.0 mtpa\nfull",
    col       = "#16a085",
    ct        = "ct_10", cc = "cc_10",
    alp       = 0.18
  ),
  list(
    key       = "lc",
    label     = "LNG \u2014 low solar buildout (~55% use)",
    rhs_label = "LNG: low solar\n(~55% use)",
    short     = "HECO\nlow solar",
    col       = "#2980b9",
    ct        = "ct_lc", cc = "cc_lc",
    alp       = 0.28
  ),
  list(
    key       = "pref",
    label     = "LNG \u2014 high solar buildout (~25% use)",
    rhs_label = "LNG: high solar\n(~25% use)",
    short     = "HECO\nhigh solar",
    col       = "#8e44ad",
    ct        = "ct_pref", cc = "cc_pref",
    alp       = 0.30
  )
)

# ── Design A: single-panel band plot ─────────────────────────────────────────
# show_note: TRUE → standalone (with CT/CC key annotation + caption)
#            FALSE → document-embedded (note is in the figure caption / text)
make_bands_plot <- function(scens, show_note = TRUE) {

  brent_rng <- range(curve$brent)
  lsfo_now  <- approx(curve$brent, curve$lsfo, xout = CURRENT_BRENT)$y
  lsfo_rhs  <- approx(curve$brent, curve$lsfo, xout = 150)$y

  # Per-scenario data frames for ribbons
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

  # Right-margin y positions (band midpoint at Brent = 150)
  rhs_y <- sapply(scens, function(sc) {
    lo150 <- approx(curve$brent, curve[[sc$cc]], xout = 150)$y
    hi150 <- approx(curve$brent, curve[[sc$ct]], xout = 150)$y
    (lo150 + hi150) / 2
  })

  # ── Base: solar band ──────────────────────────────────────────────────────
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

  # ── LNG bands (back-to-front so narrower bands sit on top) ────────────────
  for (i in seq_along(scens)) {
    sc  <- scens[[i]]
    dat <- scen_dats[[i]]
    p <- p +
      geom_ribbon(data = dat,
                  aes(x = brent, ymin = lo, ymax = hi, fill = label),
                  alpha = sc$alp) +
      geom_line(data = dat, aes(x = brent, y = hi),
                colour = sc$col, linewidth = 0.70, linetype = "solid") +
      geom_line(data = dat, aes(x = brent, y = lo),
                colour = sc$col, linewidth = 0.65, linetype = "dashed")
  }

  # ── LSFO steam (existing) ─────────────────────────────────────────────────
  p <- p +
    geom_line(data = curve, aes(x = brent, y = lsfo),
              colour = COL_LSFO, linewidth = 1.4)

  # ── Current Brent reference ───────────────────────────────────────────────
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

  # ── CT/CC edge key (standalone only) ─────────────────────────────────────
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

  # ── Right-margin band labels ──────────────────────────────────────────────
  rhs_layers <- lapply(seq_along(scens), function(i) {
    annotate("text",
             x = 152, y = rhs_y[i],
             label = scens[[i]]$short,
             colour = scens[[i]]$col,
             hjust = 0, vjust = 0.5, size = 2.6, lineheight = 0.9)
  })
  p <- p + rhs_layers

  p <- p +
    annotate("text",
             x = 152, y = lsfo_rhs,
             label = "HECO LSFO\n(steam, 11.1\nMMBtu/MWh)",
             colour = COL_LSFO, hjust = 0, vjust = 0.5, size = 2.6, lineheight = 0.9)

  # ── Scales, labels ────────────────────────────────────────────────────────
  caption_txt <- paste0(
    "LNG: HSEO/FGE (Aug 2024) Brent-indexed formula (0.118 \u00d7 Brent + $0.60/MMBtu) ",
    "plus regasification. Each band spans simple-cycle CT (9.5 MMBtu/MWh, upper edge) to ",
    "CCGT (6.5 MMBtu/MWh, lower edge).\n",
    "Regasification at full capacity: $3.93/MMBtu (0.4 mtpa), $1.68/MMBtu (1.0 mtpa). ",
    "FSRU costs are largely fixed so per-MMBtu cost scales inversely with utilization:\n",
    "HECO low solar (~55% avg) \u2192 $7.15/MMBtu; ",
    "HECO high solar (~25% avg) \u2192 $15.72/MMBtu. ",
    "(Low solar = IGP land-constrained; high solar = IGP preferred — land constraints limit ",
    "the renewable buildout pace in the low-solar scenario.)\n",
    "LSFO: R3 contract steady-state model (UHERO), steam turbine (11.1 MMBtu/MWh). ",
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
    coord_cartesian(xlim = c(49, 151), ylim = c(4, 36), clip = "off") +
    labs(
      x       = "Brent crude oil price ($/barrel)",
      y       = "Fuel cost (\u00a2/kWh)",
      caption = if (show_note) caption_txt else NULL
    ) +
    brief_theme +
    guides(fill = guide_legend(
      nrow = 2,
      override.aes = list(alpha = 0.5, colour = NA, linetype = 0)
    ))
}

# ── Design B: two-panel figure (stacked) ──────────────────────────────────────
# Top panel:    CCGT (6.5 MMBtu/MWh)
# Bottom panel: CT   (9.5 MMBtu/MWh)
#
# Each panel shows:
#   LSFO — existing steam (black solid, thick) — same in both; the status quo baseline
#   LSFO — same technology as LNG (grey dashed) — isolates pure fuel-price difference
#   LNG scenarios at the panel heat rate (colored solid lines)
#   Solar + battery band (amber)
#
# Lines are labeled directly on the right margin (no legend).
# scens_3: list of 3 scenarios to include (typically SCENARIOS[c(1,3,4)])

# ── Helper: nudge y-positions so labels don't overlap ────────────────────────
nudge_labels <- function(y_vals, min_gap = 0.85, max_iter = 60) {
  ys <- y_vals
  for (iter in seq_len(max_iter)) {
    ord <- order(ys)
    changed <- FALSE
    for (i in seq_len(length(ys) - 1)) {
      a <- ord[i]; b <- ord[i + 1]
      gap <- ys[b] - ys[a]
      if (gap < min_gap) {
        push <- (min_gap - gap) / 2
        ys[a] <- ys[a] - push
        ys[b] <- ys[b] + push
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  ys
}

make_two_panel_plot <- function(scens_3, show_note = TRUE) {

  PANEL_CC <- "CCGT \u2014 combined-cycle (6.5 MMBtu/MWh)"
  PANEL_CT <- "CT \u2014 simple-cycle combustion turbine (9.5 MMBtu/MWh)"
  panel_names <- c(PANEL_CC, PANEL_CT)

  nb <- nrow(curve)

  # ── Long-format line data ─────────────────────────────────────────────────
  rows <- list(
    # LSFO steam — identical in both panels
    data.frame(
      brent  = rep(curve$brent, 2),
      value  = rep(curve$lsfo, 2),
      series = "LSFO \u2014 existing steam turbines (11.1 MMBtu/MWh)",
      panel  = rep(panel_names, each = nb),
      stringsAsFactors = FALSE
    ),
    # LSFO at CC heat rate (top panel)
    data.frame(
      brent  = curve$brent,
      value  = curve$lsfo_cc,
      series = "LSFO \u2014 same technology as LNG",
      panel  = PANEL_CC,
      stringsAsFactors = FALSE
    ),
    # LSFO at CT heat rate (bottom panel)
    data.frame(
      brent  = curve$brent,
      value  = curve$lsfo_ct,
      series = "LSFO \u2014 same technology as LNG",
      panel  = PANEL_CT,
      stringsAsFactors = FALSE
    )
  )

  for (sc in scens_3) {
    rows[[length(rows) + 1]] <- data.frame(
      brent  = rep(curve$brent, 2),
      value  = c(curve[[sc$cc]], curve[[sc$ct]]),
      series = sc$label,
      panel  = rep(panel_names, each = nb),
      stringsAsFactors = FALSE
    )
  }

  plot_df <- do.call(rbind, rows)

  series_order <- c(
    "LSFO \u2014 existing steam turbines (11.1 MMBtu/MWh)",
    "LSFO \u2014 same technology as LNG",
    sapply(scens_3, `[[`, "label")
  )
  plot_df$series <- factor(plot_df$series, levels = series_order)
  plot_df$panel  <- factor(plot_df$panel,  levels = panel_names)

  # ── Aesthetic mappings ────────────────────────────────────────────────────
  n3 <- length(scens_3)
  col_vals <- c(
    "LSFO \u2014 existing steam turbines (11.1 MMBtu/MWh)" = COL_LSFO,
    "LSFO \u2014 same technology as LNG"                    = COL_LSFO_GRY,
    setNames(sapply(scens_3, `[[`, "col"), sapply(scens_3, `[[`, "label"))
  )
  lty_vals <- c(
    "LSFO \u2014 existing steam turbines (11.1 MMBtu/MWh)" = "solid",
    "LSFO \u2014 same technology as LNG"                    = "dashed",
    setNames(rep("solid", n3), sapply(scens_3, `[[`, "label"))
  )
  lw_vals <- c(
    "LSFO \u2014 existing steam turbines (11.1 MMBtu/MWh)" = 1.40,
    "LSFO \u2014 same technology as LNG"                    = 0.80,
    setNames(rep(0.85, n3), sapply(scens_3, `[[`, "label"))
  )

  brent_rng <- range(curve$brent)
  label_x   <- 150   # x position for right-margin labels

  # ── Direct line labels (right margin) ────────────────────────────────────
  # Raw y-values at label_x for each series in each panel
  lsfo_at <- function(x) approx(curve$brent, curve$lsfo,    xout = x)$y
  lsfo_cc_at <- function(x) approx(curve$brent, curve$lsfo_cc, xout = x)$y
  lsfo_ct_at <- function(x) approx(curve$brent, curve$lsfo_ct, xout = x)$y
  lng_at  <- function(col, x) approx(curve$brent, curve[[col]], xout = x)$y

  # Label text for each series (short, 2-line max)
  lsfo_steam_lbl <- "LSFO\n(steam, existing)"
  lsfo_match_lbl <- "LSFO\n(same tech.)"
  lng_labels      <- sapply(scens_3, `[[`, "rhs_label")
  lng_names       <- sapply(scens_3, `[[`, "label")
  lng_cols_cc     <- sapply(scens_3, `[[`, "cc")
  lng_cols_ct     <- sapply(scens_3, `[[`, "ct")
  lng_colors      <- sapply(scens_3, `[[`, "col")

  # CC panel: compute raw y, nudge, build data frame
  y_cc_raw <- c(
    lsfo_steam = lsfo_at(label_x),
    lsfo_match = lsfo_cc_at(label_x),
    setNames(sapply(lng_cols_cc, lng_at, x = label_x), lng_names)
  )
  y_cc_nudged <- nudge_labels(y_cc_raw)
  label_cc_df <- data.frame(
    x      = label_x + 1,
    y      = y_cc_nudged,
    label  = c(lsfo_steam_lbl, lsfo_match_lbl, lng_labels),
    colour = c(COL_LSFO, COL_LSFO_GRY, lng_colors),
    panel  = factor(PANEL_CC, levels = panel_names),
    stringsAsFactors = FALSE
  )

  # CT panel
  y_ct_raw <- c(
    lsfo_steam = lsfo_at(label_x),
    lsfo_match = lsfo_ct_at(label_x),
    setNames(sapply(lng_cols_ct, lng_at, x = label_x), lng_names)
  )
  y_ct_nudged <- nudge_labels(y_ct_raw)
  label_ct_df <- data.frame(
    x      = label_x + 1,
    y      = y_ct_nudged,
    label  = c(lsfo_steam_lbl, lsfo_match_lbl, lng_labels),
    colour = c(COL_LSFO, COL_LSFO_GRY, lng_colors),
    panel  = factor(PANEL_CT, levels = panel_names),
    stringsAsFactors = FALSE
  )

  label_df <- rbind(label_cc_df, label_ct_df)

  # ── Solar band (both panels) ──────────────────────────────────────────────
  solar_band_df <- data.frame(
    panel = factor(panel_names, levels = panel_names),
    xmin = brent_rng[1], xmax = brent_rng[2],
    ymin = SOLAR_LO,     ymax = SOLAR_HI
  )
  solar_mid_df <- data.frame(
    panel = factor(panel_names, levels = panel_names),
    x = brent_rng[1], xend = brent_rng[2],
    y = SOLAR_MID,    yend = SOLAR_MID
  )
  solar_lbl_df <- data.frame(
    panel = factor(panel_names, levels = panel_names),
    x = 53, y = SOLAR_HI + 0.8,
    label = "Solar + 4-hr battery  (9\u201311 \u00a2/kWh)"
  )

  # Current Brent label (top panel only to avoid clutter)
  lsfo_now <- lsfo_at(CURRENT_BRENT)
  brent_lbl_df <- data.frame(
    panel = factor(PANEL_CC, levels = panel_names),
    x = CURRENT_BRENT - 1.5, y = 34.5,
    label = "Current\nspot\n~$113/bbl"
  )
  lsfo_dot_df <- data.frame(
    panel = factor(PANEL_CC, levels = panel_names),
    x = CURRENT_BRENT, y = lsfo_now
  )

  # ── Caption ───────────────────────────────────────────────────────────────
  caption_txt <- paste0(
    "LNG: HSEO/FGE (Aug 2024) Brent-indexed formula (0.118 \u00d7 Brent + $0.60/MMBtu) plus regasification. ",
    "Best case assumes terminal fully utilized at 0.4 mtpa (FGE August 2024 minimum viable scale); ",
    "regasification = $3.93/MMBtu. ",
    "Low solar buildout (~55% avg utilization) \u2192 $7.15/MMBtu; ",
    "high solar buildout (~25% avg utilization) \u2192 $15.72/MMBtu.\n",
    "Low/high solar correspond to the IGP land-constrained and IGP preferred scenarios respectively ",
    "(land-use constraints slow renewable deployment in the low-solar case). ",
    "LSFO: UHERO R3 steady-state model. \u2018LSFO \u2014 same technology\u2019 (grey dashed): LSFO cost at the ",
    "panel\u2019s heat rate \u2014 gap between the two LSFO lines = efficiency gain; ",
    "gap between grey dashed and colored LNG lines = fuel-price difference.\n",
    "Solar+battery: NREL ATB 2030 moderate + 62% Hawaii premium (Leidos/EIA 2016) = 9\u201311 \u00a2/kWh. ",
    "Current Brent: $112.85/bbl (Mar 31, 2026)."
  )

  # ── Build plot ────────────────────────────────────────────────────────────
  p <- ggplot() +
    # Solar band (both panels)
    geom_rect(data = solar_band_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = COL_SOLAR, alpha = 0.18, inherit.aes = FALSE) +
    geom_segment(data = solar_mid_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 colour = COL_SOLAR, linewidth = 1.0, inherit.aes = FALSE) +
    geom_text(data = solar_lbl_df,
              aes(x = x, y = y, label = label),
              colour = COL_SOLAR_DK, size = 2.7, hjust = 0,
              fontface = "italic", inherit.aes = FALSE) +
    # All fuel-cost lines
    geom_line(data = plot_df,
              aes(x = brent, y = value,
                  colour    = series,
                  linetype  = series,
                  linewidth = series)) +
    # Current Brent vertical (both panels)
    geom_vline(xintercept = CURRENT_BRENT,
               colour = "#c0392b", linewidth = 0.7) +
    # Current Brent label (top panel only)
    geom_text(data = brent_lbl_df,
              aes(x = x, y = y, label = label),
              colour = "#c0392b", size = 2.5, hjust = 1, vjust = 1,
              fontface = "bold", inherit.aes = FALSE) +
    # LSFO current-price dot (top panel only)
    geom_point(data = lsfo_dot_df,
               aes(x = x, y = y),
               colour = COL_LSFO, size = 2.5, inherit.aes = FALSE) +
    # Direct right-margin labels (outside clip area)
    geom_text(data = label_df,
              aes(x = x, y = y, label = label, colour = I(colour)),
              hjust = 0, vjust = 0.5, size = 2.55, lineheight = 0.88,
              inherit.aes = FALSE) +
    facet_wrap(~panel, ncol = 1) +
    # Scales — legend is suppressed; direct labels carry the information
    scale_colour_manual(name = NULL, values = col_vals, breaks = series_order,
                        guide = "none") +
    scale_linetype_manual(name = NULL, values = lty_vals, breaks = series_order,
                          guide = "none") +
    scale_linewidth_manual(name = NULL, values = lw_vals, breaks = series_order,
                           guide = "none") +
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
    coord_cartesian(ylim = c(4, 36), xlim = c(49, 151), clip = "off") +
    labs(
      x       = "Brent crude oil price ($/barrel)",
      y       = "Fuel cost (\u00a2/kWh)",
      caption = if (show_note) caption_txt else NULL
    ) +
    brief_theme +
    theme(
      legend.position  = "none",
      strip.text       = element_text(size = 9.5, face = "bold"),
      # Wide right margin to accommodate direct labels
      plot.margin      = margin(t = 6, r = 110, b = 6, l = 6, unit = "pt")
    )

  p
}

# ── Output helpers ────────────────────────────────────────────────────────────
quartz_pdf <- function(filename, width, height, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = "white")
}

save_bands <- function(p_standalone, p_doc, stem, w = 9.5, h = 7) {
  ggsave(file.path(out_dir, paste0(stem, ".png")),
         p_standalone, width = w, height = h, dpi = 200, bg = "white")
  ggsave(file.path(out_dir, paste0(stem, "_standalone.pdf")),
         p_standalone, width = w, height = h, device = quartz_pdf)
  ggsave(file.path(out_dir, paste0(stem, "_doc.pdf")),
         p_doc, width = w, height = h, device = quartz_pdf)
  ggsave(file.path(out_dir, paste0(stem, "_doc.png")),
         p_doc, width = w, height = h, dpi = 200, bg = "white")
  message("Saved: ", stem, "  (.png / _standalone.pdf / _doc.pdf / _doc.png)")
}

# ── Generate figures ──────────────────────────────────────────────────────────

# Design A — opt1: all 4 LNG scenarios
save_bands(
  make_bands_plot(SCENARIOS,             show_note = TRUE),
  make_bands_plot(SCENARIOS,             show_note = FALSE),
  "fig_fuel_cost_opt1"
)

# Design A — opt2: 3 scenarios (drop 1.0 mtpa shared)
save_bands(
  make_bands_plot(SCENARIOS[c(1, 3, 4)], show_note = TRUE),
  make_bands_plot(SCENARIOS[c(1, 3, 4)], show_note = FALSE),
  "fig_fuel_cost_opt2"
)

# Design B — two-panel stacked: 3 scenarios (best case + low solar + high solar)
# Stacked vertically (ncol=1): each panel gets full width, no x-axis label crowding.
# Wide right margin for direct line labels.
save_bands(
  make_two_panel_plot(SCENARIOS[c(1, 3, 4)], show_note = TRUE),
  make_two_panel_plot(SCENARIOS[c(1, 3, 4)], show_note = FALSE),
  "fig_fuel_cost_twopanel",
  w = 9.5, h = 11
)
