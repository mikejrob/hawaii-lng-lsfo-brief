## generate_brief_figures.R
## Generates all six figures for the UHERO LNG/LSFO brief.
## Outputs saved to brief_figures/ as 7×4.5 inch PNGs at 200 dpi.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lubridate)
  library(scales)
})

base_dir <- "/Users/mike/Library/CloudStorage/GoogleDrive-michael@ae4cast.com/Other computers/My Mac (1)/EIA/lng_vs_oil"
misc_dir <- "/Users/mike/Library/CloudStorage/GoogleDrive-michael@ae4cast.com/Other computers/My Mac (1)/EIA/miscData"
out_dir  <- file.path(base_dir, "brief_figures")
dir.create(out_dir, showWarnings = FALSE)

# ── Shared theme ──────────────────────────────────────────────────────────────
clr <- list(
  brent   = "#1a6faf",   # blue
  vlsfo   = "#e07b00",   # orange
  lsfo    = "#333333",   # near-black
  r1      = "#c0392b",   # red
  r3      = "#1a6faf",   # blue
  lng_b   = "#27ae60",   # green
  lng_hh  = "#8e44ad",   # purple
  savings = "#27ae60",
  loss    = "#c0392b",
  cumul   = "#1a6faf",
  hh      = "#e07b00",
  jlng    = "#27ae60"
)

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

quartz_pdf <- function(filename, width, height, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = "white")
}

save_fig <- function(p, name, w = 7, h = 4.5) {
  png_path <- file.path(out_dir, paste0(name, ".png"))
  pdf_path <- file.path(out_dir, paste0(name, ".pdf"))
  ggsave(png_path, p, width = w, height = h, dpi = 200, bg = "white")
  ggsave(pdf_path, p, width = w, height = h, device = quartz_pdf)
  message("Saved: ", png_path)
  message("Saved: ", pdf_path)
}

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 1: Brent, VLSFO, HECO LSFO (2022–2026)
# ─────────────────────────────────────────────────────────────────────────────
vlsfo_raw <- fread(file.path(base_dir, "vlsfo_prices.csv")) |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  filter(!is.na(singapore_vlsfo_usd_mt)) |>
  mutate(vlsfo_bbl = singapore_vlsfo_usd_mt / 7.4)   # ~0.85 kg/L density

# Monthly average VLSFO
vlsfo_mo <- vlsfo_raw |>
  mutate(month = floor_date(date, "month")) |>
  group_by(date = month) |>
  summarise(vlsfo_bbl = mean(vlsfo_bbl, na.rm = TRUE), .groups = "drop")

brent_mo <- fread(file.path(base_dir, "fred_poilbreusdm_brent_global.csv")) |>
  as_tibble() |>
  transmute(date = as.Date(observation_date), brent = POILBREUSDM) |>
  filter(date >= "2022-01-01")

heco_raw <- fread(file.path(misc_dir, "heco_lsfo.csv")) |>
  as_tibble() |>
  mutate(date = mdy(Date)) |>
  transmute(date, lsfo_bbl = Honolulu_LSFO) |>
  filter(!is.na(lsfo_bbl), date >= "2022-01-01") |>
  arrange(date)

fig1_dat <- brent_mo |>
  full_join(vlsfo_mo, by = "date") |>
  full_join(heco_raw, by = "date") |>
  filter(date >= "2022-01-01", date <= "2026-04-01") |>
  arrange(date) |>
  pivot_longer(c(brent, vlsfo_bbl, lsfo_bbl), names_to = "series", values_to = "price") |>
  filter(!is.na(price)) |>
  mutate(series = factor(series,
    levels = c("brent", "vlsfo_bbl", "lsfo_bbl"),
    labels = c("Brent crude (monthly avg)", "Singapore VLSFO spot (monthly avg)",
               "HECO reported LSFO price")))

p1 <- ggplot(fig1_dat, aes(date, price, colour = series, linetype = series)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(clr$brent, clr$vlsfo, clr$lsfo), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "solid"), name = NULL) +
  annotate("rect", xmin = as.Date("2024-03-01"), xmax = as.Date("2024-11-01"),
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.07) +
  annotate("text", x = as.Date("2024-07-01"), y = 145,
           label = "Contract\nrenegotiation", size = 3, colour = "steelblue4", hjust = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "/bbl"),
                     limits = c(50, 170)) +
  labs(x = NULL, y = "Price ($/barrel)") +
  brief_theme

save_fig(p1, "fig1_price_chain")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 2: HECO model fit by regime
# ─────────────────────────────────────────────────────────────────────────────
savings_dat <- fread(file.path(base_dir, "policy_historical_savings.csv")) |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  filter(!is.na(lsfo_bbl), lsfo_bbl > 0)

# Regime break points (approximate from model)
r1_end <- as.Date("2024-03-01")
r3_start <- as.Date("2024-11-01")

fig2_dat <- savings_dat |>
  mutate(regime = case_when(
    date < r1_end   ~ "R1 (pre-renegotiation)",
    date < r3_start ~ "R2 (transition)",
    TRUE            ~ "R3 (current contract)"
  ))

p2 <- ggplot(fig2_dat, aes(date)) +
  # Regime shading
  annotate("rect", xmin = as.Date("2024-03-01"), xmax = as.Date("2024-10-15"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.25) +
  annotate("text", x = as.Date("2024-06-15"), y = 137,
           label = "Transition\n(R2)", size = 3, colour = "grey40") +
  # Actual LSFO
  geom_line(aes(y = lsfo_bbl, colour = "HECO LSFO (actual)"), linewidth = 0.9) +
  # R1 counterfactual
  geom_line(aes(y = r1_counterfactual_bbl, colour = "R1 counterfactual"),
            linewidth = 0.9, linetype = "dashed") +
  scale_colour_manual(
    values = c("HECO LSFO (actual)" = clr$r3,
               "R1 counterfactual"  = clr$r1),
    name = NULL) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "/bbl")) +
  labs(x = NULL, y = "HECO LSFO price ($/barrel)") +
  brief_theme

save_fig(p2, "fig2_model_fit")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 3: Monthly savings + cumulative
# ─────────────────────────────────────────────────────────────────────────────
fig3_dat <- savings_dat |>
  filter(estimated_barrels > 0) |>   # drop months with missing generation data
  mutate(
    savings_M = monthly_savings_usd / 1e6,
    cumul_M   = cumulative_savings_usd / 1e6,
    positive  = savings_M >= 0
  )

# secondary axis scaling
max_cumul <- max(fig3_dat$cumul_M, na.rm = TRUE)
max_bar   <- max(abs(fig3_dat$savings_M), na.rm = TRUE)
scale_k   <- max_bar / max_cumul * 1.05

p3 <- ggplot(fig3_dat, aes(date)) +
  geom_col(aes(y = savings_M, fill = positive), width = 25, show.legend = FALSE) +
  geom_line(aes(y = cumul_M * scale_k, colour = "Cumulative savings (right axis)"),
            linewidth = 1.1) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  scale_fill_manual(values = c("TRUE" = clr$savings, "FALSE" = clr$loss)) +
  scale_colour_manual(values = c("Cumulative savings (right axis)" = clr$cumul), name = NULL) +
  scale_y_continuous(
    name   = "Monthly savings vs. R1 ($ million)",
    labels = dollar_format(prefix = "$", suffix = "M"),
    sec.axis = sec_axis(~ . / scale_k,
                        name   = "Cumulative savings ($ million)",
                        labels = dollar_format(prefix = "$", suffix = "M"))
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
               expand = expansion(mult = 0.01)) +
  labs(x = NULL) +
  brief_theme +
  theme(legend.position = "right")

save_fig(p3, "fig3_savings")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 4: Long-run LSFO vs LNG all-in as function of Brent
# ─────────────────────────────────────────────────────────────────────────────
curve <- fread(file.path(base_dir, "heco_lsfo_long_run_curve.csv")) |>
  as_tibble() |>
  rename_with(trimws) |>
  transmute(
    brent  = p,
    lsfo   = heco_lsfo_bbl / 6.22,           # $/bbl → $/MMBtu
    lng_b  = 0.118 * brent + 0.60 + 3.93,    # Brent-indexed 0.4 mtpa
    lng_hh = 1.15 * 3.50  + 2.75 + 1.50 + 3.93  # HH flat AEO 2030 ≈ $3.50
  ) |>
  filter(brent <= 150)

# HH-indexed is a flat line in brent-space (at AEO HH ~$3.50)
hh_30 <- 1.15 * 3.50  + 2.75 + 1.50 + 3.93   # ~$12.2/MMBtu
hh_40 <- 1.15 * 4.50  + 2.75 + 1.50 + 3.93   # ~$13.4/MMBtu (AEO 2040)

# Current Brent spot (Investing.com cash/nearest futures, March 31, 2026)
current_brent <- 112.85

# EIA AEO Brent reference-case reference lines
aeo_refs <- tibble(
  brent = c(82.7, 99.2, 112.9),
  year  = c("2030", "2035", "2040")
)

curve_long <- curve |>
  select(brent, lsfo, lng_b) |>
  pivot_longer(-brent, names_to = "series", values_to = "price_mmbtu") |>
  mutate(series = recode(series,
    lsfo  = "HECO LSFO (R3 contract)",
    lng_b = "LNG — Brent-indexed (HECO-only scale)"
  ))

p4 <- ggplot(curve_long, aes(brent, price_mmbtu, colour = series, linetype = series)) +
  geom_line(linewidth = 1) +
  # HH-indexed band (AEO 2030–2040 range)
  annotate("rect", xmin = 55, xmax = 150,
           ymin = hh_30, ymax = hh_40,
           fill = clr$lng_hh, alpha = 0.12) +
  annotate("text", x = 58, y = (hh_30 + hh_40) / 2,
           label = "LNG — Henry Hub-indexed\n(AEO 2030–2040 HH range)",
           colour = clr$lng_hh, size = 3, hjust = 0) +
  # AEO Brent reference lines
  geom_vline(data = aeo_refs, aes(xintercept = brent),
             linetype = "dotted", colour = "grey50", linewidth = 0.6) +
  geom_text(data = aeo_refs,
            aes(x = brent, y = 27.5, label = paste0("AEO\n", year)),
            inherit.aes = FALSE, size = 2.8, colour = "grey40", hjust = 0.5) +
  # Current market spot price (FRED DCOILBRENTEU, March 23, 2026)
  geom_vline(xintercept = current_brent, colour = "#c0392b",
             linewidth = 0.9, linetype = "solid") +
  annotate("text", x = current_brent - 1.5, y = 24,
           label = "Current\nspot\n(Mar 31 '26\n~$113)", size = 2.7,
           colour = "#c0392b", hjust = 1, fontface = "bold") +
  scale_colour_manual(
    values = c(clr$lsfo, clr$lng_b), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "/bbl")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "/MMBtu"),
                     limits = c(6, 29)) +
  labs(x = "Brent crude price ($/barrel)",
       y = "Delivered fuel cost ($/MMBtu)") +
  brief_theme

save_fig(p4, "fig4_longrun_curve")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 5: Six-month forward LSFO path vs LNG
# ─────────────────────────────────────────────────────────────────────────────
fwd <- fread(file.path(base_dir, "policy_forward_comparison.csv")) |>
  as_tibble() |>
  mutate(date = as.Date(date))

p5 <- ggplot(fwd, aes(date)) +
  # LNG cost range ribbon: lower bound (larger/shared facility) to upper (HECO-only)
  geom_ribbon(aes(ymin = lng_allin_10_mmbtu, ymax = lng_allin_04_mmbtu,
                  fill = "LNG all-in cost range"),
              alpha = 0.28) +
  # Boundary dashed lines (unlabeled in legend — annotated directly)
  geom_line(aes(y = lng_allin_04_mmbtu), colour = clr$lng_b,
            linewidth = 0.75, linetype = "dashed") +
  geom_line(aes(y = lng_allin_10_mmbtu), colour = clr$lng_b,
            linewidth = 0.75, linetype = "dotdash") +
  # LSFO line (sole colour-legend entry)
  geom_line(aes(y = r3_lsfo_mmbtu, colour = "HECO LSFO (R3 contract)"),
            linewidth = 1.2) +
  # Direct text annotations for LNG bounds (avoid legend clutter)
  annotate("text", x = as.Date("2026-01-20"), y = 10.5,
           label = "LNG lower bound\n(shared/multi-customer facility)",
           size = 2.6, colour = clr$lng_b, hjust = 0, fontface = "italic") +
  annotate("text", x = as.Date("2026-01-20"), y = 13.6,
           label = "LNG upper bound\n(HECO-only facility)",
           size = 2.6, colour = clr$lng_b, hjust = 0, fontface = "italic") +
  scale_fill_manual(values   = c("LNG all-in cost range" = clr$lng_b), name = NULL) +
  scale_colour_manual(values = c("HECO LSFO (R3 contract)"   = clr$lsfo),  name = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
               expand = expansion(mult = 0.02)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "/MMBtu"),
                     limits = c(9, 20)) +
  labs(x = NULL, y = "Delivered fuel cost ($/MMBtu)") +
  brief_theme

save_fig(p5, "fig5_forward_comparison")

# ─────────────────────────────────────────────────────────────────────────────
# FIGURE 6: JKM / Japan LNG import price vs Henry Hub vs Brent (2012–2026)
# ─────────────────────────────────────────────────────────────────────────────
hh <- fread(file.path(base_dir, "fred_mhhngsp_henry_hub.csv")) |>
  as_tibble() |>
  transmute(date = as.Date(observation_date), hh = as.numeric(MHHNGSP)) |>
  filter(!is.na(hh), date >= "2012-01-01")

jlng <- fread(file.path(base_dir, "fred_pngasjpusdm_lng_japan.csv")) |>
  as_tibble() |>
  transmute(date = as.Date(observation_date), jlng = as.numeric(PNGASJPUSDM)) |>
  filter(!is.na(jlng), date >= "2012-01-01")

brent_mo2 <- fread(file.path(base_dir, "fred_poilbreusdm_brent_global.csv")) |>
  as_tibble() |>
  transmute(date = as.Date(observation_date),
            brent_mmbtu = POILBREUSDM / 5.8) |>   # crude: ~5.8 MMBtu/bbl
  filter(date >= "2012-01-01")

fig6_dat <- hh |>
  full_join(jlng,      by = "date") |>
  full_join(brent_mo2, by = "date") |>
  filter(date <= "2026-03-01") |>
  arrange(date) |>
  pivot_longer(c(hh, jlng, brent_mmbtu), names_to = "series", values_to = "price") |>
  filter(!is.na(price)) |>
  mutate(series = factor(series,
    levels = c("jlng", "brent_mmbtu", "hh"),
    labels = c("Japan LNG import price", "Brent crude (÷ 5.8 MMBtu/bbl)",
               "Henry Hub (U.S. piped gas)")))

# Annotation events
events <- tibble(
  date  = as.Date(c("2022-03-01", "2020-01-01", "2026-02-01")),
  label = c("Ukraine\ninvasion", "IMO\n2020", "Iran War\nspike"),
  y     = c(36,  18, 28),
  hjust = c(0.5, 0.5, 0.5)
)

p6 <- ggplot(fig6_dat, aes(date, price, colour = series)) +
  # Crisis shading
  annotate("rect", xmin = as.Date("2021-10-01"), xmax = as.Date("2023-03-01"),
           ymin = -Inf, ymax = Inf, fill = "#c0392b", alpha = 0.06) +
  annotate("rect", xmin = as.Date("2026-01-01"), xmax = as.Date("2026-03-15"),
           ymin = -Inf, ymax = Inf, fill = "#c0392b", alpha = 0.06) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2022-02-24"), linetype = "dotted",
             colour = "#c0392b", linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted",
             colour = "grey50", linewidth = 0.6) +
  geom_text(data = events, aes(x = date, y = y, label = label),
            inherit.aes = FALSE, size = 2.8, colour = "grey30", hjust = 0.5) +
  scale_colour_manual(values = c(clr$jlng, clr$brent, clr$hh), name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "/MMBtu")) +
  labs(x = NULL, y = "Price ($/MMBtu)") +
  brief_theme

save_fig(p6, "fig6_jkm_vs_hh")

message("\nAll figures saved to: ", out_dir)
