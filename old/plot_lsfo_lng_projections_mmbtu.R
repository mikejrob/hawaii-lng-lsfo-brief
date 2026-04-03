# ==============================================================================
# Plot projected LSFO and LNG prices in $/MMBtu
# ============================================================================== 

library(data.table)
library(ggplot2)

get_script_dir <- function() {
  if (!is.null(sys.frame(1)$ofile)) {
    dirname(normalizePath(sys.frame(1)$ofile))
  } else {
    getwd()
  }
}

download_or_use_cache <- function(url, path, label, cache_candidates = character()) {
  used_cache <- FALSE
  tmp_path <- paste0(path, ".tmp")
  ok <- tryCatch({
    download.file(url, tmp_path, quiet = TRUE)
    if (file.exists(tmp_path)) {
      file.copy(tmp_path, path, overwrite = TRUE)
      file.remove(tmp_path)
    }
    TRUE
  }, error = function(e) FALSE)

  if (!ok && !file.exists(path) && length(cache_candidates) > 0) {
    for (candidate in cache_candidates) {
      if (file.exists(candidate)) {
        file.copy(candidate, path, overwrite = TRUE)
        message("Using cached ", label, ": ", candidate)
        used_cache <- TRUE
        break
      }
    }
  }

  if (!ok && !file.exists(path)) {
    stop("Failed to download ", label, " and no cached file found.")
  }
  if (!ok && file.exists(path) && !used_cache) {
    message("Using cached ", label, ": ", path)
  }
}

to_numeric_fred <- function(x) {
  suppressWarnings(as.numeric(ifelse(x %in% c(".", "", "NA"), NA_character_, as.character(x))))
}

base_dir <- get_script_dir()
input_path <- file.path(base_dir, "lsfo_projection_from_adjusted_brent_real.csv")
out_png <- file.path(base_dir, "lsfo_lng_projections_mmbtu.png")
out_png_with_margins <- file.path(base_dir, "lsfo_lng_projections_mmbtu_with_margins.png")
out_png_hist <- file.path(base_dir, "fred_lng_henryhub_brent_real_2024_mmbtu.png")
out_png_scatter <- file.path(base_dir, "lng_vs_optimal_brent_ma_scatter_2024_mmbtu.png")
out_png_hist_v1 <- file.path(base_dir, "fred_timeseries_v1_brent_lng_hh_2024_mmbtu.png")
out_png_hist_v2 <- file.path(base_dir, "fred_timeseries_v2_plus_hseo_2024_mmbtu.png")
out_png_hist_v3 <- file.path(base_dir, "fred_timeseries_v3_plus_lsfo_all_2024_mmbtu.png")
out_png_scatter_v1 <- file.path(base_dir, "scatter_v1_parity_bestfit_hseo.png")
out_png_scatter_v2 <- file.path(base_dir, "scatter_v2_plus_adders.png")
out_png_scatter_v3 <- file.path(base_dir, "scatter_v3_plus_lsfo.png")
optimal_coef_path <- file.path(base_dir, "lng_brent_optimal_ma_coefficients.csv")
lsfo_rel_path <- file.path(base_dir, "lsfo_brent_pre_post_relationships.csv")
lsfo_actual_path <- file.path(base_dir, "..", "processed", "heco_lsfo_with_brent_ma3.csv")

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path)
}
if (!file.exists(lsfo_actual_path)) {
  stop("Missing historical LSFO file: ", lsfo_actual_path)
}

dt <- fread(input_path)
required_cols <- c(
  "year",
  "projection_flag",
  "projected_lsfo_pre_may2022_mmbtu",
  "projected_lsfo_post_may2022_mmbtu",
  "projected_lng_price_mmbtu_rule_0118_060",
  "projected_lng_price_mmbtu_optimal_ma"
)
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required projection column(s): ", paste(missing_cols, collapse = ", "))
}

plot_dt <- dt[projection_flag == "future_projection"]
if (nrow(plot_dt) == 0) {
  stop("No rows with projection_flag == 'future_projection'.")
}

plot_long <- melt(
  plot_dt,
  id.vars = "year",
  measure.vars = c(
    "projected_lsfo_pre_may2022_mmbtu",
    "projected_lsfo_post_may2022_mmbtu",
    "projected_lng_price_mmbtu_rule_0118_060",
    "projected_lng_price_mmbtu_optimal_ma"
  ),
  variable.name = "series",
  value.name = "price_mmbtu"
)

plot_long[, series_label := fcase(
  series == "projected_lsfo_pre_may2022_mmbtu", "LSFO (Pre-May 2022 fit)",
  series == "projected_lsfo_post_may2022_mmbtu", "LSFO (Post-May 2022 fit)",
  series == "projected_lng_price_mmbtu_rule_0118_060", "LNG (HSEO)",
  series == "projected_lng_price_mmbtu_optimal_ma", "LNG (Best Fit)",
  default = series
)]

label_dt <- plot_long[year == max(year), .SD, by = series_label]

p <- ggplot(plot_long, aes(x = year, y = price_mmbtu, color = series_label)) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(data = label_dt, size = 2.4, na.rm = TRUE) +
  geom_text(
    data = label_dt,
    aes(label = series_label),
    hjust = 0,
    nudge_x = 0.35,
    size = 3.6,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = pretty(plot_long$year, n = 8),
    expand = expansion(mult = c(0.01, 0.22))
  ) +
  labs(
    title = "Projected LSFO and LNG Prices in 2024$ per MMBtu",
    subtitle = "Future projection years only",
    x = NULL,
    y = "Price (2024$ / MMBtu)",
    color = "Projection"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_blank()
  )

ggsave(out_png, p, width = 11, height = 6, dpi = 170)
cat("Saved:", out_png, "\n")

# Add variant with LNG FSRU margin adders, using matching colors and dashed/dotted style.
fsru_margin_1 <- 3.93
fsru_margin_2 <- 3.93 * 4 / 1.1
lng_series <- c("projected_lng_price_mmbtu_rule_0118_060", "projected_lng_price_mmbtu_optimal_ma")

plot_long_margins <- rbindlist(list(
  plot_long[, .(year, series, series_label, price_mmbtu, line_style = "base")],
  plot_long[series %in% lng_series, .(
    year, series, series_label,
    price_mmbtu = price_mmbtu + fsru_margin_1,
    line_style = "dashed"
  )],
  plot_long[series %in% lng_series, .(
    year, series, series_label,
    price_mmbtu = price_mmbtu + fsru_margin_2,
    line_style = "dotted"
  )]
), use.names = TRUE)

margin_label_dt <- plot_long_margins[
  year == max(year) &
    series_label %in% c("LNG (HSEO)", "LNG (Best Fit)") &
    line_style %in% c("dashed", "dotted")
]
margin_label_dt[, margin_label := fifelse(
  line_style == "dashed",
  "HSEO fuel volume",
  "IGP-pref fuel volume"
)]
# Keep labels aligned with their own lines; apply only slight offset to clear line strokes.
margin_label_dt[, y_nudge := fifelse(line_style == "dashed", 0.18, 0.18)]

base_colors <- c(
  "LSFO (Pre-May 2022 fit)" = "#F8766D",
  "LSFO (Post-May 2022 fit)" = "#7CAE00",
  "LNG (HSEO)" = "#00BFC4",
  "LNG (Best Fit)" = "#C77CFF"
)

p_margin <- ggplot(plot_long_margins, aes(x = year, y = price_mmbtu, color = series_label, linetype = line_style)) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_point(data = label_dt, aes(x = year, y = price_mmbtu, color = series_label), size = 2.4, inherit.aes = FALSE, na.rm = TRUE) +
  geom_text(
    data = label_dt,
    aes(x = year, y = price_mmbtu, label = series_label, color = series_label),
    hjust = 0,
    nudge_x = 0.35,
    size = 3.6,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = margin_label_dt,
    aes(x = year, y = price_mmbtu + y_nudge, label = margin_label, color = series_label),
    hjust = 0,
    nudge_x = 0.28,
    size = 3.35,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = base_colors) +
  scale_linetype_manual(values = c(base = "solid", dashed = "dashed", dotted = "dotted")) +
  scale_x_continuous(
    breaks = pretty(plot_long_margins$year, n = 8),
    expand = expansion(mult = c(0.01, 0.22))
  ) +
  labs(
    title = "Projected LSFO and LNG Prices in 2024$ per MMBtu",
    subtitle = "Future projection years only",
    x = NULL,
    y = "Price (2024$ / MMBtu)",
    color = "Projection"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title.x = element_blank()
  )

ggsave(out_png_with_margins, p_margin, width = 11, height = 6, dpi = 170)
cat("Saved:", out_png_with_margins, "\n")

# ==============================================================================
# Additional plot: historical FRED LNG vs Henry Hub vs Brent-converted in 2024$
# ==============================================================================

lng_path <- file.path(base_dir, "fred_pngasjpusdm_lng_japan.csv")
brent_path <- file.path(base_dir, "fred_poilbreusdm_brent_global.csv")
hh_path <- file.path(base_dir, "fred_mhhngsp_henry_hub.csv")
cpi_path <- file.path(base_dir, "fred_cpiaucsl.csv")

download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=PNGASJPUSDM",
  lng_path,
  "FRED PNGASJPUSDM (LNG Japan)"
)
download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=POILBREUSDM",
  brent_path,
  "FRED POILBREUSDM (Brent)"
)
download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MHHNGSP",
  hh_path,
  "FRED MHHNGSP (Henry Hub)"
)
download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
  cpi_path,
  "FRED CPIAUCSL"
)

lng <- fread(lng_path)
brent <- fread(brent_path)
hh <- fread(hh_path)
cpi <- fread(cpi_path)

setnames(lng, c("DATE", "lng_nominal_mmbtu"))
setnames(brent, c("DATE", "brent_nominal_bbl"))
setnames(hh, c("DATE", "henry_hub_nominal_mmbtu"))
setnames(cpi, c("DATE", "cpi"))

for (x in list(lng, brent, hh, cpi)) {
  x[, DATE := as.Date(DATE)]
}

lng[, lng_nominal_mmbtu := to_numeric_fred(lng_nominal_mmbtu)]
brent[, brent_nominal_bbl := to_numeric_fred(brent_nominal_bbl)]
hh[, henry_hub_nominal_mmbtu := to_numeric_fred(henry_hub_nominal_mmbtu)]
cpi[, cpi := to_numeric_fred(cpi)]

hist_dt <- merge(lng, brent, by = "DATE", all = FALSE)
hist_dt <- merge(hist_dt, hh, by = "DATE", all = FALSE)
hist_dt <- merge(hist_dt, cpi, by = "DATE", all = FALSE)
hist_dt <- hist_dt[
  !is.na(DATE) &
  !is.na(lng_nominal_mmbtu) &
  !is.na(brent_nominal_bbl) &
  !is.na(henry_hub_nominal_mmbtu) &
  !is.na(cpi)
]
setorder(hist_dt, DATE)

cpi_2024 <- cpi[format(DATE, "%Y") == "2024", mean(cpi, na.rm = TRUE)]
if (length(cpi_2024) == 0 || is.na(cpi_2024)) {
  stop("CPI 2024 monthly average not found for historical FRED comparison plot.")
}

brent_mmbtu_per_bbl <- 5.817
hist_dt[, deflator_2024 := cpi_2024 / cpi]
hist_dt[, lng_real_2024_mmbtu := lng_nominal_mmbtu * deflator_2024]
hist_dt[, henry_hub_real_2024_mmbtu := henry_hub_nominal_mmbtu * deflator_2024]
hist_dt[, brent_real_2024_bbl := brent_nominal_bbl * deflator_2024]
hist_dt[, brent_real_2024_mmbtu := (brent_nominal_bbl / brent_mmbtu_per_bbl) * deflator_2024]

# Actual Honolulu LSFO ($/bbl, already real-2024 in upstream processing) -> $/MMBtu.
lsfo_actual <- fread(lsfo_actual_path)
lsfo_actual[, DATE := as.Date(date)]
lsfo_actual[, honolulu_lsfo := suppressWarnings(as.numeric(honolulu_lsfo))]
lsfo_actual <- lsfo_actual[!is.na(DATE) & !is.na(honolulu_lsfo), .(DATE, lsfo_real_2024_mmbtu = honolulu_lsfo / 5.817)]
hist_dt <- merge(hist_dt, lsfo_actual, by = "DATE", all.x = TRUE)

# HSEO contract rule: LNG($/MMBtu) = 0.6 + 0.118 * Brent($/bbl),
# using lagged 4-month Brent MA (lags 1:4).
for (k in 1:4) {
  hist_dt[, paste0("brent_real_lag", k) := shift(brent_real_2024_bbl, n = k, type = "lag")]
}
hist_dt[, brent_real_ma4_lagged_bbl := rowMeans(.SD), .SDcols = paste0("brent_real_lag", 1:4)]
hist_dt[, hseo_contract_lagged_ma4_real_2024_mmbtu := 0.6 + 0.118 * brent_real_ma4_lagged_bbl]

hist_long <- melt(
  hist_dt,
  id.vars = "DATE",
  measure.vars = c(
    "lng_real_2024_mmbtu",
    "henry_hub_real_2024_mmbtu",
    "brent_real_2024_mmbtu",
    "hseo_contract_lagged_ma4_real_2024_mmbtu",
    "lsfo_real_2024_mmbtu"
  ),
  variable.name = "series",
  value.name = "price_real_2024_mmbtu"
)

hist_long[, series_label := fcase(
  series == "lng_real_2024_mmbtu", "LNG Japan (PNGASJPUSDM)",
  series == "henry_hub_real_2024_mmbtu", "Henry Hub (MHHNGSP)",
  series == "brent_real_2024_mmbtu", "Brent converted to $/MMBtu (POILBREUSDM/5.8)",
  series == "hseo_contract_lagged_ma4_real_2024_mmbtu", "HSEO contract: 0.6 + 0.118*Brent MA(1:4)",
  series == "lsfo_real_2024_mmbtu", "Actual LSFO (Honolulu)",
  default = series
)]

hist_x_limits <- range(hist_dt$DATE, na.rm = TRUE)
hist_y_limits <- range(hist_long$price_real_2024_mmbtu, na.rm = TRUE)
hist_y_pad <- 0.04 * diff(hist_y_limits)
hist_y_limits <- c(hist_y_limits[1] - hist_y_pad, hist_y_limits[2] + hist_y_pad)

series_colors <- c(
  "LNG Japan (PNGASJPUSDM)" = "#b06af3",
  "Henry Hub (MHHNGSP)" = "#6ca300",
  "Brent converted to $/MMBtu (POILBREUSDM/5.8)" = "#f26f63",
  "HSEO contract: 0.6 + 0.118*Brent MA(1:4)" = "#12b6be",
  "Actual LSFO (Honolulu)" = "#111111"
)

make_hist_label_data <- function(series_to_plot) {
  labels <- list()
  if ("brent_real_2024_mmbtu" %in% series_to_plot) {
    brent_peak <- hist_dt[which.max(brent_real_2024_mmbtu), .(DATE, y = brent_real_2024_mmbtu)]
    labels[[length(labels) + 1]] <- data.table(
      label = "Brent",
      x = brent_peak$DATE - 365,
      y = brent_peak$y,
      series_label = "Brent converted to $/MMBtu (POILBREUSDM/5.8)",
      hjust = 0.5,
      vjust = 0
    )
  }
  if ("lng_real_2024_mmbtu" %in% series_to_plot) {
    lng_peak <- hist_dt[which.max(lng_real_2024_mmbtu), .(DATE, y = lng_real_2024_mmbtu)]
    labels[[length(labels) + 1]] <- data.table(
      label = "Global LNG",
      x = lng_peak$DATE - 90,
      y = lng_peak$y,
      series_label = "LNG Japan (PNGASJPUSDM)",
      hjust = 1,
      vjust = 0.3
    )
  }
  if ("henry_hub_real_2024_mmbtu" %in% series_to_plot) {
    hh_low_0508 <- hist_dt[
      DATE >= as.Date("2005-01-01") & DATE <= as.Date("2008-06-30") &
      !is.na(henry_hub_real_2024_mmbtu)
    ][which.min(henry_hub_real_2024_mmbtu), .(DATE, y = henry_hub_real_2024_mmbtu)]
    labels[[length(labels) + 1]] <- data.table(
      label = "Henry Hub",
      x = hh_low_0508$DATE + 690,
      y = hh_low_0508$y - 0.8,
      series_label = "Henry Hub (MHHNGSP)",
      hjust = 1,
      vjust = 1
    )
  }
  if ("hseo_contract_lagged_ma4_real_2024_mmbtu" %in% series_to_plot) {
    hseo_2013 <- hist_dt[which.min(abs(as.numeric(DATE - as.Date("2013-06-01")))),
                        .(DATE, y = hseo_contract_lagged_ma4_real_2024_mmbtu)]
    labels[[length(labels) + 1]] <- data.table(
      label = "HSEO contract",
      x = hseo_2013$DATE - 140,
      y = hseo_2013$y - 1,
      series_label = "HSEO contract: 0.6 + 0.118*Brent MA(1:4)",
      hjust = 0.5,
      vjust = 1
    )
  }
  if ("lsfo_real_2024_mmbtu" %in% series_to_plot) {
    lsfo_2017 <- hist_dt[which.min(abs(as.numeric(DATE - as.Date("2017-06-01")))),
                        .(DATE, y = lsfo_real_2024_mmbtu)]
    labels[[length(labels) + 1]] <- data.table(
      label = "LSFO",
      x = lsfo_2017$DATE + 120,
      y = lsfo_2017$y + 8.1,
      series_label = "Actual LSFO (Honolulu)",
      hjust = 0,
      vjust = 0
    )
  }
  rbindlist(labels, use.names = TRUE, fill = TRUE)
}

plot_hist_version <- function(series_to_plot, title_txt, subtitle_txt, out_file) {
  dtp <- hist_long[series %in% series_to_plot]
  labels <- make_hist_label_data(series_to_plot)

  p <- ggplot(dtp, aes(x = DATE, y = price_real_2024_mmbtu, color = series_label)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_label(
      data = labels,
      aes(x = x, y = y, label = label, color = series_label),
      inherit.aes = FALSE,
      size = 4.8,
      fontface = "bold",
      fill = scales::alpha("white", 0.82),
      linewidth = 0,
      hjust = labels$hjust,
      vjust = labels$vjust,
      show.legend = FALSE
    ) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = "Price (2024$ / MMBtu)"
    ) +
    scale_color_manual(values = series_colors) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.12))) +
    coord_cartesian(xlim = hist_x_limits, ylim = hist_y_limits) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 19),
      plot.subtitle = element_text(size = 16),
      legend.position = "none"
    )

  ggsave(out_file, p, width = 11.5, height = 6.5, dpi = 170)
  cat("Saved:", out_file, "\n")
}

plot_hist_version(
  c("brent_real_2024_mmbtu", "lng_real_2024_mmbtu", "henry_hub_real_2024_mmbtu"),
  "Historical Brent, Global LNG, and Henry Hub (2024$ / MMBtu)",
  "FRED monthly series",
  out_png_hist_v1
)
plot_hist_version(
  c("brent_real_2024_mmbtu", "lng_real_2024_mmbtu", "henry_hub_real_2024_mmbtu",
    "hseo_contract_lagged_ma4_real_2024_mmbtu"),
  "Historical Brent, Global LNG, Henry Hub, and HSEO Contract (2024$ / MMBtu)",
  "FRED monthly series; HSEO = 0.6 + 0.118*Brent MA(1:4)",
  out_png_hist_v2
)
plot_hist_version(
  c("brent_real_2024_mmbtu", "lng_real_2024_mmbtu", "henry_hub_real_2024_mmbtu",
    "hseo_contract_lagged_ma4_real_2024_mmbtu", "lsfo_real_2024_mmbtu"),
  "Historical LNG, Henry Hub, Brent, HSEO Contract, and Actual LSFO (2024$ / MMBtu)",
  "FRED monthly series plus Honolulu LSFO",
  out_png_hist_v3
)
# Keep legacy filename synced to all-lines version for continuity.
invisible(file.copy(out_png_hist_v3, out_png_hist, overwrite = TRUE))
cat("Saved:", out_png_hist, "\n")

# ==============================================================================
# Additional plot: LNG vs optimal Brent MA (both in 2024$ / MMBtu)
# ==============================================================================

if (!file.exists(optimal_coef_path)) {
  stop("Missing optimal coefficient file: ", optimal_coef_path,
       ". Run lng_brent_fred_regression_test.R first.")
}
if (!file.exists(lsfo_rel_path)) {
  stop("Missing LSFO relationship file: ", lsfo_rel_path,
       ". Run lsfo_projection_from_adjusted_brent_real.R first.")
}

opt <- fread(optimal_coef_path)
required_opt_cols <- c("start_lag", "end_lag", "sample", "model_id", "model_label")
missing_opt_cols <- setdiff(required_opt_cols, names(opt))
if (length(missing_opt_cols) > 0) {
  stop("Optimal coefficient file missing required column(s): ",
       paste(missing_opt_cols, collapse = ", "))
}
opt <- opt[1]
start_lag <- as.integer(opt$start_lag[1])
end_lag <- as.integer(opt$end_lag[1])
opt_sample <- as.character(opt$sample[1])

if (is.na(start_lag) || is.na(end_lag) || start_lag > end_lag) {
  stop("Invalid lag bounds in optimal coefficient file.")
}

scatter_dt <- copy(hist_dt)
max_needed_lag <- end_lag
for (k in 0:max_needed_lag) {
  scatter_dt[, paste0("brent_lag", k) := shift(brent_nominal_bbl * deflator_2024, n = k, type = "lag")]
}

lag_cols <- paste0("brent_lag", start_lag:end_lag)
scatter_dt[, brent_opt_ma_real_2024_bbl := rowMeans(.SD), .SDcols = lag_cols]
scatter_dt[, brent_opt_ma_real_2024_mmbtu := brent_opt_ma_real_2024_bbl / brent_mmbtu_per_bbl]
scatter_dt[, lng_real_2024_mmbtu := lng_nominal_mmbtu * deflator_2024]

if (identical(opt_sample, "excluding_spike_2021_08_to_2022_12")) {
  scatter_dt <- scatter_dt[!(DATE >= as.Date("2021-08-01") & DATE <= as.Date("2022-12-31"))]
}

scatter_dt <- scatter_dt[
  complete.cases(scatter_dt[, .(DATE, lng_real_2024_mmbtu, brent_opt_ma_real_2024_mmbtu)])
]

fit_scatter <- lm(lng_real_2024_mmbtu ~ brent_opt_ma_real_2024_mmbtu, data = scatter_dt)
fit_coef <- coef(fit_scatter)
fit_intercept <- unname(fit_coef["(Intercept)"])
fit_slope <- unname(fit_coef["brent_opt_ma_real_2024_mmbtu"])

# LNG rule was estimated as: LNG($/MMBtu) = 0.118 * Brent($/bbl) + 0.60.
# Convert to x-axis units Brent($/MMBtu): Brent($/bbl) = Brent($/MMBtu) * 5.8.
rule_intercept <- 0.60
rule_slope <- 0.118 * brent_mmbtu_per_bbl
fsru_margin_1 <- 1.68
fsru_margin_2 <- 3.93

lsfo_rel <- fread(lsfo_rel_path)
required_lsfo_cols <- c("period", "intercept", "slope_brent")
missing_lsfo_cols <- setdiff(required_lsfo_cols, names(lsfo_rel))
if (length(missing_lsfo_cols) > 0) {
  stop("LSFO relationship file missing required column(s): ",
       paste(missing_lsfo_cols, collapse = ", "))
}
lsfo_pre <- lsfo_rel[period == "pre_may2022"][1]
lsfo_post <- lsfo_rel[period == "post_may2022"][1]
if (nrow(lsfo_pre) == 0 || nrow(lsfo_post) == 0) {
  stop("LSFO relationship file must contain both pre_may2022 and post_may2022 rows.")
}

# LSFO relationships are estimated as $/bbl on Brent($/bbl).
# With x = Brent($/MMBtu) and y = LSFO($/MMBtu):
# y = (a + b * (x * 5.817)) / 5.817
lsfo_pre_intercept_mmbtu <- as.numeric(lsfo_pre$intercept[1]) / 5.817
lsfo_pre_slope_mmbtu <- as.numeric(lsfo_pre$slope_brent[1]) * brent_mmbtu_per_bbl / 5.817
lsfo_post_intercept_mmbtu <- as.numeric(lsfo_post$intercept[1]) / 5.817
lsfo_post_slope_mmbtu <- as.numeric(lsfo_post$slope_brent[1]) * brent_mmbtu_per_bbl / 5.817

parity_min <- min(c(scatter_dt$brent_opt_ma_real_2024_mmbtu, scatter_dt$lng_real_2024_mmbtu), na.rm = TRUE)
parity_max <- max(c(scatter_dt$brent_opt_ma_real_2024_mmbtu, scatter_dt$lng_real_2024_mmbtu), na.rm = TRUE)
x_lab <- parity_min + 0.80 * (parity_max - parity_min)
y_fit_lab <- fit_intercept + fit_slope * x_lab
y_rule_lab <- rule_intercept + rule_slope * x_lab
y_parity_lab <- x_lab
x_lsfo_lab <- parity_min + 0.56 * (parity_max - parity_min)
y_lsfo_pre_lab <- lsfo_pre_intercept_mmbtu + lsfo_pre_slope_mmbtu * x_lsfo_lab
y_lsfo_post_lab <- lsfo_post_intercept_mmbtu + lsfo_post_slope_mmbtu * x_lsfo_lab

# Visual angle for y=x depends on axis ranges and output aspect ratio.
scatter_w <- 9.8
scatter_h <- 6.5
x_rng <- range(scatter_dt$brent_opt_ma_real_2024_mmbtu, na.rm = TRUE)
x_pad <- 0.04 * diff(x_rng)
scatter_x_limits <- c(x_rng[1] - x_pad, x_rng[2] + x_pad)

line_range <- function(intercept, slope, xlim) {
  y1 <- intercept + slope * xlim[1]
  y2 <- intercept + slope * xlim[2]
  c(min(y1, y2), max(y1, y2))
}

y_ranges <- rbind(
  range(scatter_dt$lng_real_2024_mmbtu, na.rm = TRUE),
  line_range(0, 1, scatter_x_limits),
  line_range(fit_intercept, fit_slope, scatter_x_limits),
  line_range(fit_intercept + fsru_margin_1, fit_slope, scatter_x_limits),
  line_range(fit_intercept + fsru_margin_2, fit_slope, scatter_x_limits),
  line_range(rule_intercept, rule_slope, scatter_x_limits),
  line_range(rule_intercept + fsru_margin_1, rule_slope, scatter_x_limits),
  line_range(rule_intercept + fsru_margin_2, rule_slope, scatter_x_limits),
  line_range(lsfo_pre_intercept_mmbtu, lsfo_pre_slope_mmbtu, scatter_x_limits),
  line_range(lsfo_post_intercept_mmbtu, lsfo_post_slope_mmbtu, scatter_x_limits)
)
scatter_y_limits <- c(0, 40)

parity_angle <- atan((scatter_h * diff(scatter_x_limits)) / (scatter_w * diff(scatter_y_limits))) * 180 / pi
parity_label_angle <- parity_angle - 2.0
x_parity_label <- 21
y_parity_label <- x_parity_label + 0.35
x_lsfo_pre_label <- 29.8
y_lsfo_pre_label <- lsfo_pre_intercept_mmbtu + lsfo_pre_slope_mmbtu * 30 + 1.0

make_scatter_plot <- function(include_adders = FALSE, include_lsfo = FALSE, out_file = out_png_scatter_v1) {
  p <- ggplot(scatter_dt, aes(x = brent_opt_ma_real_2024_mmbtu, y = lng_real_2024_mmbtu)) +
    geom_point(alpha = 0.62, size = 1.9, color = "#1f77b4") +
    geom_abline(intercept = fit_intercept, slope = fit_slope, linewidth = 1.05, color = "#d62728") +
    geom_abline(intercept = rule_intercept, slope = rule_slope, linewidth = 0.95, color = "#2ca02c") +
    geom_abline(intercept = 0, slope = 1, linewidth = 2.2, linetype = "solid", color = "#7a7a7a", alpha = 0.45) +
    labs(
      title = "LNG vs Optimal Brent MA (2024$ / MMBtu)",
      x = "Optimal Brent MA (2024$ / MMBtu)",
      y = "LNG Japan (2024$ / MMBtu)"
    ) +
    annotate("text", x = x_lab + 4, y = y_fit_lab + 6, label = "Best Fit", hjust = 0, vjust = 0, size = 5, color = "#d62728") +
    annotate("text", x = x_lab, y = y_rule_lab - 2, label = "HSEO Assumption", hjust = 0, vjust = 0, size = 5, color = "#2ca02c") +
    annotate(
      "text",
      x = x_parity_label,
      y = y_parity_label,
      label = "Parity: LNG = Brent",
      angle = parity_label_angle,
      hjust = 0,
      vjust = 0,
      size = 5,
      fontface = "bold",
      color = "#808080",
      alpha = 0.78
    )

  if (include_adders) {
    p <- p +
      geom_abline(intercept = fit_intercept + fsru_margin_1, slope = fit_slope, linewidth = 0.8, linetype = "dashed", color = "#d62728") +
      geom_abline(intercept = fit_intercept + fsru_margin_2, slope = fit_slope, linewidth = 0.8, linetype = "dotted", color = "#d62728") +
      geom_abline(intercept = rule_intercept + fsru_margin_1, slope = rule_slope, linewidth = 0.8, linetype = "dashed", color = "#2ca02c") +
      geom_abline(intercept = rule_intercept + fsru_margin_2, slope = rule_slope, linewidth = 0.8, linetype = "dotted", color = "#2ca02c") +
      annotate(
        "segment",
        x = 27.8, xend = 30.2, y = 4.7, yend = 4.7,
        linetype = "dashed", linewidth = 1.1, color = "#5f5f5f", alpha = 0.95
      ) +
      annotate(
        "segment",
        x = 27.8, xend = 30.2, y = 3.7, yend = 3.7,
        linetype = "dotted", linewidth = 1.1, color = "#5f5f5f", alpha = 0.95
      ) +
      annotate(
        "text",
        x = 27.6, y = 4.7,
        label = "LNG + CapX with 1 mmtpa over 10 years",
        hjust = 1, vjust = 0.5, size = 3.25, color = "#5f5f5f", fontface = "bold"
      ) +
      annotate(
        "text",
        x = 27.6, y = 3.7,
        label = "LNG + CapX with 0.4 mmtpa over 10 years",
        hjust = 1, vjust = 0.5, size = 3.25, color = "#5f5f5f", fontface = "bold"
      )
  }

  if (include_lsfo) {
    p <- p +
      geom_abline(intercept = lsfo_pre_intercept_mmbtu, slope = lsfo_pre_slope_mmbtu, linewidth = 0.95, color = "#9467bd") +
      geom_abline(intercept = lsfo_post_intercept_mmbtu, slope = lsfo_post_slope_mmbtu, linewidth = 0.95, color = "#ff7f0e") +
      annotate("text", x = x_lsfo_pre_label, y = y_lsfo_pre_label, label = "LSFO Pre-5/2022", hjust = 1, vjust = 0, size = 4.2, color = "#9467bd") +
      annotate("text", x = x_lsfo_lab - 3, y = y_lsfo_post_lab + 3.6, label = "LSFO Post-5/2022", hjust = 0, vjust = 0, size = 4.2, color = "#ff7f0e")
  }

  p <- p +
    coord_cartesian(xlim = scatter_x_limits, ylim = scatter_y_limits) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    )
  ggsave(out_file, p, width = 9.8, height = 6.5, dpi = 170)
  cat("Saved:", out_file, "\n")
}

make_scatter_plot(include_adders = FALSE, include_lsfo = FALSE, out_file = out_png_scatter_v1)
make_scatter_plot(include_adders = TRUE, include_lsfo = FALSE, out_file = out_png_scatter_v2)
make_scatter_plot(include_adders = TRUE, include_lsfo = TRUE, out_file = out_png_scatter_v3)
# Keep legacy filename synced to full version for continuity.
invisible(file.copy(out_png_scatter_v3, out_png_scatter, overwrite = TRUE))
cat("Saved:", out_png_scatter, "\n")
