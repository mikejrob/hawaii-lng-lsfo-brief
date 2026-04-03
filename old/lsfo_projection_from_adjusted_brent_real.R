# ==============================================================================
# Project Honolulu LSFO using adjusted Brent real path (2024 dollars)
# - Fits two historical relationships (pre-May 2022 and post-May 2022):
#     honolulu_lsfo ~ brent_ma3
# - Applies both relationships to adjusted_brent_real forecasts
# - Applies LNG-from-Brent optimal MA coefficients estimated from
#   lng_brent_fred_regression_test.R (2024$ basis)
# ==============================================================================

library(data.table)

get_script_dir <- function() {
  if (!is.null(sys.frame(1)$ofile)) {
    dirname(normalizePath(sys.frame(1)$ofile))
  } else {
    getwd()
  }
}

base_dir <- get_script_dir()
hist_path <- file.path(base_dir, "..", "processed", "heco_lsfo_with_brent_ma3.csv")
forecast_path <- file.path(base_dir, "brent_nominal_real_path_adjusted_to_steo2026.csv")
out_projection <- file.path(base_dir, "lsfo_projection_from_adjusted_brent_real.csv")
out_relationships <- file.path(base_dir, "lsfo_brent_pre_post_relationships.csv")
optimal_lng_coef_path <- file.path(base_dir, "lng_brent_optimal_ma_coefficients.csv")

if (!file.exists(hist_path)) {
  stop("Missing historical merge file: ", hist_path)
}
if (!file.exists(forecast_path)) {
  stop("Missing forecast file: ", forecast_path)
}
if (!file.exists(optimal_lng_coef_path)) {
  stop("Missing optimal LNG coefficient file: ", optimal_lng_coef_path,
       ". Run lng_brent_fred_regression_test.R first.")
}

# ------------------------------------------------------------------------------
# 1) Historical data (expected to be CPI-adjusted to 2024$ in upstream pipeline)
# ------------------------------------------------------------------------------
hist <- fread(hist_path)
required_hist <- c("date", "honolulu_lsfo", "brent_ma3")
missing_hist <- setdiff(required_hist, names(hist))
if (length(missing_hist) > 0) {
  stop("Historical file missing required column(s): ", paste(missing_hist, collapse = ", "))
}

hist[, date := as.Date(date)]
hist[, honolulu_lsfo := suppressWarnings(as.numeric(honolulu_lsfo))]
hist[, brent_ma3 := suppressWarnings(as.numeric(brent_ma3))]
hist <- hist[!is.na(date) & !is.na(honolulu_lsfo) & !is.na(brent_ma3)]

if (nrow(hist) == 0) {
  stop("Historical dataset has no valid rows after numeric/date filtering.")
}

break_date <- as.Date("2022-05-01")
hist[, period := fifelse(date < break_date, "pre_may2022", "post_may2022")]

fit_pre <- lm(honolulu_lsfo ~ brent_ma3, data = hist[period == "pre_may2022"])
fit_post <- lm(honolulu_lsfo ~ brent_ma3, data = hist[period == "post_may2022"])

coef_pre <- coef(fit_pre)
coef_post <- coef(fit_post)

relationship_dt <- data.table(
  period = c("pre_may2022", "post_may2022"),
  intercept = c(unname(coef_pre["(Intercept)"]), unname(coef_post["(Intercept)"])),
  slope_brent = c(unname(coef_pre["brent_ma3"]), unname(coef_post["brent_ma3"])),
  n_obs = c(sum(hist$period == "pre_may2022"), sum(hist$period == "post_may2022")),
  r_squared = c(summary(fit_pre)$r.squared, summary(fit_post)$r.squared),
  rmse = c(sqrt(mean(residuals(fit_pre)^2)), sqrt(mean(residuals(fit_post)^2)))
)

fwrite(relationship_dt, out_relationships)

# ------------------------------------------------------------------------------
# 2) Optimal LNG-from-Brent relationship (estimated on 2024$ monthly data)
# ------------------------------------------------------------------------------
opt_lng <- fread(optimal_lng_coef_path)
required_opt <- c("intercept", "slope_ma", "model_id", "model_label", "sample")
missing_opt <- setdiff(required_opt, names(opt_lng))
if (length(missing_opt) > 0) {
  stop("Optimal LNG coefficient file missing required column(s): ",
       paste(missing_opt, collapse = ", "))
}

opt_lng <- opt_lng[1]
opt_intercept <- as.numeric(opt_lng$intercept[1])
opt_slope <- as.numeric(opt_lng$slope_ma[1])

if (is.na(opt_intercept) || is.na(opt_slope)) {
  stop("Optimal LNG coefficients are missing or non-numeric.")
}

# ------------------------------------------------------------------------------
# 3) Forecast path and LSFO/LNG projections (2024$ basis)
# ------------------------------------------------------------------------------
fc <- fread(forecast_path)
setnames(fc, names(fc), trimws(tolower(names(fc))))

if ("adjusted_brent_real" %in% names(fc) && !("adjusted_real_brent" %in% names(fc))) {
  fc[, adjusted_real_brent := adjusted_brent_real]
}

required_fc <- c("year", "adjusted_real_brent")
missing_fc <- setdiff(required_fc, names(fc))
if (length(missing_fc) > 0) {
  stop("Forecast file missing required column(s): ", paste(missing_fc, collapse = ", "))
}

fc[, adjusted_real_brent := suppressWarnings(as.numeric(adjusted_real_brent))]
fc <- fc[!is.na(year) & !is.na(adjusted_real_brent)]
setorder(fc, year)

fc[, projected_lsfo_pre_may2022 := coef_pre["(Intercept)"] + coef_pre["brent_ma3"] * adjusted_real_brent]
fc[, projected_lsfo_post_may2022 := coef_post["(Intercept)"] + coef_post["brent_ma3"] * adjusted_real_brent]

# Legacy LNG heuristic retained for comparison.
fc[, projected_lng_price_mmbtu_rule_0118_060 := 0.118 * adjusted_real_brent + 0.60]

# Optimal LNG estimate from regression test. Coefficients are 2024$; applying to
# adjusted_real_brent keeps output in 2024$ per MMBtu.
fc[, projected_lng_price_mmbtu_optimal_ma := opt_intercept + opt_slope * adjusted_real_brent]

# LSFO alternatives converted from $/bbl to $/MMBtu
fc[, projected_lsfo_pre_may2022_mmbtu := projected_lsfo_pre_may2022 / 5.817]
fc[, projected_lsfo_post_may2022_mmbtu := projected_lsfo_post_may2022 / 5.817]

max_hist_year <- max(as.integer(format(hist$date, "%Y")), na.rm = TRUE)
fc[, projection_flag := fifelse(year > max_hist_year, "future_projection", "in_sample_or_overlap")]

setnames(fc, "adjusted_real_brent", "adjusted_brent_real")

out_cols <- c(
  "year", "adjusted_brent_real",
  "projected_lsfo_pre_may2022", "projected_lsfo_post_may2022",
  "projected_lsfo_pre_may2022_mmbtu", "projected_lsfo_post_may2022_mmbtu",
  "projected_lng_price_mmbtu_rule_0118_060",
  "projected_lng_price_mmbtu_optimal_ma",
  "projection_flag"
)
optional_cols <- c("source", "target_vintage", "base_vintage")
out_cols <- c(out_cols, intersect(optional_cols, names(fc)))

meta <- data.table(
  metric = c(
    "lng_model_id",
    "lng_model_label",
    "lng_sample",
    "lng_intercept",
    "lng_slope_ma"
  ),
  value = c(
    as.character(opt_lng$model_id[1]),
    as.character(opt_lng$model_label[1]),
    as.character(opt_lng$sample[1]),
    as.character(opt_intercept),
    as.character(opt_slope)
  )
)
meta_path <- file.path(base_dir, "lsfo_lng_projection_model_metadata.csv")

fwrite(fc[, ..out_cols], out_projection)
fwrite(meta, meta_path)

cat("Saved:", out_relationships, "\n")
cat("Saved:", out_projection, "\n")
cat("Saved:", meta_path, "\n")
