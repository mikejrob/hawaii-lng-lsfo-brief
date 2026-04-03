# ==============================================================================
# Test monthly FRED LNG vs Brent relationship in 2024 dollars with lag structure
# - Series (monthly, from FRED):
#   PNGASJPUSDM : LNG import price in Japan ($/MMBtu)
#   POILBREUSDM : Global Brent crude oil price ($/barrel)
#   CPIAUCSL    : CPI-U (SA index)
# - Method:
#   1) Distributed lag selection: LNG_t ~ Brent_t + ... + Brent_{t-6}, dropping the
#      farthest lag until all included lag terms are significant (p <= 0.05).
#   2) Moving-average comparison using the selected lag span.
#   3) Explicit MA structure grid test (includes requested six structures and
#      lagged 4-6 period variants) on a common sample window for fair R^2
#      comparison.
# - Samples:
#   1) Full sample
#   2) Excluding LNG spike window: 2021-08 through 2022-12 (inclusive)
# ==============================================================================

library(data.table)
library(zoo)

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

build_lagged_dataset <- function(dt, max_lag = 6) {
  out <- copy(dt)
  setorder(out, DATE)
  for (k in 0:max_lag) {
    nm <- paste0("brent_lag", k)
    out[, (nm) := shift(brent_real_2024_bbl, n = k, type = "lag")]
  }
  out
}

fit_best_lag_model <- function(dt_lagged, max_lag = 6, alpha = 0.05) {
  k <- max_lag
  last_fit <- NULL
  model_dt <- NULL
  while (k >= 0) {
    terms <- paste0("brent_lag", 0:k)
    fml <- as.formula(paste("lng_real_2024_mmbtu ~", paste(terms, collapse = " + ")))
    model_dt <- dt_lagged[complete.cases(dt_lagged[, c("lng_real_2024_mmbtu", terms), with = FALSE])]
    fit <- lm(fml, data = model_dt)
    last_fit <- fit
    sm <- summary(fit)$coefficients
    lag_rows <- rownames(sm) %in% terms
    pvals <- sm[lag_rows, "Pr(>|t|)"]
    if (all(!is.na(pvals) & pvals <= alpha)) {
      return(list(fit = fit, k = k, terms = terms, data = model_dt))
    }
    k <- k - 1
  }
  list(fit = last_fit, k = 0, terms = "brent_lag0", data = model_dt)
}

fit_ma_model <- function(dt_lagged, k, start_lag = 0) {
  lag_terms <- paste0("brent_lag", start_lag:k)
  model_dt <- copy(dt_lagged)
  model_dt[, brent_ma := rowMeans(.SD), .SDcols = lag_terms]
  model_dt <- model_dt[complete.cases(model_dt[, c("lng_real_2024_mmbtu", "brent_ma"), with = FALSE])]
  fit <- lm(lng_real_2024_mmbtu ~ brent_ma, data = model_dt)
  list(fit = fit, data = model_dt, lag_terms = lag_terms)
}

coef_long <- function(fit, sample_label, model_label, model_class, k) {
  sm <- summary(fit)
  cf <- as.data.table(sm$coefficients, keep.rownames = "term")
  setnames(cf, c("term", "estimate", "std_error", "t_value", "p_value"))
  cf[, `:=`(
    sample = sample_label,
    model = model_label,
    model_class = model_class,
    lag_span_k = k,
    n = nobs(fit),
    r_squared = sm$r.squared,
    adj_r_squared = sm$adj.r.squared,
    rmse = sqrt(mean(residuals(fit)^2)),
    bic = BIC(fit),
    aic = AIC(fit)
  )]
  setcolorder(cf, c("sample", "model", "model_class", "lag_span_k", "term",
                    "estimate", "std_error", "t_value", "p_value",
                    "n", "r_squared", "adj_r_squared", "rmse", "bic", "aic"))
  cf
}

run_sample_models <- function(dt_monthly, sample_label, exclude_spike = FALSE,
                              max_lag = 6, alpha = 0.05) {
  dt <- copy(dt_monthly)
  if (exclude_spike) {
    dt <- dt[!(DATE >= as.Date("2021-08-01") & DATE <= as.Date("2022-12-31"))]
  }
  dt_lagged <- build_lagged_dataset(dt, max_lag = max_lag)

  lag_model <- fit_best_lag_model(dt_lagged, max_lag = max_lag, alpha = alpha)
  ma_model <- fit_ma_model(dt_lagged, k = lag_model$k, start_lag = 0)

  lag_fit <- lag_model$fit
  ma_fit <- ma_model$fit
  bic_diff <- as.numeric(BIC(ma_fit) - BIC(lag_fit))

  metrics <- data.table(
    sample = sample_label,
    model = c("best_lag_model", "moving_average_model"),
    model_class = c("distributed_lag", "moving_average"),
    lag_span_k = lag_model$k,
    lag_count_x = lag_model$k + 1L,
    n = c(nobs(lag_fit), nobs(ma_fit)),
    r_squared = c(summary(lag_fit)$r.squared, summary(ma_fit)$r.squared),
    adj_r_squared = c(summary(lag_fit)$adj.r.squared, summary(ma_fit)$adj.r.squared),
    rmse = c(sqrt(mean(residuals(lag_fit)^2)), sqrt(mean(residuals(ma_fit)^2))),
    bic = c(BIC(lag_fit), BIC(ma_fit)),
    aic = c(AIC(lag_fit), AIC(ma_fit)),
    delta_bic_vs_best_lag = c(0, bic_diff),
    near_equal_fit_by_bic = c(TRUE, bic_diff <= 2)
  )

  coefs <- rbindlist(list(
    coef_long(lag_fit, sample_label, "best_lag_model", "distributed_lag", lag_model$k),
    coef_long(ma_fit, sample_label, "moving_average_model", "moving_average", lag_model$k)
  ), use.names = TRUE, fill = TRUE)

  list(metrics = metrics, coefs = coefs, lag_model = lag_fit, ma_model = ma_fit)
}

# Explicit MA model grid requested by user.
# compare_on_common_sample = TRUE compares all models on same rows that satisfy max lag.
test_ma_structures <- function(dt_monthly, sample_label, exclude_spike = TRUE,
                               compare_on_common_sample = TRUE) {
  dt <- copy(dt_monthly)
  if (exclude_spike) {
    dt <- dt[!(DATE >= as.Date("2021-08-01") & DATE <= as.Date("2022-12-31"))]
  }

  max_lag_needed <- 6
  dt_lagged <- build_lagged_dataset(dt, max_lag = max_lag_needed)

  specs <- list(
    list(model_id = "ma_lag0", label = "MA: lag 0", start_lag = 0, end_lag = 0),
    list(model_id = "ma_lag0_1", label = "MA: lags 0-1", start_lag = 0, end_lag = 1),
    list(model_id = "ma_lag0_2", label = "MA: lags 0-2", start_lag = 0, end_lag = 2),
    list(model_id = "ma_lag1", label = "MA: lag 1", start_lag = 1, end_lag = 1),
    list(model_id = "ma_lag1_2", label = "MA: lags 1-2", start_lag = 1, end_lag = 2),
    list(model_id = "ma_lag1_3", label = "MA: lags 1-3", start_lag = 1, end_lag = 3),
    list(model_id = "ma_lag1_4", label = "MA: lags 1-4", start_lag = 1, end_lag = 4),
    list(model_id = "ma_lag1_5", label = "MA: lags 1-5", start_lag = 1, end_lag = 5),
    list(model_id = "ma_lag1_6", label = "MA: lags 1-6", start_lag = 1, end_lag = 6)
  )

  if (compare_on_common_sample) {
    common_cols <- paste0("brent_lag", 0:max_lag_needed)
    dt_lagged <- dt_lagged[complete.cases(dt_lagged[, c("lng_real_2024_mmbtu", common_cols), with = FALSE])]
  }

  model_rows <- lapply(specs, function(s) {
    lag_terms <- paste0("brent_lag", s$start_lag:s$end_lag)
    tmp <- copy(dt_lagged)
    tmp[, brent_ma := rowMeans(.SD), .SDcols = lag_terms]
    tmp <- tmp[complete.cases(tmp[, c("lng_real_2024_mmbtu", "brent_ma"), with = FALSE])]

    fit <- lm(lng_real_2024_mmbtu ~ brent_ma, data = tmp)
    sm <- summary(fit)
    cf <- coef(fit)

    data.table(
      sample = sample_label,
      model_id = s$model_id,
      model_label = s$label,
      start_lag = s$start_lag,
      end_lag = s$end_lag,
      lag_count = length(lag_terms),
      include_current_month = s$start_lag == 0,
      n = nobs(fit),
      intercept = unname(cf["(Intercept)"]),
      slope_ma = unname(cf["brent_ma"]),
      r_squared = sm$r.squared,
      adj_r_squared = sm$adj.r.squared,
      rmse = sqrt(mean(residuals(fit)^2)),
      bic = BIC(fit),
      aic = AIC(fit)
    )
  })

  out <- rbindlist(model_rows, use.names = TRUE, fill = TRUE)
  out[, rank_r2 := frank(-r_squared, ties.method = "min")]
  out[, optimal_by_r2 := rank_r2 == 1]
  out[, delta_r2_vs_best := max(r_squared) - r_squared]
  out
}

base_dir <- get_script_dir()
out_dir <- base_dir
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

lng_path <- file.path(out_dir, "fred_pngasjpusdm_lng_japan.csv")
cpi_path <- file.path(out_dir, "fred_cpiaucsl.csv")
brent_fred_path <- file.path(out_dir, "fred_poilbreusdm_brent_global.csv")

download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=PNGASJPUSDM",
  lng_path,
  "FRED PNGASJPUSDM (Asia LNG / Japan LNG)"
)
download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=POILBREUSDM",
  brent_fred_path,
  "FRED POILBREUSDM (Global Brent)"
)
download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
  cpi_path,
  "FRED CPIAUCSL",
  cache_candidates = c(file.path(base_dir, "..", "processed", "cpi_fred_cpiaucl.csv"),
                       file.path(base_dir, "..", "826Data", "processed", "cpi_fred_cpiaucl.csv"))
)

lng <- fread(lng_path)
brent <- fread(brent_fred_path)
cpi <- fread(cpi_path)

setnames(lng, c("DATE", "lng_nominal_mmbtu"))
setnames(brent, c("DATE", "brent_nominal_bbl"))
setnames(cpi, c("DATE", "cpi"))

for (dt in list(lng, brent, cpi)) {
  dt[, DATE := as.Date(DATE)]
}

lng[, lng_nominal_mmbtu := to_numeric_fred(lng_nominal_mmbtu)]
brent[, brent_nominal_bbl := to_numeric_fred(brent_nominal_bbl)]
cpi[, cpi := to_numeric_fred(cpi)]

dt <- merge(lng, brent, by = "DATE", all = FALSE)
dt <- merge(dt, cpi, by = "DATE", all = FALSE)
dt <- dt[!is.na(DATE) & !is.na(lng_nominal_mmbtu) & !is.na(brent_nominal_bbl) & !is.na(cpi)]
setorder(dt, DATE)

cpi_2024 <- cpi[format(DATE, "%Y") == "2024", mean(cpi, na.rm = TRUE)]
if (length(cpi_2024) == 0 || is.na(cpi_2024)) {
  stop("CPI 2024 monthly average not found.")
}

dt[, deflator_2024 := cpi_2024 / cpi]
dt[, lng_real_2024_mmbtu := lng_nominal_mmbtu * deflator_2024]
dt[, brent_real_2024_bbl := brent_nominal_bbl * deflator_2024]

res_full <- run_sample_models(
  dt_monthly = dt,
  sample_label = "full_sample",
  exclude_spike = FALSE,
  max_lag = 6,
  alpha = 0.05
)
res_no_spike <- run_sample_models(
  dt_monthly = dt,
  sample_label = "excluding_spike_2021_08_to_2022_12",
  exclude_spike = TRUE,
  max_lag = 6,
  alpha = 0.05
)

metrics_table <- rbindlist(list(res_full$metrics, res_no_spike$metrics), use.names = TRUE, fill = TRUE)
coef_table <- rbindlist(list(res_full$coefs, res_no_spike$coefs), use.names = TRUE, fill = TRUE)

ma_grid_ex_spike <- test_ma_structures(
  dt_monthly = dt,
  sample_label = "excluding_spike_2021_08_to_2022_12",
  exclude_spike = TRUE,
  compare_on_common_sample = TRUE
)
ma_grid_full <- test_ma_structures(
  dt_monthly = dt,
  sample_label = "full_sample",
  exclude_spike = FALSE,
  compare_on_common_sample = TRUE
)
ma_grid_all <- rbindlist(list(ma_grid_full, ma_grid_ex_spike), use.names = TRUE, fill = TRUE)

optimal_ma_ex_spike <- ma_grid_ex_spike[rank_r2 == 1][order(model_id)][1]
optimal_coef <- data.table(
  sample = optimal_ma_ex_spike$sample,
  selection_method = "max_r_squared_common_sample",
  model_id = optimal_ma_ex_spike$model_id,
  model_label = optimal_ma_ex_spike$model_label,
  start_lag = optimal_ma_ex_spike$start_lag,
  end_lag = optimal_ma_ex_spike$end_lag,
  lag_count = optimal_ma_ex_spike$lag_count,
  include_current_month = optimal_ma_ex_spike$include_current_month,
  intercept = optimal_ma_ex_spike$intercept,
  slope_ma = optimal_ma_ex_spike$slope_ma,
  r_squared = optimal_ma_ex_spike$r_squared,
  adj_r_squared = optimal_ma_ex_spike$adj_r_squared,
  rmse = optimal_ma_ex_spike$rmse,
  bic = optimal_ma_ex_spike$bic,
  aic = optimal_ma_ex_spike$aic,
  n = optimal_ma_ex_spike$n
)

out_metrics <- file.path(out_dir, "lng_brent_lag_regression_model_metrics.csv")
out_coefs <- file.path(out_dir, "lng_brent_lag_regression_coefficients.csv")
out_data <- file.path(out_dir, "lng_brent_real_2024_monthly_merged.csv")
out_fit_full_lag <- file.path(out_dir, "lng_brent_best_lag_full_sample.txt")
out_fit_full_ma <- file.path(out_dir, "lng_brent_moving_average_full_sample.txt")
out_fit_no_spike_lag <- file.path(out_dir, "lng_brent_best_lag_excluding_spike.txt")
out_fit_no_spike_ma <- file.path(out_dir, "lng_brent_moving_average_excluding_spike.txt")
out_ma_grid <- file.path(out_dir, "lng_brent_ma_structure_grid_results.csv")
out_optimal_ma <- file.path(out_dir, "lng_brent_optimal_ma_coefficients.csv")

fwrite(metrics_table, out_metrics)
fwrite(coef_table, out_coefs)
fwrite(dt, out_data)
fwrite(ma_grid_all, out_ma_grid)
fwrite(optimal_coef, out_optimal_ma)

capture.output(summary(res_full$lag_model), file = out_fit_full_lag)
capture.output(summary(res_full$ma_model), file = out_fit_full_ma)
capture.output(summary(res_no_spike$lag_model), file = out_fit_no_spike_lag)
capture.output(summary(res_no_spike$ma_model), file = out_fit_no_spike_ma)

cat("Saved:", out_metrics, "\n")
cat("Saved:", out_coefs, "\n")
cat("Saved:", out_data, "\n")
cat("Saved:", out_fit_full_lag, "\n")
cat("Saved:", out_fit_full_ma, "\n")
cat("Saved:", out_fit_no_spike_lag, "\n")
cat("Saved:", out_fit_no_spike_ma, "\n")
cat("Saved:", out_ma_grid, "\n")
cat("Saved:", out_optimal_ma, "\n")
