# ==============================================================================
# Calibrate daily VLSFO-to-Brent relationship using FRED Brent series
# - Downloads DCOILBRENTEU (Europe Brent Spot Price FOB, daily) from FRED
# - Joins Brent with daily VLSFO indices from shipandbunker-derived history
# - Searches across:
#     * VLSFO index definitions
#     * Brent rolling windows
#     * Brent leads/lags
#     * Simple non-linear functional forms
# - Selects the preferred specification using BIC
# ==============================================================================

library(data.table)
library(parallel)
setDTthreads(1L)   # overridden to 1 inside each mclapply worker below

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_idx <- grep(file_arg, cmd_args)
  if (length(file_idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", cmd_args[file_idx[1]]))))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }

  cwd <- normalizePath(getwd())
  candidate <- file.path(cwd, "lng_vs_oil")
  if (file.exists(file.path(candidate, "vlsfo_prices.csv"))) {
    return(candidate)
  }

  cwd
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

shift_by_days <- function(x, offset_days) {
  if (offset_days > 0) {
    shift(x, n = offset_days, type = "lag")
  } else if (offset_days < 0) {
    shift(x, n = abs(offset_days), type = "lead")
  } else {
    x
  }
}

get_ma_col_name <- function(source_id, window_days) {
  paste0(source_id, "_ma_", window_days)
}

flatten_spec_list <- function(x) {
  out <- x
  while (
    length(out) > 0 &&
    is.list(out[[1]]) &&
    is.null(out[[1]]$model_family) &&
    is.null(out[[1]]$form_id)
  ) {
    out <- unlist(out, recursive = FALSE)
  }
  out
}

build_feature_frame <- function(dt, target_col, spec, y_transform) {
  out <- data.table(
    date = dt$date,
    y_raw = dt[[target_col]]
  )

  if (spec$model_family == "single_ma") {
    out[, x := shift_by_days(dt[[get_ma_col_name(spec$source_id, spec$window_days)]], spec$offset_days)]

    if (spec$form_id == "linear") {
      fml <- y ~ x
    } else if (spec$form_id == "quadratic") {
      out[, x2 := x^2]
      fml <- y ~ x + x2
    } else if (spec$form_id == "log_x") {
      out <- out[x > 0]
      out[, log_x := log(x)]
      fml <- y ~ log_x
    } else if (spec$form_id == "log_x_quadratic") {
      out <- out[x > 0]
      out[, `:=`(log_x = log(x), log_x2 = log(x)^2)]
      fml <- y ~ log_x + log_x2
    } else {
      stop("Unknown single_ma form_id: ", spec$form_id)
    }
  } else if (spec$model_family == "single_ma_plus_spot") {
    out[, `:=`(
      x = shift_by_days(dt[[get_ma_col_name(spec$source_id, spec$window_days)]], spec$offset_days),
      spot = shift_by_days(dt[[paste0(spec$spot_source_id, "_bbl")]], spec$offset_days)
    )]

    if (spec$form_id == "linear_plus_spot") {
      fml <- y ~ x + spot
    } else if (spec$form_id == "quadratic_plus_spot") {
      out[, x2 := x^2]
      fml <- y ~ x + x2 + spot
    } else if (spec$form_id == "log_quadratic_plus_spot") {
      out <- out[x > 0 & spot > 0]
      out[, `:=`(log_x = log(x), log_x2 = log(x)^2, log_spot = log(spot))]
      fml <- y ~ log_x + log_x2 + log_spot
    } else {
      stop("Unknown single_ma_plus_spot form_id: ", spec$form_id)
    }
  } else if (spec$model_family == "two_ma") {
    out[, `:=`(
      x_short = shift_by_days(dt[[get_ma_col_name(spec$short_source_id, spec$short_window_days)]], spec$offset_days),
      x_long = shift_by_days(dt[[get_ma_col_name(spec$long_source_id, spec$long_window_days)]], spec$offset_days)
    )]

    if (spec$form_id == "linear_combo") {
      fml <- y ~ x_short + x_long
    } else if (spec$form_id == "quadratic_combo") {
      out[, `:=`(x_short2 = x_short^2, x_long2 = x_long^2)]
      fml <- y ~ x_short + x_long + x_short2 + x_long2
    } else if (spec$form_id == "log_combo") {
      out <- out[x_short > 0 & x_long > 0]
      out[, `:=`(log_x_short = log(x_short), log_x_long = log(x_long))]
      fml <- y ~ log_x_short + log_x_long
    } else if (spec$form_id == "log_quadratic_combo") {
      out <- out[x_short > 0 & x_long > 0]
      out[, `:=`(
        log_x_short = log(x_short),
        log_x_long = log(x_long),
        log_x_short2 = log(x_short)^2,
        log_x_long2 = log(x_long)^2
      )]
      fml <- y ~ log_x_short + log_x_long + log_x_short2 + log_x_long2
    } else {
      stop("Unknown two_ma form_id: ", spec$form_id)
    }
  } else if (spec$model_family == "two_ma_plus_spot") {
    out[, `:=`(
      x_short = shift_by_days(dt[[get_ma_col_name(spec$short_source_id, spec$short_window_days)]], spec$offset_days),
      x_long = shift_by_days(dt[[get_ma_col_name(spec$long_source_id, spec$long_window_days)]], spec$offset_days),
      spot = shift_by_days(dt[[paste0(spec$spot_source_id, "_bbl")]], spec$offset_days)
    )]

    if (spec$form_id == "linear_combo_plus_spot") {
      fml <- y ~ x_short + x_long + spot
    } else if (spec$form_id == "log_quadratic_combo_plus_spot") {
      out <- out[x_short > 0 & x_long > 0 & spot > 0]
      out[, `:=`(
        log_x_short = log(x_short),
        log_x_long = log(x_long),
        log_x_short2 = log(x_short)^2,
        log_x_long2 = log(x_long)^2,
        log_spot = log(spot)
      )]
      fml <- y ~ log_x_short + log_x_long + log_x_short2 + log_x_long2 + log_spot
    } else {
      stop("Unknown two_ma_plus_spot form_id: ", spec$form_id)
    }
  } else {
    stop("Unknown model_family: ", spec$model_family)
  }

  if (y_transform == "level") {
    out[, y := y_raw]
  } else if (y_transform == "log") {
    out <- out[y_raw > 0]
    out[, y := log(y_raw)]
  } else {
    stop("Unknown y_transform: ", y_transform)
  }

  out <- out[complete.cases(out)]
  numeric_cols <- setdiff(names(out), "date")
  for (nm in numeric_cols) {
    out <- out[is.finite(get(nm))]
  }

  list(data = out, formula = fml)
}

calc_raw_metrics <- function(actual_raw, pred_raw, k_params) {
  resid_raw <- actual_raw - pred_raw
  n <- length(resid_raw)
  mse_raw <- mean(resid_raw^2)
  sst <- sum((actual_raw - mean(actual_raw))^2)
  ssr <- sum(resid_raw^2)

  data.table(
    n_raw = n,
    raw_rmse = sqrt(mse_raw),
    raw_mae = mean(abs(resid_raw)),
    raw_r_squared = if (sst > 0) 1 - ssr / sst else NA_real_,
    raw_bic = n * log(mse_raw) + k_params * log(n),
    raw_aic = n * log(mse_raw) + 2 * k_params
  )
}

calc_residual_diagnostics <- function(resid_raw, fitted_raw) {
  e <- resid_raw
  n <- length(e)
  dw <- if (n > 1L) sum(diff(e)^2) / sum(e^2) else NA_real_
  acf1 <- if (n > 1L) cor(e[-1], e[-n]) else NA_real_
  lb10 <- tryCatch(Box.test(e, lag = min(10L, floor(n / 5)), type = "Ljung-Box"), error = function(e) NULL)
  lb20 <- tryCatch(Box.test(e, lag = min(20L, floor(n / 5)), type = "Ljung-Box"), error = function(e) NULL)

  white_dt <- data.table(e2 = e^2, fitted = fitted_raw, fitted2 = fitted_raw^2)
  white_fit <- lm(e2 ~ fitted + fitted2, data = white_dt)
  white_lm <- length(e) * summary(white_fit)$r.squared
  white_df <- length(coef(white_fit)) - 1L

  data.table(
    durbin_watson = dw,
    residual_acf1 = acf1,
    ljung_box_lag10_stat = if (!is.null(lb10)) unname(lb10$statistic) else NA_real_,
    ljung_box_lag10_p_value = if (!is.null(lb10)) lb10$p.value else NA_real_,
    ljung_box_lag20_stat = if (!is.null(lb20)) unname(lb20$statistic) else NA_real_,
    ljung_box_lag20_p_value = if (!is.null(lb20)) lb20$p.value else NA_real_,
    white_test_stat = white_lm,
    white_test_df = white_df,
    white_test_p_value = pchisq(white_lm, df = white_df, lower.tail = FALSE)
  )
}

fit_single_response_model <- function(dt, target_col, response_component, spec, y_transform,
                                      min_obs = 120L) {
  feature <- build_feature_frame(dt, target_col, spec, y_transform)
  model_dt <- feature$data

  if (nrow(model_dt) < min_obs) {
    return(NULL)
  }

  fit <- lm(feature$formula, data = model_dt)
  sm <- summary(fit)
  fitted_trans <- fitted(fit)
  resid_trans <- residuals(fit)
  smear_factor <- if (y_transform == "log") mean(exp(resid_trans)) else 1
  pred_raw <- if (y_transform == "log") exp(fitted_trans) * smear_factor else fitted_trans

  fitted_dt <- data.table(
    date = model_dt$date,
    response_component = response_component,
    actual_raw = model_dt$y_raw,
    pred_raw = pred_raw,
    residual_raw = model_dt$y_raw - pred_raw
  )

  metrics_dt <- data.table(
    response_component = response_component,
    transformed_n = nobs(fit),
    transformed_r_squared = sm$r.squared,
    transformed_adj_r_squared = sm$adj.r.squared,
    transformed_rmse = sqrt(mean(resid_trans^2)),
    transformed_mae = mean(abs(resid_trans)),
    native_bic = BIC(fit),
    native_aic = AIC(fit),
    response_mean_raw = mean(model_dt$y_raw),
    fitted_mean_raw = mean(pred_raw),
    start_date = min(model_dt$date),
    end_date = max(model_dt$date),
    k_params = length(coef(fit)),
    smear_factor = smear_factor
  )

  list(
    fit = fit,
    fitted_dt = fitted_dt,
    metrics = metrics_dt
  )
}

fit_candidate <- function(dt, strategy_id, strategy_label, spec, y_transform,
                          min_obs = 120L) {
  if (strategy_id == "direct_avg") {
    avg_fit <- fit_single_response_model(
      dt = dt,
      target_col = "avg_sing_hou_bbl",
      response_component = "avg_sing_hou_bbl",
      spec = spec,
      y_transform = y_transform,
      min_obs = min_obs
    )

    if (is.null(avg_fit)) return(NULL)

    raw_metrics <- calc_raw_metrics(
      actual_raw = avg_fit$fitted_dt$actual_raw,
      pred_raw = avg_fit$fitted_dt$pred_raw,
      k_params = avg_fit$metrics$k_params[1]
    )

    result_row <- data.table(
      target_strategy = strategy_id,
      strategy_label = strategy_label,
      target_index_id = "avg_sing_hou_bbl",
      target_index_label = "Average Singapore/Houston VLSFO ($/bbl)",
      y_transform = y_transform,
      model_family = spec$model_family,
      source_id = if (!is.null(spec$source_id)) spec$source_id else NA_character_,
      short_source_id = if (!is.null(spec$short_source_id)) spec$short_source_id else NA_character_,
      long_source_id = if (!is.null(spec$long_source_id)) spec$long_source_id else NA_character_,
      spot_source_id = if (!is.null(spec$spot_source_id)) spec$spot_source_id else NA_character_,
      form_id = spec$form_id,
      window_days = if (!is.null(spec$window_days)) spec$window_days else NA_integer_,
      short_window_days = if (!is.null(spec$short_window_days)) spec$short_window_days else NA_integer_,
      long_window_days = if (!is.null(spec$long_window_days)) spec$long_window_days else NA_integer_,
      brent_offset_days = spec$offset_days,
      n = raw_metrics$n_raw,
      transformed_r_squared = avg_fit$metrics$transformed_r_squared[1],
      transformed_adj_r_squared = avg_fit$metrics$transformed_adj_r_squared[1],
      transformed_rmse = avg_fit$metrics$transformed_rmse[1],
      transformed_mae = avg_fit$metrics$transformed_mae[1],
      native_bic = avg_fit$metrics$native_bic[1],
      native_aic = avg_fit$metrics$native_aic[1],
      raw_r_squared = raw_metrics$raw_r_squared,
      raw_rmse = raw_metrics$raw_rmse,
      raw_mae = raw_metrics$raw_mae,
      raw_bic = raw_metrics$raw_bic,
      raw_aic = raw_metrics$raw_aic,
      k_params = avg_fit$metrics$k_params[1],
      response_mean = avg_fit$metrics$response_mean_raw[1],
      fitted_mean = avg_fit$metrics$fitted_mean_raw[1],
      start_date = avg_fit$metrics$start_date[1],
      end_date = avg_fit$metrics$end_date[1]
    )

    return(list(
      result = result_row,
      fits = list(avg_sing_hou_bbl = avg_fit$fit),
      fitted_dt = avg_fit$fitted_dt
    ))
  }

  if (strategy_id == "component_avg") {
    sing_fit <- fit_single_response_model(
      dt = dt,
      target_col = "sing_bbl",
      response_component = "sing_bbl",
      spec = spec,
      y_transform = y_transform,
      min_obs = min_obs
    )
    hou_fit <- fit_single_response_model(
      dt = dt,
      target_col = "houston_bbl",
      response_component = "houston_bbl",
      spec = spec,
      y_transform = y_transform,
      min_obs = min_obs
    )

    if (is.null(sing_fit) || is.null(hou_fit)) return(NULL)

    merged_pred <- merge(
      sing_fit$fitted_dt[, .(date, sing_actual_raw = actual_raw, sing_pred_raw = pred_raw)],
      hou_fit$fitted_dt[, .(date, hou_actual_raw = actual_raw, hou_pred_raw = pred_raw)],
      by = "date",
      all = FALSE
    )

    if (nrow(merged_pred) < min_obs) return(NULL)

    merged_pred[, `:=`(
      actual_raw = (sing_actual_raw + hou_actual_raw) / 2,
      pred_raw = (sing_pred_raw + hou_pred_raw) / 2
    )]
    merged_pred[, residual_raw := actual_raw - pred_raw]

    k_total <- sing_fit$metrics$k_params[1] + hou_fit$metrics$k_params[1]
    raw_metrics <- calc_raw_metrics(
      actual_raw = merged_pred$actual_raw,
      pred_raw = merged_pred$pred_raw,
      k_params = k_total
    )

    result_row <- data.table(
      target_strategy = strategy_id,
      strategy_label = strategy_label,
      target_index_id = "avg_sing_hou_bbl",
      target_index_label = "Average Singapore/Houston VLSFO ($/bbl)",
      y_transform = y_transform,
      model_family = spec$model_family,
      source_id = if (!is.null(spec$source_id)) spec$source_id else NA_character_,
      short_source_id = if (!is.null(spec$short_source_id)) spec$short_source_id else NA_character_,
      long_source_id = if (!is.null(spec$long_source_id)) spec$long_source_id else NA_character_,
      spot_source_id = if (!is.null(spec$spot_source_id)) spec$spot_source_id else NA_character_,
      form_id = spec$form_id,
      window_days = if (!is.null(spec$window_days)) spec$window_days else NA_integer_,
      short_window_days = if (!is.null(spec$short_window_days)) spec$short_window_days else NA_integer_,
      long_window_days = if (!is.null(spec$long_window_days)) spec$long_window_days else NA_integer_,
      brent_offset_days = spec$offset_days,
      n = raw_metrics$n_raw,
      transformed_r_squared = mean(c(
        sing_fit$metrics$transformed_r_squared[1],
        hou_fit$metrics$transformed_r_squared[1]
      )),
      transformed_adj_r_squared = mean(c(
        sing_fit$metrics$transformed_adj_r_squared[1],
        hou_fit$metrics$transformed_adj_r_squared[1]
      )),
      transformed_rmse = mean(c(
        sing_fit$metrics$transformed_rmse[1],
        hou_fit$metrics$transformed_rmse[1]
      )),
      transformed_mae = mean(c(
        sing_fit$metrics$transformed_mae[1],
        hou_fit$metrics$transformed_mae[1]
      )),
      native_bic = sing_fit$metrics$native_bic[1] + hou_fit$metrics$native_bic[1],
      native_aic = sing_fit$metrics$native_aic[1] + hou_fit$metrics$native_aic[1],
      raw_r_squared = raw_metrics$raw_r_squared,
      raw_rmse = raw_metrics$raw_rmse,
      raw_mae = raw_metrics$raw_mae,
      raw_bic = raw_metrics$raw_bic,
      raw_aic = raw_metrics$raw_aic,
      k_params = k_total,
      response_mean = mean(merged_pred$actual_raw),
      fitted_mean = mean(merged_pred$pred_raw),
      start_date = min(merged_pred$date),
      end_date = max(merged_pred$date)
    )

    fitted_dt <- merged_pred[, .(
      date,
      actual_raw,
      pred_raw,
      residual_raw,
      sing_pred_raw,
      hou_pred_raw
    )]

    return(list(
      result = result_row,
      fits = list(sing_bbl = sing_fit$fit, houston_bbl = hou_fit$fit),
      fitted_dt = fitted_dt
    ))
  }

  stop("Unknown strategy_id: ", strategy_id)
}

coef_table <- function(fit, model_info) {
  cf <- as.data.table(summary(fit)$coefficients, keep.rownames = "term")
  setnames(cf, c("term", "estimate", "std_error", "t_value", "p_value"))
  for (nm in names(model_info)) {
    cf[, (nm) := model_info[[nm]]]
  }
  setcolorder(cf, c(
    names(model_info),
    "term", "estimate", "std_error", "t_value", "p_value"
  ))
  cf
}

# ---------------------------------------------------------------------------
# Monotone-increasing constraint
# ---------------------------------------------------------------------------
# After fitting a candidate model, verify that predicted VLSFO is
# non-decreasing in both Brent and WTI over the range [25, 200] $/bbl.
# Each crude price is varied while the other is held at a representative
# in-sample median; all MA columns are set equal to the varied price so the
# check reflects steady-state extrapolation behavior.
#
# Non-monotone models (e.g. log-quadratic that curves back down at low or
# high prices) are excluded from best-model selection, even if they have
# lower BIC on the estimation sample.
check_monotone_vlsfo <- function(candidate, spec, y_transform,
                                  crude_range = seq(25, 200, by = 5)) {
  n           <- length(crude_range)
  all_windows <- unique(na.omit(c(spec$window_days, spec$short_window_days,
                                   spec$long_window_days)))
  # Representative in-sample median prices used to hold the "other" variable.
  median_brent <- 70
  median_wti   <- 68

  # Build a synthetic data.table where every MA column equals the supplied
  # price vector directly (steady-state: constant price → all MAs = price).
  # offset_days is forced to 0 so shift_by_days() is a no-op; the direction
  # of monotonicity is invariant to a pure time-shift.
  spec_check              <- spec
  spec_check$offset_days  <- 0L

  make_panel <- function(brent_vec, wti_vec) {
    dt <- data.table(
      date      = seq.Date(as.Date("2020-01-01"), by = "day", length.out = n),
      y_raw     = 1,
      brent_bbl = brent_vec,
      wti_bbl   = wti_vec
    )
    for (w in all_windows) {
      dt[, (get_ma_col_name("brent", w)) := brent_vec]
      dt[, (get_ma_col_name("wti",   w)) := wti_vec]
    }
    dt[, avg_sing_hou_bbl := 1]
    dt[, sing_bbl         := 1]
    dt[, houston_bbl      := 1]
    dt
  }

  is_nondecreasing <- function(v) {
    if (is.null(v) || length(v) < 2L) return(TRUE)
    all(diff(v) >= -0.01)   # 1-cent tolerance for floating-point noise
  }

  for (fit_name in names(candidate$fits)) {
    fit_obj <- candidate$fits[[fit_name]]

    # Vary Brent; hold WTI at median.
    panel_b <- make_panel(crude_range, rep(median_wti, n))
    ff_b    <- build_feature_frame(panel_b, fit_name, spec_check, y_transform)
    if (nrow(ff_b$data) >= 2L) {
      p_b <- predict(fit_obj, newdata = ff_b$data)
      if (y_transform == "log") p_b <- exp(p_b)
      if (!is_nondecreasing(p_b)) return(FALSE)
    }

    # Vary WTI; hold Brent at median.
    panel_w <- make_panel(rep(median_brent, n), crude_range)
    ff_w    <- build_feature_frame(panel_w, fit_name, spec_check, y_transform)
    if (nrow(ff_w$data) >= 2L) {
      p_w <- predict(fit_obj, newdata = ff_w$data)
      if (y_transform == "log") p_w <- exp(p_w)
      if (!is_nondecreasing(p_w)) return(FALSE)
    }
  }

  TRUE
}

base_dir <- get_script_dir()
vlsfo_path <- file.path(base_dir, "vlsfo_prices.csv")
brent_path <- file.path(base_dir, "fred_dcoilbrenteu_daily_brent.csv")
wti_path <- file.path(base_dir, "fred_dcoilwtico_daily_wti.csv")
merged_path <- file.path(base_dir, "brent_vlsfo_daily_merged.csv")
search_path <- file.path(base_dir, "vlsfo_brent_model_search_results.csv")
coef_path <- file.path(base_dir, "vlsfo_brent_best_model_coefficients.csv")
fitted_path <- file.path(base_dir, "vlsfo_brent_best_model_fitted.csv")
summary_path <- file.path(base_dir, "vlsfo_brent_best_model_summary.csv")
diag_path <- file.path(base_dir, "vlsfo_brent_best_model_diagnostics.csv")

if (!file.exists(vlsfo_path)) {
  stop("Missing VLSFO input file: ", vlsfo_path)
}

download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILBRENTEU",
  brent_path,
  "FRED DCOILBRENTEU daily Brent"
)
download_or_use_cache(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILWTICO",
  wti_path,
  "FRED DCOILWTICO daily WTI"
)

VLSFO_MT_TO_BBL <- 6.35

vlsfo_raw <- fread(vlsfo_path)
required_vlsfo <- c(
  "date",
  "singapore_vlsfo_usd_mt",
  "global_20ports_vlsfo_usd_mt",
  "houston_vlsfo_usd_mt"
)
missing_vlsfo <- setdiff(required_vlsfo, names(vlsfo_raw))
if (length(missing_vlsfo) > 0) {
  stop("VLSFO file missing required column(s): ", paste(missing_vlsfo, collapse = ", "))
}

vlsfo <- vlsfo_raw[, .(
  date = as.Date(date),
  sing_bbl = as.numeric(singapore_vlsfo_usd_mt) / VLSFO_MT_TO_BBL,
  glob20_bbl = as.numeric(global_20ports_vlsfo_usd_mt) / VLSFO_MT_TO_BBL,
  houston_bbl = as.numeric(houston_vlsfo_usd_mt) / VLSFO_MT_TO_BBL
)]
vlsfo[, avg_sing_glob_bbl := fifelse(
  is.na(sing_bbl) | is.na(glob20_bbl),
  NA_real_,
  (sing_bbl + glob20_bbl) / 2
)]
vlsfo[, avg_sing_hou_bbl := fifelse(
  is.na(sing_bbl) | is.na(houston_bbl),
  NA_real_,
  (sing_bbl + houston_bbl) / 2
)]
vlsfo <- vlsfo[!is.na(date)]
setorder(vlsfo, date)

brent <- fread(brent_path)
brent_date_col <- intersect(c("DATE", "date", "observation_date"), names(brent))[1]
brent_value_col <- intersect(c("DCOILBRENTEU", "dcoilbrenteu"), names(brent))[1]

if (is.na(brent_date_col) || is.na(brent_value_col)) {
  stop(
    "Brent file missing expected columns. Found: ",
    paste(names(brent), collapse = ", ")
  )
}

brent <- brent[, .(
  date = as.Date(get(brent_date_col)),
  brent_bbl = to_numeric_fred(get(brent_value_col))
)]
brent <- brent[!is.na(date) & !is.na(brent_bbl)]
setorder(brent, date)

wti <- fread(wti_path)
wti_date_col <- intersect(c("DATE", "date", "observation_date"), names(wti))[1]
wti_value_col <- intersect(c("DCOILWTICO", "dcoilwtico"), names(wti))[1]

if (is.na(wti_date_col) || is.na(wti_value_col)) {
  stop(
    "WTI file missing expected columns. Found: ",
    paste(names(wti), collapse = ", ")
  )
}

wti <- wti[, .(
  date = as.Date(get(wti_date_col)),
  wti_bbl = to_numeric_fred(get(wti_value_col))
)]
wti <- wti[!is.na(date) & !is.na(wti_bbl)]
setorder(wti, date)

merged <- merge(vlsfo, brent, by = "date", all = FALSE)
merged <- merge(merged, wti, by = "date", all = FALSE)
merged <- merged[is.finite(brent_bbl) & is.finite(wti_bbl)]
setorder(merged, date)

if (nrow(merged) == 0) {
  stop("No overlapping non-missing daily Brent and VLSFO observations were found.")
}

window_grid <- c(1L, 2L, 3L, 5L, 7L, 10L, 14L, 21L, 30L, 45L)
offset_grid <- 0L:14L
y_transform_grid <- c("level", "log")
single_form_grid <- c("linear", "quadratic", "log_x", "log_x_quadratic")
two_ma_form_grid <- c("linear_combo", "quadratic_combo", "log_combo", "log_quadratic_combo")
single_plus_spot_form_grid <- c("linear_plus_spot", "quadratic_plus_spot", "log_quadratic_plus_spot")
two_ma_plus_spot_form_grid <- c("linear_combo_plus_spot", "log_quadratic_combo_plus_spot")

for (w in window_grid) {
  merged[, (get_ma_col_name("brent", w)) := frollmean(brent_bbl, n = w, align = "right")]
  merged[, (get_ma_col_name("wti", w)) := frollmean(wti_bbl, n = w, align = "right")]
}

fwrite(merged, merged_path)

strategy_specs <- list(
  list(id = "direct_avg", label = "Direct model of Singapore/Houston average"),
  list(id = "component_avg", label = "Average of separate Singapore and Houston predictions")
)

two_ma_pairs <- CJ(short_window_days = window_grid, long_window_days = window_grid)[short_window_days < long_window_days]
source_grid <- c("brent", "wti")

single_specs <- lapply(source_grid, function(source_id) {
  lapply(window_grid, function(w) {
    lapply(single_form_grid, function(form_id) {
      list(
        model_family = "single_ma",
        source_id = source_id,
        form_id = form_id,
        window_days = w
      )
    })
  })
})
single_specs <- flatten_spec_list(single_specs)

single_plus_spot_specs <- lapply(source_grid, function(source_id) {
  lapply(source_grid, function(spot_source_id) {
    lapply(window_grid, function(w) {
      lapply(single_plus_spot_form_grid, function(form_id) {
        list(
          model_family = "single_ma_plus_spot",
          source_id = source_id,
          spot_source_id = spot_source_id,
          form_id = form_id,
          window_days = w
        )
      })
    })
  })
})
single_plus_spot_specs <- flatten_spec_list(single_plus_spot_specs)

two_ma_specs <- lapply(seq_len(nrow(two_ma_pairs)), function(i) {
  pair <- two_ma_pairs[i]
  lapply(source_grid, function(short_source_id) {
    lapply(source_grid, function(long_source_id) {
      lapply(two_ma_form_grid, function(form_id) {
        list(
          model_family = "two_ma",
          form_id = form_id,
          short_source_id = short_source_id,
          long_source_id = long_source_id,
          short_window_days = pair$short_window_days,
          long_window_days = pair$long_window_days
        )
      })
    })
  })
})
two_ma_specs <- flatten_spec_list(two_ma_specs)

two_ma_plus_spot_specs <- lapply(seq_len(nrow(two_ma_pairs)), function(i) {
  pair <- two_ma_pairs[i]
  lapply(source_grid, function(short_source_id) {
    lapply(source_grid, function(long_source_id) {
      lapply(source_grid, function(spot_source_id) {
        lapply(two_ma_plus_spot_form_grid, function(form_id) {
          list(
            model_family = "two_ma_plus_spot",
            form_id = form_id,
            short_source_id = short_source_id,
            long_source_id = long_source_id,
            spot_source_id = spot_source_id,
            short_window_days = pair$short_window_days,
            long_window_days = pair$long_window_days
          )
        })
      })
    })
  })
})
two_ma_plus_spot_specs <- flatten_spec_list(two_ma_plus_spot_specs)

all_specs <- c(single_specs, single_plus_spot_specs, two_ma_specs, two_ma_plus_spot_specs)

# ── Flatten all (strategy × y_transform × offset × spec) into one job list ───
# This lets mclapply distribute work evenly across cores without any shared
# mutable state (no row_idx counter, no in-place list writes).
all_jobs <- vector(
  "list",
  length(strategy_specs) * length(y_transform_grid) * length(offset_grid) * length(all_specs)
)
job_idx <- 1L
for (strategy_spec in strategy_specs) {
  for (y_transform in y_transform_grid) {
    for (offset_days in offset_grid) {
      for (spec in all_specs) {
        s <- spec
        s$offset_days <- offset_days
        all_jobs[[job_idx]] <- list(
          strategy_spec = strategy_spec,
          y_transform   = y_transform,
          spec          = s
        )
        job_idx <- job_idx + 1L
      }
    }
  }
}

# ── Parallel model search ─────────────────────────────────────────────────────
# parallel::mclapply uses fork-based parallelism.  On macOS/Linux each worker
# gets a copy-on-write view of the parent's memory (including `merged` and all
# helper functions), so no explicit data export is needed.
#
# NOTE FOR WINDOWS USERS: mclapply silently falls back to mc.cores = 1
# (sequential) on Windows because fork() is unavailable.  For true parallelism
# on Windows, replace the mclapply block with:
#
#   cl <- parallel::makeCluster(n_cores)
#   parallel::clusterExport(cl, c("merged", "fit_candidate", "check_monotone_vlsfo",
#                                   "build_feature_frame", "fit_single_response_model",
#                                   "calc_raw_metrics", "get_ma_col_name", "shift_by_days"))
#   parallel::clusterEvalQ(cl, { library(data.table); setDTthreads(1L) })
#   raw_results <- parallel::parLapply(cl, all_jobs, worker_fn)
#   parallel::stopCluster(cl)
#
n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
cat(sprintf("Model search: %d jobs across %d cores...\n", length(all_jobs), n_cores))

raw_results <- parallel::mclapply(
  all_jobs,
  function(job) {
    data.table::setDTthreads(1L)   # prevent nested threading within each worker
    candidate <- fit_candidate(
      dt             = merged,
      strategy_id    = job$strategy_spec$id,
      strategy_label = job$strategy_spec$label,
      spec           = job$spec,
      y_transform    = job$y_transform
    )
    if (is.null(candidate)) return(NULL)
    mono_pass <- check_monotone_vlsfo(candidate, job$spec, job$y_transform)
    candidate$result[, monotone_pass := mono_pass]
    # Return copies so data.table's reference semantics don't cause issues
    # when results are collected in the parent process after forking.
    list(result = data.table::copy(candidate$result), candidate = candidate)
  },
  mc.cores = n_cores
)

# Collect non-NULL results; assign sequential candidate_ids that index into
# candidate_store so best_candidate lookup still works after filtering.
valid_jobs      <- Filter(Negate(is.null), raw_results)
candidate_store <- lapply(valid_jobs, `[[`, "candidate")
result_rows     <- lapply(seq_along(valid_jobs), function(i) {
  r <- valid_jobs[[i]]$result
  r[, candidate_id := i]
  r
})

results <- rbindlist(result_rows, use.names = TRUE, fill = TRUE)
results <- results[!is.na(raw_bic)]

if (nrow(results) == 0) {
  stop("Model search produced no valid candidate fits.")
}

# Apply monotone-increasing constraint: only consider models whose predicted
# VLSFO is non-decreasing in Brent and WTI across the full extrapolation range.
# The full results table (including non-monotone models) is still saved to CSV
# with a monotone_pass flag so the selection can be audited.
results_mono <- results[monotone_pass == TRUE]
n_mono <- nrow(results_mono)
n_total <- nrow(results)

if (n_mono == 0L) {
  warning(sprintf(
    "No models passed the monotone-increasing check (%d candidates evaluated). ",
    "Proceeding with all models -- review functional form assumptions.",
    n_total
  ))
  results_mono <- results
}

cat(sprintf(
  "Monotone filter: %d / %d candidates pass (%.1f%%).\n",
  n_mono, n_total, 100 * n_mono / n_total
))

setorder(results_mono, raw_bic, raw_rmse, native_bic)
results_mono[, delta_raw_bic := raw_bic - min(raw_bic)]
results_mono[, raw_bic_rank  := seq_len(.N)]

# Save all candidates (monotone + non-monotone) for audit; rank reflects
# order within the monotone-passing set.
setorder(results, raw_bic, raw_rmse, native_bic)
results[, delta_raw_bic := raw_bic - min(raw_bic)]
results[, raw_bic_rank  := seq_len(.N)]
fwrite(results[, !c("candidate_id"), with = FALSE], search_path)

best <- results_mono[1]
best_candidate <- candidate_store[[best$candidate_id]]
best_fitted_dt <- copy(best_candidate$fitted_dt)

model_info <- as.list(best[, .(
  target_strategy,
  strategy_label,
  target_index_id,
  target_index_label,
  y_transform,
  model_family,
  source_id,
  short_source_id,
  long_source_id,
  spot_source_id,
  window_days,
  short_window_days,
  long_window_days,
  brent_offset_days,
  form_id,
  n,
  transformed_r_squared,
  transformed_adj_r_squared,
  transformed_rmse,
  transformed_mae,
  native_bic,
  native_aic,
  raw_r_squared,
  raw_rmse,
  raw_mae,
  raw_bic,
  raw_aic,
  k_params,
  delta_raw_bic
)])

coef_tables <- lapply(names(best_candidate$fits), function(component_name) {
  fit_obj <- best_candidate$fits[[component_name]]
  component_info <- c(model_info, list(response_component = component_name))
  coef_table(fit_obj, component_info)
})
best_coefs <- rbindlist(coef_tables, use.names = TRUE, fill = TRUE)
fwrite(best_coefs, coef_path)

fwrite(best_fitted_dt, fitted_path)

diag_dt <- cbind(model_info, calc_residual_diagnostics(
  resid_raw = best_fitted_dt$residual_raw,
  fitted_raw = best_fitted_dt$pred_raw
))
fwrite(diag_dt, diag_path)

summary_dt <- data.table(
  metric = c(
    "selected_target_strategy",
    "selected_strategy_label",
    "selected_target_index_id",
    "selected_target_index_label",
    "selected_y_transform",
    "selected_model_family",
    "selected_source_id",
    "selected_short_source_id",
    "selected_long_source_id",
    "selected_spot_source_id",
    "selected_window_days",
    "selected_short_window_days",
    "selected_long_window_days",
    "selected_brent_offset_days",
    "selected_form_id",
    "n",
    "transformed_r_squared",
    "transformed_adj_r_squared",
    "transformed_rmse",
    "transformed_mae",
    "native_bic",
    "native_aic",
    "raw_r_squared",
    "raw_rmse",
    "raw_mae",
    "raw_bic",
    "raw_aic",
    "durbin_watson",
    "residual_acf1",
    "ljung_box_lag10_p_value",
    "ljung_box_lag20_p_value",
    "white_test_p_value",
    "sample_start",
    "sample_end",
    "monotone_pass",
    "n_monotone_candidates",
    "n_total_candidates",
    "brent_source_series",
    "brent_source_url"
  ),
  value = c(
    as.character(best$target_strategy),
    as.character(best$strategy_label),
    as.character(best$target_index_id),
    as.character(best$target_index_label),
    as.character(best$y_transform),
    as.character(best$model_family),
    as.character(best$source_id),
    as.character(best$short_source_id),
    as.character(best$long_source_id),
    as.character(best$spot_source_id),
    as.character(best$window_days),
    as.character(best$short_window_days),
    as.character(best$long_window_days),
    as.character(best$brent_offset_days),
    as.character(best$form_id),
    as.character(best$n),
    as.character(best$transformed_r_squared),
    as.character(best$transformed_adj_r_squared),
    as.character(best$transformed_rmse),
    as.character(best$transformed_mae),
    as.character(best$native_bic),
    as.character(best$native_aic),
    as.character(best$raw_r_squared),
    as.character(best$raw_rmse),
    as.character(best$raw_mae),
    as.character(best$raw_bic),
    as.character(best$raw_aic),
    as.character(diag_dt$durbin_watson[1]),
    as.character(diag_dt$residual_acf1[1]),
    as.character(diag_dt$ljung_box_lag10_p_value[1]),
    as.character(diag_dt$ljung_box_lag20_p_value[1]),
    as.character(diag_dt$white_test_p_value[1]),
    as.character(min(best_fitted_dt$date)),
    as.character(max(best_fitted_dt$date)),
    as.character(best$monotone_pass),
    as.character(n_mono),
    as.character(n_total),
    "DCOILBRENTEU",
    "https://fred.stlouisfed.org/series/DCOILBRENTEU"
  )
)
fwrite(summary_dt, summary_path)

cat("Saved:", brent_path, "\n")
cat("Saved:", merged_path, "\n")
cat("Saved:", search_path, "\n")
cat("Saved:", coef_path, "\n")
cat("Saved:", fitted_path, "\n")
cat("Saved:", diag_path, "\n")
cat("Saved:", summary_path, "\n")
cat(
  "Best model:",
  as.character(best$strategy_label),
  "| target =", as.character(best$target_index_label),
  "| y_transform =", as.character(best$y_transform),
  "| family =", as.character(best$model_family),
  "| source =", as.character(best$source_id),
  "| short_source =", as.character(best$short_source_id),
  "| long_source =", as.character(best$long_source_id),
  "| spot_source =", as.character(best$spot_source_id),
  "| window =", best$window_days,
  "| short =", best$short_window_days,
  "| long =", best$long_window_days,
  "| offset =", best$brent_offset_days,
  "| form =", as.character(best$form_id),
  "| raw BIC =", round(best$raw_bic, 2),
  "| raw RMSE =", round(best$raw_rmse, 4),
  "\n"
)
