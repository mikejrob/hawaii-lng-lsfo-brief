# ==============================================================================
# HECO LSFO projection from Brent/WTI crude, via daily VLSFO and HECO contract
# structure
#
# Outputs:
#   1) Long-run steady-state HECO LSFO curve for Brent = WTI = p
#   2) Short-run forecast (~6 months) with Brent/WTI held flat at current levels
#   3) Long-run forecast from an external monthly Brent/WTI futures curve
#
# Requirements:
#   - Best daily crude->VLSFO bridge already estimated in vlsfo_brent_daily_link.R
#   - Historical HECO LSFO and daily VLSFO data available locally
#   - Optional futures curve file for part 3:
#       lng_vs_oil/crude_futures_curve.csv
#     with columns:
#       contract_month, brent_bbl, wti_bbl
# ==============================================================================

library(tidyverse)
library(lubridate)
library(slider)
library(data.table)
library(parallel)
setDTthreads(1L)

VLSFO_MT_TO_BBL <- 6.35
ANCHOR_DAY_R3 <- 24L
SHORT_TERM_MONTHS_AHEAD <- 6L

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

shift_by_days <- function(x, offset_days) {
  if (offset_days > 0) {
    dplyr::lag(x, n = offset_days)
  } else if (offset_days < 0) {
    dplyr::lead(x, n = abs(offset_days))
  } else {
    x
  }
}

get_ma_col_name <- function(source_id, window_days) {
  paste0(source_id, "_ma_", window_days)
}

get_coeff <- function(coefs, term) {
  # Named numeric vectors (from coef()) throw "subscript out of bounds" for
  # missing names, unlike lists which return NULL.  Base-level regime terms
  # (e.g. regimeR1, pred:regimeR1) are absorbed into the intercept/slope and
  # are absent from the coefficient vector — treat them as zero.
  if (!term %in% names(coefs)) return(0)
  val <- coefs[[term]]
  if (is.null(val) || length(val) == 0L || is.na(val)) 0 else unname(val[[1]])
}

get_coef_from_table <- function(coef_tbl, term) {
  val <- coef_tbl |>
    filter(.data$term == .env$term) |>
    pull(estimate)
  if (!length(val) || is.na(val[1])) 0 else unname(val[1])
}

parse_metric_summary <- function(path) {
  dt <- fread(path)
  setNames(as.list(dt$value), dt$metric)
}

normalize_meta_value <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || identical(x, "")) NA_character_ else as.character(x)
}

as_int_or_na <- function(x) {
  x <- normalize_meta_value(x)
  if (is.na(x)) NA_integer_ else as.integer(x)
}

build_weekday_sequence <- function(date_from, date_to) {
  tibble(date = seq.Date(as.Date(date_from), as.Date(date_to), by = "day")) |>
    filter(lubridate::wday(date, week_start = 1) <= 5L) |>
    pull(date)
}

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

build_curved_w2_path <- function(n_months, start_level, end_level, curvature = 1) {
  if (n_months <= 0L) return(integer(0))
  if (n_months == 1L) return(as.integer(round(start_level)))
  t <- seq(0, 1, length.out = n_months)^curvature
  as.integer(round(start_level + (end_level - start_level) * t))
}

build_shock_pulse <- function(dates, break_date, duration_months) {
  if (duration_months <= 0L) return(rep(0, length(dates)))
  map_dbl(as.Date(dates), function(d) {
    if (d < break_date) return(0)
    m <- interval(break_date, d) %/% months(1)
    max(1 - m / duration_months, 0)
  })
}

build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
  daily_int <- as.integer(daily$date)

  tibble(date = as.Date(target_dates)) |>
    mutate(pred = map_dbl(date, function(m) {
      m <- as.Date(m)

      anchor <- if (anchor_day > 0) {
        floor_date(m, "month") %m-% months(1) + days(anchor_day - 1L)
      } else if (anchor_day == 0) {
        ceiling_date(m, "month") %m-% months(1) - days(1)
      } else {
        floor_date(m, "month") + days(-anchor_day - 1L)
      }

      win_start <- anchor - days(w2 - 1L)
      i_end <- findInterval(as.integer(anchor), daily_int)
      i_start <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

build_vlsfo_feature_frame <- function(dt, target_col, spec, y_transform) {
  out <- tibble(
    date = as.Date(dt$date),
    y_raw = dt[[target_col]]
  )

  if (spec$model_family == "single_ma") {
    out <- out |>
      mutate(x = shift_by_days(dt[[get_ma_col_name(spec$source_id, spec$window_days)]], spec$offset_days))

    if (spec$form_id == "quadratic") {
      out <- out |> mutate(x2 = x^2)
    } else if (spec$form_id == "log_x") {
      out <- out |> filter(x > 0) |> mutate(log_x = log(x))
    } else if (spec$form_id == "log_x_quadratic") {
      out <- out |> filter(x > 0) |> mutate(log_x = log(x), log_x2 = log(x)^2)
    }
  } else if (spec$model_family == "single_ma_plus_spot") {
    out <- out |>
      mutate(
        x = shift_by_days(dt[[get_ma_col_name(spec$source_id, spec$window_days)]], spec$offset_days),
        spot = shift_by_days(dt[[paste0(spec$spot_source_id, "_bbl")]], spec$offset_days)
      )

    if (spec$form_id == "quadratic_plus_spot") {
      out <- out |> mutate(x2 = x^2)
    } else if (spec$form_id == "log_quadratic_plus_spot") {
      out <- out |>
        filter(x > 0, spot > 0) |>
        mutate(log_x = log(x), log_x2 = log(x)^2, log_spot = log(spot))
    }
  } else if (spec$model_family == "two_ma") {
    out <- out |>
      mutate(
        x_short = shift_by_days(dt[[get_ma_col_name(spec$short_source_id, spec$short_window_days)]], spec$offset_days),
        x_long = shift_by_days(dt[[get_ma_col_name(spec$long_source_id, spec$long_window_days)]], spec$offset_days)
      )

    if (spec$form_id == "quadratic_combo") {
      out <- out |> mutate(x_short2 = x_short^2, x_long2 = x_long^2)
    } else if (spec$form_id == "log_combo") {
      out <- out |> filter(x_short > 0, x_long > 0) |>
        mutate(log_x_short = log(x_short), log_x_long = log(x_long))
    } else if (spec$form_id == "log_quadratic_combo") {
      out <- out |> filter(x_short > 0, x_long > 0) |>
        mutate(
          log_x_short = log(x_short),
          log_x_long = log(x_long),
          log_x_short2 = log(x_short)^2,
          log_x_long2 = log(x_long)^2
        )
    }
  } else if (spec$model_family == "two_ma_plus_spot") {
    out <- out |>
      mutate(
        x_short = shift_by_days(dt[[get_ma_col_name(spec$short_source_id, spec$short_window_days)]], spec$offset_days),
        x_long = shift_by_days(dt[[get_ma_col_name(spec$long_source_id, spec$long_window_days)]], spec$offset_days),
        spot = shift_by_days(dt[[paste0(spec$spot_source_id, "_bbl")]], spec$offset_days)
      )

    if (spec$form_id == "log_quadratic_combo_plus_spot") {
      out <- out |> filter(x_short > 0, x_long > 0, spot > 0) |>
        mutate(
          log_x_short = log(x_short),
          log_x_long = log(x_long),
          log_x_short2 = log(x_short)^2,
          log_x_long2 = log(x_long)^2,
          log_spot = log(spot)
        )
    }
  } else {
    stop("Unsupported VLSFO model family: ", spec$model_family)
  }

  out <- if (y_transform == "log") {
    out |> filter(y_raw > 0) |> mutate(y = log(y_raw))
  } else {
    out |> mutate(y = y_raw)
  }

  out |>
    filter(if_all(-date, \(x) is.finite(x))) |>
    arrange(date)
}

predict_from_coef_table <- function(feature_dt, coef_tbl, y_transform, actual_col = "y_raw") {
  eta <- rep(get_coef_from_table(coef_tbl, "(Intercept)"), nrow(feature_dt))
  for (i in seq_len(nrow(coef_tbl))) {
    term <- coef_tbl$term[i]
    if (term == "(Intercept)") next
    eta <- eta + coef_tbl$estimate[i] * feature_dt[[term]]
  }

  if (y_transform == "log") {
    resid_trans <- log(feature_dt[[actual_col]]) - eta
    smear_factor <- mean(exp(resid_trans), na.rm = TRUE)
    pred_raw <- exp(eta) * smear_factor
  } else {
    smear_factor <- 1
    pred_raw <- eta
  }

  list(pred_raw = pred_raw, eta = eta, smear_factor = smear_factor)
}

build_crude_ma_panel <- function(crude_dt, window_grid) {
  out <- as.data.table(crude_dt)
  setorder(out, date)
  for (w in window_grid) {
    out[, (get_ma_col_name("brent", w)) := frollmean(brent_bbl, n = w, align = "right")]
    out[, (get_ma_col_name("wti", w)) := frollmean(wti_bbl, n = w, align = "right")]
  }
  as_tibble(out)
}

get_bridge_window_grid <- function(spec) {
  c(
    spec$window_days,
    spec$short_window_days,
    spec$long_window_days
  ) |>
    na.omit() |>
    unique()
}

prepare_bridge_history_panel <- function(crude_vlsfo_daily, spec) {
  window_grid <- get_bridge_window_grid(spec)
  if (!length(window_grid)) {
    return(crude_vlsfo_daily)
  }

  build_crude_ma_panel(crude_vlsfo_daily, window_grid)
}

expected_bridge_terms <- function(spec) {
  if (spec$model_family == "single_ma") {
    if (spec$form_id == "linear") return("x")
    if (spec$form_id == "quadratic") return(c("x", "x2"))
    if (spec$form_id == "log_x") return("log_x")
    if (spec$form_id == "log_x_quadratic") return(c("log_x", "log_x2"))
  } else if (spec$model_family == "single_ma_plus_spot") {
    if (spec$form_id == "linear_plus_spot") return(c("x", "spot"))
    if (spec$form_id == "quadratic_plus_spot") return(c("x", "x2", "spot"))
    if (spec$form_id == "log_quadratic_plus_spot") return(c("log_x", "log_x2", "log_spot"))
  } else if (spec$model_family == "two_ma") {
    if (spec$form_id == "linear_combo") return(c("x_short", "x_long"))
    if (spec$form_id == "quadratic_combo") return(c("x_short", "x_long", "x_short2", "x_long2"))
    if (spec$form_id == "log_combo") return(c("log_x_short", "log_x_long"))
    if (spec$form_id == "log_quadratic_combo") {
      return(c("log_x_short", "log_x_long", "log_x_short2", "log_x_long2"))
    }
  } else if (spec$model_family == "two_ma_plus_spot") {
    if (spec$form_id == "linear_combo_plus_spot") return(c("x_short", "x_long", "spot"))
    if (spec$form_id == "log_quadratic_combo_plus_spot") {
      return(c("log_x_short", "log_x_long", "log_x_short2", "log_x_long2", "log_spot"))
    }
  }

  stop(
    "Unsupported bridge specification in summary metadata: ",
    spec$model_family, " / ", spec$form_id
  )
}

validate_bridge_artifacts <- function(spec, coef_split) {
  observed_terms <- coef_split |>
    map(\(tbl) setdiff(tbl$term, "(Intercept)")) |>
    unlist(use.names = FALSE) |>
    unique()
  expected_terms <- expected_bridge_terms(spec)

  if (!all(observed_terms %in% expected_terms)) {
    stop(
      paste0(
        "Bridge summary and coefficient files are inconsistent. ",
        "Summary selects ", spec$model_family, " / ", spec$form_id,
        " but coefficients use term(s): ",
        paste(sort(observed_terms), collapse = ", "),
        ". Expected term(s): ",
        paste(expected_terms, collapse = ", "),
        ". Re-run lng_vs_oil/vlsfo_brent_daily_link.R after regenerating the bridge outputs."
      )
    )
  }
}

load_best_vlsfo_bridge <- function(base_dir, crude_vlsfo_daily) {
  summary_path <- file.path(base_dir, "vlsfo_brent_best_model_summary.csv")
  coef_path <- file.path(base_dir, "vlsfo_brent_best_model_coefficients.csv")

  meta_raw <- parse_metric_summary(summary_path)
  coef_dt <- fread(coef_path) |> as_tibble()

  spec <- list(
    target_strategy = normalize_meta_value(meta_raw$selected_target_strategy),
    target_index_id = normalize_meta_value(meta_raw$selected_target_index_id),
    y_transform = normalize_meta_value(meta_raw$selected_y_transform),
    model_family = normalize_meta_value(meta_raw$selected_model_family),
    source_id = normalize_meta_value(meta_raw$selected_source_id),
    short_source_id = normalize_meta_value(meta_raw$selected_short_source_id),
    long_source_id = normalize_meta_value(meta_raw$selected_long_source_id),
    spot_source_id = normalize_meta_value(meta_raw$selected_spot_source_id),
    form_id = normalize_meta_value(meta_raw$selected_form_id),
    window_days = as_int_or_na(meta_raw$selected_window_days),
    short_window_days = as_int_or_na(meta_raw$selected_short_window_days),
    long_window_days = as_int_or_na(meta_raw$selected_long_window_days),
    offset_days = as_int_or_na(meta_raw$selected_brent_offset_days)
  )

  crude_vlsfo_daily <- prepare_bridge_history_panel(crude_vlsfo_daily, spec)
  coef_split <- split(coef_dt, coef_dt$response_component)
  validate_bridge_artifacts(spec, coef_split)
  smear_factors <- list()

  for (component_name in names(coef_split)) {
    feature_dt <- build_vlsfo_feature_frame(
      crude_vlsfo_daily,
      target_col = component_name,
      spec = spec,
      y_transform = spec$y_transform
    )
    pred_obj <- predict_from_coef_table(feature_dt, coef_split[[component_name]], spec$y_transform)
    smear_factors[[component_name]] <- pred_obj$smear_factor
  }

  list(spec = spec, coef_split = coef_split, smear_factors = smear_factors)
}

predict_vlsfo_bridge <- function(crude_panel, bridge, actual_targets = NULL) {
  spec <- bridge$spec
  coef_split <- bridge$coef_split
  y_transform <- spec$y_transform

  if (identical(spec$target_strategy, "direct_avg")) {
    component_name <- names(coef_split)[1]
    feature_dt <- build_vlsfo_feature_frame(
      dt = if (is.null(actual_targets)) mutate(crude_panel, !!component_name := 1) else left_join(crude_panel, actual_targets, by = "date"),
      target_col = component_name,
      spec = spec,
      y_transform = y_transform
    )

    coef_tbl <- coef_split[[component_name]]
    eta <- rep(get_coef_from_table(coef_tbl, "(Intercept)"), nrow(feature_dt))
    for (i in seq_len(nrow(coef_tbl))) {
      term <- coef_tbl$term[i]
      if (term == "(Intercept)") next
      eta <- eta + coef_tbl$estimate[i] * feature_dt[[term]]
    }
    pred_raw <- if (y_transform == "log") exp(eta) * bridge$smear_factors[[component_name]] else eta

    return(tibble(date = feature_dt$date, avg_sing_hou_bbl = pred_raw))
  }

  component_preds <- map(names(coef_split), function(component_name) {
    feature_dt <- build_vlsfo_feature_frame(
      dt = if (is.null(actual_targets)) mutate(crude_panel, !!component_name := 1) else left_join(crude_panel, actual_targets, by = "date"),
      target_col = component_name,
      spec = spec,
      y_transform = y_transform
    )
    coef_tbl <- coef_split[[component_name]]
    eta <- rep(get_coef_from_table(coef_tbl, "(Intercept)"), nrow(feature_dt))
    for (i in seq_len(nrow(coef_tbl))) {
      term <- coef_tbl$term[i]
      if (term == "(Intercept)") next
      eta <- eta + coef_tbl$estimate[i] * feature_dt[[term]]
    }
    pred_raw <- if (y_transform == "log") exp(eta) * bridge$smear_factors[[component_name]] else eta
    tibble(date = feature_dt$date, component = component_name, pred_raw = pred_raw)
  })

  bind_rows(component_preds) |>
    pivot_wider(names_from = component, values_from = pred_raw) |>
    mutate(avg_sing_hou_bbl = (sing_bbl + houston_bbl) / 2)
}

load_heco_history <- function(project_root) {
  fread(file.path(project_root, "miscData", "heco_lsfo.csv")) |>
    as_tibble() |>
    mutate(
      date = as.Date(floor_date(parse_date_time(Date, orders = c("mdy", "ymd", "dmy")), "month")),
      lsfo_bbl = Honolulu_LSFO
    ) |>
    filter(date >= as.Date("2022-02-01"), !is.na(lsfo_bbl)) |>
    select(date, lsfo_bbl) |>
    arrange(date)
}

load_vlsfo_history <- function(base_dir) {
  fread(file.path(base_dir, "vlsfo_prices.csv")) |>
    as_tibble() |>
    transmute(
      date = as.Date(date),
      sing = as.numeric(singapore_vlsfo_usd_mt) / VLSFO_MT_TO_BBL,
      glob = as.numeric(global_20ports_vlsfo_usd_mt) / VLSFO_MT_TO_BBL,
      houston = as.numeric(houston_vlsfo_usd_mt) / VLSFO_MT_TO_BBL,
      avg_sing_hou = (as.numeric(singapore_vlsfo_usd_mt) + as.numeric(houston_vlsfo_usd_mt)) / 2 / VLSFO_MT_TO_BBL
    ) |>
    filter(!is.na(date)) |>
    arrange(date)
}

reconstruct_heco_final_model <- function(heco, vlsfo, n_cores = 1L) {
  y_all <- heco$lsfo_bbl
  sst_h13 <- sum((y_all - mean(y_all))^2)

  break1 <- as.Date("2024-03-01")
  break2 <- as.Date("2024-11-01")
  r1_series <- "avg_sing_hou"
  w1_r1 <- 39L
  w1_r2 <- 41L
  w1_r3 <- 30L
  inv_hi_pre <- 66L
  inv_break1 <- 20L
  inv_break2 <- 0L
  inv_hi_post <- 92L
  curvature <- 1.2
  shock1_dur <- 3L
  shock2_dur <- 2L

  idx_r1 <- which(as.Date(heco$date) < break1)
  idx_r2 <- which(as.Date(heco$date) >= break1 & as.Date(heco$date) < break2)
  idx_r3 <- which(as.Date(heco$date) >= break2)

  r1_months <- as.Date(heco$date[idx_r1])
  r2_months <- as.Date(heco$date[idx_r2])
  r3_months <- as.Date(heco$date[idx_r3])

  path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
  path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
  path_r3 <- build_curved_w2_path(length(r3_months), inv_break2, inv_hi_post, curvature)

  spot_r1 <- vlsfo[[r1_series]]
  spot_rg <- vlsfo$avg_sing_hou
  dates_v <- vlsfo$date
  pred_r1 <- parallel::mcmapply(
    \(d, w2) build_predictor_anchor(spot_r1, dates_v, w1_r1, w2, d, 24L)$pred[1],
    r1_months, path_r1, mc.cores = n_cores
  )
  pred_r2 <- parallel::mcmapply(
    \(d, w2) build_predictor_anchor(spot_rg, dates_v, w1_r2, w2, d, 24L)$pred[1],
    r2_months, path_r2, mc.cores = n_cores
  )
  pred_r3 <- parallel::mcmapply(
    \(d, w2) build_predictor_anchor(spot_rg, dates_v, w1_r3, w2, d, ANCHOR_DAY_R3)$pred[1],
    r3_months, path_r3, mc.cores = n_cores
  )

  df <- bind_rows(
    tibble(date = r1_months, lsfo_bbl = y_all[idx_r1], pred = pred_r1, regime = "R1", inv_days = path_r1),
    tibble(date = r2_months, lsfo_bbl = y_all[idx_r2], pred = pred_r2, regime = "R2", inv_days = path_r2),
    tibble(date = r3_months, lsfo_bbl = y_all[idx_r3], pred = pred_r3, regime = "R3", inv_days = path_r3)
  ) |>
    mutate(regime = factor(regime, levels = c("R1", "R2", "R3"))) |>
    filter(!is.na(pred), !is.na(lsfo_bbl))

  shock1 <- build_shock_pulse(df$date, break1, shock1_dur)
  shock2 <- build_shock_pulse(df$date, break2, shock2_dur)

  fit <- lm(lsfo_bbl ~ pred * regime + shock1 + shock2,
            data = mutate(df, shock1 = shock1, shock2 = shock2))

  ssr <- sum(residuals(fit)^2)
  model_df <- mutate(df, shock1 = shock1, shock2 = shock2,
                     fitted = fitted(fit), residual = residuals(fit))

  list(
    summary = tibble(
      break1 = break1,
      break2 = break2,
      r1_series = r1_series,
      w1_r1 = w1_r1,
      w1_r2 = w1_r2,
      w1_r3 = w1_r3,
      inv_hi_pre = inv_hi_pre,
      inv_break1 = inv_break1,
      inv_break2 = inv_break2,
      inv_hi_post = inv_hi_post,
      curvature = curvature,
      shock1_dur = shock1_dur,
      shock2_dur = shock2_dur,
      r2 = 1 - ssr / sst_h13,
      rmse = sqrt(ssr / nrow(model_df)),
      n = nrow(model_df)
    ),
    fit = fit,
    df = model_df
  )
}

extract_regime_coefficients <- function(fit, regime = "R3") {
  b <- coef(fit)
  list(
    intercept = get_coeff(b, "(Intercept)") + get_coeff(b, paste0("regime", regime)),
    slope = get_coeff(b, "pred") + get_coeff(b, paste0("pred:regime", regime))
  )
}

predict_heco_r3 <- function(fit, pred_values, shock1 = 0, shock2 = 0) {
  newdata <- tibble(
    pred = pred_values,
    regime = factor("R3", levels = c("R1", "R2", "R3")),
    shock1 = shock1,
    shock2 = shock2
  )
  as.numeric(predict(fit, newdata = newdata))
}

build_long_run_curve <- function(bridge, heco_model, crude_range) {
  r1_coef <- extract_regime_coefficients(heco_model$fit, "R1")
  r3_coef <- extract_regime_coefficients(heco_model$fit, "R3")
  curve_dt <- tibble(p = crude_range) |>
    mutate(
      brent_bbl = p,
      wti_bbl = p,
      date = as.Date("2099-01-01")
    )

  curve_crude <- build_crude_ma_panel(curve_dt, window_grid = c(
    bridge$spec$window_days,
    bridge$spec$short_window_days,
    bridge$spec$long_window_days
  ) |> na.omit() |> unique())

  # Under steady-state constant crude, all relevant MAs equal the constant price.
  for (w in c(na.omit(c(bridge$spec$window_days, bridge$spec$short_window_days, bridge$spec$long_window_days)))) {
    curve_crude[[get_ma_col_name("brent", w)]] <- curve_crude$brent_bbl
    curve_crude[[get_ma_col_name("wti", w)]] <- curve_crude$wti_bbl
  }

  vlsfo_long_run <- predict_vlsfo_bridge(curve_crude, bridge)

  curve_dt |>
    transmute(
      p = p,
      avg_sing_hou_bbl = vlsfo_long_run$avg_sing_hou_bbl
    ) |>
    mutate(
      heco_lsfo_bbl   = r3_coef$intercept + r3_coef$slope * avg_sing_hou_bbl,
      r1_lsfo_bbl     = r1_coef$intercept + r1_coef$slope * avg_sing_hou_bbl
    )
}

prepare_short_term_forecast <- function(bridge, heco_model, crude_hist, vlsfo_hist, heco_hist,
                                        n_cores = 1L) {
  latest_daily_date <- max(crude_hist$date)
  latest_brent <- crude_hist$brent_bbl[which.max(crude_hist$date)]
  latest_wti <- crude_hist$wti_bbl[which.max(crude_hist$date)]

  future_daily_dates <- build_weekday_sequence(
    latest_daily_date + days(1),
    latest_daily_date %m+% months(SHORT_TERM_MONTHS_AHEAD)
  )

  future_crude <- tibble(
    date = future_daily_dates,
    brent_bbl = latest_brent,
    wti_bbl = latest_wti
  )

  crude_extended <- bind_rows(
    crude_hist |> select(date, brent_bbl, wti_bbl),
    future_crude
  ) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)

  ma_windows <- c(
    bridge$spec$window_days,
    bridge$spec$short_window_days,
    bridge$spec$long_window_days
  ) |>
    na.omit() |>
    unique()

  crude_panel_extended <- build_crude_ma_panel(crude_extended, ma_windows)
  historical_targets <- vlsfo_hist |> transmute(date, sing_bbl = sing, houston_bbl = houston, avg_sing_hou_bbl = avg_sing_hou)

  # Do NOT pass actual_targets here: build_vlsfo_feature_frame filters rows where
  # y_raw is non-finite, so joining actual targets would drop all future rows whose
  # VLSFO hasn't been observed yet.  Predictions need only the crude MA columns.
  future_vlsfo_pred <- predict_vlsfo_bridge(crude_panel_extended, bridge) |>
    filter(date > latest_daily_date)

  vlsfo_daily_extended <- bind_rows(
    vlsfo_hist |> transmute(date, avg_sing_hou = avg_sing_hou, source = "actual"),
    future_vlsfo_pred |> transmute(date, avg_sing_hou = avg_sing_hou_bbl, source = "projected_flat_crude")
  ) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)

  last_actual_heco_month <- max(heco_hist$date)
  future_heco_months <- seq.Date(last_actual_heco_month %m+% months(1), by = "month", length.out = SHORT_TERM_MONTHS_AHEAD)

  r3_hist_months <- heco_model$df |>
    filter(regime == "R3") |>
    pull(date) |>
    as.Date()
  r3_all_months <- seq.Date(min(r3_hist_months), by = "month", length.out = length(r3_hist_months) + length(future_heco_months))
  r3_all_inv <- build_curved_w2_path(
    n_months = length(r3_all_months),
    start_level = heco_model$summary$inv_break2[[1]],
    end_level = heco_model$summary$inv_hi_post[[1]],
    curvature = heco_model$summary$curvature[[1]]
  )
  future_inv <- tail(r3_all_inv, length(future_heco_months))

  ext_spot  <- vlsfo_daily_extended$avg_sing_hou
  ext_dates <- vlsfo_daily_extended$date
  w1_r3_val <- heco_model$summary$w1_r3[[1]]
  future_heco_pred <- parallel::mcmapply(
    \(m, inv_days) build_predictor_anchor(
      spot         = ext_spot,
      dates        = ext_dates,
      w1           = w1_r3_val,
      w2           = inv_days,
      target_dates = m,
      anchor_day   = ANCHOR_DAY_R3
    )$pred[[1]],
    future_heco_months, future_inv,
    mc.cores = n_cores
  )

  future_heco <- tibble(
    date = future_heco_months,
    inv_days = future_inv,
    vlsfo_predictor_bbl = future_heco_pred,
    projected_lsfo_bbl = predict_heco_r3(heco_model$fit, future_heco_pred, shock1 = 0, shock2 = 0),
    # Store the flat-crude Brent assumption so downstream scripts (policy_analysis.R)
    # can use the same price for LNG comparisons.
    flat_brent_bbl = latest_brent
  )

  list(
    latest_brent = latest_brent,
    latest_wti = latest_wti,
    future_vlsfo_daily = future_vlsfo_pred,
    future_heco_monthly = future_heco,
    # Full actual + bridge-predicted daily VLSFO series — used by policy_analysis.R
    # to build the R1 counterfactual on the same VLSFO basis as R3.
    vlsfo_daily_extended = vlsfo_daily_extended |>
      transmute(date, avg_sing_hou_bbl = avg_sing_hou, source)
  )
}

load_futures_curve <- function(base_dir) {
  curve_path <- file.path(base_dir, "crude_futures_curve.csv")
  if (!file.exists(curve_path)) {
    return(NULL)
  }

  curve <- fread(curve_path) |>
    as_tibble()

  names_lower <- tolower(names(curve))
  names(curve) <- names_lower

  required <- c("contract_month", "brent_bbl", "wti_bbl")
  missing <- setdiff(required, names(curve))
  if (length(missing) > 0) {
    stop("crude_futures_curve.csv is missing columns: ", paste(missing, collapse = ", "))
  }

  curve |>
    transmute(
      contract_month = as.Date(contract_month),
      brent_bbl = as.numeric(brent_bbl),
      wti_bbl = as.numeric(wti_bbl)
    ) |>
    filter(!is.na(contract_month), !is.na(brent_bbl), !is.na(wti_bbl)) |>
    arrange(contract_month)
}

build_long_term_projection <- function(futures_curve, bridge, heco_model, short_term_end_month) {
  if (is.null(futures_curve) || nrow(futures_curve) == 0) return(NULL)

  r3_coef <- extract_regime_coefficients(heco_model$fit, "R3")

  futures_use <- futures_curve |>
    filter(contract_month > short_term_end_month) |>
    arrange(contract_month)

  if (nrow(futures_use) == 0) return(NULL)

  ma_windows <- c(
    bridge$spec$window_days,
    bridge$spec$short_window_days,
    bridge$spec$long_window_days
  ) |>
    na.omit() |>
    unique()

  crude_ss <- futures_use |>
    transmute(
      date = contract_month,
      brent_bbl = brent_bbl,
      wti_bbl = wti_bbl
    )
  crude_ss <- build_crude_ma_panel(crude_ss, ma_windows)

  for (w in ma_windows) {
    crude_ss[[get_ma_col_name("brent", w)]] <- crude_ss$brent_bbl
    crude_ss[[get_ma_col_name("wti", w)]] <- crude_ss$wti_bbl
  }

  vlsfo_ss <- predict_vlsfo_bridge(crude_ss, bridge)

  futures_use |>
    left_join(vlsfo_ss, by = c("contract_month" = "date")) |>
    mutate(
      projected_lsfo_bbl = r3_coef$intercept + r3_coef$slope * avg_sing_hou_bbl
    )
}

plot_and_save <- function(plot_obj, path, width = 10, height = 6) {
  ggsave(path, plot = plot_obj, width = width, height = height, dpi = 160)
}

base_dir     <- get_script_dir()
project_root <- normalizePath(file.path(base_dir, ".."))

# Parallel workers for month-by-month predictor loops.
# Uses fork-based parallelism (mcmapply) — works on macOS/Linux.
# NOTE FOR WINDOWS USERS: mcmapply falls back to sequential on Windows.
# Replace with parallel::clusterMap() + makeCluster()/stopCluster() for
# true Windows parallelism (see vlsfo_brent_daily_link.R for a template).
n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

heco_hist <- load_heco_history(project_root)
vlsfo_hist <- load_vlsfo_history(base_dir)
crude_hist <- fread(file.path(base_dir, "brent_vlsfo_daily_merged.csv")) |>
  as_tibble() |>
  transmute(date = as.Date(date), brent_bbl = as.numeric(brent_bbl), wti_bbl = as.numeric(wti_bbl)) |>
  filter(!is.na(date), !is.na(brent_bbl), !is.na(wti_bbl)) |>
  arrange(date)

crude_vlsfo_daily <- crude_hist |>
  left_join(
    vlsfo_hist |>
      transmute(date, sing_bbl = sing, houston_bbl = houston, avg_sing_hou_bbl = avg_sing_hou),
    by = "date"
  )

bridge <- load_best_vlsfo_bridge(base_dir, crude_vlsfo_daily)
heco_model <- reconstruct_heco_final_model(heco_hist, vlsfo_hist, n_cores = n_cores)

crude_range <- seq(
  floor(min(c(crude_hist$brent_bbl, crude_hist$wti_bbl), na.rm = TRUE) / 5) * 5,
  # Extend well above the historical data ceiling so approxfun in policy_analysis.R
  # never flat-extrapolates.  Both the VLSFO bridge and the HECO regressions are
  # fully linear, so the extrapolation is exact regardless of price level.
  max(ceiling(max(c(crude_hist$brent_bbl, crude_hist$wti_bbl), na.rm = TRUE) / 5) * 5, 160L),
  by = 1
)

long_run_curve <- build_long_run_curve(bridge, heco_model, crude_range)
short_term <- prepare_short_term_forecast(bridge, heco_model, crude_hist, vlsfo_hist, heco_hist,
                                          n_cores = n_cores)
futures_curve <- load_futures_curve(base_dir)
long_term_projection <- build_long_term_projection(
  futures_curve = futures_curve,
  bridge = bridge,
  heco_model = heco_model,
  short_term_end_month = max(short_term$future_heco_monthly$date)
)

# Export model parameters and per-regime coefficients so policy_analysis.R can
# build counterfactuals without re-running the full reconstruction.
r1_coef <- extract_regime_coefficients(heco_model$fit, "R1")
r2_coef <- extract_regime_coefficients(heco_model$fit, "R2")
r3_coef <- extract_regime_coefficients(heco_model$fit, "R3")

model_params_dt <- data.table(
  parameter = c(
    "break1", "break2",
    "w1_r1", "w1_r2", "w1_r3",
    "anchor_day_r3",
    "inv_hi_pre", "inv_break1", "inv_break2", "inv_hi_post",
    "curvature",
    "shock1_dur", "shock2_dur",
    "r1_intercept", "r1_slope",
    "r2_intercept", "r2_slope",
    "r3_intercept", "r3_slope",
    "model_r2", "model_rmse", "model_n"
  ),
  value = c(
    as.character(heco_model$summary$break1),
    as.character(heco_model$summary$break2),
    as.character(heco_model$summary$w1_r1),
    as.character(heco_model$summary$w1_r2),
    as.character(heco_model$summary$w1_r3),
    as.character(ANCHOR_DAY_R3),
    as.character(heco_model$summary$inv_hi_pre),
    as.character(heco_model$summary$inv_break1),
    as.character(heco_model$summary$inv_break2),
    as.character(heco_model$summary$inv_hi_post),
    as.character(heco_model$summary$curvature),
    as.character(heco_model$summary$shock1_dur),
    as.character(heco_model$summary$shock2_dur),
    as.character(r1_coef$intercept),
    as.character(r1_coef$slope),
    as.character(r2_coef$intercept),
    as.character(r2_coef$slope),
    as.character(r3_coef$intercept),
    as.character(r3_coef$slope),
    as.character(heco_model$summary$r2),
    as.character(heco_model$summary$rmse),
    as.character(heco_model$summary$n)
  )
)

model_params_path <- file.path(base_dir, "heco_lsfo_model_params.csv")
fwrite(model_params_dt, model_params_path)
cat("Saved:", model_params_path, "\n")

long_run_curve_path <- file.path(base_dir, "heco_lsfo_long_run_curve.csv")
long_run_curve_png <- file.path(base_dir, "heco_lsfo_long_run_curve.png")
short_vlsfo_path <- file.path(base_dir, "heco_lsfo_short_term_daily_vlsfo_projection.csv")
short_heco_path <- file.path(base_dir, "heco_lsfo_short_term_monthly_projection.csv")
short_heco_png <- file.path(base_dir, "heco_lsfo_short_term_monthly_projection.png")
long_term_path <- file.path(base_dir, "heco_lsfo_long_term_projection.csv")
long_term_png <- file.path(base_dir, "heco_lsfo_long_term_projection.png")

fwrite(long_run_curve, long_run_curve_path)
fwrite(short_term$vlsfo_daily_extended, short_vlsfo_path)
fwrite(short_term$future_heco_monthly, short_heco_path)

p_long_run <- ggplot(long_run_curve, aes(x = p, y = heco_lsfo_bbl)) +
  geom_line(color = "#1b7837", linewidth = 1.1) +
  labs(
    title = "Long-run HECO LSFO under Brent = WTI = p",
    subtitle = "Current-contract steady state after VLSFO and HECO contract/inventory dynamics fully settle",
    x = "Brent = WTI crude price ($/bbl)",
    y = "HECO LSFO ($/bbl)"
  ) +
  theme_minimal(base_size = 11)
plot_and_save(p_long_run, long_run_curve_png)

p_short_heco <- ggplot(
  bind_rows(
    heco_hist |> mutate(series = "Historical HECO LSFO", value = lsfo_bbl) |> select(date, series, value),
    short_term$future_heco_monthly |> mutate(series = "Projected HECO LSFO", value = projected_lsfo_bbl) |> select(date, series, value)
  ),
  aes(x = date, y = value, color = series)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Short-run HECO LSFO projection",
    subtitle = sprintf(
      "Brent and WTI held flat at latest daily levels: Brent %.2f, WTI %.2f $/bbl",
      short_term$latest_brent, short_term$latest_wti
    ),
    x = NULL,
    y = "HECO LSFO ($/bbl)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
plot_and_save(p_short_heco, short_heco_png)

if (!is.null(long_term_projection) && nrow(long_term_projection) > 0) {
  fwrite(long_term_projection, long_term_path)

  p_long_term <- ggplot(long_term_projection, aes(x = contract_month, y = projected_lsfo_bbl)) +
    geom_line(color = "#762a83", linewidth = 1.1) +
    geom_point(size = 1.6, color = "#762a83") +
    labs(
      title = "Long-run HECO LSFO projection from Brent/WTI futures curve",
      subtitle = "Steady-state current-contract mapping applied to monthly Brent and WTI futures prices",
      x = NULL,
      y = "HECO LSFO ($/bbl)"
    ) +
    theme_minimal(base_size = 11)
  plot_and_save(p_long_term, long_term_png)
}

cat("Saved:", long_run_curve_path, "\n")
cat("Saved:", long_run_curve_png, "\n")
cat("Saved:", short_vlsfo_path, "\n")
cat("Saved:", short_heco_path, "\n")
cat("Saved:", short_heco_png, "\n")
if (!is.null(long_term_projection) && nrow(long_term_projection) > 0) {
  cat("Saved:", long_term_path, "\n")
  cat("Saved:", long_term_png, "\n")
} else {
  cat("Skipped long-term futures projection: provide crude_futures_curve.csv with columns contract_month, brent_bbl, wti_bbl; run investing_crude_futures_update.py to generate it\n")
}
