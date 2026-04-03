# heco_lsfo_vlsfo.R
#
# Purpose:
#   Reconstruct and explain HECO's reported monthly LSFO purchase price using
#   public VLSFO market indices. The central idea is that HECO's reported price
#   reflects a nested moving-average structure: a contract-price average over
#   published market quotes, layered with an inventory-like averaging window
#   that carries costs into later months.
#
# Default behavior:
#   The script now runs in a fast "production" mode. It skips the long
#   exploratory searches and directly reconstructs the selected final H20-fast-8
#   model, then produces:
#     1. a benchmark comparison against a small set of candidate models,
#     2. fitted-vs-actual and residual diagnostics,
#     3. pre-/post-contract relationship plots with uncertainty bands,
#     4. the implied inventory path, and
#     5. a forward projection under flat market prices and constant inventory,
#     6. an old-contract counterfactual path through the transition and new
#        contract periods, and
#     7. historical and projected fuel-expense savings estimates from the
#        contract renegotiation.
#
# Exploratory behavior:
#   Set RUN_EXPLORATORY <- TRUE below to rerun the intermediate model searches
#   (H1-H20) that led to the final specification. Those blocks remain in the
#   script for replication and auditability.
#
# Documentation:
#   See /Users/mike/EIA/lng_vs_oil/heco_lsfo_vlsfo_readme.txt for a fuller
#   narrative of the model sequence, findings, and intended usage.

library(tidyverse)
library(lubridate)
library(slider)

# ── 1. Load ────────────────────────────────────────────────────────────────────
heco_raw  <- read_csv("/Users/mike/EIA/miscData/heco_lsfo.csv",  show_col_types = FALSE)
vlsfo_raw <- read_csv("/Users/mike/EIA/lng_vs_oil/vlsfo_prices.csv", show_col_types = FALSE)

VLSFO_MT_TO_BBL <- 6.35

heco <- heco_raw |>
  mutate(date = as.Date(floor_date(parse_date_time(Date, orders = c("mdy","ymd","dmy")), "month"))) |>
  filter(date >= as.Date("2022-02-01")) |>
  select(date, lsfo_bbl = Honolulu_LSFO) |>
  filter(!is.na(lsfo_bbl)) |>
  arrange(date)

# Identify the global 20-port column name robustly.
glob_col <- grep("global|20port", names(vlsfo_raw), ignore.case = TRUE, value = TRUE)[1]

vlsfo <- vlsfo_raw |>
  transmute(
    date     = as.Date(date),
    sing     = singapore_vlsfo_usd_mt          / VLSFO_MT_TO_BBL,
    glob     = .data[[glob_col]]               / VLSFO_MT_TO_BBL,
    houston  = houston_vlsfo_usd_mt            / VLSFO_MT_TO_BBL,
    # Singapore + global average proxy used in some exploratory pre-contract tests
    avg_sing_glob = (singapore_vlsfo_usd_mt + .data[[glob_col]]) / 2 / VLSFO_MT_TO_BBL,
    # Main candidate index for the 2024+ contract regime
    avg_sing_hou = (singapore_vlsfo_usd_mt + houston_vlsfo_usd_mt) / 2 / VLSFO_MT_TO_BBL
  ) |>
  filter(!is.na(sing)) |>
  arrange(date)

cat(sprintf("HECO: %d months (%s to %s)\n", nrow(heco), min(heco$date), max(heco$date)))
cat(sprintf("VLSFO daily: %d obs (%s to %s)\n", nrow(vlsfo), min(vlsfo$date), max(vlsfo$date)))

# ── 2. Predictor builder ───────────────────────────────────────────────────────
# For HECO month t:
#   anchor = day 24 of month (t-1)
#   contract_price(d) = mean(spot[d-w1+1 .. d])   [layer 1]
#   predictor = mean(contract_price[anchor-w2+1 .. anchor])  [layer 2]
#
# Arguments:
#   spot         - numeric vector of daily VLSFO spot prices
#   dates        - Date vector aligned with spot (same length)
#   w1           - layer-1 contract MA window (days)
#   w2           - layer-2 inventory window (days)
#   target_dates - Date vector of months for which to compute the predictor
#                  (typically heco$date; passed explicitly to avoid implicit
#                  dependency on the global heco object)
#
# Returns a tibble with columns: date, pred

build_predictor <- function(spot, dates, w1, w2, target_dates) {
  cp <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily <- tibble(date = dates, cp = cp) |> filter(!is.na(cp))

  # Convert dates to integer once for binary search via findInterval()
  daily_int <- as.integer(daily$date)

  tibble(date = target_dates) |>
    mutate(pred = map_dbl(date, function(m) {
      # anchor = day 24 of month (t-1), stated explicitly
      anchor    <- floor_date(as.Date(m), "month") %m-% months(1) + days(23)
      win_start <- anchor - days(w2 - 1L)

      # Binary search: findInterval returns the index of the last value <= x.
      # i_end   = last daily obs on or before anchor
      # i_start = last daily obs strictly before win_start, so first included
      #           obs is i_start + 1
      i_end   <- findInterval(as.integer(anchor),    daily_int)
      i_start <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

# ── Blend-weight calculator ────────────────────────────────────────────────────
# Computes, for each HECO month, the fraction of the w2-day inventory window
# that falls on or after contract_date (i.e., the "new-regime" weight).
#
# For HECO month m:
#   anchor    = day 24 of month (t-1)  [floor_date(m) %m-% months(1) + days(23)]
#   win_start = anchor - (w2 - 1) days
#   new_days  = calendar days in [max(win_start, contract_date), anchor]
#   blend     = min(new_days / w2, 1)
#
# A blend of 0 means the entire inventory was priced under the old regime;
# a blend of 1 means the entire window falls under the new regime.
#
# Arguments:
#   heco_months   - Date vector of HECO month-start dates
#   contract_date - single Date; the day the new pricing regime began
#   w2            - inventory window length (days)
#
# Returns a numeric vector of blend weights in [0, 1], same length as heco_months.

compute_blend <- function(heco_months, contract_date, w2) {
  map_dbl(heco_months, function(m) {
    anchor    <- floor_date(as.Date(m), "month") %m-% months(1) + days(23)
    win_start <- anchor - days(w2 - 1L)
    new_days  <- as.numeric(pmax(0, anchor - pmax(win_start, as.Date(contract_date) - 1)))
    min(new_days / w2, 1)
  })
}

# ── Fast default execution -----------------------------------------------------
# By default the script skips the large exploratory searches and instead
# reconstructs the selected final H20 model directly from its chosen
# parameters. Set RUN_EXPLORATORY <- TRUE to rerun the intermediate model
# searches documented below.

RUN_EXPLORATORY <- FALSE
y_all <- heco$lsfo_bbl
sst_h13 <- sum((y_all - mean(y_all))^2)
anchor_day_h14 <- 24L

# Shared helpers used by the fast final-model reconstruction and, when enabled,
# by the exploratory search path.
build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end     <- findInterval(as.integer(anchor), daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
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

flag_boundary <- function(label, value, seq_vals) {
  seq_vals <- seq_vals[!is.na(seq_vals)]
  if (length(seq_vals) == 0L || length(value) != 1L || is.na(value)) return(invisible(FALSE))
  hit <- value <= min(seq_vals) || value >= max(seq_vals)
  if (isTRUE(hit)) {
    cat(sprintf("Boundary flag: %s = %s is at edge [%s, %s]\n",
                label, as.character(value), as.character(min(seq_vals)), as.character(max(seq_vals))))
  }
  invisible(hit)
}

# ── 3. Grid search ─────────────────────────────────────────────────────────────
# w1: 1 (= spot) then 5-90 in 3-day steps
# w2: inventory window 60-150 days in 5-day steps
if (RUN_EXPLORATORY) {

# Findings summary for the commented search path:
# - H17: strong three-regime mixed-index refinement, RMSE about 1.27
# - H19: cost-layer release interpretation underperformed, RMSE about 2.14
# - H20-fast-7: smooth inventory + repricing shocks, RMSE about 0.915
# - H20-fast-8: nested non-linear R3 rebuild further improved fit to RMSE about 0.902
# Uncomment by setting RUN_EXPLORATORY <- TRUE if full replication of the search
# path is desired.

w1_seq      <- c(1L, seq(5L, 90L, by = 3L))
w2_seq      <- seq(60L, 150L, by = 5L)
series_list <- c("sing", "glob", "avg_sing_hou")

grid <- expand_grid(series = series_list, w1 = w1_seq, w2 = w2_seq) |>
  mutate(r2 = NA_real_, rmse = NA_real_, intercept = NA_real_,
         slope = NA_real_, n = NA_integer_)

cat(sprintf("Running %d grid cells...\n", nrow(grid)))

for (i in seq_len(nrow(grid))) {
  s  <- grid$series[i]
  w1 <- grid$w1[i]
  w2 <- grid$w2[i]

  pred_df <- build_predictor(vlsfo[[s]], vlsfo$date, w1, w2, heco$date)
  df <- inner_join(heco, pred_df, by = "date") |>
    filter(!is.na(lsfo_bbl), !is.na(pred))

  if (nrow(df) < 5) next

  fit <- lm(lsfo_bbl ~ pred, data = df)
  ss  <- summary(fit)
  grid$r2[i]        <- ss$r.squared
  grid$rmse[i]      <- sqrt(mean(residuals(fit)^2))
  grid$intercept[i] <- coef(fit)[1]
  grid$slope[i]     <- coef(fit)[2]
  grid$n[i]         <- nrow(df)
}

results <- grid |> filter(!is.na(r2)) |> arrange(desc(r2))

# ── 4. Report ──────────────────────────────────────────────────────────────────
cat("\n── Top 15 fits across all series ───────────────────────────────────\n")
results |> slice_head(n = 15) |>
  mutate(across(c(r2, rmse, slope, intercept), \(x) round(x, 4))) |>
  print()

cat("\n── Best fit per series ──────────────────────────────────────────────\n")
results |>
  slice_max(r2, by = series) |>
  select(series, w1, w2, r2, rmse, slope, intercept, n) |>
  mutate(across(c(r2, rmse, slope, intercept), \(x) round(x, 4))) |>
  arrange(desc(r2)) |>
  print()

best <- results |> slice_head(n = 1)
cat(sprintf(
  "\nBest: series=%s | w1=%d days | w2=%d days | R2=%.4f | RMSE=%.4f $/BBL\n",
  best$series, best$w1, best$w2, best$r2, best$rmse
))
cat(sprintf(
  "  LSFO = %.4f + %.4f * nested_MA  [n=%d]\n\n",
  best$intercept, best$slope, best$n
))

# ── 5. Rebuild best series ─────────────────────────────────────────────────────
df_best <- heco |>
  inner_join(build_predictor(vlsfo[[best$series]], vlsfo$date, best$w1, best$w2, heco$date),
             by = "date") |>
  filter(!is.na(lsfo_bbl), !is.na(pred)) |>
  mutate(fitted   = best$intercept + best$slope * pred,
         residual = lsfo_bbl - fitted)

# ── 6. Plots ───────────────────────────────────────────────────────────────────

# 6a. Heatmap: w1 x w2 for best series
p_heat <- results |>
  filter(series == best$series) |>
  ggplot(aes(x = factor(w1), y = factor(w2), fill = r2)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "R2", limits = c(0, 1)) +
  scale_x_discrete(breaks = as.character(c(1, seq(10, 90, by = 15)))) +
  labs(title = sprintf("R2 heatmap: w1 (contract MA) x w2 (inventory) — %s", best$series),
       x = "w1: contract MA (days)", y = "w2: inventory window (days)") +
  theme_minimal(base_size = 10)
print(p_heat)

# 6b. Best R2 by w2 per series
p_w2 <- results |>
  summarise(best_r2 = max(r2), best_w1 = w1[which.max(r2)], .by = c(series, w2)) |>
  ggplot(aes(x = w2, y = best_r2, color = series)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Best R2 by inventory window (w2), optimised over w1",
       x = "w2: inventory window (days)", y = "Best R2", color = NULL) +
  theme_minimal()
print(p_w2)

# 6c. Time series: actual vs fitted
p_ts <- df_best |>
  select(date, Actual = lsfo_bbl, Fitted = fitted) |>
  pivot_longer(-date) |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(Actual = "#1b7837", Fitted = "#762a83")) +
  labs(title = sprintf("HECO LSFO vs Nested MA (%s, w1=%dd, w2=%dd)",
                       best$series, best$w1, best$w2),
       subtitle = sprintf("R2=%.3f | RMSE=%.2f $/BBL | slope=%.3f | intercept=%.2f | n=%d",
                          best$r2, best$rmse, best$slope, best$intercept, best$n),
       x = NULL, y = "Price ($/BBL)", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
print(p_ts)

# 6d. Scatter
p_scatter <- df_best |>
  ggplot(aes(x = pred, y = lsfo_bbl)) +
  geom_point(size = 2.5, alpha = 0.8, color = "#1b7837") +
  geom_abline(slope = best$slope, intercept = best$intercept,
              color = "#762a83", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  labs(title = "HECO LSFO vs Best Nested VLSFO MA",
       subtitle = "Dashed = 1:1; solid = OLS",
       x = sprintf("Nested MA (%s, w1=%d, w2=%d days) $/BBL", best$series, best$w1, best$w2),
       y = "HECO LSFO ($/BBL)") +
  theme_minimal()
print(p_scatter)

# 6e. Residuals
p_resid <- df_best |>
  ggplot(aes(x = date, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(color = "#1b7837", linewidth = 0.8) +
  geom_point(color = "#1b7837", size = 2) +
  labs(title = "Residuals over time (actual - fitted)",
       x = NULL, y = "Residual ($/BBL)") +
  theme_minimal()
print(p_resid)


# ══════════════════════════════════════════════════════════════════════════════
# PART 2: Model extensions to explain residual structure
#
# Three hypotheses:
#   H1: Rate-of-change effect  -- add d(pred)/dt as second predictor
#   H2: Structural break       -- search for break date; allow different
#                                 intercept+slope in each half
#   H3: Time-varying w2        -- search over (w2_early, w2_late, break_date)
#
# All use best (series, w1, w2) from Part 1 as the base predictor.
# ══════════════════════════════════════════════════════════════════════════════

# ── Base predictor (best from Part 1) ─────────────────────────────────────────
# Uses: best$series, best$w1, best$w2, vlsfo, heco$date

base_pred <- build_predictor(vlsfo[[best$series]], vlsfo$date,
                             best$w1, best$w2, heco$date)


df_ext <- heco |>
  mutate(
    pred     = base_pred$pred,
    # month-over-month change in predictor (rate of change)
    d_pred   = pred - lag(pred),
    # time index for break search
    t        = as.numeric(date)
  ) |>
  filter(!is.na(pred), !is.na(d_pred))

cat(sprintf("Extension dataset: %d months\n", nrow(df_ext)))

# ── H1: Rate-of-change model ───────────────────────────────────────────────────
# LSFO ~ pred + d(pred)
# If slope on d_pred is negative: when prices falling fast, HECO pays more
# than current MA (inventory bought at higher prices) -> negative residual

fit_h1 <- lm(lsfo_bbl ~ pred + d_pred, data = df_ext)
ss_h1  <- summary(fit_h1)

cat("\n── H1: Rate-of-change model ─────────────────────────────────────────\n")
print(ss_h1)
cat(sprintf("RMSE: %.4f $/BBL\n", sqrt(mean(residuals(fit_h1)^2))))

# ── H2: Structural break -- searched break date ────────────────────────────────
# For each candidate break date, fit two separate OLS models (pre/post)
# Minimise total SSR (equivalent to maximising combined R²)
# Candidate breaks: every month from 6 months in to 6 months from end

candidate_breaks <- df_ext$date[7:(nrow(df_ext) - 6)]

break_results <- map_dfr(candidate_breaks, function(bd) {
  pre  <- df_ext |> filter(date <  bd)
  post <- df_ext |> filter(date >= bd)

  if (nrow(pre) < 4 || nrow(post) < 4) return(NULL)

  fit_pre  <- lm(lsfo_bbl ~ pred, data = pre)
  fit_post <- lm(lsfo_bbl ~ pred, data = post)

  ssr <- sum(residuals(fit_pre)^2) + sum(residuals(fit_post)^2)
  sst <- sum((df_ext$lsfo_bbl - mean(df_ext$lsfo_bbl))^2)

  tibble(
    break_date  = bd,
    r2_combined = 1 - ssr / sst,
    rmse        = sqrt(ssr / nrow(df_ext)),
    n_pre       = nrow(pre),
    n_post      = nrow(post),
    int_pre     = coef(fit_pre)[1],
    slope_pre   = coef(fit_pre)[2],
    int_post    = coef(fit_post)[1],
    slope_post  = coef(fit_post)[2]
  )
})

best_break <- break_results |> slice_max(r2_combined, n = 1)

cat("\n── H2: Structural break model ───────────────────────────────────────\n")
cat(sprintf("Best break date: %s\n", best_break$break_date))
cat(sprintf("R² (combined):   %.4f\n", best_break$r2_combined))
cat(sprintf("RMSE:            %.4f $/BBL\n", best_break$rmse))
cat(sprintf("Pre-break:  intercept=%.3f  slope=%.4f  n=%d\n",
            best_break$int_pre,  best_break$slope_pre,  best_break$n_pre))
cat(sprintf("Post-break: intercept=%.3f  slope=%.4f  n=%d\n",
            best_break$int_post, best_break$slope_post, best_break$n_post))

# ── H3: Time-varying w2 (shrinking inventory) ─────────────────────────────────
# Split data at a searched break date; use different w2 in each half.
# Fix w1 = best$w1 and series = best$series; search over w2_early, w2_late, break.
# Grid: w2_early in 30:120 (5-day), w2_late in 20:100 (5-day), break every 6 months

w2_early_seq   <- seq(30L, 120L, by = 10L)
w2_late_seq    <- seq(20L,  100L, by = 10L)
break_seq      <- df_ext$date[6:(nrow(df_ext) - 6)]
# Thin break_seq to every ~3 months to keep runtime reasonable
break_seq      <- break_seq[seq(1, length(break_seq), by = 3)]

cat(sprintf("\nH3 grid: %d cells — may take ~30 seconds...\n",
            length(w2_early_seq) * length(w2_late_seq) * length(break_seq)))

h3_results <- expand_grid(
  w2e = w2_early_seq,
  w2l = w2_late_seq,
  bd  = break_seq
) |>
  mutate(r2 = NA_real_, rmse = NA_real_)

spot <- vlsfo[[best$series]]

# Pre-compute all unique predictor tibbles (one slide_dbl pass per unique w2).
# Keyed as character(w2) for O(1) lookup inside the loop.
all_w2_h3 <- unique(c(w2_early_seq, w2_late_seq))
pred_cache_h3 <- setNames(
  lapply(all_w2_h3, \(w2)
    build_predictor(spot, vlsfo$date, best$w1, w2, heco$date)),
  as.character(all_w2_h3)
)

for (i in seq_len(nrow(h3_results))) {
  w2e <- h3_results$w2e[i]
  w2l <- h3_results$w2l[i]
  bd  <- h3_results$bd[i]

  pred_early <- pred_cache_h3[[as.character(w2e)]] |> filter(date <  bd)
  pred_late  <- pred_cache_h3[[as.character(w2l)]] |> filter(date >= bd)

  df_h3 <- bind_rows(
    heco |> filter(date <  bd) |> left_join(pred_early, by = "date"),
    heco |> filter(date >= bd) |> left_join(pred_late,  by = "date")
  ) |> filter(!is.na(lsfo_bbl), !is.na(pred))

  if (nrow(df_h3) < 8) next

  fit  <- lm(lsfo_bbl ~ pred, data = df_h3)
  ssr  <- sum(residuals(fit)^2)
  sst  <- sum((df_h3$lsfo_bbl - mean(df_h3$lsfo_bbl))^2)

  h3_results$r2[i]   <- 1 - ssr / sst
  h3_results$rmse[i] <- sqrt(ssr / nrow(df_h3))
}

best_h3 <- h3_results |> filter(!is.na(r2)) |> slice_max(r2, n = 1)

cat("\n── H3: Time-varying w2 model ────────────────────────────────────────\n")
cat(sprintf("Best break date: %s\n", best_h3$bd))
cat(sprintf("w2 early:        %d days  (~%.1f months)\n",
            best_h3$w2e, best_h3$w2e / 30.4))
cat(sprintf("w2 late:         %d days  (~%.1f months)\n",
            best_h3$w2l, best_h3$w2l / 30.4))
cat(sprintf("R²:              %.4f\n", best_h3$r2))
cat(sprintf("RMSE:            %.4f $/BBL\n", best_h3$rmse))

# ── Summary comparison ─────────────────────────────────────────────────────────
cat("\n══ Model comparison ══════════════════════════════════════════════════\n")
base_r2   <- summary(lm(lsfo_bbl ~ pred, data = df_ext))$r.squared
base_rmse <- sqrt(mean(residuals(lm(lsfo_bbl ~ pred, data = df_ext))^2))

tibble(
  Model        = c("Base (fixed w2)",
                   "H1: + rate-of-change",
                   "H2: structural break (coefficients)",
                   "H3: structural break (w2 only)"),
  R2           = c(base_r2,
                   ss_h1$r.squared,
                   best_break$r2_combined,
                   best_h3$r2),
  RMSE         = c(base_rmse,
                   sqrt(mean(residuals(fit_h1)^2)),
                   best_break$rmse,
                   best_h3$rmse),
  Extra_params = c(0L, 1L, 2L, 2L)
) |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4))) |>
  print()

# ── Plots ──────────────────────────────────────────────────────────────────────

# H2: R² by break date
p_break <- break_results |>
  ggplot(aes(x = break_date, y = r2_combined)) +
  geom_line(color = "#762a83", linewidth = 1) +
  geom_vline(xintercept = best_break$break_date,
             linetype = "dashed", color = "grey40") +
  annotate("text", x = best_break$break_date, y = min(break_results$r2_combined),
           label = format(best_break$break_date, "%b %Y"),
           hjust = -0.1, size = 3.5) +
  labs(title = "H2: Combined R² by structural break date",
       x = "Break date", y = "Combined R²") +
  theme_minimal()
print(p_break)

# H2: fitted values pre/post break
df_h2 <- bind_rows(
  df_ext |> filter(date <  best_break$break_date) |>
    mutate(fitted = best_break$int_pre  + best_break$slope_pre  * pred),
  df_ext |> filter(date >= best_break$break_date) |>
    mutate(fitted = best_break$int_post + best_break$slope_post * pred)
) |> mutate(residual = lsfo_bbl - fitted)

p_h2_ts <- df_h2 |>
  select(date, `HECO LSFO (actual)` = lsfo_bbl, `H2 fitted` = fitted) |>
  pivot_longer(-date) |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = best_break$break_date,
             linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("HECO LSFO (actual)" = "#1b7837",
                                "H2 fitted"          = "#762a83")) +
  labs(
    title = sprintf("H2: Structural break at %s", format(best_break$break_date, "%b %Y")),
    subtitle = sprintf("R²=%.3f | RMSE=%.2f | pre slope=%.3f | post slope=%.3f",
                       best_break$r2_combined, best_break$rmse,
                       best_break$slope_pre,   best_break$slope_post),
    x = NULL, y = "Price ($/BBL)", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_h2_ts)

# H2 residuals
p_h2_resid <- df_h2 |>
  ggplot(aes(x = date, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = best_break$break_date,
             linetype = "dashed", color = "grey40") +
  geom_line(color = "#1b7837", linewidth = 0.8) +
  geom_point(color = "#1b7837", size = 2) +
  labs(title = "H2 residuals over time", x = NULL, y = "Residual ($/BBL)") +
  theme_minimal()
print(p_h2_resid)

# ══════════════════════════════════════════════════════════════════════════════
# PART 3: Blended-regime model
#
# Hypothesis: contract changed on a single date (contract_date), but because
# HECO inventory is a rolling w2-day average of daily contract prices, the
# *reported* monthly price transitions gradually over ~w2 days after the break.
#
# For each HECO month t:
#   anchor = day 24 of month (t-1)
#   inventory window = [anchor - w2 + 1, anchor]
#   days in window before contract_date -> priced under OLD regime
#   days in window on/after contract_date -> priced under NEW regime
#   blend_weight = fraction of inventory window under NEW regime (0 to 1)
#
# Model:
#   pred_old = nested MA predictor using old-regime coefficients
#   pred_new = nested MA predictor (same w1, w2, series -- only coefs change)
#   pred_blended = (1 - blend) * pred_old_coefs + blend * pred_new_coefs
#
# Since both regimes use the same physical VLSFO MA predictor (pred), this
# simplifies to a single blended OLS:
#   lsfo_bbl ~ (1-blend)*pred + blend*pred  [same predictor, different coefs]
#
# More precisely: we fit intercept and slope as functions of blend_weight:
#   intercept(blend) = int_old + blend * (int_new - int_old)
#   slope(blend)     = slope_old + blend * (slope_new - slope_old)
#
# Grid search over contract_date; for each date compute blend_weight per month,
# then fit: lsfo_bbl ~ pred + blend*pred  (i.e., include blend:pred interaction)
# which gives intercept and slope as linear functions of blend.
# ══════════════════════════════════════════════════════════════════════════════


# ── Selected model: H4 blended transition ─────────────────────────────────────
# Contract changed sometime in late 2023 / early 2024:
#   Old regime: Singapore VLSFO only          -> fit OLS on sing
#   New regime: avg of Singapore + Houston    -> fit OLS on avg_sing_hou
# Both the index AND the coefficients change at the break.
# Grid-search over candidate break dates to find the empirically optimal one.

# Build regime predictors once (w1, w2 fixed at best from Part 1)
pred_old_full <- build_predictor(vlsfo$sing,         vlsfo$date, best$w1, best$w2, heco$date)
pred_new_full <- build_predictor(vlsfo$avg_sing_hou, vlsfo$date, best$w1, best$w2, heco$date)

df_h4_base <- heco |>
  mutate(
    pred_old = pred_old_full$pred,
    pred_new = pred_new_full$pred
  ) |>
  filter(!is.na(pred_old), !is.na(pred_new))

# Candidate break dates: monthly, leaving at least 6 obs on each side
h4_candidate_dates <- df_h4_base$date[7:(nrow(df_h4_base) - 6)]

h4_grid <- map_dfr(h4_candidate_dates, function(cd) {
  blend <- compute_blend(df_h4_base$date, cd, best$w2)

  df <- df_h4_base |> mutate(blend = blend)

  df_o <- df |> filter(blend <= 0.05)
  df_n <- df |> filter(blend >= 0.50)

  if (nrow(df_o) < 3 || nrow(df_n) < 3) return(NULL)

  fo <- lm(lsfo_bbl ~ pred_old, data = df_o)
  fn <- lm(lsfo_bbl ~ pred_new, data = df_n)

  io <- coef(fo)[1]; so <- coef(fo)[2]
  int_new <- coef(fn)[1]; sn <- coef(fn)[2]

  df <- df |> mutate(
    fitted   = (1 - blend) * (io + so * pred_old) + blend * (int_new + sn * pred_new),
    residual = lsfo_bbl - fitted
  )

  ssr <- sum(df$residual^2, na.rm = TRUE)
  sst <- sum((df$lsfo_bbl - mean(df$lsfo_bbl))^2, na.rm = TRUE)

  tibble(
    contract_date = cd,
    r2   = 1 - ssr / sst,
    rmse = sqrt(ssr / nrow(df)),
    n_old = nrow(df_o),
    n_new = nrow(df_n),
    int_old = io, slope_old = so,
    int_new = int_new, slope_new = sn
  )
})

best_h4 <- h4_grid |> slice_max(r2, n = 1)
CONTRACT_DATE <- best_h4$contract_date

cat(sprintf("\nH4 break date grid: %d candidates evaluated\n", nrow(h4_grid)))
cat(sprintf("Optimal contract date: %s\n", CONTRACT_DATE))
cat(sprintf("R²=%.4f | RMSE=%.4f | n_old=%d | n_new=%d\n",
            best_h4$r2, best_h4$rmse, best_h4$n_old, best_h4$n_new))

# Plot R² by candidate break date
p_h4_break <- h4_grid |>
  ggplot(aes(x = contract_date, y = r2)) +
  geom_line(color = "#762a83", linewidth = 1) +
  geom_vline(xintercept = CONTRACT_DATE, linetype = "dashed", color = "grey40") +
  annotate("text", x = CONTRACT_DATE, y = min(h4_grid$r2),
           label = format(CONTRACT_DATE, "%b %Y"),
           hjust = -0.1, size = 3.5) +
  labs(title = "H4: R² by candidate contract break date",
       subtitle = "Old regime = sing; New regime = avg_sing_hou",
       x = "Candidate break date", y = "Overall R²") +
  theme_minimal()
print(p_h4_break)

# ── Rebuild final df_final with optimal break date ─────────────────────────────
blend_final <- compute_blend(heco$date, CONTRACT_DATE, best$w2)

df_final <- heco |>
  mutate(
    pred_old = pred_old_full$pred,
    pred_new = pred_new_full$pred,
    blend    = blend_final
  ) |>
  filter(!is.na(pred_old), !is.na(pred_new))

df_old <- df_final |> filter(blend <= 0.05)
df_new <- df_final |> filter(blend >= 0.50)

cat(sprintf("Regime obs for OLS — old (blend<=0.05): %d | new (blend>=0.50): %d\n",
            nrow(df_old), nrow(df_new)))

if (nrow(df_old) < 3) stop("Too few old-regime months to fit OLS (blend <= 0.05)")
if (nrow(df_new) < 3) stop("Too few new-regime months to fit OLS (blend >= 0.50)")

fit_old <- lm(lsfo_bbl ~ pred_old, data = df_old)
fit_new <- lm(lsfo_bbl ~ pred_new, data = df_new)

int_old <- coef(fit_old)[1]; slope_old <- coef(fit_old)[2]
int_new <- coef(fit_new)[1]; slope_new <- coef(fit_new)[2]

df_final <- df_final |>
  mutate(
    fitted_old = int_old  + slope_old * pred_old,
    fitted_new = int_new  + slope_new * pred_new,
    fitted     = (1 - blend) * fitted_old + blend * fitted_new,
    residual   = lsfo_bbl - fitted
  )

ssr_h4  <- sum(df_final$residual^2, na.rm = TRUE)
sst_h4  <- sum((df_final$lsfo_bbl - mean(df_final$lsfo_bbl))^2, na.rm = TRUE)
r2_h4   <- 1 - ssr_h4 / sst_h4
rmse_h4 <- sqrt(ssr_h4 / nrow(df_final))

cat("\n── Selected model: H4 blended, Singapore (old) + Singapore/Houston (new) ──\n")
cat(sprintf("Optimal contract date: %s\n", CONTRACT_DATE))
cat(sprintf("w1=%d days (contract MA) | w2=%d days (inventory window)\n", best$w1, best$w2))
cat(sprintf("Overall R²=%.4f | RMSE=%.4f $/BBL | n=%d\n", r2_h4, rmse_h4, nrow(df_final)))
cat(sprintf("Old regime (sing):         intercept=%.4f  slope=%.4f  n=%d\n",
            int_old, slope_old, nrow(df_old)))
cat(sprintf("New regime (avg_sing_hou): intercept=%.4f  slope=%.4f  n=%d\n",
            int_new, slope_new, nrow(df_new)))

# ── Plots ──────────────────────────────────────────────────────────────────────

p_ts_final <- df_final |>
  select(date, `HECO LSFO (actual)` = lsfo_bbl, `Fitted` = fitted) |>
  pivot_longer(-date, names_to = "series", values_to = "price") |>
  ggplot(aes(x = date, y = price, color = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = CONTRACT_DATE, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("HECO LSFO (actual)" = "#1b7837", "Fitted" = "#762a83")) +
  annotate("text", x = CONTRACT_DATE, y = Inf,
           label = "Contract\nrenegotiation", vjust = 1.5, hjust = -0.05,
           size = 3, color = "grey40") +
  labs(
    title = "HECO LSFO: Blended Singapore (old) / Singapore+Houston (new) model",
    subtitle = sprintf(
      "Pre-%s: sing (b0=%.2f, b1=%.3f) | Post-%s: avg_sing_hou (b0=%.2f, b1=%.3f)\nR²=%.3f | RMSE=%.2f $/BBL | w1=%dd | w2=%dd",
      format(CONTRACT_DATE, "%b %Y"), int_old, slope_old,
      format(CONTRACT_DATE, "%b %Y"), int_new, slope_new,
      r2_h4, rmse_h4, best$w1, best$w2),
    x = NULL, y = "Price ($/BBL)", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_ts_final)

p_resid_final <- df_final |>
  ggplot(aes(x = date, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = CONTRACT_DATE, linetype = "dashed", color = "grey50") +
  geom_line(color = "#1b7837", linewidth = 0.8) +
  geom_point(color = "#1b7837", size = 2) +
  labs(title = "Residuals: blended VLSFO model (actual − fitted)",
       x = NULL, y = "Residual ($/BBL)") +
  theme_minimal()
print(p_resid_final)

p_scatter_final <- df_final |>
  mutate(
    pred_used = if_else(blend < 0.5, pred_old, pred_new),
    regime    = if_else(blend < 0.5,
                        sprintf("Pre-%s (Singapore)", format(CONTRACT_DATE, "%b %Y")),
                        sprintf("Post-%s (Sing+Houston)", format(CONTRACT_DATE, "%b %Y")))
  ) |>
  ggplot(aes(x = pred_used, y = lsfo_bbl, color = regime)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_abline(slope = slope_old, intercept = int_old,
              color = "#d95f02", linewidth = 0.8, linetype = "solid") +
  geom_abline(slope = slope_new, intercept = int_new,
              color = "#1b7837", linewidth = 0.8, linetype = "solid") +
  scale_color_manual(values = setNames(
    c("#d95f02", "#1b7837"),
    c(sprintf("Pre-%s (Singapore)",    format(CONTRACT_DATE, "%b %Y")),
      sprintf("Post-%s (Sing+Houston)", format(CONTRACT_DATE, "%b %Y")))
  )) +
  labs(
    title = "HECO LSFO vs VLSFO predictor by regime",
    subtitle = "Each line = regime-specific OLS; x-axis = dominant regime predictor",
    x = "VLSFO nested MA predictor ($/BBL)", y = "HECO LSFO ($/BBL)", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_scatter_final)

# ── H12: Three-regime model ────────────────────────────────────────────────────
# Three regimes identified by grid search:
#   R1 Pre-contract:  Jun 2023 – Mar 2024  (avg_sing_hou, w1=33, w2=40)
#   R2 Transition:    Apr 2024 – Oct 2024  (avg_sing_hou, w1=35, w2=15)
#   R3 Post-contract: Nov 2024 – present   (avg_sing_hou, w1=31, w2=35)
# Break 1 (contract change):   2024-04-01
# Break 2 (inventory flush):   2024-11-01
# Index is avg of Singapore and Houston VLSFO throughout all regimes.
# w1 = contract pricing window (days); w2 = inventory accumulation window (days)

break1 <- as.Date("2024-04-01")
break2 <- as.Date("2024-11-01")

# ── H12 setup ─────────────────────────────────────────────────────────────────
# Response vector and total sum of squares (used in every R² calculation)
y_all <- heco$lsfo_bbl
sst   <- sum((y_all - mean(y_all))^2)

# Series to search over in R1 (pre-contract); R2/R3 are fixed to POST_SERIES
pre_series_list <- c("sing", "avg_sing_hou")

# The post-contract index (fixed for R2 and R3)
POST_SERIES <- "avg_sing_hou"

# Fine-grid window ranges centred on the Part-1 best parameters (±10 days each)
w1_fine <- seq(max(5L,  best$w1 - 10L), best$w1 + 10L, by = 1L)
w2_fine <- seq(max(5L,  best$w2 - 10L), best$w2 + 10L, by = 1L)

# Minimal OLS helpers (avoids lm() overhead inside the hot loop)
#
# fast_ols_ssr(y, x) — returns residual sum of squares for y ~ x
# fast_ols_coef(y, x) — returns named numeric(2): intercept, slope
fast_ols_ssr <- function(y, x) {
  xc    <- x - mean(x)
  slope <- sum(xc * (y - mean(y))) / sum(xc^2)
  int_  <- mean(y) - slope * mean(x)
  sum((y - (int_ + slope * x))^2)
}

fast_ols_coef <- function(y, x) {
  xc    <- x - mean(x)
  slope <- sum(xc * (y - mean(y))) / sum(xc^2)
  int_  <- mean(y) - slope * mean(x)
  c(intercept = int_, slope = slope)
}

# Pre-compute all (series × w1 × w2) predictor vectors over the fine grid.
# Keyed as "series_w1_w2" for O(1) lookup inside the grid-search loop.
all_series_h12 <- unique(c(pre_series_list, POST_SERIES))

pred_cache <- list()
for (.s in all_series_h12) {
  for (.w1 in w1_fine) {
    for (.w2 in w2_fine) {
      key              <- paste(.s, .w1, .w2, sep = "_")
      pred_cache[[key]] <- build_predictor(vlsfo[[.s]], vlsfo$date,
                                           .w1, .w2, heco$date)$pred
    }
  }
}
rm(.s, .w1, .w2, key)

# ── Grid search for break2 (break1 fixed at 2024-04-01) ───────────────────────
break2_seq <- heco$date[heco$date > break1 + 60 & heco$date < max(heco$date) - 120]

cat(sprintf("H12: searching %d break2 candidates\n", length(break2_seq)))

idx_r1_fixed <- which(heco$date < break1)
best_r2_h12  <- -Inf
best_row_h12 <- NULL

for (bd2 in as.list(break2_seq)) {
  bd2    <- as.Date(bd2)
  idx_r2 <- which(heco$date >= break1 & heco$date < bd2)
  idx_r3 <- which(heco$date >= bd2)
  if (length(idx_r2) < 4 || length(idx_r3) < 4) next

  for (sp in pre_series_list) {
    for (w1pr in w1_fine) {
      for (w2pr in w2_fine) {
        x_r1   <- pred_cache[[paste(sp, w1pr, w2pr, sep = "_")]][idx_r1_fixed]
        ok_r1  <- !is.na(x_r1)
        if (sum(ok_r1) < 4) next
        ssr_r1 <- fast_ols_ssr(y_all[idx_r1_fixed][ok_r1], x_r1[ok_r1])
        if (!is.finite(ssr_r1)) next

        for (w1_r2 in w1_fine) {
          for (w2_r2 in w2_fine) {
            x_r2   <- pred_cache[[paste(POST_SERIES, w1_r2, w2_r2, sep = "_")]][idx_r2]
            ok_r2  <- !is.na(x_r2)
            if (sum(ok_r2) < 4) next
            ssr_r2 <- fast_ols_ssr(y_all[idx_r2][ok_r2], x_r2[ok_r2])
            if (!is.finite(ssr_r2)) next

            for (w1_r3 in w1_fine) {
              for (w2_r3 in w2_fine) {
                x_r3  <- pred_cache[[paste(POST_SERIES, w1_r3, w2_r3, sep = "_")]][idx_r3]
                ok_r3 <- !is.na(x_r3)
                if (sum(ok_r3) < 4) next

                ssr <- ssr_r1 + ssr_r2 +
                       fast_ols_ssr(y_all[idx_r3][ok_r3], x_r3[ok_r3])
                r2  <- 1 - ssr / sst
                n   <- sum(ok_r1) + sum(ok_r2) + sum(ok_r3)

                if (r2 > best_r2_h12) {
                  best_r2_h12  <- r2
                  best_row_h12 <- list(
                    break2     = bd2,
                    series_pre = sp,
                    w1_r1 = w1pr,   w2_r1 = w2pr,
                    w1_r2 = w1_r2,  w2_r2 = w2_r2,
                    w1_r3 = w1_r3,  w2_r3 = w2_r3,
                    r2   = r2,
                    rmse = sqrt(ssr / n)
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}

h12 <- best_row_h12
break2 <- h12$break2

# ── Recover coefficients ───────────────────────────────────────────────────────
idx_r1 <- which(heco$date <  break1)
idx_r2 <- which(heco$date >= break1 & heco$date < break2)
idx_r3 <- which(heco$date >= break2)

x_r1 <- pred_cache[[paste(h12$series_pre, h12$w1_r1, h12$w2_r1, sep = "_")]][idx_r1]
x_r2 <- pred_cache[[paste(POST_SERIES,    h12$w1_r2, h12$w2_r2, sep = "_")]][idx_r2]
x_r3 <- pred_cache[[paste(POST_SERIES,    h12$w1_r3, h12$w2_r3, sep = "_")]][idx_r3]

ok_r1 <- !is.na(x_r1); ok_r2 <- !is.na(x_r2); ok_r3 <- !is.na(x_r3)

coef_r1 <- fast_ols_coef(y_all[idx_r1][ok_r1], x_r1[ok_r1])
coef_r2 <- fast_ols_coef(y_all[idx_r2][ok_r2], x_r2[ok_r2])
coef_r3 <- fast_ols_coef(y_all[idx_r3][ok_r3], x_r3[ok_r3])

ssr_r1 <- fast_ols_ssr(y_all[idx_r1][ok_r1], x_r1[ok_r1])
ssr_r2 <- fast_ols_ssr(y_all[idx_r2][ok_r2], x_r2[ok_r2])
ssr_r3 <- fast_ols_ssr(y_all[idx_r3][ok_r3], x_r3[ok_r3])
n_h12  <- sum(ok_r1) + sum(ok_r2) + sum(ok_r3)

# ── Summary table ─────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("  HECO LSFO Pricing Model — Three-Regime Summary (H12)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  Overall R² = %.4f    RMSE = %.4f $/BBL    n = %d\n\n",
            h12$r2, h12$rmse, n_h12))
cat(sprintf("  Break 1 (contract change):   %s\n", break1))
cat(sprintf("  Break 2 (inventory flush):   %s\n\n", break2))

summary_h12 <- tibble(
  Regime    = c("R1: Pre-contract", "R2: Transition", "R3: Post-contract"),
  Period    = c(sprintf("%s to %s", min(heco$date[idx_r1]), max(heco$date[idx_r1])),
                sprintf("%s to %s", min(heco$date[idx_r2]), max(heco$date[idx_r2])),
                sprintf("%s to %s", min(heco$date[idx_r3]), max(heco$date[idx_r3]))),
  Index     = rep("avg_sing_hou", 3),
  w1_days   = c(h12$w1_r1, h12$w1_r2, h12$w1_r3),
  w2_days   = c(h12$w2_r1, h12$w2_r2, h12$w2_r3),
  Intercept = round(c(coef_r1["intercept"], coef_r2["intercept"], coef_r3["intercept"]), 3),
  Slope     = round(c(coef_r1["slope"],     coef_r2["slope"],     coef_r3["slope"]),     4),
  n         = c(sum(ok_r1), sum(ok_r2), sum(ok_r3)),
  RMSE      = round(sqrt(c(ssr_r1/sum(ok_r1), ssr_r2/sum(ok_r2), ssr_r3/sum(ok_r3))), 3)
)
summary_h12

# ── Fitted values and residuals ───────────────────────────────────────────────

df_h12 <- bind_rows(
  tibble(date   = heco$date[idx_r1],
         actual = y_all[idx_r1],
         pred   = x_r1,
         regime = "R1: Pre-contract") |>
    filter(!is.na(pred)) |>
    mutate(fitted   = coef_r1["intercept"] + coef_r1["slope"] * pred,
           residual = actual - fitted),
  tibble(date   = heco$date[idx_r2],
         actual = y_all[idx_r2],
         pred   = x_r2,
         regime = "R2: Transition") |>
    filter(!is.na(pred)) |>
    mutate(fitted   = coef_r2["intercept"] + coef_r2["slope"] * pred,
           residual = actual - fitted),
  tibble(date   = heco$date[idx_r3],
         actual = y_all[idx_r3],
         pred   = x_r3,
         regime = "R3: Post-contract") |>
    filter(!is.na(pred)) |>
    mutate(fitted   = coef_r3["intercept"] + coef_r3["slope"] * pred,
           residual = actual - fitted)
)

# ── Time series: actual vs fitted ─────────────────────────────────────────────
p_h12_ts <- df_h12 |>
  pivot_longer(c(actual, fitted), names_to = "series", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = series, linetype = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(actual = "black", fitted = "steelblue")) +
  scale_linetype_manual(values = c(actual = "solid", fitted = "dashed")) +
  annotate("text", x = break1, y = max(df_h12$actual),
           label = "Contract\nchange", hjust = -0.05, size = 3, color = "grey30") +
  annotate("text", x = break2, y = max(df_h12$actual) - 4,
           label = "Inventory\nflush", hjust = -0.05, size = 3, color = "grey30") +
  labs(title = "H12: Three-Regime Model — Actual vs Fitted HECO LSFO",
       subtitle = sprintf("R²=%.4f  RMSE=%.2f $/BBL", h12$r2, h12$rmse),
       x = NULL, y = "LSFO ($/BBL)", color = NULL, linetype = NULL) +
  theme_minimal()
print(p_h12_ts)

# ── Residuals over time ───────────────────────────────────────────────────────
p_h12_resid <- df_h12 |>
  ggplot(aes(x = date, y = residual, fill = regime)) +
  geom_col(width = as.numeric(mean(diff(sort(unique(df_h12$date)))) * 0.8)) +
  geom_hline(yintercept = 0, color = "grey30") +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("R1: Pre-contract"  = "#4393c3",
                                "R2: Transition"    = "#f4a582",
                                "R3: Post-contract" = "#2ca25f")) +
  labs(title = "H12: Residuals by Regime",
       x = NULL, y = "Residual ($/BBL)", fill = NULL) +
  theme_minimal()
print(p_h12_resid)

# ── ACF of residuals ──────────────────────────────────────────────────────────
acf_h12 <- acf(df_h12$residual, plot = FALSE, lag.max = 12)
ci_h12  <- 1.96 / sqrt(nrow(df_h12))

p_h12_acf <- tibble(lag = as.numeric(acf_h12$lag[-1]),
                     acf = as.numeric(acf_h12$acf[-1])) |>
  ggplot(aes(x = lag, y = acf)) +
  geom_col(fill = "#762a83", width = 0.6) +
  geom_hline(yintercept = c(-ci_h12, ci_h12),
             linetype = "dashed", color = "firebrick") +
  scale_x_continuous(breaks = 1:12) +
  labs(title = "H12: ACF of Residuals",
       x = "Lag (months)", y = "ACF") +
  theme_minimal()
print(p_h12_acf)

# ── Scatter: fitted vs actual by regime ───────────────────────────────────────
p_h12_scatter <- df_h12 |>
  ggplot(aes(x = fitted, y = actual, color = regime)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("R1: Pre-contract"  = "#4393c3",
                                 "R2: Transition"    = "#d6604d",
                                 "R3: Post-contract" = "#2ca25f")) +
  labs(title = "H12: Fitted vs Actual by Regime",
       x = "Fitted ($/BBL)", y = "Actual ($/BBL)", color = NULL) +
  theme_minimal()
print(p_h12_scatter)



# ── H13: Anchor-day grid search for R3 ────────────────────────────────────────
#
# Hypothesis: The post-contract (R3) autocorrelation arises from a phase error
# in the anchor date. The current model anchors the inventory window to day 24
# of the prior month. If the actual contract uses a different billing calendar
# (e.g. last-business-day, month-end, 1st of current month), the predictor is
# consistently shifted, producing the slow oscillatory residual pattern observed.
#
# This search holds R1 and R2 fixed at their H12 best parameters, and varies
# only the R3 anchor day-of-month (1–31), plus ±10-day refinement of w1/w2
# around the H12 R3 optimum (w1=21, w2=50).
#
# The anchor day is interpreted as: floor_date(month) %m-% months(1) + days(d - 1)
# i.e. day `d` of the prior month, where d=24 is the current assumption.
# We also test d=0 as a special case meaning "last day of the prior month"
# and d=-N meaning "Nth day of the current month" (e.g. d=-1 = 1st of month t).

# ── Fixed H12 R1/R2 parameters ────────────────────────────────────────────────
w1_r1_fix <- h12$w1_r1
w2_r1_fix <- h12$w2_r1
w1_r2_fix <- h12$w1_r2
w2_r2_fix <- h12$w2_r2

# Pre-computed R1 and R2 SSR (fixed)
x_r1_fix <- pred_cache[[paste(h12$series_pre, w1_r1_fix, w2_r1_fix, sep = "_")]][idx_r1]
x_r2_fix <- pred_cache[[paste(POST_SERIES,    w1_r2_fix, w2_r2_fix, sep = "_")]][idx_r2]

ok_r1_fix <- !is.na(x_r1_fix)
ok_r2_fix <- !is.na(x_r2_fix)

ssr_r1_fix <- fast_ols_ssr(y_all[idx_r1][ok_r1_fix], x_r1_fix[ok_r1_fix])
ssr_r2_fix <- fast_ols_ssr(y_all[idx_r2][ok_r2_fix], x_r2_fix[ok_r2_fix])

# ── Anchor-day-aware predictor builder ────────────────────────────────────────
# anchor_day: integer. Positive = day of prior month (1-31).
#             0 = last calendar day of prior month.
#             Negative = day of the *current* billing month (e.g. -1 = 1st,
#             -15 = 15th of current month t).
build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = dates, cp = cp) |> filter(!is.na(cp))
  daily_int <- as.integer(daily$date)

  tibble(date = target_dates) |>
    mutate(pred = map_dbl(date, function(m) {
      m <- as.Date(m)

      anchor <- if (anchor_day > 0) {
        # Day anchor_day of prior month
        floor_date(m, "month") %m-% months(1) + days(anchor_day - 1L)
      } else if (anchor_day == 0) {
        # Last calendar day of prior month
        ceiling_date(m, "month") %m-% months(1) - days(1)
      } else {
        # Day (-anchor_day) of the current billing month
        floor_date(m, "month") + days(-anchor_day - 1L)
      }

      win_start <- anchor - days(w2 - 1L)
      i_end     <- findInterval(as.integer(anchor),          daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L,  daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

# ── Grid ──────────────────────────────────────────────────────────────────────
# Anchor days: 1–31 (prior month) + 0 (last day of prior month)
# Plus first few days of the current month: -1 (1st), -2 (2nd), -3 (3rd)
anchor_day_seq <- c(-3L, -2L, -1L, 0L, 1L:31L)

# R3 w1/w2: ±8 days around H12 optimum
w1_r3_seq <- seq(max(5L, h12$w1_r3 - 8L), h12$w1_r3 + 8L, by = 1L)
w2_r3_seq <- seq(max(5L, h12$w2_r3 - 8L), h12$w2_r3 + 8L, by = 1L)

spot_vec_h13  <- vlsfo[[POST_SERIES]]
dates_vec_h13 <- vlsfo$date
target_h13    <- heco$date[idx_r3]
y_r3_h13      <- y_all[idx_r3]

sst_h13 <- sum((y_all - mean(y_all))^2)
ssr_fixed <- ssr_r1_fix + ssr_r2_fix

total_h13 <- length(anchor_day_seq) * length(w1_r3_seq) * length(w2_r3_seq)
cat(sprintf("h13 grid: %d anchor days × %d w1 × %d w2 = %d cells\n",
            length(anchor_day_seq), length(w1_r3_seq), length(w2_r3_seq), total_h13))

# ── Search ────────────────────────────────────────────────────────────────────
h13_results <- vector("list", total_h13)
cell <- 0L

for (ad in anchor_day_seq) {
  for (w1v in w1_r3_seq) {
    for (w2v in w2_r3_seq) {
      cell <- cell + 1L
      pv   <- build_predictor_anchor(spot_vec_h13, dates_vec_h13,
                                      w1v, w2v, target_h13, ad)$pred
      ok   <- !is.na(pv)
      if (sum(ok) < 4L) {
        h13_results[[cell]] <- list(anchor_day=ad, w1=w1v, w2=w2v, r2=NA, rmse=NA, ssr_r3=NA)
        next
      }
      ssr_r3 <- fast_ols_ssr(y_r3_h13[ok], pv[ok])
      total_ssr <- ssr_fixed + ssr_r3
      r2  <- 1 - total_ssr / sst_h13
      n   <- sum(ok_r1_fix) + sum(ok_r2_fix) + sum(ok)
      h13_results[[cell]] <- list(
        anchor_day = ad,
        w1         = w1v,
        w2         = w2v,
        r2         = r2,
        rmse       = sqrt(total_ssr / n),
        ssr_r3     = ssr_r3
      )
    }
  }
}

h13_tbl <- bind_rows(lapply(h13_results, as_tibble)) |>
  filter(!is.na(r2)) |>
  arrange(desc(r2))

cat("\n── h13: Top 20 anchor-day fits (R3 only) ───────────────────────────────────\n")
h13_tbl |>
  mutate(
    anchor_label = case_when(
      anchor_day  > 0 ~ paste0("prior mo. day ", anchor_day),
      anchor_day == 0 ~ "last day of prior mo.",
      TRUE            ~ paste0("current mo. day ", -anchor_day)
    )
  ) |>
  select(anchor_label, anchor_day, w1, w2, r2, rmse) |>
  slice_head(n = 20) |>
  mutate(across(c(r2, rmse), \(x) round(x, 5)))


# ── H14: Inventory ramp in R3 ─────────────────────────────────────────────────
#
# A key physical insight is: inventory was gradually drawn down before the contract
# switch (pre-break2) and gradually rebuilt afterward (post-break2). The H12 model
# uses a fixed w2 for each regime, which can't capture this. H14 keeps R1 and R2
# fixed, then lets the R3 inventory window recover linearly from a short starting
# w2 toward a longer steady-state w2 over N months.
#
# Month j of R3 uses:
#   w2_j = round(w2_start + min(j / rebuild_months, 1) * (w2_end - w2_start))
#
# The residual pattern in R3 suggests exactly this type of misspecification:
# a fixed w2 that is too long immediately after break2 and too short later.

best_h13 <- h13_tbl |> slice_head(n = 1)

anchor_day_h14 <- best_h13$anchor_day[[1]]
w1_h14         <- best_h13$w1[[1]]

build_predictor_recovering_w2 <- function(spot, dates, w1, target_dates,
                                          anchor_day, w2_path, pred_cache_by_w2 = NULL) {
  if (length(w2_path) != length(target_dates)) {
    stop("w2_path must have the same length as target_dates")
  }

  w2_path <- as.integer(round(w2_path))
  uniq_w2 <- sort(unique(w2_path))

  if (is.null(pred_cache_by_w2)) {
    pred_cache_by_w2 <- setNames(
      lapply(uniq_w2, \(w2)
        build_predictor_anchor(spot, dates, w1, w2, target_dates, anchor_day)$pred),
      as.character(uniq_w2)
    )
  }

  pred_out <- numeric(length(target_dates))
  for (j in seq_along(target_dates)) {
    pred_out[j] <- pred_cache_by_w2[[as.character(w2_path[j])]][j]
  }

  tibble(date = target_dates, pred = pred_out, w2_dynamic = w2_path)
}

r3_months <- heco$date[idx_r3]
y_r3      <- y_all[idx_r3]

w2_start_seq      <- seq(5L, 35L, by = 5L)
w2_end_seq        <- seq(25L, 80L, by = 5L)
rebuild_month_seq <- 1L:12L

valid_h14_grid <- expand_grid(
  w2_start = w2_start_seq,
  w2_end = w2_end_seq,
  rebuild_months = rebuild_month_seq
) |>
  filter(w2_end >= w2_start)

total_h14 <- nrow(valid_h14_grid)
cat(sprintf("\nH14 grid: %d start-w2 × %d end-w2 × %d rebuild months = %d cells\n",
            length(w2_start_seq), length(w2_end_seq), length(rebuild_month_seq), total_h14))

all_w2_h14 <- seq(min(w2_start_seq), max(w2_end_seq), by = 1L)
pred_cache_h14 <- setNames(
  lapply(all_w2_h14, \(w2)
    build_predictor_anchor(spot_vec_h13, dates_vec_h13, w1_h14, w2,
                           r3_months, anchor_day_h14)$pred),
  as.character(all_w2_h14)
)

h14_results <- vector("list", total_h14)
cell <- 0L

for (i in seq_len(nrow(valid_h14_grid))) {
  cell <- cell + 1L

  w2_start       <- valid_h14_grid$w2_start[i]
  w2_end         <- valid_h14_grid$w2_end[i]
  rebuild_months <- valid_h14_grid$rebuild_months[i]

  month_idx <- seq_along(r3_months) - 1L
  frac      <- pmin(month_idx / rebuild_months, 1)
  w2_path   <- as.integer(round(w2_start + frac * (w2_end - w2_start)))

  pred_df <- build_predictor_recovering_w2(
    spot = spot_vec_h13,
    dates = dates_vec_h13,
    w1 = w1_h14,
    target_dates = r3_months,
    anchor_day = anchor_day_h14,
    w2_path = w2_path,
    pred_cache_by_w2 = pred_cache_h14
  )

  ok <- !is.na(pred_df$pred)
  if (sum(ok) < 4L) {
    h14_results[[cell]] <- list(
      w2_start = w2_start, w2_end = w2_end, rebuild_months = rebuild_months,
      r2 = NA_real_, rmse = NA_real_, ssr_r3 = NA_real_
    )
    next
  }

  ssr_r3    <- fast_ols_ssr(y_r3[ok], pred_df$pred[ok])
  total_ssr <- ssr_fixed + ssr_r3
  n_total   <- sum(ok_r1_fix) + sum(ok_r2_fix) + sum(ok)

  h14_results[[cell]] <- list(
    w2_start       = w2_start,
    w2_end         = w2_end,
    rebuild_months = rebuild_months,
    r2             = 1 - total_ssr / sst_h13,
    rmse           = sqrt(total_ssr / n_total),
    ssr_r3         = ssr_r3
  )
}

h14_tbl <- bind_rows(lapply(h14_results, as_tibble)) |>
  filter(!is.na(r2)) |>
  arrange(desc(r2))

best_h14 <- h14_tbl |> slice_head(n = 1)

month_idx_h14 <- seq_along(r3_months) - 1L
frac_h14      <- pmin(month_idx_h14 / best_h14$rebuild_months, 1)
w2_path_h14   <- as.integer(round(best_h14$w2_start + frac_h14 * (best_h14$w2_end - best_h14$w2_start)))

pred_r3_h14 <- build_predictor_recovering_w2(
  spot = spot_vec_h13,
  dates = dates_vec_h13,
  w1 = w1_h14,
  target_dates = r3_months,
  anchor_day = anchor_day_h14,
  w2_path = w2_path_h14,
  pred_cache_by_w2 = pred_cache_h14
)

ok_r3_h14   <- !is.na(pred_r3_h14$pred)
coef_r3_h14 <- fast_ols_coef(y_r3[ok_r3_h14], pred_r3_h14$pred[ok_r3_h14])

df_h14 <- bind_rows(
  tibble(date   = heco$date[idx_r1],
         actual = y_all[idx_r1],
         pred   = x_r1_fix,
         regime = "R1: Pre-contract",
         w2_dynamic = w2_r1_fix) |>
    filter(!is.na(pred)) |>
    mutate(fitted   = coef_r1["intercept"] + coef_r1["slope"] * pred,
           residual = actual - fitted),
  tibble(date   = heco$date[idx_r2],
         actual = y_all[idx_r2],
         pred   = x_r2_fix,
         regime = "R2: Transition",
         w2_dynamic = w2_r2_fix) |>
    filter(!is.na(pred)) |>
    mutate(fitted   = coef_r2["intercept"] + coef_r2["slope"] * pred,
           residual = actual - fitted),
  tibble(date   = pred_r3_h14$date,
         actual = y_r3,
         pred   = pred_r3_h14$pred,
         regime = "R3: Rebuilding inventory",
         w2_dynamic = pred_r3_h14$w2_dynamic) |>
    filter(!is.na(pred)) |>
    mutate(fitted   = coef_r3_h14["intercept"] + coef_r3_h14["slope"] * pred,
           residual = actual - fitted)
)

cat("\n── H14: R3 rebuilding-inventory model ──────────────────────────────\n")
cat(sprintf("Anchor day (from H13): %d\n", anchor_day_h14))
cat(sprintf("R3 w1 (from H13):      %d days\n", w1_h14))
cat(sprintf("R3 w2 path:            %d -> %d days over %d months\n",
            best_h14$w2_start, best_h14$w2_end, best_h14$rebuild_months))
cat(sprintf("Overall R²:            %.4f\n", best_h14$r2))
cat(sprintf("Overall RMSE:          %.4f $/BBL\n", best_h14$rmse))
cat(sprintf("R3 coefficients:       intercept=%.4f  slope=%.4f\n",
            coef_r3_h14["intercept"], coef_r3_h14["slope"]))

cat("\n── H14: Top 15 fits ─────────────────────────────────────────────────\n")
h14_tbl |>
  slice_head(n = 15) |>
  mutate(across(c(r2, rmse), \(x) round(x, 5))) |>
  print()

p_h14_w2 <- tibble(
  date = r3_months,
  w2_dynamic = w2_path_h14
) |>
  ggplot(aes(x = date, y = w2_dynamic)) +
  geom_line(color = "#762a83", linewidth = 1) +
  geom_point(color = "#762a83", size = 2) +
  labs(title = "H14: Rebuilding inventory window in R3",
       subtitle = sprintf("w2 recovers from %d to %d days over %d months",
                          best_h14$w2_start, best_h14$w2_end, best_h14$rebuild_months),
       x = NULL, y = "Dynamic w2 (days)") +
  theme_minimal()
print(p_h14_w2)

p_h14_ts <- df_h14 |>
  pivot_longer(c(actual, fitted), names_to = "series", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = series, linetype = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(actual = "black", fitted = "steelblue")) +
  scale_linetype_manual(values = c(actual = "solid", fitted = "dashed")) +
  labs(title = "H14: Three-regime model with rebuilding inventory in R3",
       subtitle = sprintf("R²=%.4f  RMSE=%.2f $/BBL  |  R3 w2: %d -> %d over %d months",
                          best_h14$r2, best_h14$rmse,
                          best_h14$w2_start, best_h14$w2_end, best_h14$rebuild_months),
       x = NULL, y = "LSFO ($/BBL)", color = NULL, linetype = NULL) +
  theme_minimal()
print(p_h14_ts)

p_h14_resid <- df_h14 |>
  ggplot(aes(x = date, y = residual, fill = regime)) +
  geom_col(width = as.numeric(mean(diff(sort(unique(df_h14$date)))) * 0.8)) +
  geom_hline(yintercept = 0, color = "grey30") +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("R1: Pre-contract" = "#4393c3",
                               "R2: Transition" = "#f4a582",
                               "R3: Rebuilding inventory" = "#2ca25f")) +
  labs(title = "H14: Residuals by regime",
       x = NULL, y = "Residual ($/BBL)", fill = NULL) +
  theme_minimal()
print(p_h14_resid)

cat("\n══ H12 vs H14 ════════════════════════════════════════════════════════\n")
tibble(
  Model = c("H12: fixed R3 w2", "H14: rebuilding R3 w2"),
  R2    = c(h12$r2, best_h14$r2),
  RMSE  = c(h12$rmse, best_h14$rmse)
) |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4))) |>
  print()


# ── H15/H16: Gradual drawdown in R1/R2 with regime-specific index search ─────
#
# H15 refines R1 and H16 refines R2, sequentially:
#   1. Hold H14's R3 fixed and H12's R2 fixed; search R1 index, w1, and a
#      monotone declining w2 path across the whole pre-contract regime.
#   2. Hold refined R1 and H14's R3 fixed; search R2 index, w1, and a
#      monotone declining w2 path across the transition regime.
#
# This encodes the physical hypothesis that inventory was being drawn down
# gradually approaching the contract reset, one day of consumption at a time.

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

summarise_regime_fit <- function(y, pred) {
  ok <- !is.na(pred)
  if (sum(ok) < 4L) return(NULL)

  coef <- fast_ols_coef(y[ok], pred[ok])
  ssr  <- fast_ols_ssr(y[ok], pred[ok])

  list(
    ok = ok,
    coef = coef,
    ssr = ssr,
    n = sum(ok),
    rmse = sqrt(ssr / sum(ok))
  )
}

idx_r1_hx <- idx_r1
idx_r2_hx <- idx_r2
idx_r3_hx <- idx_r3

r1_months <- heco$date[idx_r1_hx]
r2_months <- heco$date[idx_r2_hx]
r3_months <- heco$date[idx_r3_hx]

y_r1_hx <- y_all[idx_r1_hx]
y_r2_hx <- y_all[idx_r2_hx]
y_r3_hx <- y_all[idx_r3_hx]

# Fix R3 at the user-selected H14 path.
w2_path_h14_fixed <- build_linear_w2_path(length(r3_months), 20L, 70L)

pred_r3_fixed <- build_predictor_recovering_w2(
  spot = spot_vec_h13,
  dates = dates_vec_h13,
  w1 = w1_h14,
  target_dates = r3_months,
  anchor_day = anchor_day_h14,
  w2_path = w2_path_h14_fixed,
  pred_cache_by_w2 = pred_cache_h14
)

r3_fit_fixed <- summarise_regime_fit(y_r3_hx, pred_r3_fixed$pred)
if (is.null(r3_fit_fixed)) stop("Failed to build fixed R3 fit for H15/H16")

# Keep the H12 R2 fixed for the R1 search.
r2_fit_h12_fixed <- summarise_regime_fit(y_r2_hx, x_r2_fix)
if (is.null(r2_fit_h12_fixed)) stop("Failed to build fixed R2 fit for H15")

series_regime_seq <- c("sing", "glob", "avg_sing_glob", "avg_sing_hou")
w1_regime_seq     <- seq(5L, 40L, by = 2L)

search_drawdown_regime <- function(months, y, series_seq, w1_seq, w2_start_seq,
                                   w2_end_seq, anchor_day, vlsfo_tbl,
                                   fixed_ssr, sst_total, total_n_fixed) {
  valid_grid <- expand_grid(
    series = series_seq,
    w1 = w1_seq,
    w2_start = w2_start_seq,
    w2_end = w2_end_seq
  ) |>
    filter(w2_end <= w2_start)

  all_w2 <- seq(min(w2_end_seq), max(w2_start_seq), by = 1L)
  cache <- list()
  for (.s in series_seq) {
    for (.w1 in w1_seq) {
      for (.w2 in all_w2) {
        cache[[paste(.s, .w1, .w2, sep = "_")]] <-
          build_predictor_anchor(vlsfo_tbl[[.s]], vlsfo_tbl$date, .w1, .w2,
                                 months, anchor_day)$pred
      }
    }
  }

  results <- vector("list", nrow(valid_grid))
  best_r2 <- -Inf
  best_row <- NULL
  best_pred <- NULL
  best_fit <- NULL

  for (i in seq_len(nrow(valid_grid))) {
    s  <- valid_grid$series[i]
    w1 <- valid_grid$w1[i]
    ws <- valid_grid$w2_start[i]
    we <- valid_grid$w2_end[i]

    w2_path <- build_linear_w2_path(length(months), ws, we)
    pred <- numeric(length(months))
    for (j in seq_along(months)) {
      pred[j] <- cache[[paste(s, w1, w2_path[j], sep = "_")]][j]
    }

    fit_reg <- summarise_regime_fit(y, pred)
    if (is.null(fit_reg)) {
      results[[i]] <- list(series = s, w1 = w1, w2_start = ws, w2_end = we,
                           r2 = NA_real_, rmse = NA_real_)
      next
    }

    total_ssr <- fixed_ssr + fit_reg$ssr
    r2_total  <- 1 - total_ssr / sst_total
    rmse_tot  <- sqrt(total_ssr / (total_n_fixed + fit_reg$n))

    results[[i]] <- list(
      series = s,
      w1 = w1,
      w2_start = ws,
      w2_end = we,
      r2 = r2_total,
      rmse = rmse_tot
    )

    if (r2_total > best_r2) {
      best_r2   <- r2_total
      best_row  <- list(series = s, w1 = w1, w2_start = ws, w2_end = we,
                        r2 = r2_total, rmse = rmse_tot)
      best_pred <- pred
      best_fit  <- fit_reg
    }
  }

  list(
    table = bind_rows(lapply(results, as_tibble)) |> filter(!is.na(r2)) |> arrange(desc(r2)),
    best = best_row,
    pred = best_pred,
    fit = best_fit
  )
}

# H15: refine R1
w2_r1_start_seq <- seq(40L, 120L, by = 5L)
w2_r1_end_seq   <- seq(10L, 80L, by = 5L)

cat(sprintf("\nH15 grid (R1 drawdown): %d series × %d w1 × %d start-w2 × %d end-w2\n",
            length(series_regime_seq), length(w1_regime_seq),
            length(w2_r1_start_seq), length(w2_r1_end_seq)))

h15 <- search_drawdown_regime(
  months = r1_months,
  y = y_r1_hx,
  series_seq = series_regime_seq,
  w1_seq = w1_regime_seq,
  w2_start_seq = w2_r1_start_seq,
  w2_end_seq = w2_r1_end_seq,
  anchor_day = 24L,
  vlsfo_tbl = vlsfo,
  fixed_ssr = r2_fit_h12_fixed$ssr + r3_fit_fixed$ssr,
  sst_total = sst_h13,
  total_n_fixed = r2_fit_h12_fixed$n + r3_fit_fixed$n
)

w2_path_r1_h15 <- build_linear_w2_path(length(r1_months), h15$best$w2_start, h15$best$w2_end)

cat("\n── H15: R1 gradual drawdown model ──────────────────────────────────\n")
cat(sprintf("R1 index:              %s\n", h15$best$series))
cat(sprintf("R1 w1:                 %d days\n", h15$best$w1))
cat(sprintf("R1 w2 path:            %d -> %d days\n", h15$best$w2_start, h15$best$w2_end))
cat(sprintf("Overall R²:            %.4f\n", h15$best$r2))
cat(sprintf("Overall RMSE:          %.4f $/BBL\n", h15$best$rmse))
cat(sprintf("R1 coefficients:       intercept=%.4f  slope=%.4f\n",
            h15$fit$coef["intercept"], h15$fit$coef["slope"]))

cat("\n── H15: Top 15 fits ─────────────────────────────────────────────────\n")
h15$table |>
  slice_head(n = 15) |>
  mutate(across(c(r2, rmse), \(x) round(x, 5))) |>
  print()

# H16: refine R2 using H15-refined R1 and fixed H14 R3
r1_fit_h15_fixed <- h15$fit

w2_r2_start_seq <- seq(10L, 70L, by = 5L)
w2_r2_end_seq   <- seq(5L, 50L, by = 5L)

cat(sprintf("\nH16 grid (R2 drawdown): %d series × %d w1 × %d start-w2 × %d end-w2\n",
            length(series_regime_seq), length(w1_regime_seq),
            length(w2_r2_start_seq), length(w2_r2_end_seq)))

h16 <- search_drawdown_regime(
  months = r2_months,
  y = y_r2_hx,
  series_seq = series_regime_seq,
  w1_seq = w1_regime_seq,
  w2_start_seq = w2_r2_start_seq,
  w2_end_seq = w2_r2_end_seq,
  anchor_day = 24L,
  vlsfo_tbl = vlsfo,
  fixed_ssr = r1_fit_h15_fixed$ssr + r3_fit_fixed$ssr,
  sst_total = sst_h13,
  total_n_fixed = r1_fit_h15_fixed$n + r3_fit_fixed$n
)

w2_path_r2_h16 <- build_linear_w2_path(length(r2_months), h16$best$w2_start, h16$best$w2_end)

cat("\n── H16: R2 gradual drawdown model ──────────────────────────────────\n")
cat(sprintf("R2 index:              %s\n", h16$best$series))
cat(sprintf("R2 w1:                 %d days\n", h16$best$w1))
cat(sprintf("R2 w2 path:            %d -> %d days\n", h16$best$w2_start, h16$best$w2_end))
cat(sprintf("Overall R²:            %.4f\n", h16$best$r2))
cat(sprintf("Overall RMSE:          %.4f $/BBL\n", h16$best$rmse))
cat(sprintf("R2 coefficients:       intercept=%.4f  slope=%.4f\n",
            h16$fit$coef["intercept"], h16$fit$coef["slope"]))

cat("\n── H16: Top 15 fits ─────────────────────────────────────────────────\n")
h16$table |>
  slice_head(n = 15) |>
  mutate(across(c(r2, rmse), \(x) round(x, 5))) |>
  print()

df_h16 <- bind_rows(
  tibble(date = r1_months,
         actual = y_r1_hx,
         pred = h15$pred,
         regime = "R1: Drawdown",
         w2_dynamic = w2_path_r1_h15) |>
    filter(!is.na(pred)) |>
    mutate(fitted = h15$fit$coef["intercept"] + h15$fit$coef["slope"] * pred,
           residual = actual - fitted),
  tibble(date = r2_months,
         actual = y_r2_hx,
         pred = h16$pred,
         regime = "R2: Drawdown",
         w2_dynamic = w2_path_r2_h16) |>
    filter(!is.na(pred)) |>
    mutate(fitted = h16$fit$coef["intercept"] + h16$fit$coef["slope"] * pred,
           residual = actual - fitted),
  tibble(date = r3_months,
         actual = y_r3_hx,
         pred = pred_r3_fixed$pred,
         regime = "R3: Rebuild",
         w2_dynamic = pred_r3_fixed$w2_dynamic) |>
    filter(!is.na(pred)) |>
    mutate(fitted = r3_fit_fixed$coef["intercept"] + r3_fit_fixed$coef["slope"] * pred,
           residual = actual - fitted)
)

p_h16_w2 <- bind_rows(
  tibble(date = r1_months, regime = "R1: Drawdown", w2_dynamic = w2_path_r1_h15),
  tibble(date = r2_months, regime = "R2: Drawdown", w2_dynamic = w2_path_r2_h16),
  tibble(date = r3_months, regime = "R3: Rebuild",  w2_dynamic = pred_r3_fixed$w2_dynamic)
) |>
  ggplot(aes(x = date, y = w2_dynamic, color = regime)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("R1: Drawdown" = "#1b7837",
                                "R2: Drawdown" = "#d95f02",
                                "R3: Rebuild" = "#762a83")) +
  labs(title = "Dynamic inventory window by regime",
       subtitle = "R1/R2 draw down into the contract reset; R3 rebuilds afterward",
       x = NULL, y = "w2 (days)", color = NULL) +
  theme_minimal()
print(p_h16_w2)

p_h16_ts <- df_h16 |>
  pivot_longer(c(actual, fitted), names_to = "series", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = series, linetype = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(actual = "black", fitted = "steelblue")) +
  scale_linetype_manual(values = c(actual = "solid", fitted = "dashed")) +
  labs(title = "H16: Dynamic three-regime model",
       subtitle = sprintf("R²=%.4f  RMSE=%.2f $/BBL", h16$best$r2, h16$best$rmse),
       x = NULL, y = "LSFO ($/BBL)", color = NULL, linetype = NULL) +
  theme_minimal()
print(p_h16_ts)

p_h16_resid <- df_h16 |>
  ggplot(aes(x = date, y = residual, fill = regime)) +
  geom_col(width = as.numeric(mean(diff(sort(unique(df_h16$date)))) * 0.8)) +
  geom_hline(yintercept = 0, color = "grey30") +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("R1: Drawdown" = "#1b7837",
                               "R2: Drawdown" = "#d95f02",
                               "R3: Rebuild" = "#762a83")) +
  labs(title = "H16: Residuals by regime",
       x = NULL, y = "Residual ($/BBL)", fill = NULL) +
  theme_minimal()
print(p_h16_resid)

cat("\n══ H14 vs H16 ════════════════════════════════════════════════════════\n")
tibble(
  Model = c("H14: R3 rebuild only", "H16: R1/R2 drawdown + R3 rebuild"),
  R2 = c(best_h14$r2, h16$best$r2),
  RMSE = c(best_h14$rmse, h16$best$rmse)
) |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4))) |>
  print()


# ── H17: Joint local refinement around first-pass regime solutions ────────────
#
# Strategy:
#   1. Search one regime at a time (H15, H16) to get sensible centres.
#   2. Search jointly over narrow windows around those centres.
#   3. Report if any optimum is on a search boundary so the user knows whether
#      to widen that dimension in the next iteration.

is_on_boundary <- function(value, seq_vals) {
  value <= min(seq_vals) || value >= max(seq_vals)
}

make_local_seq <- function(center, lower, upper, step = 1L) {
  seq(max(lower, center - 2L * step), min(upper, center + 2L * step), by = step)
}

flag_boundary <- function(label, value, seq_vals) {
  if (is_on_boundary(value, seq_vals)) {
    cat(sprintf("Boundary flag: %s = %s is at edge [%s, %s]\n",
                label, value, min(seq_vals), max(seq_vals)))
  }
}

r1_series_local <- h15$best$series
r2_series_local <- h16$best$series
r3_series_local <- POST_SERIES

w1_r1_local_seq <- make_local_seq(h15$best$w1, 5L, 40L, step = 1L)
w2_r1_start_local_seq <- make_local_seq(h15$best$w2_start, 20L, 140L, step = 2L)
w2_r1_end_local_seq   <- make_local_seq(h15$best$w2_end,   5L, 100L, step = 2L)

w1_r2_local_seq <- make_local_seq(h16$best$w1, 5L, 40L, step = 1L)
w2_r2_start_local_seq <- make_local_seq(h16$best$w2_start, 5L, 80L, step = 2L)
w2_r2_end_local_seq   <- make_local_seq(h16$best$w2_end,   5L, 60L, step = 2L)

w1_r3_local_seq <- make_local_seq(w1_h14, 5L, 40L, step = 1L)
w2_r3_start_local_seq <- make_local_seq(20L, 5L, 50L, step = 2L)
w2_r3_end_local_seq   <- make_local_seq(70L, 20L, 100L, step = 2L)

cat(sprintf(
  "\nH17 joint local grid: R1(%d×%d×%d) × R2(%d×%d×%d) × R3(%d×%d×%d)\n",
  length(w1_r1_local_seq), length(w2_r1_start_local_seq), length(w2_r1_end_local_seq),
  length(w1_r2_local_seq), length(w2_r2_start_local_seq), length(w2_r2_end_local_seq),
  length(w1_r3_local_seq), length(w2_r3_start_local_seq), length(w2_r3_end_local_seq)
))

precompute_dynamic_regime_cache <- function(series_name, months, w1_seq, w2_seq, anchor_day) {
  cache <- list()
  for (.w1 in w1_seq) {
    for (.w2 in w2_seq) {
      cache[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo[[series_name]], vlsfo$date, .w1, .w2, months, anchor_day)$pred
    }
  }
  cache
}

r1_cache_local <- precompute_dynamic_regime_cache(
  r1_series_local, r1_months, w1_r1_local_seq,
  seq(min(w2_r1_end_local_seq), max(w2_r1_start_local_seq), by = 1L), 24L
)

r2_cache_local <- precompute_dynamic_regime_cache(
  r2_series_local, r2_months, w1_r2_local_seq,
  seq(min(w2_r2_end_local_seq), max(w2_r2_start_local_seq), by = 1L), 24L
)

r3_cache_local <- precompute_dynamic_regime_cache(
  r3_series_local, r3_months, w1_r3_local_seq,
  seq(min(w2_r3_start_local_seq), max(w2_r3_end_local_seq), by = 1L), anchor_day_h14
)

best_h17 <- NULL
best_r2_h17 <- -Inf

for (w1_r1_v in w1_r1_local_seq) {
  for (w2_r1_start_v in w2_r1_start_local_seq) {
    for (w2_r1_end_v in w2_r1_end_local_seq) {
      if (w2_r1_end_v > w2_r1_start_v) next

      path_r1_v <- build_linear_w2_path(length(r1_months), w2_r1_start_v, w2_r1_end_v)
      pred_r1_v <- numeric(length(r1_months))
      for (j in seq_along(r1_months)) {
        pred_r1_v[j] <- r1_cache_local[[paste(w1_r1_v, path_r1_v[j], sep = "_")]][j]
      }
      fit_r1_v <- summarise_regime_fit(y_r1_hx, pred_r1_v)
      if (is.null(fit_r1_v)) next

      for (w1_r2_v in w1_r2_local_seq) {
        for (w2_r2_start_v in w2_r2_start_local_seq) {
          for (w2_r2_end_v in w2_r2_end_local_seq) {
            if (w2_r2_end_v > w2_r2_start_v) next

            path_r2_v <- build_linear_w2_path(length(r2_months), w2_r2_start_v, w2_r2_end_v)
            pred_r2_v <- numeric(length(r2_months))
            for (j in seq_along(r2_months)) {
              pred_r2_v[j] <- r2_cache_local[[paste(w1_r2_v, path_r2_v[j], sep = "_")]][j]
            }
            fit_r2_v <- summarise_regime_fit(y_r2_hx, pred_r2_v)
            if (is.null(fit_r2_v)) next

            for (w1_r3_v in w1_r3_local_seq) {
              for (w2_r3_start_v in w2_r3_start_local_seq) {
                for (w2_r3_end_v in w2_r3_end_local_seq) {
                  if (w2_r3_end_v < w2_r3_start_v) next

                  path_r3_v <- build_linear_w2_path(length(r3_months), w2_r3_start_v, w2_r3_end_v)
                  pred_r3_v <- numeric(length(r3_months))
                  for (j in seq_along(r3_months)) {
                    pred_r3_v[j] <- r3_cache_local[[paste(w1_r3_v, path_r3_v[j], sep = "_")]][j]
                  }
                  fit_r3_v <- summarise_regime_fit(y_r3_hx, pred_r3_v)
                  if (is.null(fit_r3_v)) next

                  total_ssr <- fit_r1_v$ssr + fit_r2_v$ssr + fit_r3_v$ssr
                  total_n   <- fit_r1_v$n + fit_r2_v$n + fit_r3_v$n
                  r2_total  <- 1 - total_ssr / sst_h13

                  if (r2_total > best_r2_h17) {
                    best_r2_h17 <- r2_total
                    best_h17 <- list(
                      r2 = r2_total,
                      rmse = sqrt(total_ssr / total_n),
                      r1 = list(series = r1_series_local, w1 = w1_r1_v,
                                w2_start = w2_r1_start_v, w2_end = w2_r1_end_v,
                                fit = fit_r1_v, pred = pred_r1_v, path = path_r1_v),
                      r2_reg = list(series = r2_series_local, w1 = w1_r2_v,
                                    w2_start = w2_r2_start_v, w2_end = w2_r2_end_v,
                                    fit = fit_r2_v, pred = pred_r2_v, path = path_r2_v),
                      r3 = list(series = r3_series_local, w1 = w1_r3_v,
                                w2_start = w2_r3_start_v, w2_end = w2_r3_end_v,
                                fit = fit_r3_v, pred = pred_r3_v, path = path_r3_v)
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

cat("\n── H17: Joint local refinement ─────────────────────────────────────\n")
cat(sprintf("Overall R²:            %.4f\n", best_h17$r2))
cat(sprintf("Overall RMSE:          %.4f $/BBL\n", best_h17$rmse))
cat(sprintf("R1: %s | w1=%d | w2=%d -> %d\n",
            best_h17$r1$series, best_h17$r1$w1, best_h17$r1$w2_start, best_h17$r1$w2_end))
cat(sprintf("R2: %s | w1=%d | w2=%d -> %d\n",
            best_h17$r2_reg$series, best_h17$r2_reg$w1, best_h17$r2_reg$w2_start, best_h17$r2_reg$w2_end))
cat(sprintf("R3: %s | w1=%d | w2=%d -> %d\n",
            best_h17$r3$series, best_h17$r3$w1, best_h17$r3$w2_start, best_h17$r3$w2_end))

cat("\n── H17 boundary checks ─────────────────────────────────────────────\n")
flag_boundary("H17 R1 w1", best_h17$r1$w1, w1_r1_local_seq)
flag_boundary("H17 R1 w2_start", best_h17$r1$w2_start, w2_r1_start_local_seq)
flag_boundary("H17 R1 w2_end", best_h17$r1$w2_end, w2_r1_end_local_seq)
flag_boundary("H17 R2 w1", best_h17$r2_reg$w1, w1_r2_local_seq)
flag_boundary("H17 R2 w2_start", best_h17$r2_reg$w2_start, w2_r2_start_local_seq)
flag_boundary("H17 R2 w2_end", best_h17$r2_reg$w2_end, w2_r2_end_local_seq)
flag_boundary("H17 R3 w1", best_h17$r3$w1, w1_r3_local_seq)
flag_boundary("H17 R3 w2_start", best_h17$r3$w2_start, w2_r3_start_local_seq)
flag_boundary("H17 R3 w2_end", best_h17$r3$w2_end, w2_r3_end_local_seq)

df_h17 <- bind_rows(
  tibble(date = r1_months, actual = y_r1_hx, pred = best_h17$r1$pred,
         regime = "R1", w2_dynamic = best_h17$r1$path) |>
    filter(!is.na(pred)) |>
    mutate(fitted = best_h17$r1$fit$coef["intercept"] + best_h17$r1$fit$coef["slope"] * pred,
           residual = actual - fitted),
  tibble(date = r2_months, actual = y_r2_hx, pred = best_h17$r2_reg$pred,
         regime = "R2", w2_dynamic = best_h17$r2_reg$path) |>
    filter(!is.na(pred)) |>
    mutate(fitted = best_h17$r2_reg$fit$coef["intercept"] + best_h17$r2_reg$fit$coef["slope"] * pred,
           residual = actual - fitted),
  tibble(date = r3_months, actual = y_r3_hx, pred = best_h17$r3$pred,
         regime = "R3", w2_dynamic = best_h17$r3$path) |>
    filter(!is.na(pred)) |>
    mutate(fitted = best_h17$r3$fit$coef["intercept"] + best_h17$r3$fit$coef["slope"] * pred,
           residual = actual - fitted)
)

p_h17_w2 <- bind_rows(
  tibble(date = r1_months, regime = "R1", w2_dynamic = best_h17$r1$path),
  tibble(date = r2_months, regime = "R2", w2_dynamic = best_h17$r2_reg$path),
  tibble(date = r3_months, regime = "R3", w2_dynamic = best_h17$r3$path)
) |>
  ggplot(aes(x = date, y = w2_dynamic, color = regime)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  labs(title = "H17: Jointly refined dynamic inventory paths",
       x = NULL, y = "w2 (days)", color = NULL) +
  theme_minimal()
print(p_h17_w2)

cat("\n══ H16 vs H17 ════════════════════════════════════════════════════════\n")
tibble(
  Model = c("H16: sequential regime search", "H17: joint local refinement"),
  R2 = c(h16$best$r2, best_h17$r2),
  RMSE = c(h16$best$rmse, best_h17$rmse)
) |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4))) |>
  print()


# ── H18: Singapore-only joint refinement with shared inventory handoffs ───────
#
# H17 can produce an unphysical boundary condition if the R1 ending inventory
# differs sharply from the R2 starting inventory. H18 addresses that in two ways:
#   1. force all three regimes onto the Singapore index
#   2. parameterize inventory with shared boundary levels:
#        inv_hi_pre  -> inv_break1  -> inv_break2  -> inv_hi_post
#      so the end of one regime equals the start of the next.

series_h18 <- "sing"

build_inventory_path <- function(n_months, start_level, end_level) {
  build_linear_w2_path(n_months, start_level, end_level)
}

w1_r1_h18_seq <- make_local_seq(best_h17$r1$w1, 5L, 40L, step = 1L)
w1_r2_h18_seq <- make_local_seq(best_h17$r2_reg$w1, 5L, 40L, step = 1L)
w1_r3_h18_seq <- make_local_seq(best_h17$r3$w1, 5L, 40L, step = 1L)

inv_hi_pre_seq  <- make_local_seq(best_h17$r1$w2_start, 20L, 140L, step = 2L)
inv_break1_seq  <- make_local_seq(round(mean(c(best_h17$r1$w2_end, best_h17$r2_reg$w2_start))), 5L, 100L, step = 2L)
inv_break2_seq  <- make_local_seq(round(mean(c(best_h17$r2_reg$w2_end, best_h17$r3$w2_start))), 5L, 80L, step = 2L)
inv_hi_post_seq <- make_local_seq(best_h17$r3$w2_end, 20L, 120L, step = 2L)

cat(sprintf(
  "\nH18 Singapore-only grid: w1_R1=%d × w1_R2=%d × w1_R3=%d × inv=(%d,%d,%d,%d)\n",
  length(w1_r1_h18_seq), length(w1_r2_h18_seq), length(w1_r3_h18_seq),
  length(inv_hi_pre_seq), length(inv_break1_seq), length(inv_break2_seq), length(inv_hi_post_seq)
))

cache_h18_r1 <- precompute_dynamic_regime_cache(
  series_h18, r1_months, w1_r1_h18_seq,
  seq(min(inv_break1_seq), max(inv_hi_pre_seq), by = 1L), 24L
)
cache_h18_r2 <- precompute_dynamic_regime_cache(
  series_h18, r2_months, w1_r2_h18_seq,
  seq(min(inv_break2_seq), max(inv_break1_seq), by = 1L), 24L
)
cache_h18_r3 <- precompute_dynamic_regime_cache(
  series_h18, r3_months, w1_r3_h18_seq,
  seq(min(inv_break2_seq), max(inv_hi_post_seq), by = 1L), anchor_day_h14
)

best_h18 <- NULL
best_r2_h18 <- -Inf

for (w1_r1_v in w1_r1_h18_seq) {
  for (w1_r2_v in w1_r2_h18_seq) {
    for (w1_r3_v in w1_r3_h18_seq) {
      for (inv_hi_pre in inv_hi_pre_seq) {
        for (inv_break1 in inv_break1_seq) {
          if (inv_break1 > inv_hi_pre) next

          for (inv_break2 in inv_break2_seq) {
            if (inv_break2 > inv_break1) next

            for (inv_hi_post in inv_hi_post_seq) {
              if (inv_hi_post < inv_break2) next

              path_r1_v <- build_inventory_path(length(r1_months), inv_hi_pre, inv_break1)
              path_r2_v <- build_inventory_path(length(r2_months), inv_break1, inv_break2)
              path_r3_v <- build_inventory_path(length(r3_months), inv_break2, inv_hi_post)

              pred_r1_v <- numeric(length(r1_months))
              pred_r2_v <- numeric(length(r2_months))
              pred_r3_v <- numeric(length(r3_months))

              for (j in seq_along(r1_months)) {
                pred_r1_v[j] <- cache_h18_r1[[paste(w1_r1_v, path_r1_v[j], sep = "_")]][j]
              }
              for (j in seq_along(r2_months)) {
                pred_r2_v[j] <- cache_h18_r2[[paste(w1_r2_v, path_r2_v[j], sep = "_")]][j]
              }
              for (j in seq_along(r3_months)) {
                pred_r3_v[j] <- cache_h18_r3[[paste(w1_r3_v, path_r3_v[j], sep = "_")]][j]
              }

              fit_r1_v <- summarise_regime_fit(y_r1_hx, pred_r1_v)
              fit_r2_v <- summarise_regime_fit(y_r2_hx, pred_r2_v)
              fit_r3_v <- summarise_regime_fit(y_r3_hx, pred_r3_v)
              if (is.null(fit_r1_v) || is.null(fit_r2_v) || is.null(fit_r3_v)) next

              total_ssr <- fit_r1_v$ssr + fit_r2_v$ssr + fit_r3_v$ssr
              total_n   <- fit_r1_v$n + fit_r2_v$n + fit_r3_v$n
              r2_total  <- 1 - total_ssr / sst_h13

              if (r2_total > best_r2_h18) {
                best_r2_h18 <- r2_total
                best_h18 <- list(
                  r2 = r2_total,
                  rmse = sqrt(total_ssr / total_n),
                  inv_hi_pre = inv_hi_pre,
                  inv_break1 = inv_break1,
                  inv_break2 = inv_break2,
                  inv_hi_post = inv_hi_post,
                  r1 = list(series = series_h18, w1 = w1_r1_v, fit = fit_r1_v,
                            pred = pred_r1_v, path = path_r1_v),
                  r2_reg = list(series = series_h18, w1 = w1_r2_v, fit = fit_r2_v,
                                pred = pred_r2_v, path = path_r2_v),
                  r3 = list(series = series_h18, w1 = w1_r3_v, fit = fit_r3_v,
                            pred = pred_r3_v, path = path_r3_v)
                )
              }
            }
          }
        }
      }
    }
  }
}

cat("\n── H18: Singapore-only joint refinement ────────────────────────────\n")
cat(sprintf("Overall R²:            %.4f\n", best_h18$r2))
cat(sprintf("Overall RMSE:          %.4f $/BBL\n", best_h18$rmse))
cat(sprintf("Inventory path:        %d -> %d -> %d -> %d days\n",
            best_h18$inv_hi_pre, best_h18$inv_break1, best_h18$inv_break2, best_h18$inv_hi_post))
cat(sprintf("R1: series=%s | w1=%d\n", best_h18$r1$series, best_h18$r1$w1))
cat(sprintf("R2: series=%s | w1=%d\n", best_h18$r2_reg$series, best_h18$r2_reg$w1))
cat(sprintf("R3: series=%s | w1=%d\n", best_h18$r3$series, best_h18$r3$w1))

cat("\n── H18 boundary checks ─────────────────────────────────────────────\n")
flag_boundary("H18 R1 w1", best_h18$r1$w1, w1_r1_h18_seq)
flag_boundary("H18 R2 w1", best_h18$r2_reg$w1, w1_r2_h18_seq)
flag_boundary("H18 R3 w1", best_h18$r3$w1, w1_r3_h18_seq)
flag_boundary("H18 inv_hi_pre", best_h18$inv_hi_pre, inv_hi_pre_seq)
flag_boundary("H18 inv_break1", best_h18$inv_break1, inv_break1_seq)
flag_boundary("H18 inv_break2", best_h18$inv_break2, inv_break2_seq)
flag_boundary("H18 inv_hi_post", best_h18$inv_hi_post, inv_hi_post_seq)

df_h18 <- bind_rows(
  tibble(date = r1_months, actual = y_r1_hx, pred = best_h18$r1$pred,
         regime = "R1", w2_dynamic = best_h18$r1$path) |>
    filter(!is.na(pred)) |>
    mutate(fitted = best_h18$r1$fit$coef["intercept"] + best_h18$r1$fit$coef["slope"] * pred,
           residual = actual - fitted),
  tibble(date = r2_months, actual = y_r2_hx, pred = best_h18$r2_reg$pred,
         regime = "R2", w2_dynamic = best_h18$r2_reg$path) |>
    filter(!is.na(pred)) |>
    mutate(fitted = best_h18$r2_reg$fit$coef["intercept"] + best_h18$r2_reg$fit$coef["slope"] * pred,
           residual = actual - fitted),
  tibble(date = r3_months, actual = y_r3_hx, pred = best_h18$r3$pred,
         regime = "R3", w2_dynamic = best_h18$r3$path) |>
    filter(!is.na(pred)) |>
    mutate(fitted = best_h18$r3$fit$coef["intercept"] + best_h18$r3$fit$coef["slope"] * pred,
           residual = actual - fitted)
)

p_h18_w2 <- bind_rows(
  tibble(date = r1_months, regime = "R1", w2_dynamic = best_h18$r1$path),
  tibble(date = r2_months, regime = "R2", w2_dynamic = best_h18$r2_reg$path),
  tibble(date = r3_months, regime = "R3", w2_dynamic = best_h18$r3$path)
) |>
  ggplot(aes(x = date, y = w2_dynamic, color = regime)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = c(break1, break2), linetype = "dashed", color = "grey40") +
  labs(title = "H18: Singapore-only dynamic inventory paths",
       subtitle = "Shared boundary levels enforce inventory continuity across regimes",
       x = NULL, y = "w2 (days)", color = NULL) +
  theme_minimal()
print(p_h18_w2)

cat("\n══ H17 vs H18 ════════════════════════════════════════════════════════\n")
tibble(
  Model = c("H17: mixed-index local refinement", "H18: Singapore-only with shared handoffs"),
  R2 = c(best_h17$r2, best_h18$r2),
  RMSE = c(best_h17$rmse, best_h18$rmse)
) |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4))) |>
  print()


# ── H19: Transition-period cost-layer release model ───────────────────────────
#
# Physical inventory remains continuous across regimes, but the reported R2 cost
# is allowed to release the legacy R1 cost layer over time instead of switching
# instantly. R1 uses one of four candidate raw indices; R2/R3 use Singapore.
#
# For month j in R2:
#   alpha_j = max(1 - j / release_months, 0)
#   fitted_R2_j = alpha_j * legacy_cost_j + (1 - alpha_j) * new_cost_j
#
# where:
#   legacy_cost_j = R1 coefficients applied to the R1 index over R2 months
#   new_cost_j    = R3-style Singapore coefficients applied to R2 months

make_local_date_seq <- function(center_date, all_dates, months_radius = 2L) {
  if (inherits(center_date, "Date")) {
    cd <- center_date
  } else if (is.numeric(center_date) && length(center_date) == 1L) {
    cd <- as.Date(center_date, origin = "1970-01-01")
  } else {
    stop("center_date must be a Date or numeric Date offset")
  }

  all_dates <- as.Date(all_dates)
  lo <- cd %m-% months(months_radius)
  hi <- cd %m+% months(months_radius)
  all_dates[all_dates >= lo & all_dates <= hi]
}

if (!exists("best_h18")) stop("best_h18 must exist before H19")

all_months_h19 <- as.Date(sort(unique(heco$date)))

break1_center <- if (!is.null(best_h18$break1)) {
  if (inherits(best_h18$break1, "Date")) best_h18$break1 else as.Date(best_h18$break1, origin = "1970-01-01")
} else {
  break1
}

break2_center <- if (!is.null(best_h18$break2)) {
  if (inherits(best_h18$break2, "Date")) best_h18$break2 else as.Date(best_h18$break2, origin = "1970-01-01")
} else {
  break2
}

break1_h19_seq <- make_local_date_seq(break1_center, all_months_h19, months_radius = 2L)
break2_h19_seq <- make_local_date_seq(break2_center, all_months_h19, months_radius = 2L)

r1_series_h19_seq <- c("glob", "sing", "houston", "avg_sing_hou")

w1_r1_h19_seq <- make_local_seq(best_h18$r1$w1, 5L, 60L, step = 1L)
w1_r2_h19_seq <- make_local_seq(best_h18$r2_reg$w1, 5L, 60L, step = 1L)
w1_r3_h19_seq <- make_local_seq(best_h18$r3$w1, 5L, 60L, step = 1L)

inv_hi_pre_h19_seq  <- make_local_seq(best_h18$inv_hi_pre, 20L, 160L, step = 2L)
inv_break1_h19_seq  <- make_local_seq(best_h18$inv_break1, 5L, 120L, step = 2L)
inv_break2_h19_seq  <- make_local_seq(best_h18$inv_break2, 5L, 100L, step = 2L)
inv_hi_post_h19_seq <- make_local_seq(best_h18$inv_hi_post, 20L, 140L, step = 2L)

cat(sprintf(
  "\nH19 grid: break1=%d × break2=%d × R1series=%d × w1=(%d,%d,%d) × inv=(%d,%d,%d,%d)\n",
  length(break1_h19_seq), length(break2_h19_seq), length(r1_series_h19_seq),
  length(w1_r1_h19_seq), length(w1_r2_h19_seq), length(w1_r3_h19_seq),
  length(inv_hi_pre_h19_seq), length(inv_break1_h19_seq),
  length(inv_break2_h19_seq), length(inv_hi_post_h19_seq)
))

best_h19 <- NULL
best_r2_h19 <- -Inf

for (b1 in break1_h19_seq) {
  for (b2 in break2_h19_seq) {
    if (b2 <= b1) next

    idx_r1_h19 <- which(heco$date < b1)
    idx_r2_h19 <- which(heco$date >= b1 & heco$date < b2)
    idx_r3_h19 <- which(heco$date >= b2)

    if (length(idx_r1_h19) < 4L || length(idx_r2_h19) < 4L || length(idx_r3_h19) < 4L) next

    r1_months_h19 <- heco$date[idx_r1_h19]
    r2_months_h19 <- heco$date[idx_r2_h19]
    r3_months_h19 <- heco$date[idx_r3_h19]

    y_r1_h19 <- y_all[idx_r1_h19]
    y_r2_h19 <- y_all[idx_r2_h19]
    y_r3_h19 <- y_all[idx_r3_h19]

    release_months_seq <- seq(1L, length(r2_months_h19), by = 1L)

    # Caches for all series/w1/w2 combinations needed in this break-date cell.
    cache_r1_main <- list()
    for (.s in r1_series_h19_seq) {
      all_w2_r1 <- seq(min(inv_break1_h19_seq), max(inv_hi_pre_h19_seq), by = 1L)
      for (.w1 in w1_r1_h19_seq) {
        for (.w2 in all_w2_r1) {
          cache_r1_main[[paste(.s, .w1, .w2, sep = "_")]] <-
            build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r1_months_h19, 24L)$pred
        }
      }
    }

    cache_r1_legacy_r2 <- list()
    for (.s in r1_series_h19_seq) {
      all_w2_r2_legacy <- seq(min(inv_break2_h19_seq), max(inv_break1_h19_seq), by = 1L)
      for (.w1 in w1_r1_h19_seq) {
        for (.w2 in all_w2_r2_legacy) {
          cache_r1_legacy_r2[[paste(.s, .w1, .w2, sep = "_")]] <-
            build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r2_months_h19, 24L)$pred
        }
      }
    }

    cache_r2_new <- list()
    for (.w1 in w1_r2_h19_seq) {
      for (.w2 in seq(min(inv_break2_h19_seq), max(inv_break1_h19_seq), by = 1L)) {
        cache_r2_new[[paste(.w1, .w2, sep = "_")]] <-
          build_predictor_anchor(vlsfo$sing, vlsfo$date, .w1, .w2, r2_months_h19, 24L)$pred
      }
    }

    cache_r3 <- list()
    for (.w1 in w1_r3_h19_seq) {
      for (.w2 in seq(min(inv_break2_h19_seq), max(inv_hi_post_h19_seq), by = 1L)) {
        cache_r3[[paste(.w1, .w2, sep = "_")]] <-
          build_predictor_anchor(vlsfo$sing, vlsfo$date, .w1, .w2, r3_months_h19, anchor_day_h14)$pred
      }
    }

    for (r1_series_h19 in r1_series_h19_seq) {
      for (w1_r1_v in w1_r1_h19_seq) {
        for (w1_r2_v in w1_r2_h19_seq) {
          for (w1_r3_v in w1_r3_h19_seq) {
            for (inv_hi_pre in inv_hi_pre_h19_seq) {
              for (inv_break1 in inv_break1_h19_seq) {
                if (inv_break1 > inv_hi_pre) next

                for (inv_break2 in inv_break2_h19_seq) {
                  if (inv_break2 > inv_break1) next

                  for (inv_hi_post in inv_hi_post_h19_seq) {
                    if (inv_hi_post < inv_break2) next

                    path_r1_v <- build_inventory_path(length(r1_months_h19), inv_hi_pre, inv_break1)
                    path_r2_v <- build_inventory_path(length(r2_months_h19), inv_break1, inv_break2)
                    path_r3_v <- build_inventory_path(length(r3_months_h19), inv_break2, inv_hi_post)

                    pred_r1_v <- numeric(length(r1_months_h19))
                    pred_r2_legacy_v <- numeric(length(r2_months_h19))
                    pred_r2_new_v <- numeric(length(r2_months_h19))
                    pred_r3_v <- numeric(length(r3_months_h19))

                    for (j in seq_along(r1_months_h19)) {
                      pred_r1_v[j] <- cache_r1_main[[paste(r1_series_h19, w1_r1_v, path_r1_v[j], sep = "_")]][j]
                    }
                    for (j in seq_along(r2_months_h19)) {
                      pred_r2_legacy_v[j] <- cache_r1_legacy_r2[[paste(r1_series_h19, w1_r1_v, path_r2_v[j], sep = "_")]][j]
                      pred_r2_new_v[j]    <- cache_r2_new[[paste(w1_r2_v, path_r2_v[j], sep = "_")]][j]
                    }
                    for (j in seq_along(r3_months_h19)) {
                      pred_r3_v[j] <- cache_r3[[paste(w1_r3_v, path_r3_v[j], sep = "_")]][j]
                    }

                    fit_r1_v <- summarise_regime_fit(y_r1_h19, pred_r1_v)
                    fit_r3_v <- summarise_regime_fit(y_r3_h19, pred_r3_v)
                    if (is.null(fit_r1_v) || is.null(fit_r3_v)) next
                    if (any(is.na(pred_r2_legacy_v)) || any(is.na(pred_r2_new_v))) next

                    legacy_cost_r2 <- fit_r1_v$coef["intercept"] + fit_r1_v$coef["slope"] * pred_r2_legacy_v
                    new_cost_r2    <- fit_r3_v$coef["intercept"] + fit_r3_v$coef["slope"] * pred_r2_new_v

                    for (release_months in release_months_seq) {
                      month_idx <- seq_along(r2_months_h19) - 1L
                      alpha <- pmax(1 - month_idx / release_months, 0)
                      fitted_r2 <- alpha * legacy_cost_r2 + (1 - alpha) * new_cost_r2
                      ssr_r2 <- sum((y_r2_h19 - fitted_r2)^2)

                      total_ssr <- fit_r1_v$ssr + ssr_r2 + fit_r3_v$ssr
                      total_n   <- fit_r1_v$n + length(r2_months_h19) + fit_r3_v$n
                      r2_total  <- 1 - total_ssr / sst_h13

                      if (r2_total > best_r2_h19) {
                        best_r2_h19 <- r2_total
                        best_h19 <- list(
                          r2 = r2_total,
                          rmse = sqrt(total_ssr / total_n),
                          break1 = b1,
                          break2 = b2,
                          release_months = release_months,
                          inv_hi_pre = inv_hi_pre,
                          inv_break1 = inv_break1,
                          inv_break2 = inv_break2,
                          inv_hi_post = inv_hi_post,
                          r1 = list(
                            series = r1_series_h19,
                            w1 = w1_r1_v,
                            fit = fit_r1_v,
                            pred = pred_r1_v,
                            path = path_r1_v,
                            months = r1_months_h19,
                            y = y_r1_h19
                          ),
                          r2_reg = list(
                            series_legacy = r1_series_h19,
                            series_new = "sing",
                            w1_legacy = w1_r1_v,
                            w1_new = w1_r2_v,
                            alpha = alpha,
                            pred_legacy = pred_r2_legacy_v,
                            pred_new = pred_r2_new_v,
                            fitted = fitted_r2,
                            path = path_r2_v,
                            months = r2_months_h19,
                            y = y_r2_h19,
                            ssr = ssr_r2
                          ),
                          r3 = list(
                            series = "sing",
                            w1 = w1_r3_v,
                            fit = fit_r3_v,
                            pred = pred_r3_v,
                            path = path_r3_v,
                            months = r3_months_h19,
                            y = y_r3_h19
                          )
                        )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

cat("\n── H19: Transition-period cost-layer release model ─────────────────\n")
cat(sprintf("Overall R²:            %.4f\n", best_h19$r2))
cat(sprintf("Overall RMSE:          %.4f $/BBL\n", best_h19$rmse))
cat(sprintf("Break1:                %s\n", best_h19$break1))
cat(sprintf("Break2:                %s\n", best_h19$break2))
cat(sprintf("Release months in R2:  %d\n", best_h19$release_months))
cat(sprintf("Inventory path:        %d -> %d -> %d -> %d days\n",
            best_h19$inv_hi_pre, best_h19$inv_break1, best_h19$inv_break2, best_h19$inv_hi_post))
cat(sprintf("R1 index:              %s | w1=%d\n", best_h19$r1$series, best_h19$r1$w1))
cat(sprintf("R2: legacy=%s/new=sing | w1 legacy=%d | w1 new=%d\n",
            best_h19$r2_reg$series_legacy, best_h19$r2_reg$w1_legacy, best_h19$r2_reg$w1_new))
cat(sprintf("R3 index:              sing | w1=%d\n", best_h19$r3$w1))

cat("\n── H19 boundary checks ─────────────────────────────────────────────\n")
flag_boundary("H19 break1", best_h19$break1, break1_h19_seq)
flag_boundary("H19 break2", best_h19$break2, break2_h19_seq)
flag_boundary("H19 release_months", best_h19$release_months, release_months_seq)
flag_boundary("H19 R1 w1", best_h19$r1$w1, w1_r1_h19_seq)
flag_boundary("H19 R2 legacy w1", best_h19$r2_reg$w1_legacy, w1_r1_h19_seq)
flag_boundary("H19 R2 new w1", best_h19$r2_reg$w1_new, w1_r2_h19_seq)
flag_boundary("H19 R3 w1", best_h19$r3$w1, w1_r3_h19_seq)
flag_boundary("H19 inv_hi_pre", best_h19$inv_hi_pre, inv_hi_pre_h19_seq)
flag_boundary("H19 inv_break1", best_h19$inv_break1, inv_break1_h19_seq)
flag_boundary("H19 inv_break2", best_h19$inv_break2, inv_break2_h19_seq)
flag_boundary("H19 inv_hi_post", best_h19$inv_hi_post, inv_hi_post_h19_seq)

df_h19 <- bind_rows(
  tibble(
    date = best_h19$r1$months,
    actual = best_h19$r1$y,
    pred = best_h19$r1$pred,
    regime = "R1",
    w2_dynamic = best_h19$r1$path
  ) |>
    filter(!is.na(pred)) |>
    mutate(
      fitted = best_h19$r1$fit$coef["intercept"] + best_h19$r1$fit$coef["slope"] * pred,
      residual = actual - fitted
    ),
  tibble(
    date = best_h19$r2_reg$months,
    actual = best_h19$r2_reg$y,
    pred = best_h19$r2_reg$pred_new,
    regime = "R2",
    w2_dynamic = best_h19$r2_reg$path,
    alpha = best_h19$r2_reg$alpha
  ) |>
    mutate(
      fitted = best_h19$r2_reg$fitted,
      residual = actual - fitted
    ),
  tibble(
    date = best_h19$r3$months,
    actual = best_h19$r3$y,
    pred = best_h19$r3$pred,
    regime = "R3",
    w2_dynamic = best_h19$r3$path
  ) |>
    filter(!is.na(pred)) |>
    mutate(
      fitted = best_h19$r3$fit$coef["intercept"] + best_h19$r3$fit$coef["slope"] * pred,
      residual = actual - fitted
    )
)

p_h19_alpha <- tibble(
  date = best_h19$r2_reg$months,
  alpha = best_h19$r2_reg$alpha
) |>
  ggplot(aes(x = date, y = alpha)) +
  geom_line(color = "#762a83", linewidth = 1) +
  geom_point(color = "#762a83", size = 2) +
  labs(title = "H19: Legacy cost-layer release share in R2",
       subtitle = sprintf("Legacy share falls to zero over %d months", best_h19$release_months),
       x = NULL, y = "Legacy share alpha") +
  theme_minimal()
print(p_h19_alpha)

cat("\n══ H18 vs H19 ════════════════════════════════════════════════════════\n")
tibble(
  Model = c("H18: Singapore-only shared handoffs", "H19: transition cost-layer release"),
  R2 = c(best_h18$r2, best_h19$r2),
  RMSE = c(best_h18$rmse, best_h19$rmse)
) |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4))) |>
  print()


# ── H20-fast-3: corrected coarse exploratory search ---------------------------
#
# Purpose:
#   Quick, robust coarse search to locate a promising region for H20.
#
# Key correction vs H20-fast-2:
#   Allows inventory to fall into break1 and then recover by break2, i.e.
#     inv_hi_pre -> inv_break1 -> inv_break2 -> inv_hi_post
#   with inv_break2 allowed to exceed inv_break1.
#
# Uses:
#   heco, vlsfo, y_all, sst_h13, anchor_day_h14

library(tidyverse)
library(lubridate)
library(slider)
library(parallel)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- helpers ----

build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end     <- findInterval(as.integer(anchor),         daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

build_shock_pulse <- function(dates, break_date, duration_months) {
  if (duration_months <= 0L) return(rep(0, length(dates)))
  map_dbl(as.Date(dates), function(d) {
    if (d < break_date) return(0)
    m <- interval(break_date, d) %/% months(1)
    max(1 - m / duration_months, 0)
  })
}

safe_int_seq <- function(lo, hi) {
  lo <- suppressWarnings(as.integer(lo))
  hi <- suppressWarnings(as.integer(hi))
  if (length(lo) != 1L || length(hi) != 1L || is.na(lo) || is.na(hi) || lo > hi) integer(0) else seq(lo, hi, by = 1L)
}

flag_boundary <- function(label, value, seq_vals) {
  if (length(value) != 1L || is.na(value)) {
    cat(sprintf("Boundary check skipped: %s has invalid value\n", label))
    return(invisible(FALSE))
  }
  seq_vals <- seq_vals[!is.na(seq_vals)]
  if (length(seq_vals) == 0L) {
    cat(sprintf("Boundary check skipped: %s has empty search range\n", label))
    return(invisible(FALSE))
  }

  if (inherits(value, "Date") || inherits(seq_vals, "Date")) {
    value_cmp <- as.Date(value)
    seq_cmp <- as.Date(seq_vals)
  } else {
    value_cmp <- as.numeric(value)
    seq_cmp <- as.numeric(seq_vals)
  }

  hit <- value_cmp <= min(seq_cmp) || value_cmp >= max(seq_cmp)
  if (isTRUE(hit)) {
    cat(sprintf(
      "Boundary flag: %s = %s is at edge [%s, %s]\n",
      label, as.character(value), as.character(min(seq_cmp)), as.character(max(seq_cmp))
    ))
  }
  invisible(hit)
}

# ---- tighter exploratory grid ----

r1_series_seq <- c("glob", "sing", "avg_sing_hou")

break1_seq <- as.Date(c("2024-02-01", "2024-03-01", "2024-04-01"))
break2_seq <- as.Date(c("2024-10-01", "2024-11-01", "2024-12-01"))

w1_r1_seq <- c(31L, 33L, 35L)
w1_r2_seq <- c(37L, 39L, 41L)
w1_r3_seq <- c(14L, 16L, 18L)

inv_hi_pre_seq  <- c(34L, 38L, 42L)
inv_break1_seq  <- c(6L, 8L, 10L)
inv_break2_seq  <- c(12L, 16L, 20L)
inv_hi_post_seq <- c(64L, 68L, 72L)

shock1_dur_seq <- c(0L, 1L, 2L)
shock2_dur_seq <- c(0L, 1L, 2L)

out_dir <- "tmp/h20_fast3_pairs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf(
  "H20-fast-3 grid: break1=%d × break2=%d × R1series=%d × w1=(%d,%d,%d) × inv=(%d,%d,%d,%d) × shock=(%d,%d)\n",
  length(break1_seq), length(break2_seq), length(r1_series_seq),
  length(w1_r1_seq), length(w1_r2_seq), length(w1_r3_seq),
  length(inv_hi_pre_seq), length(inv_break1_seq), length(inv_break2_seq), length(inv_hi_post_seq),
  length(shock1_dur_seq), length(shock2_dur_seq)
))

break_pairs <- expand_grid(b1 = break1_seq, b2 = break2_seq) |>
  filter(b2 > b1)

search_break_pair_fast3 <- function(b1, b2) {
  pair_file <- file.path(
    out_dir,
    sprintf("b1_%s__b2_%s.rds", format(b1, "%Y-%m-%d"), format(b2, "%Y-%m-%d"))
  )
  if (file.exists(pair_file)) return(readRDS(pair_file))

  idx_r1 <- which(as.Date(heco$date) < b1)
  idx_r2 <- which(as.Date(heco$date) >= b1 & as.Date(heco$date) < b2)
  idx_r3 <- which(as.Date(heco$date) >= b2)

  if (length(idx_r1) < 6L || length(idx_r2) < 6L || length(idx_r3) < 6L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "too_few_months")
    saveRDS(out, pair_file)
    return(out)
  }

  r1_months <- as.Date(heco$date[idx_r1])
  r2_months <- as.Date(heco$date[idx_r2])
  r3_months <- as.Date(heco$date[idx_r3])

  y_r1 <- y_all[idx_r1]
  y_r2 <- y_all[idx_r2]
  y_r3 <- y_all[idx_r3]

  # Viability precheck with center candidate
  center_pred <- build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, 33L, 38L, r1_months, 24L)$pred
  if (sum(!is.na(center_pred)) < 6L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "r1_not_viable")
    saveRDS(out, pair_file)
    return(out)
  }

  w2_vals_r1 <- safe_int_seq(min(c(inv_break1_seq, inv_hi_pre_seq)), max(c(inv_break1_seq, inv_hi_pre_seq)))
  w2_vals_r2 <- safe_int_seq(min(c(inv_break1_seq, inv_break2_seq)), max(c(inv_break1_seq, inv_break2_seq)))
  w2_vals_r3 <- safe_int_seq(min(c(inv_break2_seq, inv_hi_post_seq)), max(c(inv_break2_seq, inv_hi_post_seq)))

  if (length(w2_vals_r1) == 0L || length(w2_vals_r2) == 0L || length(w2_vals_r3) == 0L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "empty_w2_range")
    saveRDS(out, pair_file)
    return(out)
  }

  cache_r1 <- list()
  for (.s in r1_series_seq) {
    for (.w1 in w1_r1_seq) {
      for (.w2 in w2_vals_r1) {
        cache_r1[[paste(.s, .w1, .w2, sep = "_")]] <-
          build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r1_months, 24L)$pred
      }
    }
  }

  cache_r2 <- list()
  for (.w1 in w1_r2_seq) {
    for (.w2 in w2_vals_r2) {
      cache_r2[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r2_months, 24L)$pred
    }
  }

  cache_r3 <- list()
  for (.w1 in w1_r3_seq) {
    for (.w2 in w2_vals_r3) {
      cache_r3[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r3_months, anchor_day_h14)$pred
    }
  }

  best_local <- NULL
  best_r2 <- -Inf
  n_valid <- 0L
  rows <- vector("list", 0L)

  for (r1_series in r1_series_seq) {
    for (w1_r1 in w1_r1_seq) {
      for (w1_r2 in w1_r2_seq) {
        for (w1_r3 in w1_r3_seq) {
          for (inv_hi_pre in inv_hi_pre_seq) {
            for (inv_break1 in inv_break1_seq) {
              if (inv_break1 > inv_hi_pre) next

              for (inv_break2 in inv_break2_seq) {
                # allow R2 inventory to recover relative to break1
                for (inv_hi_post in inv_hi_post_seq) {
                  if (inv_hi_post < inv_break2) next

                  path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
                  path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
                  path_r3 <- build_linear_w2_path(length(r3_months), inv_break2, inv_hi_post)

                  pred_r1 <- numeric(length(r1_months))
                  pred_r2 <- numeric(length(r2_months))
                  pred_r3 <- numeric(length(r3_months))

                  for (j in seq_along(r1_months)) {
                    key <- paste(r1_series, w1_r1, path_r1[j], sep = "_")
                    pred_r1[j] <- cache_r1[[key]][j]
                  }
                  for (j in seq_along(r2_months)) {
                    key <- paste(w1_r2, path_r2[j], sep = "_")
                    pred_r2[j] <- cache_r2[[key]][j]
                  }
                  for (j in seq_along(r3_months)) {
                    key <- paste(w1_r3, path_r3[j], sep = "_")
                    pred_r3[j] <- cache_r3[[key]][j]
                  }

                  df <- bind_rows(
                    tibble(date = r1_months, lsfo_bbl = y_r1, pred = pred_r1, regime = "R1"),
                    tibble(date = r2_months, lsfo_bbl = y_r2, pred = pred_r2, regime = "R2"),
                    tibble(date = r3_months, lsfo_bbl = y_r3, pred = pred_r3, regime = "R3")
                  ) |>
                    mutate(regime = factor(regime, levels = c("R1", "R2", "R3"))) |>
                    filter(!is.na(pred), !is.na(lsfo_bbl))

                  if (nrow(df) < 18L) next
                  if (n_distinct(df$regime) < 3L) next
                  n_by_regime <- count(df, regime)
                  if (any(n_by_regime$n < 6L)) next

                  for (shock1_dur in shock1_dur_seq) {
                    shock1 <- build_shock_pulse(df$date, b1, shock1_dur)

                    for (shock2_dur in shock2_dur_seq) {
                      shock2 <- build_shock_pulse(df$date, b2, shock2_dur)

                      fit <- lm(
                        lsfo_bbl ~ pred * regime + shock1 + shock2,
                        data = mutate(df, shock1 = shock1, shock2 = shock2)
                      )

                      cf <- coef(fit)
                      if (!all(is.finite(cf))) next

                      ssr <- sum(residuals(fit)^2)
                      r2  <- 1 - ssr / sst_h13
                      if (!is.finite(r2)) next

                      n_valid <- n_valid + 1L
                      rmse <- sqrt(ssr / nrow(df))

                      row <- tibble(
                        break1 = b1,
                        break2 = b2,
                        r1_series = r1_series,
                        w1_r1 = w1_r1,
                        w1_r2 = w1_r2,
                        w1_r3 = w1_r3,
                        inv_hi_pre = inv_hi_pre,
                        inv_break1 = inv_break1,
                        inv_break2 = inv_break2,
                        inv_hi_post = inv_hi_post,
                        shock1_dur = shock1_dur,
                        shock2_dur = shock2_dur,
                        r2 = r2,
                        rmse = rmse,
                        n = nrow(df)
                      )
                      rows[[length(rows) + 1L]] <- row

                      if (r2 > best_r2) {
                        best_r2 <- r2
                        best_local <- list(
                          summary = row,
                          fit = fit,
                          df = mutate(df, shock1 = shock1, shock2 = shock2,
                                      fitted = fitted(fit), residual = residuals(fit))
                        )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  top_tbl <- if (length(rows)) bind_rows(rows) |> arrange(desc(r2)) |> slice_head(n = 20) else tibble()

  out <- list(
    best = best_local,
    top = top_tbl,
    n_valid = n_valid,
    break1 = b1,
    break2 = b2,
    skipped = NULL
  )
  saveRDS(out, pair_file)
  out
}

# ---- parallel run ----

n_cores <- max(1L, detectCores(logical = FALSE) - 2L)
cat("Using cores:", n_cores, "\n")

pair_results <- mclapply(
  seq_len(nrow(break_pairs)),
  function(i) {
    b1 <- break_pairs$b1[i]
    b2 <- break_pairs$b2[i]
    tryCatch(
      search_break_pair_fast3(b1, b2),
      error = function(e) {
        list(
          best = NULL,
          top = tibble(),
          n_valid = 0L,
          break1 = b1,
          break2 = b2,
          skipped = paste("worker_error:", conditionMessage(e))
        )
      }
    )
  },
  mc.cores = n_cores
)

# ---- summarize ----

failed_pairs <- bind_rows(lapply(pair_results, function(x) {
  tibble(
    break1 = as.Date(x$break1),
    break2 = as.Date(x$break2),
    n_valid = x$n_valid %||% 0L,
    skipped = x$skipped %||% NA_character_
  )
}))

print(failed_pairs)

valid_total <- sum(vapply(pair_results, function(x) x$n_valid %||% 0L, integer(1)))
cat("Total valid candidates:", valid_total, "\n")

all_top <- bind_rows(lapply(pair_results, function(x) {
  if (!is.null(x$top)) x$top else tibble()
})) |> arrange(desc(r2))

if (nrow(all_top) == 0L) {
  stop("No valid candidates found in H20-fast-3. Inspect `failed_pairs`.")
}

print(all_top |> slice_head(n = 25))

best_candidates <- Filter(Negate(is.null), lapply(pair_results, `[[`, "best"))
if (length(best_candidates) == 0L) {
  stop("No best candidates found in H20-fast-3. Inspect `failed_pairs`.")
}

best_h20_fast3 <- best_candidates[[which.max(vapply(best_candidates, function(x) x$summary$r2, numeric(1)))]]

cat("\n── H20-fast-3 best ─────────────────────────────────────────────────\n")
print(best_h20_fast3$summary)

p_h20_fast3_shocks <- best_h20_fast3$df |>
  select(date, shock1, shock2) |>
  pivot_longer(-date, names_to = "shock", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = shock)) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = c(best_h20_fast3$summary$break1, best_h20_fast3$summary$break2),
    linetype = "dashed", color = "grey40"
  ) +
  theme_minimal() +
  labs(title = "H20-fast-3 shock pulses", x = NULL, y = "Shock weight", color = NULL)
print(p_h20_fast3_shocks)

cat("\n── H20-fast-3 boundary checks ──────────────────────────────────────\n")
flag_boundary("break1", best_h20_fast3$summary$break1, break1_seq)
flag_boundary("break2", best_h20_fast3$summary$break2, break2_seq)
flag_boundary("w1_r1", best_h20_fast3$summary$w1_r1, w1_r1_seq)
flag_boundary("w1_r2", best_h20_fast3$summary$w1_r2, w1_r2_seq)
flag_boundary("w1_r3", best_h20_fast3$summary$w1_r3, w1_r3_seq)
flag_boundary("inv_hi_pre", best_h20_fast3$summary$inv_hi_pre, inv_hi_pre_seq)
flag_boundary("inv_break1", best_h20_fast3$summary$inv_break1, inv_break1_seq)
flag_boundary("inv_break2", best_h20_fast3$summary$inv_break2, inv_break2_seq)
flag_boundary("inv_hi_post", best_h20_fast3$summary$inv_hi_post, inv_hi_post_seq)
flag_boundary("shock1_dur", best_h20_fast3$summary$shock1_dur, shock1_dur_seq)
flag_boundary("shock2_dur", best_h20_fast3$summary$shock2_dur, shock2_dur_seq)

# ── H20-fast-4: local refinement around H20-fast-3 winner ---------------------
#
# Centered on:
#   break1 = 2024-03-01
#   break2 = 2024-11-01
#   R1 index = glob
#   w1 = (35, 39, 16)
#   inventory = 42 -> 10 -> 12 -> 72
#   shock durations = 2, 2
#
# Uses:
#   heco, vlsfo, y_all, sst_h13, anchor_day_h14

library(tidyverse)
library(lubridate)
library(slider)
library(parallel)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- helpers ----

build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end     <- findInterval(as.integer(anchor),         daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

build_shock_pulse <- function(dates, break_date, duration_months) {
  if (duration_months <= 0L) return(rep(0, length(dates)))
  map_dbl(as.Date(dates), function(d) {
    if (d < break_date) return(0)
    m <- interval(break_date, d) %/% months(1)
    max(1 - m / duration_months, 0)
  })
}

safe_int_seq <- function(lo, hi) {
  lo <- suppressWarnings(as.integer(lo))
  hi <- suppressWarnings(as.integer(hi))
  if (length(lo) != 1L || length(hi) != 1L || is.na(lo) || is.na(hi) || lo > hi) integer(0) else seq(lo, hi, by = 1L)
}

flag_boundary <- function(label, value, seq_vals) {
  if (length(value) != 1L || is.na(value)) {
    cat(sprintf("Boundary check skipped: %s has invalid value\n", label))
    return(invisible(FALSE))
  }
  seq_vals <- seq_vals[!is.na(seq_vals)]
  if (length(seq_vals) == 0L) {
    cat(sprintf("Boundary check skipped: %s has empty search range\n", label))
    return(invisible(FALSE))
  }

  if (inherits(value, "Date") || inherits(seq_vals, "Date")) {
    value_cmp <- as.Date(value)
    seq_cmp <- as.Date(seq_vals)
  } else {
    value_cmp <- as.numeric(value)
    seq_cmp <- as.numeric(seq_vals)
  }

  hit <- value_cmp <= min(seq_cmp) || value_cmp >= max(seq_cmp)
  if (isTRUE(hit)) {
    cat(sprintf(
      "Boundary flag: %s = %s is at edge [%s, %s]\n",
      label, as.character(value), as.character(min(seq_cmp)), as.character(max(seq_cmp))
    ))
  }
  invisible(hit)
}

# ---- local refinement grid ----

r1_series_seq <- c("glob", "avg_sing_hou", "sing")

break1_seq <- as.Date(c("2024-02-01", "2024-03-01", "2024-04-01"))
break2_seq <- as.Date(c("2024-10-01", "2024-11-01", "2024-12-01"))

w1_r1_seq <- c(35L, 37L, 39L)
w1_r2_seq <- c(37L, 39L, 41L)
w1_r3_seq <- c(14L, 16L, 18L)

inv_hi_pre_seq  <- c(42L, 46L, 50L)
inv_break1_seq  <- c(10L, 12L, 14L)
inv_break2_seq  <- c(8L, 10L, 12L, 14L)
inv_hi_post_seq <- c(72L, 76L, 80L)

shock1_dur_seq <- c(1L, 2L, 3L, 4L)
shock2_dur_seq <- c(1L, 2L, 3L, 4L)

out_dir <- "tmp/h20_fast4_pairs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf(
  "H20-fast-4 grid: break1=%d × break2=%d × R1series=%d × w1=(%d,%d,%d) × inv=(%d,%d,%d,%d) × shock=(%d,%d)\n",
  length(break1_seq), length(break2_seq), length(r1_series_seq),
  length(w1_r1_seq), length(w1_r2_seq), length(w1_r3_seq),
  length(inv_hi_pre_seq), length(inv_break1_seq), length(inv_break2_seq), length(inv_hi_post_seq),
  length(shock1_dur_seq), length(shock2_dur_seq)
))

break_pairs <- expand_grid(b1 = break1_seq, b2 = break2_seq) |>
  filter(b2 > b1)

search_break_pair_fast4 <- function(b1, b2) {
  pair_file <- file.path(
    out_dir,
    sprintf("b1_%s__b2_%s.rds", format(b1, "%Y-%m-%d"), format(b2, "%Y-%m-%d"))
  )
  if (file.exists(pair_file)) return(readRDS(pair_file))

  idx_r1 <- which(as.Date(heco$date) < b1)
  idx_r2 <- which(as.Date(heco$date) >= b1 & as.Date(heco$date) < b2)
  idx_r3 <- which(as.Date(heco$date) >= b2)

  if (length(idx_r1) < 6L || length(idx_r2) < 6L || length(idx_r3) < 6L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "too_few_months")
    saveRDS(out, pair_file)
    return(out)
  }

  r1_months <- as.Date(heco$date[idx_r1])
  r2_months <- as.Date(heco$date[idx_r2])
  r3_months <- as.Date(heco$date[idx_r3])

  y_r1 <- y_all[idx_r1]
  y_r2 <- y_all[idx_r2]
  y_r3 <- y_all[idx_r3]

  # Viability precheck with center candidate
  center_pred <- build_predictor_anchor(vlsfo$glob, vlsfo$date, 35L, 42L, r1_months, 24L)$pred
  if (sum(!is.na(center_pred)) < 6L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "r1_not_viable")
    saveRDS(out, pair_file)
    return(out)
  }

  w2_vals_r1 <- safe_int_seq(min(c(inv_break1_seq, inv_hi_pre_seq)), max(c(inv_break1_seq, inv_hi_pre_seq)))
  w2_vals_r2 <- safe_int_seq(min(c(inv_break1_seq, inv_break2_seq)), max(c(inv_break1_seq, inv_break2_seq)))
  w2_vals_r3 <- safe_int_seq(min(c(inv_break2_seq, inv_hi_post_seq)), max(c(inv_break2_seq, inv_hi_post_seq)))

  if (length(w2_vals_r1) == 0L || length(w2_vals_r2) == 0L || length(w2_vals_r3) == 0L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "empty_w2_range")
    saveRDS(out, pair_file)
    return(out)
  }

  cache_r1 <- list()
  for (.s in r1_series_seq) {
    for (.w1 in w1_r1_seq) {
      for (.w2 in w2_vals_r1) {
        cache_r1[[paste(.s, .w1, .w2, sep = "_")]] <-
          build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r1_months, 24L)$pred
      }
    }
  }

  cache_r2 <- list()
  for (.w1 in w1_r2_seq) {
    for (.w2 in w2_vals_r2) {
      cache_r2[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r2_months, 24L)$pred
    }
  }

  cache_r3 <- list()
  for (.w1 in w1_r3_seq) {
    for (.w2 in w2_vals_r3) {
      cache_r3[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r3_months, anchor_day_h14)$pred
    }
  }

  best_local <- NULL
  best_r2 <- -Inf
  n_valid <- 0L
  rows <- vector("list", 0L)

  for (r1_series in r1_series_seq) {
    for (w1_r1 in w1_r1_seq) {
      for (w1_r2 in w1_r2_seq) {
        for (w1_r3 in w1_r3_seq) {
          for (inv_hi_pre in inv_hi_pre_seq) {
            for (inv_break1 in inv_break1_seq) {
              if (inv_break1 > inv_hi_pre) next

              for (inv_break2 in inv_break2_seq) {
                for (inv_hi_post in inv_hi_post_seq) {
                  if (inv_hi_post < inv_break2) next

                  path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
                  path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
                  path_r3 <- build_linear_w2_path(length(r3_months), inv_break2, inv_hi_post)

                  pred_r1 <- numeric(length(r1_months))
                  pred_r2 <- numeric(length(r2_months))
                  pred_r3 <- numeric(length(r3_months))

                  for (j in seq_along(r1_months)) {
                    key <- paste(r1_series, w1_r1, path_r1[j], sep = "_")
                    pred_r1[j] <- cache_r1[[key]][j]
                  }
                  for (j in seq_along(r2_months)) {
                    key <- paste(w1_r2, path_r2[j], sep = "_")
                    pred_r2[j] <- cache_r2[[key]][j]
                  }
                  for (j in seq_along(r3_months)) {
                    key <- paste(w1_r3, path_r3[j], sep = "_")
                    pred_r3[j] <- cache_r3[[key]][j]
                  }

                  df <- bind_rows(
                    tibble(date = r1_months, lsfo_bbl = y_r1, pred = pred_r1, regime = "R1"),
                    tibble(date = r2_months, lsfo_bbl = y_r2, pred = pred_r2, regime = "R2"),
                    tibble(date = r3_months, lsfo_bbl = y_r3, pred = pred_r3, regime = "R3")
                  ) |>
                    mutate(regime = factor(regime, levels = c("R1", "R2", "R3"))) |>
                    filter(!is.na(pred), !is.na(lsfo_bbl))

                  if (nrow(df) < 18L) next
                  if (n_distinct(df$regime) < 3L) next
                  n_by_regime <- count(df, regime)
                  if (any(n_by_regime$n < 6L)) next

                  for (shock1_dur in shock1_dur_seq) {
                    shock1 <- build_shock_pulse(df$date, b1, shock1_dur)

                    for (shock2_dur in shock2_dur_seq) {
                      shock2 <- build_shock_pulse(df$date, b2, shock2_dur)

                      fit <- lm(
                        lsfo_bbl ~ pred * regime + shock1 + shock2,
                        data = mutate(df, shock1 = shock1, shock2 = shock2)
                      )

                      cf <- coef(fit)
                      if (!all(is.finite(cf))) next

                      ssr <- sum(residuals(fit)^2)
                      r2  <- 1 - ssr / sst_h13
                      if (!is.finite(r2)) next

                      n_valid <- n_valid + 1L
                      rmse <- sqrt(ssr / nrow(df))

                      row <- tibble(
                        break1 = b1,
                        break2 = b2,
                        r1_series = r1_series,
                        w1_r1 = w1_r1,
                        w1_r2 = w1_r2,
                        w1_r3 = w1_r3,
                        inv_hi_pre = inv_hi_pre,
                        inv_break1 = inv_break1,
                        inv_break2 = inv_break2,
                        inv_hi_post = inv_hi_post,
                        shock1_dur = shock1_dur,
                        shock2_dur = shock2_dur,
                        r2 = r2,
                        rmse = rmse,
                        n = nrow(df)
                      )
                      rows[[length(rows) + 1L]] <- row

                      if (r2 > best_r2) {
                        best_r2 <- r2
                        best_local <- list(
                          summary = row,
                          fit = fit,
                          df = mutate(df, shock1 = shock1, shock2 = shock2,
                                      fitted = fitted(fit), residual = residuals(fit))
                        )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  top_tbl <- if (length(rows)) bind_rows(rows) |> arrange(desc(r2)) |> slice_head(n = 20) else tibble()

  out <- list(
    best = best_local,
    top = top_tbl,
    n_valid = n_valid,
    break1 = b1,
    break2 = b2,
    skipped = NULL
  )
  saveRDS(out, pair_file)
  out
}

# ---- parallel run ----

n_cores <- max(1L, detectCores(logical = FALSE) - 2L)
cat("Using cores:", n_cores, "\n")

pair_results <- mclapply(
  seq_len(nrow(break_pairs)),
  function(i) {
    b1 <- break_pairs$b1[i]
    b2 <- break_pairs$b2[i]
    tryCatch(
      search_break_pair_fast4(b1, b2),
      error = function(e) {
        list(
          best = NULL,
          top = tibble(),
          n_valid = 0L,
          break1 = b1,
          break2 = b2,
          skipped = paste("worker_error:", conditionMessage(e))
        )
      }
    )
  },
  mc.cores = n_cores
)

# ---- summarize ----

failed_pairs <- bind_rows(lapply(pair_results, function(x) {
  tibble(
    break1 = as.Date(x$break1),
    break2 = as.Date(x$break2),
    n_valid = x$n_valid %||% 0L,
    skipped = x$skipped %||% NA_character_
  )
}))

print(failed_pairs)

valid_total <- sum(vapply(pair_results, function(x) x$n_valid %||% 0L, integer(1)))
cat("Total valid candidates:", valid_total, "\n")

all_top <- bind_rows(lapply(pair_results, function(x) {
  if (!is.null(x$top)) x$top else tibble()
})) |> arrange(desc(r2))

if (nrow(all_top) == 0L) {
  stop("No valid candidates found in H20-fast-4. Inspect `failed_pairs`.")
}

print(all_top |> slice_head(n = 25))

best_candidates <- Filter(Negate(is.null), lapply(pair_results, `[[`, "best"))
if (length(best_candidates) == 0L) {
  stop("No best candidates found in H20-fast-4. Inspect `failed_pairs`.")
}

best_h20_fast4 <- best_candidates[[which.max(vapply(best_candidates, function(x) x$summary$r2, numeric(1)))]]

cat("\n── H20-fast-4 best ─────────────────────────────────────────────────\n")
print(best_h20_fast4$summary)

p_h20_fast4_shocks <- best_h20_fast4$df |>
  select(date, shock1, shock2) |>
  pivot_longer(-date, names_to = "shock", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = shock)) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = c(best_h20_fast4$summary$break1, best_h20_fast4$summary$break2),
    linetype = "dashed", color = "grey40"
  ) +
  theme_minimal() +
  labs(title = "H20-fast-4 shock pulses", x = NULL, y = "Shock weight", color = NULL)
print(p_h20_fast4_shocks)

cat("\n── H20-fast-4 boundary checks ──────────────────────────────────────\n")
flag_boundary("break1", best_h20_fast4$summary$break1, break1_seq)
flag_boundary("break2", best_h20_fast4$summary$break2, break2_seq)
flag_boundary("w1_r1", best_h20_fast4$summary$w1_r1, w1_r1_seq)
flag_boundary("w1_r2", best_h20_fast4$summary$w1_r2, w1_r2_seq)
flag_boundary("w1_r3", best_h20_fast4$summary$w1_r3, w1_r3_seq)
flag_boundary("inv_hi_pre", best_h20_fast4$summary$inv_hi_pre, inv_hi_pre_seq)
flag_boundary("inv_break1", best_h20_fast4$summary$inv_break1, inv_break1_seq)
flag_boundary("inv_break2", best_h20_fast4$summary$inv_break2, inv_break2_seq)
flag_boundary("inv_hi_post", best_h20_fast4$summary$inv_hi_post, inv_hi_post_seq)
flag_boundary("shock1_dur", best_h20_fast4$summary$shock1_dur, shock1_dur_seq)
flag_boundary("shock2_dur", best_h20_fast4$summary$shock2_dur, shock2_dur_seq)

# ── H20-fast-5: local refinement with widened R3 w1 ---------------------------
#
# Purpose:
#   Refine around H20-fast-4 winner while explicitly testing whether
#   R3 w1 near a calendar month can compete with the short-window solution.
#
# Uses:
#   heco, vlsfo, y_all, sst_h13, anchor_day_h14

library(tidyverse)
library(lubridate)
library(slider)
library(parallel)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- helpers ----

build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end     <- findInterval(as.integer(anchor),         daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

build_shock_pulse <- function(dates, break_date, duration_months) {
  if (duration_months <= 0L) return(rep(0, length(dates)))
  map_dbl(as.Date(dates), function(d) {
    if (d < break_date) return(0)
    m <- interval(break_date, d) %/% months(1)
    max(1 - m / duration_months, 0)
  })
}

safe_int_seq <- function(lo, hi) {
  lo <- suppressWarnings(as.integer(lo))
  hi <- suppressWarnings(as.integer(hi))
  if (length(lo) != 1L || length(hi) != 1L || is.na(lo) || is.na(hi) || lo > hi) integer(0) else seq(lo, hi, by = 1L)
}

flag_boundary <- function(label, value, seq_vals) {
  if (length(value) != 1L || is.na(value)) {
    cat(sprintf("Boundary check skipped: %s has invalid value\n", label))
    return(invisible(FALSE))
  }
  seq_vals <- seq_vals[!is.na(seq_vals)]
  if (length(seq_vals) == 0L) {
    cat(sprintf("Boundary check skipped: %s has empty search range\n", label))
    return(invisible(FALSE))
  }

  if (inherits(value, "Date") || inherits(seq_vals, "Date")) {
    value_cmp <- as.Date(value)
    seq_cmp <- as.Date(seq_vals)
  } else {
    value_cmp <- as.numeric(value)
    seq_cmp <- as.numeric(seq_vals)
  }

  hit <- value_cmp <= min(seq_cmp) || value_cmp >= max(seq_cmp)
  if (isTRUE(hit)) {
    cat(sprintf(
      "Boundary flag: %s = %s is at edge [%s, %s]\n",
      label, as.character(value), as.character(min(seq_cmp)), as.character(max(seq_cmp))
    ))
  }
  invisible(hit)
}

# ---- refinement grid ----

r1_series_seq <- c("avg_sing_hou", "glob")

break1_seq <- as.Date(c("2024-02-01", "2024-03-01", "2024-04-01"))
break2_seq <- as.Date(c("2024-10-01", "2024-11-01", "2024-12-01"))

w1_r1_seq <- c(37L, 39L, 41L)
w1_r2_seq <- c(37L, 39L, 41L)

# Explicitly test short vs ~monthly R3 windows
w1_r3_seq <- c(14L, 16L, 18L, 21L, 24L, 27L, 30L, 33L)

inv_hi_pre_seq  <- c(46L, 50L, 54L)
inv_break1_seq  <- c(12L, 14L, 16L)
inv_break2_seq  <- c(6L, 8L, 10L, 12L)
inv_hi_post_seq <- c(76L, 80L, 84L)

shock1_dur_seq <- c(1L, 2L, 3L)
shock2_dur_seq <- c(1L, 2L, 3L)

out_dir <- "tmp/h20_fast5_pairs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf(
  "H20-fast-5 grid: break1=%d × break2=%d × R1series=%d × w1=(%d,%d,%d) × inv=(%d,%d,%d,%d) × shock=(%d,%d)\n",
  length(break1_seq), length(break2_seq), length(r1_series_seq),
  length(w1_r1_seq), length(w1_r2_seq), length(w1_r3_seq),
  length(inv_hi_pre_seq), length(inv_break1_seq), length(inv_break2_seq), length(inv_hi_post_seq),
  length(shock1_dur_seq), length(shock2_dur_seq)
))

break_pairs <- expand_grid(b1 = break1_seq, b2 = break2_seq) |>
  filter(b2 > b1)

search_break_pair_fast5 <- function(b1, b2) {
  pair_file <- file.path(
    out_dir,
    sprintf("b1_%s__b2_%s.rds", format(b1, "%Y-%m-%d"), format(b2, "%Y-%m-%d"))
  )
  if (file.exists(pair_file)) return(readRDS(pair_file))

  idx_r1 <- which(as.Date(heco$date) < b1)
  idx_r2 <- which(as.Date(heco$date) >= b1 & as.Date(heco$date) < b2)
  idx_r3 <- which(as.Date(heco$date) >= b2)

  if (length(idx_r1) < 6L || length(idx_r2) < 6L || length(idx_r3) < 6L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "too_few_months")
    saveRDS(out, pair_file)
    return(out)
  }

  r1_months <- as.Date(heco$date[idx_r1])
  r2_months <- as.Date(heco$date[idx_r2])
  r3_months <- as.Date(heco$date[idx_r3])

  y_r1 <- y_all[idx_r1]
  y_r2 <- y_all[idx_r2]
  y_r3 <- y_all[idx_r3]

  center_pred <- build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, 39L, 50L, r1_months, 24L)$pred
  if (sum(!is.na(center_pred)) < 6L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "r1_not_viable")
    saveRDS(out, pair_file)
    return(out)
  }

  w2_vals_r1 <- safe_int_seq(min(c(inv_break1_seq, inv_hi_pre_seq)), max(c(inv_break1_seq, inv_hi_pre_seq)))
  w2_vals_r2 <- safe_int_seq(min(c(inv_break1_seq, inv_break2_seq)), max(c(inv_break1_seq, inv_break2_seq)))
  w2_vals_r3 <- safe_int_seq(min(c(inv_break2_seq, inv_hi_post_seq)), max(c(inv_break2_seq, inv_hi_post_seq)))

  if (length(w2_vals_r1) == 0L || length(w2_vals_r2) == 0L || length(w2_vals_r3) == 0L) {
    out <- list(best = NULL, top = tibble(), n_valid = 0L, break1 = b1, break2 = b2, skipped = "empty_w2_range")
    saveRDS(out, pair_file)
    return(out)
  }

  cache_r1 <- list()
  for (.s in r1_series_seq) {
    for (.w1 in w1_r1_seq) {
      for (.w2 in w2_vals_r1) {
        cache_r1[[paste(.s, .w1, .w2, sep = "_")]] <-
          build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r1_months, 24L)$pred
      }
    }
  }

  cache_r2 <- list()
  for (.w1 in w1_r2_seq) {
    for (.w2 in w2_vals_r2) {
      cache_r2[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r2_months, 24L)$pred
    }
  }

  cache_r3 <- list()
  for (.w1 in w1_r3_seq) {
    for (.w2 in w2_vals_r3) {
      cache_r3[[paste(.w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r3_months, anchor_day_h14)$pred
    }
  }

  best_local <- NULL
  best_r2 <- -Inf
  n_valid <- 0L
  rows <- vector("list", 0L)

  for (r1_series in r1_series_seq) {
    for (w1_r1 in w1_r1_seq) {
      for (w1_r2 in w1_r2_seq) {
        for (w1_r3 in w1_r3_seq) {
          for (inv_hi_pre in inv_hi_pre_seq) {
            for (inv_break1 in inv_break1_seq) {
              if (inv_break1 > inv_hi_pre) next

              for (inv_break2 in inv_break2_seq) {
                for (inv_hi_post in inv_hi_post_seq) {
                  if (inv_hi_post < inv_break2) next

                  path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
                  path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
                  path_r3 <- build_linear_w2_path(length(r3_months), inv_break2, inv_hi_post)

                  pred_r1 <- numeric(length(r1_months))
                  pred_r2 <- numeric(length(r2_months))
                  pred_r3 <- numeric(length(r3_months))

                  for (j in seq_along(r1_months)) {
                    key <- paste(r1_series, w1_r1, path_r1[j], sep = "_")
                    pred_r1[j] <- cache_r1[[key]][j]
                  }
                  for (j in seq_along(r2_months)) {
                    key <- paste(w1_r2, path_r2[j], sep = "_")
                    pred_r2[j] <- cache_r2[[key]][j]
                  }
                  for (j in seq_along(r3_months)) {
                    key <- paste(w1_r3, path_r3[j], sep = "_")
                    pred_r3[j] <- cache_r3[[key]][j]
                  }

                  df <- bind_rows(
                    tibble(date = r1_months, lsfo_bbl = y_r1, pred = pred_r1, regime = "R1"),
                    tibble(date = r2_months, lsfo_bbl = y_r2, pred = pred_r2, regime = "R2"),
                    tibble(date = r3_months, lsfo_bbl = y_r3, pred = pred_r3, regime = "R3")
                  ) |>
                    mutate(regime = factor(regime, levels = c("R1", "R2", "R3"))) |>
                    filter(!is.na(pred), !is.na(lsfo_bbl))

                  if (nrow(df) < 18L) next
                  if (n_distinct(df$regime) < 3L) next
                  n_by_regime <- count(df, regime)
                  if (any(n_by_regime$n < 6L)) next

                  for (shock1_dur in shock1_dur_seq) {
                    shock1 <- build_shock_pulse(df$date, b1, shock1_dur)

                    for (shock2_dur in shock2_dur_seq) {
                      shock2 <- build_shock_pulse(df$date, b2, shock2_dur)

                      fit <- lm(
                        lsfo_bbl ~ pred * regime + shock1 + shock2,
                        data = mutate(df, shock1 = shock1, shock2 = shock2)
                      )

                      cf <- coef(fit)
                      if (!all(is.finite(cf))) next

                      ssr <- sum(residuals(fit)^2)
                      r2  <- 1 - ssr / sst_h13
                      if (!is.finite(r2)) next

                      n_valid <- n_valid + 1L
                      rmse <- sqrt(ssr / nrow(df))

                      row <- tibble(
                        break1 = b1,
                        break2 = b2,
                        r1_series = r1_series,
                        w1_r1 = w1_r1,
                        w1_r2 = w1_r2,
                        w1_r3 = w1_r3,
                        inv_hi_pre = inv_hi_pre,
                        inv_break1 = inv_break1,
                        inv_break2 = inv_break2,
                        inv_hi_post = inv_hi_post,
                        shock1_dur = shock1_dur,
                        shock2_dur = shock2_dur,
                        r2 = r2,
                        rmse = rmse,
                        n = nrow(df)
                      )
                      rows[[length(rows) + 1L]] <- row

                      if (r2 > best_r2) {
                        best_r2 <- r2
                        best_local <- list(
                          summary = row,
                          fit = fit,
                          df = mutate(df, shock1 = shock1, shock2 = shock2,
                                      fitted = fitted(fit), residual = residuals(fit))
                        )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  top_tbl <- if (length(rows)) bind_rows(rows) |> arrange(desc(r2)) |> slice_head(n = 20) else tibble()

  out <- list(
    best = best_local,
    top = top_tbl,
    n_valid = n_valid,
    break1 = b1,
    break2 = b2,
    skipped = NULL
  )
  saveRDS(out, pair_file)
  out
}

# ---- parallel run ----

n_cores <- max(1L, detectCores(logical = FALSE) - 2L)
cat("Using cores:", n_cores, "\n")

pair_results <- mclapply(
  seq_len(nrow(break_pairs)),
  function(i) {
    b1 <- break_pairs$b1[i]
    b2 <- break_pairs$b2[i]
    tryCatch(
      search_break_pair_fast5(b1, b2),
      error = function(e) {
        list(
          best = NULL,
          top = tibble(),
          n_valid = 0L,
          break1 = b1,
          break2 = b2,
          skipped = paste("worker_error:", conditionMessage(e))
        )
      }
    )
  },
  mc.cores = n_cores
)

# ---- summarize ----

failed_pairs <- bind_rows(lapply(pair_results, function(x) {
  tibble(
    break1 = as.Date(x$break1),
    break2 = as.Date(x$break2),
    n_valid = x$n_valid %||% 0L,
    skipped = x$skipped %||% NA_character_
  )
}))

print(failed_pairs)

valid_total <- sum(vapply(pair_results, function(x) x$n_valid %||% 0L, integer(1)))
cat("Total valid candidates:", valid_total, "\n")

all_top <- bind_rows(lapply(pair_results, function(x) {
  if (!is.null(x$top)) x$top else tibble()
})) |> arrange(desc(r2))

if (nrow(all_top) == 0L) {
  stop("No valid candidates found in H20-fast-5. Inspect `failed_pairs`.")
}

print(all_top |> slice_head(n = 25))

best_candidates <- Filter(Negate(is.null), lapply(pair_results, `[[`, "best"))
if (length(best_candidates) == 0L) {
  stop("No best candidates found in H20-fast-5. Inspect `failed_pairs`.")
}

best_h20_fast5 <- best_candidates[[which.max(vapply(best_candidates, function(x) x$summary$r2, numeric(1)))]]

cat("\n── H20-fast-5 best ─────────────────────────────────────────────────\n")
print(best_h20_fast5$summary)

p_h20_fast5_shocks <- best_h20_fast5$df |>
  select(date, shock1, shock2) |>
  pivot_longer(-date, names_to = "shock", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = shock)) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = c(best_h20_fast5$summary$break1, best_h20_fast5$summary$break2),
    linetype = "dashed", color = "grey40"
  ) +
  theme_minimal() +
  labs(title = "H20-fast-5 shock pulses", x = NULL, y = "Shock weight", color = NULL)
print(p_h20_fast5_shocks)

cat("\n── H20-fast-5 boundary checks ──────────────────────────────────────\n")
flag_boundary("break1", best_h20_fast5$summary$break1, break1_seq)
flag_boundary("break2", best_h20_fast5$summary$break2, break2_seq)
flag_boundary("w1_r1", best_h20_fast5$summary$w1_r1, w1_r1_seq)
flag_boundary("w1_r2", best_h20_fast5$summary$w1_r2, w1_r2_seq)
flag_boundary("w1_r3", best_h20_fast5$summary$w1_r3, w1_r3_seq)
flag_boundary("inv_hi_pre", best_h20_fast5$summary$inv_hi_pre, inv_hi_pre_seq)
flag_boundary("inv_break1", best_h20_fast5$summary$inv_break1, inv_break1_seq)
flag_boundary("inv_break2", best_h20_fast5$summary$inv_break2, inv_break2_seq)
flag_boundary("inv_hi_post", best_h20_fast5$summary$inv_hi_post, inv_hi_post_seq)
flag_boundary("shock1_dur", best_h20_fast5$summary$shock1_dur, shock1_dur_seq)
flag_boundary("shock2_dur", best_h20_fast5$summary$shock2_dur, shock2_dur_seq)


# ── H20-fast-7: fixed-break edge refinement + summary graphs + flat-price projection
library(tidyverse)
library(lubridate)
library(slider)

`%||%` <- function(x, y) if (is.null(x)) y else x

build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end     <- findInterval(as.integer(anchor), daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

build_shock_pulse <- function(dates, break_date, duration_months) {
  if (duration_months <= 0L) return(rep(0, length(dates)))
  map_dbl(as.Date(dates), function(d) {
    if (d < break_date) return(0)
    m <- interval(break_date, d) %/% months(1)
    max(1 - m / duration_months, 0)
  })
}

flag_boundary <- function(label, value, seq_vals) {
  seq_vals <- seq_vals[!is.na(seq_vals)]
  if (length(seq_vals) == 0L || length(value) != 1L || is.na(value)) return(invisible(FALSE))
  hit <- value <= min(seq_vals) || value >= max(seq_vals)
  if (isTRUE(hit)) {
    cat(sprintf("Boundary flag: %s = %s is at edge [%s, %s]\n",
                label, as.character(value), as.character(min(seq_vals)), as.character(max(seq_vals))))
  }
  invisible(hit)
}

# ---- fixed breaks ----

b1 <- as.Date("2024-03-01")
b2 <- as.Date("2024-11-01")

idx_r1 <- which(as.Date(heco$date) < b1)
idx_r2 <- which(as.Date(heco$date) >= b1 & as.Date(heco$date) < b2)
idx_r3 <- which(as.Date(heco$date) >= b2)

r1_months <- as.Date(heco$date[idx_r1])
r2_months <- as.Date(heco$date[idx_r2])
r3_months <- as.Date(heco$date[idx_r3])

y_r1 <- y_all[idx_r1]
y_r2 <- y_all[idx_r2]
y_r3 <- y_all[idx_r3]

# ---- refinement grid ----

r1_series_seq <- c("avg_sing_hou", "glob")

w1_r1_seq <- c(39L, 41L, 43L)
w1_r2_seq <- c(39L, 41L, 43L)
w1_r3_seq <- c(24L, 27L, 30L)

inv_hi_pre_seq  <- c(58L, 62L, 66L)
inv_break1_seq  <- c(18L, 20L, 22L, 24L)
inv_break2_seq  <- c(0L, 2L, 4L)
inv_hi_post_seq <- c(92L, 96L, 100L)

shock1_dur_seq <- c(1L, 2L, 3L)
shock2_dur_seq <- c(1L, 2L, 3L)

cat(sprintf(
  "H20-fast-7 grid: R1series=%d × w1=(%d,%d,%d) × inv=(%d,%d,%d,%d) × shock=(%d,%d)\n",
  length(r1_series_seq),
  length(w1_r1_seq), length(w1_r2_seq), length(w1_r3_seq),
  length(inv_hi_pre_seq), length(inv_break1_seq), length(inv_break2_seq), length(inv_hi_post_seq),
  length(shock1_dur_seq), length(shock2_dur_seq)
))

# ---- caches ----

w2_vals_r1 <- seq(min(c(inv_break1_seq, inv_hi_pre_seq)), max(c(inv_break1_seq, inv_hi_pre_seq)), by = 1L)
w2_vals_r2 <- seq(min(c(inv_break1_seq, inv_break2_seq)), max(c(inv_break1_seq, inv_break2_seq)), by = 1L)
w2_vals_r3 <- seq(min(c(inv_break2_seq, inv_hi_post_seq)), max(c(inv_break2_seq, inv_hi_post_seq)), by = 1L)

cache_r1 <- list()
for (.s in r1_series_seq) {
  for (.w1 in w1_r1_seq) {
    for (.w2 in w2_vals_r1) {
      cache_r1[[paste(.s, .w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r1_months, 24L)$pred
    }
  }
}

cache_r2 <- list()
for (.w1 in w1_r2_seq) {
  for (.w2 in w2_vals_r2) {
    cache_r2[[paste(.w1, .w2, sep = "_")]] <-
      build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r2_months, 24L)$pred
  }
}

cache_r3 <- list()
for (.w1 in w1_r3_seq) {
  for (.w2 in w2_vals_r3) {
    cache_r3[[paste(.w1, .w2, sep = "_")]] <-
      build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r3_months, anchor_day_h14)$pred
  }
}

# ---- search ----

best_h20_fast7 <- NULL
best_r2_fast7 <- -Inf
rows <- vector("list", 0L)
n_valid_fast7 <- 0L

for (r1_series in r1_series_seq) {
  for (w1_r1 in w1_r1_seq) {
    for (w1_r2 in w1_r2_seq) {
      for (w1_r3 in w1_r3_seq) {
        for (inv_hi_pre in inv_hi_pre_seq) {
          for (inv_break1 in inv_break1_seq) {
            if (inv_break1 > inv_hi_pre) next
            for (inv_break2 in inv_break2_seq) {
              for (inv_hi_post in inv_hi_post_seq) {
                if (inv_hi_post < inv_break2) next

                path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
                path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
                path_r3 <- build_linear_w2_path(length(r3_months), inv_break2, inv_hi_post)

                pred_r1 <- numeric(length(r1_months))
                pred_r2 <- numeric(length(r2_months))
                pred_r3 <- numeric(length(r3_months))

                for (j in seq_along(r1_months)) pred_r1[j] <- cache_r1[[paste(r1_series, w1_r1, path_r1[j], sep = "_")]][j]
                for (j in seq_along(r2_months)) pred_r2[j] <- cache_r2[[paste(w1_r2, path_r2[j], sep = "_")]][j]
                for (j in seq_along(r3_months)) pred_r3[j] <- cache_r3[[paste(w1_r3, path_r3[j], sep = "_")]][j]

                df <- bind_rows(
                  tibble(date = r1_months, lsfo_bbl = y_r1, pred = pred_r1, regime = "R1", inv_days = path_r1),
                  tibble(date = r2_months, lsfo_bbl = y_r2, pred = pred_r2, regime = "R2", inv_days = path_r2),
                  tibble(date = r3_months, lsfo_bbl = y_r3, pred = pred_r3, regime = "R3", inv_days = path_r3)
                ) |>
                  mutate(regime = factor(regime, levels = c("R1", "R2", "R3"))) |>
                  filter(!is.na(pred), !is.na(lsfo_bbl))

                if (nrow(df) < 18L) next
                if (n_distinct(df$regime) < 3L) next
                if (any(count(df, regime)$n < 6L)) next

                for (shock1_dur in shock1_dur_seq) {
                  shock1 <- build_shock_pulse(df$date, b1, shock1_dur)
                  for (shock2_dur in shock2_dur_seq) {
                    shock2 <- build_shock_pulse(df$date, b2, shock2_dur)

                    fit <- lm(lsfo_bbl ~ pred * regime + shock1 + shock2,
                              data = mutate(df, shock1 = shock1, shock2 = shock2))
                    if (!all(is.finite(coef(fit)))) next

                    ssr <- sum(residuals(fit)^2)
                    r2  <- 1 - ssr / sst_h13
                    if (!is.finite(r2)) next

                    rmse <- sqrt(ssr / nrow(df))
                    n_valid_fast7 <- n_valid_fast7 + 1L

                    row <- tibble(
                      break1 = b1, break2 = b2, r1_series = r1_series,
                      w1_r1 = w1_r1, w1_r2 = w1_r2, w1_r3 = w1_r3,
                      inv_hi_pre = inv_hi_pre, inv_break1 = inv_break1,
                      inv_break2 = inv_break2, inv_hi_post = inv_hi_post,
                      shock1_dur = shock1_dur, shock2_dur = shock2_dur,
                      r2 = r2, rmse = rmse, n = nrow(df)
                    )
                    rows[[length(rows) + 1L]] <- row

                    if (r2 > best_r2_fast7) {
                      best_r2_fast7 <- r2
                      best_h20_fast7 <- list(
                        summary = row,
                        fit = fit,
                        df = mutate(df, shock1 = shock1, shock2 = shock2,
                                    fitted = fitted(fit), residual = residuals(fit))
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

cat("Total valid candidates:", n_valid_fast7, "\n")
all_top_fast7 <- bind_rows(rows) |> arrange(desc(r2))
print(all_top_fast7 |> slice_head(n = 25))

cat("\n── H20-fast-7 best ─────────────────────────────────────────────────\n")
print(best_h20_fast7$summary)

cat("\n── H20-fast-7 boundary checks ──────────────────────────────────────\n")
flag_boundary("w1_r1", best_h20_fast7$summary$w1_r1, w1_r1_seq)
flag_boundary("w1_r2", best_h20_fast7$summary$w1_r2, w1_r2_seq)
flag_boundary("w1_r3", best_h20_fast7$summary$w1_r3, w1_r3_seq)
flag_boundary("inv_hi_pre", best_h20_fast7$summary$inv_hi_pre, inv_hi_pre_seq)
flag_boundary("inv_break1", best_h20_fast7$summary$inv_break1, inv_break1_seq)
flag_boundary("inv_break2", best_h20_fast7$summary$inv_break2, inv_break2_seq)
flag_boundary("inv_hi_post", best_h20_fast7$summary$inv_hi_post, inv_hi_post_seq)
flag_boundary("shock1_dur", best_h20_fast7$summary$shock1_dur, shock1_dur_seq)
flag_boundary("shock2_dur", best_h20_fast7$summary$shock2_dur, shock2_dur_seq)


# ── H20-fast-8: fixed-break refinement with nested non-linear R3 rebuild -------

library(tidyverse)
library(lubridate)
library(slider)

build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp        <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily     <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end     <- findInterval(as.integer(anchor), daily_int)
      i_start   <- findInterval(as.integer(win_start) - 1L, daily_int)

      if (i_end <= i_start) NA_real_ else mean(daily$cp[(i_start + 1L):i_end])
    }))
}

build_linear_w2_path <- function(n_months, w2_start, w2_end) {
  if (n_months <= 1L) return(as.integer(round(w2_end)))
  as.integer(round(seq(w2_start, w2_end, length.out = n_months)))
}

# curvature = 1 reproduces H20-fast-7 exactly
# curvature < 1 gives faster early rebuild
# curvature > 1 gives slower early rebuild
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

flag_boundary <- function(label, value, seq_vals) {
  seq_vals <- seq_vals[!is.na(seq_vals)]
  if (length(seq_vals) == 0L || length(value) != 1L || is.na(value)) return(invisible(FALSE))
  hit <- value <= min(seq_vals) || value >= max(seq_vals)
  if (isTRUE(hit)) {
    cat(sprintf("Boundary flag: %s = %s is at edge [%s, %s]\n",
                label, as.character(value), as.character(min(seq_vals)), as.character(max(seq_vals))))
  }
  invisible(hit)
}

# ---- fixed breaks from H20-fast-7 ----

b1 <- as.Date("2024-03-01")
b2 <- as.Date("2024-11-01")

idx_r1 <- which(as.Date(heco$date) < b1)
idx_r2 <- which(as.Date(heco$date) >= b1 & as.Date(heco$date) < b2)
idx_r3 <- which(as.Date(heco$date) >= b2)

r1_months <- as.Date(heco$date[idx_r1])
r2_months <- as.Date(heco$date[idx_r2])
r3_months <- as.Date(heco$date[idx_r3])

y_r1 <- y_all[idx_r1]
y_r2 <- y_all[idx_r2]
y_r3 <- y_all[idx_r3]

# ---- narrow grid around H20-fast-7 winner ----
# H20-fast-7 winner:
# avg_sing_hou, w1=(39,41,27), inv=66 -> 20 -> 0 -> 92, shocks=(3,2)

r1_series_seq <- c("avg_sing_hou", "glob")

w1_r1_seq <- c(39L, 41L, 43L)
w1_r2_seq <- c(39L, 41L, 43L)
w1_r3_seq <- c(24L, 27L, 30L)

inv_hi_pre_seq  <- c(62L, 66L, 70L)
inv_break1_seq  <- c(18L, 20L, 22L)
inv_break2_seq  <- c(0L, 2L, 4L)
inv_hi_post_seq <- c(88L, 92L, 96L)

# 1.0 nests H20-fast-7 exactly
curvature_seq <- c(0.6, 0.8, 1.0, 1.2)

shock1_dur_seq <- c(2L, 3L, 4L)
shock2_dur_seq <- c(1L, 2L, 3L)

cat(sprintf(
  "H20-fast-8 grid: R1series=%d × w1=(%d,%d,%d) × inv=(%d,%d,%d,%d) × curvature=%d × shock=(%d,%d)\n",
  length(r1_series_seq),
  length(w1_r1_seq), length(w1_r2_seq), length(w1_r3_seq),
  length(inv_hi_pre_seq), length(inv_break1_seq), length(inv_break2_seq), length(inv_hi_post_seq),
  length(curvature_seq), length(shock1_dur_seq), length(shock2_dur_seq)
))

w2_vals_r1 <- seq(min(c(inv_break1_seq, inv_hi_pre_seq)), max(c(inv_break1_seq, inv_hi_pre_seq)), by = 1L)
w2_vals_r2 <- seq(min(c(inv_break1_seq, inv_break2_seq)), max(c(inv_break1_seq, inv_break2_seq)), by = 1L)
w2_vals_r3 <- seq(min(c(inv_break2_seq, inv_hi_post_seq)), max(c(inv_break2_seq, inv_hi_post_seq)), by = 1L)

cache_r1 <- list()
for (.s in r1_series_seq) {
  for (.w1 in w1_r1_seq) {
    for (.w2 in w2_vals_r1) {
      cache_r1[[paste(.s, .w1, .w2, sep = "_")]] <-
        build_predictor_anchor(vlsfo[[.s]], vlsfo$date, .w1, .w2, r1_months, 24L)$pred
    }
  }
}

cache_r2 <- list()
for (.w1 in w1_r2_seq) {
  for (.w2 in w2_vals_r2) {
    cache_r2[[paste(.w1, .w2, sep = "_")]] <-
      build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r2_months, 24L)$pred
  }
}

cache_r3 <- list()
for (.w1 in w1_r3_seq) {
  for (.w2 in w2_vals_r3) {
    cache_r3[[paste(.w1, .w2, sep = "_")]] <-
      build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, .w1, .w2, r3_months, anchor_day_h14)$pred
  }
}

best_h20_fast8 <- NULL
best_r2_fast8 <- -Inf
rows <- vector("list", 0L)
n_valid_fast8 <- 0L

for (r1_series in r1_series_seq) {
  for (w1_r1 in w1_r1_seq) {
    for (w1_r2 in w1_r2_seq) {
      for (w1_r3 in w1_r3_seq) {
        for (inv_hi_pre in inv_hi_pre_seq) {
          for (inv_break1 in inv_break1_seq) {
            if (inv_break1 > inv_hi_pre) next

            for (inv_break2 in inv_break2_seq) {
              for (inv_hi_post in inv_hi_post_seq) {
                if (inv_hi_post < inv_break2) next

                for (curvature in curvature_seq) {
                  path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
                  path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
                  path_r3 <- build_curved_w2_path(length(r3_months), inv_break2, inv_hi_post, curvature)

                  pred_r1 <- numeric(length(r1_months))
                  pred_r2 <- numeric(length(r2_months))
                  pred_r3 <- numeric(length(r3_months))

                  for (j in seq_along(r1_months)) pred_r1[j] <- cache_r1[[paste(r1_series, w1_r1, path_r1[j], sep = "_")]][j]
                  for (j in seq_along(r2_months)) pred_r2[j] <- cache_r2[[paste(w1_r2, path_r2[j], sep = "_")]][j]
                  for (j in seq_along(r3_months)) pred_r3[j] <- cache_r3[[paste(w1_r3, path_r3[j], sep = "_")]][j]

                  df <- bind_rows(
                    tibble(date = r1_months, lsfo_bbl = y_r1, pred = pred_r1, regime = "R1", inv_days = path_r1),
                    tibble(date = r2_months, lsfo_bbl = y_r2, pred = pred_r2, regime = "R2", inv_days = path_r2),
                    tibble(date = r3_months, lsfo_bbl = y_r3, pred = pred_r3, regime = "R3", inv_days = path_r3)
                  ) |>
                    mutate(regime = factor(regime, levels = c("R1", "R2", "R3"))) |>
                    filter(!is.na(pred), !is.na(lsfo_bbl))

                  if (nrow(df) < 18L) next
                  if (n_distinct(df$regime) < 3L) next
                  if (any(count(df, regime)$n < 6L)) next

                  for (shock1_dur in shock1_dur_seq) {
                    shock1 <- build_shock_pulse(df$date, b1, shock1_dur)

                    for (shock2_dur in shock2_dur_seq) {
                      shock2 <- build_shock_pulse(df$date, b2, shock2_dur)

                      fit <- lm(
                        lsfo_bbl ~ pred * regime + shock1 + shock2,
                        data = mutate(df, shock1 = shock1, shock2 = shock2)
                      )
                      if (!all(is.finite(coef(fit)))) next

                      ssr <- sum(residuals(fit)^2)
                      r2  <- 1 - ssr / sst_h13
                      if (!is.finite(r2)) next

                      rmse <- sqrt(ssr / nrow(df))
                      n_valid_fast8 <- n_valid_fast8 + 1L

                      row <- tibble(
                        break1 = b1,
                        break2 = b2,
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
                        r2 = r2,
                        rmse = rmse,
                        n = nrow(df)
                      )
                      rows[[length(rows) + 1L]] <- row

                      if (r2 > best_r2_fast8) {
                        best_r2_fast8 <- r2
                        best_h20_fast8 <- list(
                          summary = row,
                          fit = fit,
                          df = mutate(df, shock1 = shock1, shock2 = shock2,
                                      fitted = fitted(fit), residual = residuals(fit))
                        )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

cat("Total valid candidates:", n_valid_fast8, "\n")
all_top_fast8 <- bind_rows(rows) |> arrange(desc(r2))
print(all_top_fast8 |> slice_head(n = 25))

cat("\n── H20-fast-8 best ─────────────────────────────────────────────────\n")
print(best_h20_fast8$summary)

cat("\n── H20-fast-8 boundary checks ──────────────────────────────────────\n")
flag_boundary("w1_r1", best_h20_fast8$summary$w1_r1, w1_r1_seq)
flag_boundary("w1_r2", best_h20_fast8$summary$w1_r2, w1_r2_seq)
flag_boundary("w1_r3", best_h20_fast8$summary$w1_r3, w1_r3_seq)
flag_boundary("inv_hi_pre", best_h20_fast8$summary$inv_hi_pre, inv_hi_pre_seq)
flag_boundary("inv_break1", best_h20_fast8$summary$inv_break1, inv_break1_seq)
flag_boundary("inv_break2", best_h20_fast8$summary$inv_break2, inv_break2_seq)
flag_boundary("inv_hi_post", best_h20_fast8$summary$inv_hi_post, inv_hi_post_seq)
flag_boundary("curvature", best_h20_fast8$summary$curvature, curvature_seq)
flag_boundary("shock1_dur", best_h20_fast8$summary$shock1_dur, shock1_dur_seq)
flag_boundary("shock2_dur", best_h20_fast8$summary$shock2_dur, shock2_dur_seq)

p_inv_fast8 <- best_h20_fast8$df |>
  ggplot(aes(date, inv_days, color = regime)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = c(b1, b2), linetype = "dashed", color = "grey40") +
  labs(
    title = "H20-fast-8: Implied inventory path",
    subtitle = "Curvature = 1 reproduces H20-fast-7 exactly",
    x = NULL, y = "Inventory days", color = NULL
  ) +
  theme_minimal()
print(p_inv_fast8)





}  # end exploratory search path

# ── Final consolidated model, comparison, contract plot, projection -----------
#
# This section is the intended "production" tail of the script.
# It auto-selects the best available H20 candidate among the searched local
# refinements, compares it to a small set of benchmark models, builds the
# refined contract-relationship plot, and projects forward under flat market
# prices with constant inventory.

reconstruct_final_h20_model <- function(
  break1 = as.Date("2024-03-01"),
  break2 = as.Date("2024-11-01"),
  r1_series = "avg_sing_hou",
  w1_r1 = 39L,
  w1_r2 = 41L,
  w1_r3 = 30L,
  inv_hi_pre = 66L,
  inv_break1 = 20L,
  inv_break2 = 0L,
  inv_hi_post = 92L,
  curvature = 1.2,
  shock1_dur = 3L,
  shock2_dur = 2L
) {
  idx_r1 <- which(as.Date(heco$date) < break1)
  idx_r2 <- which(as.Date(heco$date) >= break1 & as.Date(heco$date) < break2)
  idx_r3 <- which(as.Date(heco$date) >= break2)

  r1_months <- as.Date(heco$date[idx_r1])
  r2_months <- as.Date(heco$date[idx_r2])
  r3_months <- as.Date(heco$date[idx_r3])

  path_r1 <- build_linear_w2_path(length(r1_months), inv_hi_pre, inv_break1)
  path_r2 <- build_linear_w2_path(length(r2_months), inv_break1, inv_break2)
  path_r3 <- build_curved_w2_path(length(r3_months), inv_break2, inv_hi_post, curvature)

  pred_r1 <- map2_dbl(r1_months, path_r1, \(d, w2) {
    build_predictor_anchor(vlsfo[[r1_series]], vlsfo$date, w1_r1, w2, d, 24L)$pred[1]
  })
  pred_r2 <- map2_dbl(r2_months, path_r2, \(d, w2) {
    build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, w1_r2, w2, d, 24L)$pred[1]
  })
  pred_r3 <- map2_dbl(r3_months, path_r3, \(d, w2) {
    build_predictor_anchor(vlsfo$avg_sing_hou, vlsfo$date, w1_r3, w2, d, anchor_day_h14)$pred[1]
  })

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

# Default fast execution path: reconstruct the selected final model directly.
if (!exists("best_h20_fast8") || is.null(best_h20_fast8)) {
  best_h20_fast8 <- reconstruct_final_h20_model()
}

collect_final_candidates <- function() {
  out <- list()
  if (exists("best_h20_fast7") && !is.null(best_h20_fast7)) out[["H20-fast-7"]] <- best_h20_fast7
  if (exists("best_h20_fast8") && !is.null(best_h20_fast8)) out[["H20-fast-8"]] <- best_h20_fast8
  out
}

extract_summary_value <- function(x, name) {
  if (is.null(x$summary)) return(NULL)
  val <- x$summary[[name]]
  if (length(val) == 0L) NULL else val[[1]]
}

build_candidate_table <- function() {
  rows <- list()

  if (exists("best_h17") && !is.null(best_h17)) {
    rows[[length(rows) + 1L]] <- tibble(
      Model = "H17: joint local refinement",
      R2 = best_h17$r2,
      RMSE = best_h17$rmse
    )
  }

  if (exists("best_h19") && !is.null(best_h19)) {
    rows[[length(rows) + 1L]] <- tibble(
      Model = "H19: transition cost-layer release",
      R2 = best_h19$r2,
      RMSE = best_h19$rmse
    )
  }

  if (!exists("best_h17") || is.null(best_h17)) {
    rows[[length(rows) + 1L]] <- tibble(
      Model = "H17: joint local refinement",
      R2 = 0.9979,
      RMSE = 1.2667
    )
  }

  if (!exists("best_h19") || is.null(best_h19)) {
    rows[[length(rows) + 1L]] <- tibble(
      Model = "H19: transition cost-layer release",
      R2 = 0.9941,
      RMSE = 2.1367
    )
  }

  final_candidates <- collect_final_candidates()
  if (length(final_candidates)) {
    for (nm in names(final_candidates)) {
      obj <- final_candidates[[nm]]
      rows[[length(rows) + 1L]] <- tibble(
        Model = nm,
        R2 = extract_summary_value(obj, "r2"),
        RMSE = extract_summary_value(obj, "rmse")
      )
    }
  }

  bind_rows(rows) |>
    arrange(RMSE)
}

choose_final_model <- function() {
  final_candidates <- collect_final_candidates()
  if (!length(final_candidates)) stop("No H20 final candidate exists; run H20-fast-7 or H20-fast-8 first")

  candidate_rmse <- vapply(final_candidates, function(x) extract_summary_value(x, "rmse"), numeric(1))
  candidate_name <- names(final_candidates)[which.min(candidate_rmse)]
  list(name = candidate_name, obj = final_candidates[[candidate_name]])
}

build_contract_plot_data <- function(final_obj, sing_now_mt = 1120.5, hou_now_mt = 743.0) {
  b <- coef(final_obj$fit)
  V <- vcov(final_obj$fit)
  df_res <- df.residual(final_obj$fit)
  tcrit <- qt(0.975, df = df_res)

  L_r1 <- rbind(
    intercept = c(1, 0, 0, 0, 0, 0, 0, 0),
    slope     = c(0, 1, 0, 0, 0, 0, 0, 0)
  )
  colnames(L_r1) <- names(b)
  theta_r1 <- as.vector(L_r1 %*% b)
  V_r1 <- L_r1 %*% V %*% t(L_r1)

  L_r3 <- rbind(
    intercept = c(1, 0, 0, 1, 0, 0, 0, 0),
    slope     = c(0, 1, 0, 0, 0, 0, 0, 1)
  )
  colnames(L_r3) <- names(b)
  theta_r3 <- as.vector(L_r3 %*% b)
  V_r3 <- L_r3 %*% V %*% t(L_r3)

  avg_now_bbl <- ((sing_now_mt + hou_now_mt) / 2) / VLSFO_MT_TO_BBL
  x_min <- min(final_obj$df$pred[final_obj$df$regime %in% c("R1", "R3")], na.rm = TRUE)
  x_max <- max(c(final_obj$df$pred[final_obj$df$regime %in% c("R1", "R3")], avg_now_bbl), na.rm = TRUE)
  x_grid <- seq(x_min * 0.95, x_max * 1.05, length.out = 300)

  band_df <- bind_rows(
    tibble(
      index_price_bbl = x_grid,
      series = "R1 contract",
      fit = theta_r1[1] + theta_r1[2] * x_grid,
      se_fit = sqrt(V_r1[1, 1] + 2 * x_grid * V_r1[1, 2] + x_grid^2 * V_r1[2, 2])
    ),
    tibble(
      index_price_bbl = x_grid,
      series = "R3 contract",
      fit = theta_r3[1] + theta_r3[2] * x_grid,
      se_fit = sqrt(V_r3[1, 1] + 2 * x_grid * V_r3[1, 2] + x_grid^2 * V_r3[2, 2])
    )
  ) |>
    mutate(lower = fit - tcrit * se_fit, upper = fit + tcrit * se_fit)

  line_df <- bind_rows(
    tibble(index_price_bbl = x_grid, contract_price_bbl = x_grid, series = "x = y"),
    band_df |> transmute(index_price_bbl, contract_price_bbl = fit, series)
  )

  obs_df <- final_obj$df |>
    filter(regime %in% c("R1", "R3")) |>
    mutate(series = if_else(regime == "R1", "R1 contract", "R3 contract"))

  list(band_df = band_df, line_df = line_df, obs_df = obs_df)
}

project_flat_price_constant_inventory <- function(final_obj, sing_now_mt = 1120.5, hou_now_mt = 743.0,
                                                  future_start = as.Date("2026-04-01"), horizon_months = 12L) {
  avg_now_bbl <- ((sing_now_mt + hou_now_mt) / 2) / VLSFO_MT_TO_BBL
  future_dates <- seq(future_start, by = "month", length.out = horizon_months)

  future_daily <- tibble(date = seq(min(vlsfo$date), max(future_dates) + days(120), by = "day")) |>
    left_join(vlsfo |> select(date, sing, glob, houston, avg_sing_hou), by = "date") |>
    arrange(date) |>
    mutate(
      sing = if_else(date > max(vlsfo$date), sing_now_mt / VLSFO_MT_TO_BBL, sing),
      houston = if_else(date > max(vlsfo$date), hou_now_mt / VLSFO_MT_TO_BBL, houston),
      avg_sing_hou = if_else(date > max(vlsfo$date), avg_now_bbl, avg_sing_hou),
      glob = if_else(date > max(vlsfo$date), last(na.omit(vlsfo$glob)), glob)
    ) |>
    fill(sing, houston, avg_sing_hou, glob, .direction = "down")

  inv_const <- rep(extract_summary_value(final_obj, "inv_hi_post"), length(future_dates))
  w1_r3 <- extract_summary_value(final_obj, "w1_r3")

  fd_spot  <- future_daily$avg_sing_hou
  fd_dates <- future_daily$date
  proj_pred <- parallel::mcmapply(
    \(d, invd) build_predictor_anchor(fd_spot, fd_dates, w1_r3, invd, d, anchor_day_h14)$pred[1],
    future_dates, inv_const,
    mc.cores = max(1L, parallel::detectCores(logical = FALSE) - 1L)
  )

  proj_df <- tibble(
    date = future_dates,
    pred = proj_pred,
    regime = factor("R3", levels = c("R1", "R2", "R3")),
    shock1 = 0,
    shock2 = 0,
    inv_days = inv_const
  )

  proj_df$projected_lsfo_bbl <- predict(final_obj$fit, newdata = proj_df)
  proj_df
}

# Fast default uses the compact Honolulu monthly oil-generation series to avoid
# reading the full EIA-923 panel on every run. The dollars-per-barrel savings
# are converted into fuel-expense changes using:
#   monthly barrels ≈ monthly oil generation (MWh) × calibrated barrels/MWh
# where the calibration below was derived offline from Hawaiian Electric Co
# Inc.'s HI oil units in the detailed generation panel for 2022-02 onward.
#
# Calibration summary from the detailed panel:
#   barrels per MWh mean ≈ 1.7828
#   MMBtu per MWh mean   ≈ 11.0895
HECO_OAHU_BBL_PER_MWH_CALIBRATED <- 1.7828224751204167

load_monthly_fuel_use_proxy <- function() {
  gen_path <- "/Users/mike/EIA/processed/honolulu_fuel_generation_mwh_and_avg_mw.csv"
  read_csv(gen_path, show_col_types = FALSE) |>
    mutate(date = as.Date(date)) |>
    filter(county == "honolulu", fuel_bucket == "oil") |>
    transmute(
      date = floor_date(date, "month"),
      oil_generation_mwh = netgen_mwh,
      oil_avg_mw = avg_mw,
      estimated_barrels = oil_generation_mwh * HECO_OAHU_BBL_PER_MWH_CALIBRATED
    )
}

predict_under_old_contract <- function(final_obj, df_in) {
  cf_df <- df_in |>
    mutate(
      regime = factor("R1", levels = levels(final_obj$df$regime)),
      shock1 = 0,
      shock2 = 0
    )
  predict(final_obj$fit, newdata = cf_df)
}

build_historical_counterfactual <- function(final_obj, fuel_use_df) {
  hist_df <- final_obj$df |>
    mutate(
      actual_contract_lsfo_bbl = lsfo_bbl,
      old_contract_lsfo_bbl = predict_under_old_contract(final_obj, final_obj$df)
    ) |>
    select(date, regime, inv_days, pred, lsfo_bbl, actual_contract_lsfo_bbl, old_contract_lsfo_bbl) |>
    left_join(fuel_use_df, by = "date") |>
    mutate(
      savings_per_bbl = old_contract_lsfo_bbl - actual_contract_lsfo_bbl,
      monthly_savings_usd = savings_per_bbl * estimated_barrels,
      cumulative_savings_usd = cumsum(replace_na(monthly_savings_usd, 0))
    )

  hist_df
}

build_historical_risk_table <- function(final_obj, historical_df) {
  b <- coef(final_obj$fit)
  V <- vcov(final_obj$fit)
  df_res <- df.residual(final_obj$fit)
  tcrit <- qt(0.975, df = df_res)
  nm <- names(b)

  historical_df |>
    rowwise() |>
    mutate(
      old_contract_se = {
        l_old <- setNames(rep(0, length(b)), nm)
        l_old["(Intercept)"] <- 1
        l_old["pred"] <- pred
        sqrt(as.numeric(t(l_old) %*% V %*% l_old))
      },
      current_contract_se = 0,
      savings_se = old_contract_se,
      old_contract_lower = old_contract_lsfo_bbl - tcrit * old_contract_se,
      old_contract_upper = old_contract_lsfo_bbl + tcrit * old_contract_se,
      current_contract_lower = actual_contract_lsfo_bbl,
      current_contract_upper = actual_contract_lsfo_bbl,
      savings_per_bbl_lower = savings_per_bbl - tcrit * savings_se,
      savings_per_bbl_upper = savings_per_bbl + tcrit * savings_se,
      monthly_savings_usd_lower = savings_per_bbl_lower * estimated_barrels,
      monthly_savings_usd_upper = savings_per_bbl_upper * estimated_barrels
    ) |>
    ungroup() |>
    mutate(
      cumulative_savings_usd_point = cumsum(replace_na(monthly_savings_usd, 0)),
      cumulative_savings_usd_lower = cumsum(replace_na(monthly_savings_usd_lower, 0)),
      cumulative_savings_usd_upper = cumsum(replace_na(monthly_savings_usd_upper, 0))
    )
}

build_forward_counterfactual <- function(final_obj, proj_df, fuel_use_df) {
  fuel_use_recent <- fuel_use_df |>
    filter(date >= as.Date("2024-01-01"))
  avg_future_barrels <- mean(fuel_use_recent$estimated_barrels, na.rm = TRUE)
  avg_future_mw <- mean(fuel_use_recent$oil_avg_mw, na.rm = TRUE)
  avg_future_mwh <- mean(fuel_use_recent$oil_generation_mwh, na.rm = TRUE)

  out <- proj_df |>
    mutate(
      old_contract_lsfo_bbl = predict_under_old_contract(final_obj, proj_df),
      actual_contract_lsfo_bbl = projected_lsfo_bbl,
      savings_per_bbl = old_contract_lsfo_bbl - actual_contract_lsfo_bbl,
      oil_avg_mw = avg_future_mw,
      oil_generation_mwh = avg_future_mwh,
      estimated_barrels = avg_future_barrels,
      monthly_savings_usd = savings_per_bbl * estimated_barrels,
      cumulative_projected_savings_usd = cumsum(replace_na(monthly_savings_usd, 0))
    )

  attr(out, "avg_future_barrels") <- avg_future_barrels
  attr(out, "avg_future_mw") <- avg_future_mw
  out
}

build_forward_risk_table <- function(final_obj, forward_df) {
  b <- coef(final_obj$fit)
  V <- vcov(final_obj$fit)
  df_res <- df.residual(final_obj$fit)
  tcrit <- qt(0.975, df = df_res)
  nm <- names(b)

  # Old contract at index x: intercept_R1 + slope_R1 * x
  # Current contract at index x: intercept_R3 + slope_R3 * x
  # Savings per bbl from renegotiation: old - current
  savings_tbl <- forward_df |>
    rowwise() |>
    mutate(
      old_contract_se = {
        l_old <- setNames(rep(0, length(b)), nm)
        l_old["(Intercept)"] <- 1
        l_old["pred"] <- pred
        sqrt(as.numeric(t(l_old) %*% V %*% l_old))
      },
      current_contract_se = {
        l_new <- setNames(rep(0, length(b)), nm)
        l_new["(Intercept)"] <- 1
        l_new["regimeR3"] <- 1
        l_new["pred"] <- pred
        l_new["pred:regimeR3"] <- pred
        sqrt(as.numeric(t(l_new) %*% V %*% l_new))
      },
      savings_se = {
        l_old <- setNames(rep(0, length(b)), nm)
        l_new <- setNames(rep(0, length(b)), nm)
        l_old["(Intercept)"] <- 1
        l_old["pred"] <- pred
        l_new["(Intercept)"] <- 1
        l_new["regimeR3"] <- 1
        l_new["pred"] <- pred
        l_new["pred:regimeR3"] <- pred
        l_diff <- l_old - l_new
        sqrt(as.numeric(t(l_diff) %*% V %*% l_diff))
      }
    ) |>
    ungroup() |>
    mutate(
      old_contract_lower = old_contract_lsfo_bbl - tcrit * old_contract_se,
      old_contract_upper = old_contract_lsfo_bbl + tcrit * old_contract_se,
      current_contract_lower = actual_contract_lsfo_bbl - tcrit * current_contract_se,
      current_contract_upper = actual_contract_lsfo_bbl + tcrit * current_contract_se,
      savings_per_bbl_lower = savings_per_bbl - tcrit * savings_se,
      savings_per_bbl_upper = savings_per_bbl + tcrit * savings_se,
      monthly_savings_usd_lower = savings_per_bbl_lower * estimated_barrels,
      monthly_savings_usd_upper = savings_per_bbl_upper * estimated_barrels,
      cumulative_savings_usd_point = cumsum(monthly_savings_usd),
      cumulative_savings_usd_lower = cumsum(monthly_savings_usd_lower),
      cumulative_savings_usd_upper = cumsum(monthly_savings_usd_upper)
    )

  savings_tbl
}

final_choice <- choose_final_model()
final_model_name <- final_choice$name
final_model <- final_choice$obj

comparison_tbl <- build_candidate_table() |>
  mutate(across(c(R2, RMSE), \(x) round(x, 4)))

cat("\n══ Final Model Comparison ════════════════════════════════════════════\n")
print(comparison_tbl)

cat("\n── Final selected model ──────────────────────────────────────────────\n")
cat("Selection rule: lowest RMSE among searched H20 final candidates.\n")
cat(sprintf("Selected: %s\n", final_model_name))
print(final_model$summary)

b_final <- coef(final_model$fit)
coef_tbl_final <- tibble(
  term = names(b_final),
  estimate = unname(b_final),
  std.error = sqrt(diag(vcov(final_model$fit))),
  statistic = unname(b_final) / sqrt(diag(vcov(final_model$fit))),
  p.value = 2 * pt(-abs(unname(b_final) / sqrt(diag(vcov(final_model$fit)))), df = df.residual(final_model$fit))
)

cat("\n── Final coefficient table ───────────────────────────────────────────\n")
print(coef_tbl_final)

cat("\nFinal model interpretation:\n")
cat(sprintf("Pre-2/2024 contract / R1: %s with w1=%d.\n",
            extract_summary_value(final_model, "r1_series"),
            extract_summary_value(final_model, "w1_r1")))
cat(sprintf("Transition / R2: avg_sing_hou with w1=%d, inventory drawdown from %d to %d, break1 shock duration=%d months.\n",
            extract_summary_value(final_model, "w1_r2"),
            extract_summary_value(final_model, "inv_break1"),
            extract_summary_value(final_model, "inv_break2"),
            extract_summary_value(final_model, "shock1_dur")))
cat(sprintf("Current contract / R3: avg_sing_hou with w1=%d, rebuild toward %d inventory-days proxy, break2 shock duration=%d months.\n",
            extract_summary_value(final_model, "w1_r3"),
            extract_summary_value(final_model, "inv_hi_post"),
            extract_summary_value(final_model, "shock2_dur")))
if ("curvature" %in% names(final_model$summary)) {
  cat(sprintf("R3 rebuild curvature: %.2f (1.00 = linear H20-fast-7 rebuild).\n",
              extract_summary_value(final_model, "curvature")))
}

p_final_fit <- final_model$df |>
  ggplot(aes(date)) +
  geom_line(aes(y = lsfo_bbl, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = fitted, color = "Fitted"), linewidth = 1) +
  geom_vline(xintercept = c(extract_summary_value(final_model, "break1"),
                            extract_summary_value(final_model, "break2")),
             linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(Actual = "black", Fitted = "firebrick")) +
  annotate(
    "text",
    x = max(final_model$df$date),
    y = tail(final_model$df$fitted, 1) - 4,
    label = "Fitted",
    color = "firebrick",
    size = 5,
    hjust = 1
  ) +
  annotate(
    "text",
    x = final_model$df$date[nrow(final_model$df) - 1L],
    y = final_model$df$lsfo_bbl[nrow(final_model$df) - 1L] + 4,
    label = "Actual",
    color = "black",
    size = 5,
    hjust = 1
  ) +
  annotate(
    "text",
    x = extract_summary_value(final_model, "break1"),
    y = max(final_model$df$lsfo_bbl, na.rm = TRUE) + 4,
    label = "Contract Change",
    color = "grey30",
    size = 4.5,
    hjust = -0.05
  ) +
  annotate(
    "text",
    x = extract_summary_value(final_model, "break2"),
    y = max(final_model$df$lsfo_bbl, na.rm = TRUE) + 4,
    label = "End of Transition",
    color = "grey30",
    size = 4.5,
    hjust = -0.05
  ) +
  labs(x = NULL, y = "HECO LSFO Price ($/bbl)", color = NULL) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_final_fit)

regime_label_df <- final_model$df |>
  group_by(regime) |>
  summarise(
    x = median(date),
    y = median(inv_days),
    label = c("Old Contract", "Transition", "New Contract")[match(first(regime), c("R1", "R2", "R3"))],
    .groups = "drop"
  )

p_final_inv <- final_model$df |>
  ggplot(aes(date, inv_days, color = regime)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = c(extract_summary_value(final_model, "break1"),
                            extract_summary_value(final_model, "break2")),
             linetype = "dashed", color = "grey40") +
  geom_text(
    data = regime_label_df,
    aes(x = x, y = y, label = label, color = regime),
    inherit.aes = FALSE,
    size = 5,
    show.legend = FALSE
  ) +
  labs(x = NULL, y = "Estimated Inventory (days)", color = NULL) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_final_inv)

p_final_resid <- final_model$df |>
  ggplot(aes(date, residual, color = regime)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(linewidth = 1) +
  labs(title = sprintf("%s: Residuals over time", final_model_name),
       x = NULL, y = "Residual $/bbl", color = NULL) +
  theme_minimal()
print(p_final_resid)

contract_plot_data <- build_contract_plot_data(final_model)

p_contract_lines <- ggplot() +
  geom_ribbon(
    data = contract_plot_data$band_df,
    aes(x = index_price_bbl, ymin = lower, ymax = upper, fill = series),
    alpha = 0.18
  ) +
  geom_point(
    data = contract_plot_data$obs_df,
    aes(x = pred, y = lsfo_bbl, color = series),
    size = 2,
    alpha = 0.8
  ) +
  geom_line(
    data = contract_plot_data$line_df,
    aes(x = index_price_bbl, y = contract_price_bbl, color = series, linetype = series),
    linewidth = 1.2
  ) +
  annotate("text", x = 140, y = 157, label = "Current Contract", color = "firebrick", size = 5) +
  annotate("text", x = 140, y = 235, label = "Old Contract", color = "steelblue4", size = 5) +
  scale_color_manual(values = c(
    `R1 contract` = "steelblue4",
    `R3 contract` = "firebrick",
    `x = y` = "gray55"
  )) +
  scale_fill_manual(values = c(
    `R1 contract` = "steelblue4",
    `R3 contract` = "firebrick"
  )) +
  scale_linetype_manual(values = c(
    `R1 contract` = "solid",
    `R3 contract` = "solid",
    `x = y` = "dashed"
  )) +
  labs(
    x = "Average of Singapore and Houston VLSFO Price Indices, X-day MA ($/bbl)",
    y = "Contract Price ($/bbl)"
  ) +
  guides(color = "none", fill = "none", linetype = "none") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_contract_lines)

fuel_use_proxy <- load_monthly_fuel_use_proxy()
historical_counterfactual <- build_historical_counterfactual(final_model, fuel_use_proxy)
historical_risk_tbl <- build_historical_risk_table(final_model, historical_counterfactual)

cat("\n── Historical old-contract counterfactual ───────────────────────────\n")
cat("Counterfactual assumption: keep the fitted inventory path fixed, but price all transition and new-contract months off the old-contract (R1) relationship.\n")
print(
  historical_counterfactual |>
    select(date, regime, inv_days, lsfo_bbl, actual_contract_lsfo_bbl, old_contract_lsfo_bbl,
           savings_per_bbl, estimated_barrels, monthly_savings_usd) |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
)

historical_summary <- historical_counterfactual |>
  summarise(
    months = n(),
    avg_oil_avg_mw = mean(oil_avg_mw, na.rm = TRUE),
    total_estimated_barrels = sum(estimated_barrels, na.rm = TRUE),
    total_historical_savings_usd = sum(monthly_savings_usd, na.rm = TRUE)
  )

cat("\nHistorical savings summary:\n")
print(historical_summary |>
        mutate(across(where(is.numeric), \(x) round(x, 2))))

cat("\nHistorical risk summary (parameter uncertainty only):\n")
print(
  historical_risk_tbl |>
    summarise(
      historical_savings_usd_point = sum(monthly_savings_usd, na.rm = TRUE),
      historical_savings_usd_lower = sum(monthly_savings_usd_lower, na.rm = TRUE),
      historical_savings_usd_upper = sum(monthly_savings_usd_upper, na.rm = TRUE)
    ) |>
    mutate(across(everything(), \(x) round(x, 2)))
)

p_historical_cf <- historical_counterfactual |>
  select(date, actual_contract_lsfo_bbl, old_contract_lsfo_bbl) |>
  pivot_longer(-date, names_to = "series", values_to = "lsfo_bbl") |>
  mutate(series = recode(
    series,
    actual_contract_lsfo_bbl = "Selected Contract",
    old_contract_lsfo_bbl = "Old Contract Counterfactual"
  )) |>
  ggplot(aes(date, lsfo_bbl, color = series)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(
    `Selected Contract` = "firebrick",
    `Old Contract Counterfactual` = "steelblue4"
  )) +
  labs(
    x = NULL,
    y = "HECO LSFO Price ($/bbl)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_historical_cf)

p_historical_savings <- historical_counterfactual |>
  ggplot(aes(date, cumulative_savings_usd / 1e6)) +
  geom_line(linewidth = 1.1, color = "darkgreen") +
  labs(
    x = NULL,
    y = "Cumulative Estimated Savings (million $)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_historical_savings)

p_historical_risk <- historical_risk_tbl |>
  ggplot(aes(date)) +
  geom_ribbon(
    aes(
      ymin = cumulative_savings_usd_lower / 1e6,
      ymax = cumulative_savings_usd_upper / 1e6
    ),
    fill = "darkgreen",
    alpha = 0.18
  ) +
  geom_line(aes(y = cumulative_savings_usd_point / 1e6), linewidth = 1.1, color = "darkgreen") +
  labs(
    x = NULL,
    y = "Cumulative Estimated Savings (million $)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_historical_risk)

proj_df_final <- project_flat_price_constant_inventory(final_model, sing_now_mt = 1120.5, hou_now_mt = 743.0)
forward_counterfactual <- build_forward_counterfactual(final_model, proj_df_final, fuel_use_proxy)
forward_risk_tbl <- build_forward_risk_table(final_model, forward_counterfactual)

cat("\n── Flat-price, flat-inventory projection ────────────────────────────\n")
cat("Assumption: Singapore = 1120.5 $/mt, Houston = 743.0 $/mt, inventory constant at steady-state level.\n")
print(proj_df_final |> select(date, inv_days, pred, projected_lsfo_bbl))

cat("\nForward counterfactual assumption: same flat-price, flat-inventory path, but price future months under the old-contract (R1) relationship.\n")
print(
  forward_counterfactual |>
    select(date, inv_days, pred, actual_contract_lsfo_bbl, old_contract_lsfo_bbl,
           savings_per_bbl, estimated_barrels, monthly_savings_usd) |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
)

cat(sprintf(
  "\nForward fuel-use basis: %.0f barrels/month implied by %.1f average MW of Honolulu oil generation.\n",
  attr(forward_counterfactual, "avg_future_barrels"),
  attr(forward_counterfactual, "avg_future_mw")
))

p_proj_final <- forward_counterfactual |>
  select(date, actual_contract_lsfo_bbl, old_contract_lsfo_bbl) |>
  pivot_longer(-date, names_to = "series", values_to = "lsfo_bbl") |>
  mutate(series = recode(
    series,
    actual_contract_lsfo_bbl = "Selected Contract",
    old_contract_lsfo_bbl = "Old Contract Counterfactual"
  )) |>
  ggplot(aes(date, lsfo_bbl, color = series)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    `Selected Contract` = "navy",
    `Old Contract Counterfactual` = "steelblue4"
  )) +
  labs(
       title = "Projected HECO-equivalent LSFO under flat Singapore/Houston prices",
       subtitle = "Inventory proxy held constant at final model steady-state level",
       x = NULL, y = "HECO LSFO Price ($/bbl)", color = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 13)
  )
print(p_proj_final)

forward_summary <- forward_counterfactual |>
  summarise(
    projected_barrels = sum(estimated_barrels, na.rm = TRUE),
    projected_savings_usd = sum(monthly_savings_usd, na.rm = TRUE)
  )

cat("\n── Forward renegotiation risk table ─────────────────────────────────\n")
cat("Interpretation: holds the projected index path fixed and varies only the old-vs-new contract relationships using coefficient uncertainty from the fitted model.\n")
print(
  forward_risk_tbl |>
    select(
      date, pred,
      actual_contract_lsfo_bbl, current_contract_lower, current_contract_upper,
      old_contract_lsfo_bbl, old_contract_lower, old_contract_upper,
      savings_per_bbl, savings_per_bbl_lower, savings_per_bbl_upper,
      monthly_savings_usd, monthly_savings_usd_lower, monthly_savings_usd_upper
    ) |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
)

forward_risk_summary <- forward_risk_tbl |>
  summarise(
    projected_savings_usd_point = sum(monthly_savings_usd, na.rm = TRUE),
    projected_savings_usd_lower = sum(monthly_savings_usd_lower, na.rm = TRUE),
    projected_savings_usd_upper = sum(monthly_savings_usd_upper, na.rm = TRUE)
  ) |>
  mutate(across(everything(), \(x) round(x, 2)))

cat("\nForward risk summary (parameter uncertainty only):\n")
print(forward_risk_summary)

combined_savings_tbl <- tibble(
  period = c("Historical to date", "Projected forward", "Combined"),
  savings_usd = c(
    sum(historical_counterfactual$monthly_savings_usd, na.rm = TRUE),
    sum(forward_counterfactual$monthly_savings_usd, na.rm = TRUE),
    sum(historical_counterfactual$monthly_savings_usd, na.rm = TRUE) +
      sum(forward_counterfactual$monthly_savings_usd, na.rm = TRUE)
  )
) |>
  mutate(savings_million_usd = savings_usd / 1e6)

combined_risk_tbl <- tibble(
  period = c("Historical to date", "Projected forward", "Combined"),
  lower_usd = c(
    sum(historical_risk_tbl$monthly_savings_usd_lower, na.rm = TRUE),
    sum(forward_risk_tbl$monthly_savings_usd_lower, na.rm = TRUE),
    sum(historical_risk_tbl$monthly_savings_usd_lower, na.rm = TRUE) +
      sum(forward_risk_tbl$monthly_savings_usd_lower, na.rm = TRUE)
  ),
  point_usd = c(
    sum(historical_risk_tbl$monthly_savings_usd, na.rm = TRUE),
    sum(forward_risk_tbl$monthly_savings_usd, na.rm = TRUE),
    sum(historical_risk_tbl$monthly_savings_usd, na.rm = TRUE) +
      sum(forward_risk_tbl$monthly_savings_usd, na.rm = TRUE)
  ),
  upper_usd = c(
    sum(historical_risk_tbl$monthly_savings_usd_upper, na.rm = TRUE),
    sum(forward_risk_tbl$monthly_savings_usd_upper, na.rm = TRUE),
    sum(historical_risk_tbl$monthly_savings_usd_upper, na.rm = TRUE) +
      sum(forward_risk_tbl$monthly_savings_usd_upper, na.rm = TRUE)
  )
) |>
  mutate(
    lower_million_usd = lower_usd / 1e6,
    point_million_usd = point_usd / 1e6,
    upper_million_usd = upper_usd / 1e6
  )

cat("\n── Renegotiation savings summary ────────────────────────────────────\n")
print(combined_savings_tbl |>
        mutate(across(where(is.numeric), \(x) round(x, 2))))

cat("\nRenegotiation savings summary with parameter uncertainty:\n")
print(combined_risk_tbl |>
        mutate(across(where(is.numeric), \(x) round(x, 2))))

p_forward_savings <- forward_counterfactual |>
  ggplot(aes(date, cumulative_projected_savings_usd / 1e6)) +
  geom_line(linewidth = 1.1, color = "darkgreen") +
  labs(
    x = NULL,
    y = "Projected Cumulative Savings (million $)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_forward_savings)

p_forward_risk <- forward_risk_tbl |>
  ggplot(aes(date)) +
  geom_ribbon(
    aes(
      ymin = cumulative_savings_usd_lower / 1e6,
      ymax = cumulative_savings_usd_upper / 1e6
    ),
    fill = "darkgreen",
    alpha = 0.18
  ) +
  geom_line(aes(y = cumulative_savings_usd_point / 1e6), linewidth = 1.1, color = "darkgreen") +
  labs(
    x = NULL,
    y = "Projected Cumulative Savings (million $)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
print(p_forward_risk)
