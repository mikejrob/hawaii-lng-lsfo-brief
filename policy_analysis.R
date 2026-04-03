# ==============================================================================
# Policy Analysis: HECO LSFO Contract Evaluation and LNG Comparison
# ==============================================================================
#
# Addresses three policy questions:
#
#   Q1. HISTORICAL SAVINGS
#       How much has the 2024 contract renegotiation saved HECO's customers so
#       far, taking into account the strategic inventory adjustments (inventory
#       wind-down from w2 = 66 → 20 days during R1) that preceded the contract
#       change and the contract change itself?
#
#       Approach:
#         • Actual spending = observed HECO LSFO prices × fuel-use volumes
#         • Counterfactual = what the OLD contract (R1 parameters, steady-state
#           w2 = 20 days, w1 = 39 days) would have priced the same VLSFO market
#           during the R2 + R3 period (March 2024 onwards)
#         • Pre-contract inventory effect = R1 actual (declining w2) vs. R1
#           steady-state (constant w2 = 66) during the R1 period
#
#   Q2. FORWARD SAVINGS
#       How much is the current contract projected to save over the next 12
#       months, given current VLSFO/Brent/WTI prices and futures-based
#       projections?
#
#       Two scenarios:
#         • Flat-price scenario: crude held at the latest daily close
#         • Futures curve scenario: uses crude_futures_curve.csv if available
#
#   Q3. LSFO vs. LNG COST COMPARISON
#       How does the current-contract LSFO cost ($/MMBtu) compare to the LNG
#       cost under the HSEO/FGE indicative contract (FGE August 2024 report)?
#
#       LNG formula (DES commodity):  0.118 × Brent + 0.60   $/MMBtu
#       Infrastructure (regas) adders — two volume scenarios:
#         • 0.4 mtpa (FSRU purchase): + $3.93/MMBtu → all-in = 0.118B + 4.53
#         • 1.0 mtpa (FSRU purchase): + $1.68/MMBtu → all-in = 0.118B + 2.28
#
#       Assumption: WTI = Brent (for LSFO model which uses both series).
#
#       Also shown: LNG price from the optimal FRED moving-average regression
#       (lng_brent_fred_regression_test.R) as an alternative to the indicative
#       HSEO contract — useful given recent LNG price spikes.
#
# Prerequisites (produced by other scripts in this folder):
#   heco_lsfo_model_params.csv               from heco_lsfo_crude_projection.R
#   heco_lsfo_long_run_curve.csv             from heco_lsfo_crude_projection.R
#   heco_lsfo_short_term_monthly_projection.csv  from heco_lsfo_crude_projection.R
#   heco_lsfo_long_term_projection.csv       from heco_lsfo_crude_projection.R (optional)
#   vlsfo_brent_best_model_summary.csv       from vlsfo_brent_daily_link.R
#   vlsfo_brent_best_model_coefficients.csv  from vlsfo_brent_daily_link.R
#   crude_futures_curve.csv                  from investing_crude_futures_update.py
#   brent_nominal_real_path_adjusted_to_steo2026.csv  from aeo_brent_adjust.R (optional)
#   lng_brent_optimal_ma_coefficients.csv    from lng_brent_fred_regression_test.R (optional)
#   ../../miscData/heco_lsfo.csv             HECO LSFO history
#   ../../processed/honolulu_fuel_generation_mwh_and_avg_mw.csv  EIA fuel use
#
# Outputs:
#   policy_historical_savings.csv    monthly savings table with uncertainty bands
#   policy_forward_comparison.csv    forward LSFO vs. LNG price paths
#   policy_historical_savings.png    cumulative savings chart
#   policy_forward_comparison.png    forward price comparison ($/MMBtu)
#   policy_spot_comparison.png       current-spot all-in cost per MMBtu
# ==============================================================================

library(tidyverse)
library(lubridate)
library(slider)
library(data.table)
library(parallel)
library(scales)
setDTthreads(1L)

# ── Constants ──────────────────────────────────────────────────────────────────

VLSFO_MT_TO_BBL <- 6.35

# Heat content calibrated from EIA-923 HECO fleet data (2022-02 onward):
#   barrels/MWh = 1.7828,  MMBtu/MWh = 11.0895
HECO_BBL_PER_MWH   <- 1.7828224751204167
HECO_MMBTU_PER_MWH <- 11.0895
LSFO_MMBTU_PER_BBL <- HECO_MMBTU_PER_MWH / HECO_BBL_PER_MWH  # ≈ 6.22 MMBtu/bbl

# HSEO/FGE indicative LNG contract (FGE August 2024 report, energy.hawaii.gov):
#   Commodity DES price:  P_lng = LNG_SLOPE × Brent + LNG_INTERCEPT  ($/MMBtu)
#   Source: "At $80/bbl, price = 0.118 × 80 + 0.60 = $10.04/MMBtu"
LNG_SLOPE     <- 0.118   # $/MMBtu per $/bbl Brent
LNG_INTERCEPT <- 0.60    # $/MMBtu
# Infrastructure adders (regasification tariff, FSRU purchase scenario):
LNG_REGAS_04  <- 3.93    # $/MMBtu at 0.4 mtpa
LNG_REGAS_10  <- 1.68    # $/MMBtu at 1.0 mtpa

# ── Path helpers ───────────────────────────────────────────────────────────────

get_script_dir <- function() {
  cmd_args  <- commandArgs(trailingOnly = FALSE)
  file_arg  <- "--file="
  file_idx  <- grep(file_arg, cmd_args)
  if (length(file_idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", cmd_args[file_idx[1]]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  cwd       <- normalizePath(getwd())
  candidate <- file.path(cwd, "lng_vs_oil")
  if (file.exists(file.path(candidate, "vlsfo_prices.csv"))) return(candidate)
  cwd
}

base_dir     <- get_script_dir()
project_root <- normalizePath(file.path(base_dir, ".."))

# Fork-based parallelism for month-by-month predictor loops.
# Works on macOS/Linux.  NOTE FOR WINDOWS USERS: mcmapply falls back to
# sequential on Windows — see vlsfo_brent_daily_link.R for a makeCluster
# template if cross-platform parallelism is needed.
n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

# ── Shared utility functions (mirror heco_lsfo_crude_projection.R) ─────────────
# These are copied here to keep policy_analysis.R self-contained so it can
# run independently of the other scripts.

# Build the nested moving-average VLSFO predictor used in the HECO contract.
# spot       : daily VLSFO values (numeric vector)
# dates      : corresponding dates (Date vector, same length as spot)
# w1         : inner window (days) — short-term MA of daily VLSFO
# w2         : outer window (days) — lookback for the anchor-day mean of w1-MA values
# target_dates: months to forecast (Date vector, typically first-of-month)
# anchor_day : day of the prior month used as the lookback anchor (24 for R3)
build_predictor_anchor <- function(spot, dates, w1, w2, target_dates, anchor_day) {
  cp       <- slide_dbl(spot, mean, .before = w1 - 1L, .complete = TRUE)
  daily    <- tibble(date = as.Date(dates), cp = cp) |> filter(!is.na(cp))
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
      i_end   <- findInterval(as.integer(anchor),              daily_int)
      i_start <- findInterval(as.integer(win_start) - 1L, daily_int)
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

# ── Read model parameters ──────────────────────────────────────────────────────

params_path <- file.path(base_dir, "heco_lsfo_model_params.csv")
if (!file.exists(params_path)) {
  stop(
    "heco_lsfo_model_params.csv not found. ",
    "Run heco_lsfo_crude_projection.R first to generate it."
  )
}

mp <- fread(params_path) |> as_tibble()
get_param <- function(name) {
  val <- mp$value[mp$parameter == name]
  if (!length(val) || is.na(val)) stop("Missing model parameter: ", name)
  val
}
get_dbl  <- function(name) as.numeric(get_param(name))
get_int  <- function(name) as.integer(get_param(name))
get_date <- function(name) as.Date(get_param(name))

break1      <- get_date("break1")          # 2024-03-01
break2      <- get_date("break2")          # 2024-11-01
w1_r1       <- get_int("w1_r1")            # 39
w1_r3       <- get_int("w1_r3")            # 30
anchor_day  <- get_int("anchor_day_r3")    # 24
inv_hi_pre  <- get_int("inv_hi_pre")       # 66 — R1 steady-state inventory
inv_break1  <- get_int("inv_break1")       # 20 — inventory at R1→R2 break
inv_break2  <- get_int("inv_break2")       # 0
inv_hi_post <- get_int("inv_hi_post")      # 92 — R3 converging level
curvature   <- get_dbl("curvature")        # 1.2

r1_intercept <- get_dbl("r1_intercept")
r1_slope     <- get_dbl("r1_slope")
r3_intercept <- get_dbl("r3_intercept")
r3_slope     <- get_dbl("r3_slope")

# ── Load historical data ───────────────────────────────────────────────────────

heco_hist <- fread(file.path(project_root, "miscData", "heco_lsfo.csv")) |>
  as_tibble() |>
  mutate(
    date     = as.Date(floor_date(parse_date_time(Date, orders = c("mdy", "ymd", "dmy")), "month")),
    lsfo_bbl = Honolulu_LSFO
  ) |>
  filter(date >= as.Date("2022-02-01"), !is.na(lsfo_bbl)) |>
  select(date, lsfo_bbl) |>
  arrange(date)

vlsfo_hist <- fread(file.path(base_dir, "vlsfo_prices.csv")) |>
  as_tibble() |>
  transmute(
    date         = as.Date(date),
    sing         = as.numeric(singapore_vlsfo_usd_mt)   / VLSFO_MT_TO_BBL,
    houston      = as.numeric(houston_vlsfo_usd_mt)      / VLSFO_MT_TO_BBL,
    avg_sing_hou = (as.numeric(singapore_vlsfo_usd_mt) + as.numeric(houston_vlsfo_usd_mt)) / 2 / VLSFO_MT_TO_BBL
  ) |>
  filter(!is.na(date)) |>
  arrange(date)

fuel_use <- fread(
  file.path(project_root, "processed", "honolulu_fuel_generation_mwh_and_avg_mw.csv")
) |>
  as_tibble() |>
  mutate(date = as.Date(floor_date(as.Date(date), "month"))) |>
  filter(county == "honolulu", fuel_bucket == "oil") |>
  transmute(
    date              = date,
    oil_generation_mwh = netgen_mwh,
    oil_avg_mw        = avg_mw,
    estimated_barrels = oil_generation_mwh * HECO_BBL_PER_MWH
  ) |>
  arrange(date)

# ── Long-run curve (for current and old contract) ──────────────────────────────

long_run_curve <- fread(file.path(base_dir, "heco_lsfo_long_run_curve.csv")) |>
  as_tibble() |>
  mutate(r3_lsfo_bbl = heco_lsfo_bbl)

# r1_lsfo_bbl is written by heco_lsfo_crude_projection.R (build_long_run_curve).
# Fall back to local recalculation if the column is absent from an older CSV.
if (!"r1_lsfo_bbl" %in% names(long_run_curve)) {
  long_run_curve <- long_run_curve |>
    mutate(r1_lsfo_bbl = r1_intercept + r1_slope * avg_sing_hou_bbl)
}

# ── Q1: Historical savings ─────────────────────────────────────────────────────
#
# Post-break period (March 2024 onwards):
#   Counterfactual = R1 contract applied to the SAME VLSFO market, using R1
#   parameters (w1 = 39, w2 = inv_break1 = 20 — the level the old contract
#   had reached by the time of the break).  This answers: "what would HECO
#   have paid under the OLD contract for the same market conditions?"
#
# Pre-break inventory effect (entire R1 period):
#   Compares R1 actual prices (declining w2: 66 → 20) to R1 steady-state at
#   w2 = 66, using R1 coefficients.  Shows the impact of the pre-contract
#   inventory wind-down on customer bills.

post_break_months <- heco_hist |>
  filter(date >= break1) |>
  pull(date)

# Build R1 counterfactual predictor for the post-break months.
cf_pred_r1 <- build_predictor_anchor(
  spot         = vlsfo_hist$avg_sing_hou,
  dates        = vlsfo_hist$date,
  w1           = w1_r1,
  w2           = inv_break1,    # steady state at the point of contract change
  target_dates = post_break_months,
  anchor_day   = anchor_day
)

cf_prices_r1 <- r1_intercept + r1_slope * cf_pred_r1$pred

historical_savings <- heco_hist |>
  filter(date >= break1) |>
  left_join(
    tibble(date = post_break_months, r1_counterfactual_bbl = cf_prices_r1),
    by = "date"
  ) |>
  left_join(fuel_use, by = "date") |>
  mutate(
    savings_per_bbl       = r1_counterfactual_bbl - lsfo_bbl,
    savings_per_mmbtu     = savings_per_bbl / LSFO_MMBTU_PER_BBL,
    monthly_savings_usd   = savings_per_bbl * estimated_barrels,
    cumulative_savings_usd = cumsum(replace_na(monthly_savings_usd, 0))
  )

# Pre-break inventory effect: compare R1 actual to R1 steady-state (w2 = 66).
r1_months <- heco_hist |> filter(date < break1) |> pull(date)
n_r1 <- length(r1_months)

r1_actual_preds <- parallel::mcmapply(
  \(d, w2) build_predictor_anchor(
    vlsfo_hist$avg_sing_hou, vlsfo_hist$date,
    w1_r1, w2, d, anchor_day
  )$pred[1],
  r1_months,
  build_linear_w2_path(n_r1, inv_hi_pre, inv_break1),
  mc.cores = n_cores
)

r1_ss_preds <- parallel::mclapply(
  r1_months,
  \(d) build_predictor_anchor(
    vlsfo_hist$avg_sing_hou, vlsfo_hist$date,
    w1_r1, inv_hi_pre, d, anchor_day
  )$pred[1],
  mc.cores = n_cores
) |> unlist()

pre_break_effect <- heco_hist |>
  filter(date < break1) |>
  mutate(
    r1_actual_pred_bbl    = r1_intercept + r1_slope * r1_actual_preds,
    r1_steadystate_bbl    = r1_intercept + r1_slope * r1_ss_preds,
    inventory_effect_bbl  = r1_actual_pred_bbl - r1_steadystate_bbl
  ) |>
  left_join(fuel_use, by = "date") |>
  mutate(
    inventory_effect_mmbtu = inventory_effect_bbl / LSFO_MMBTU_PER_BBL,
    inventory_effect_usd   = inventory_effect_bbl * estimated_barrels
  )

# ── Q2: Forward savings ────────────────────────────────────────────────────────

short_term_proj <- fread(
  file.path(base_dir, "heco_lsfo_short_term_monthly_projection.csv")
) |>
  as_tibble() |>
  mutate(date = as.Date(date))

# Brent price used when the short-term projection was generated (flat-crude assumption).
# Using this — rather than latest_brent from FRED — ensures LSFO and LNG in the
# forward comparison are both priced at the same Brent.
proj_brent <- if ("flat_brent_bbl" %in% names(short_term_proj)) {
  mean(short_term_proj$flat_brent_bbl, na.rm = TRUE)
} else {
  warning("flat_brent_bbl not in short-term projection CSV — re-run heco_lsfo_crude_projection.R. ",
          "Falling back to latest FRED Brent ($", round(latest_brent, 2), "), ",
          "which may not match the LSFO projection.")
  latest_brent
}

# Build the R1 counterfactual predictor for the same forward months.
# Use the actual + bridge-predicted VLSFO series exported by heco_lsfo_crude_projection.R
# so that R1 and R3 are both projected on the same VLSFO basis.  Extending flat at
# the latest observed spot would contaminate the predictor with any recent VLSFO spikes.
vlsfo_ext_path <- file.path(base_dir, "heco_lsfo_short_term_daily_vlsfo_projection.csv")
if (file.exists(vlsfo_ext_path)) {
  vlsfo_extended_raw <- fread(vlsfo_ext_path) |>
    as_tibble() |>
    transmute(date = as.Date(date), avg_sing_hou = avg_sing_hou_bbl) |>
    filter(!is.na(avg_sing_hou)) |>
    arrange(date)
} else {
  warning("heco_lsfo_short_term_daily_vlsfo_projection.csv not found — ",
          "re-run heco_lsfo_crude_projection.R.  Falling back to flat VLSFO extension.")
  latest_vlsfo_date  <- max(vlsfo_hist$date)
  latest_vlsfo_price <- vlsfo_hist$avg_sing_hou[which.max(vlsfo_hist$date)]
  vlsfo_extended_raw <- bind_rows(
    vlsfo_hist |> select(date, avg_sing_hou),
    tibble(
      date         = seq.Date(latest_vlsfo_date + days(1),
                              max(short_term_proj$date) + days(30), by = "day"),
      avg_sing_hou = latest_vlsfo_price
    )
  ) |> arrange(date)
}
vlsfo_extended <- vlsfo_extended_raw

forward_months <- short_term_proj$date

fwd_cf_pred <- build_predictor_anchor(
  spot         = vlsfo_extended$avg_sing_hou,
  dates        = vlsfo_extended$date,
  w1           = w1_r1,
  w2           = inv_break1,
  target_dates = forward_months,
  anchor_day   = anchor_day
)

# Latest Brent close for LNG comparison in the flat-price scenario.
brent_hist <- fread(file.path(base_dir, "fred_dcoilbrenteu_daily_brent.csv")) |>
  as_tibble() |>
  transmute(
    date      = as.Date(observation_date),
    brent_bbl = suppressWarnings(as.numeric(ifelse(DCOILBRENTEU %in% c(".", ""), NA, DCOILBRENTEU)))
  ) |>
  filter(!is.na(date), !is.na(brent_bbl)) |>
  arrange(date)

latest_brent <- brent_hist$brent_bbl[which.max(brent_hist$date)]

forward_comparison <- short_term_proj |>
  select(date, r3_lsfo_bbl = projected_lsfo_bbl) |>
  left_join(
    tibble(date = forward_months, r1_cf_bbl = r1_intercept + r1_slope * fwd_cf_pred$pred),
    by = "date"
  ) |>
  mutate(
    # LSFO in $/MMBtu
    r3_lsfo_mmbtu  = r3_lsfo_bbl  / LSFO_MMBTU_PER_BBL,
    r1_cf_mmbtu    = r1_cf_bbl    / LSFO_MMBTU_PER_BBL,
    # Savings per MMBtu (positive = current contract cheaper)
    savings_mmbtu  = r1_cf_mmbtu - r3_lsfo_mmbtu,
    # LNG at the same Brent used to generate the LSFO projection (proj_brent).
    # Using latest_brent here would create a stale-vs-current mismatch if the
    # projection CSV was generated on a different day than policy_analysis.R runs.
    brent_for_lng  = proj_brent,
    lng_allin_04_mmbtu = LNG_SLOPE * brent_for_lng + LNG_INTERCEPT + LNG_REGAS_04,
    lng_allin_10_mmbtu = LNG_SLOPE * brent_for_lng + LNG_INTERCEPT + LNG_REGAS_10
  )

# ── Futures-curve forward comparison (if available) ────────────────────────────

futures_path <- file.path(base_dir, "crude_futures_curve.csv")
futures_comparison <- NULL

if (file.exists(futures_path)) {
  futures_curve <- fread(futures_path) |>
    as_tibble() |>
    transmute(
      contract_month = as.Date(contract_month),
      brent_bbl      = as.numeric(brent_bbl),
      wti_bbl        = as.numeric(wti_bbl)
    ) |>
    filter(!is.na(contract_month), !is.na(brent_bbl)) |>
    arrange(contract_month)

  long_term_path <- file.path(base_dir, "heco_lsfo_long_term_projection.csv")
  if (file.exists(long_term_path)) {
    long_term_proj <- fread(long_term_path) |>
      as_tibble() |>
      mutate(contract_month = as.Date(contract_month))

    # Build R1 counterfactual for futures months using the long-run VLSFO bridge.
    # For steady-state comparison: use long_run_curve to find R1 price at each
    # Brent level (linear interpolation).
    r1_fn <- approxfun(long_run_curve$p, long_run_curve$r1_lsfo_bbl, rule = 2)
    r3_fn <- approxfun(long_run_curve$p, long_run_curve$r3_lsfo_bbl, rule = 2)

    futures_base <- futures_curve |>
      filter(contract_month > max(short_term_proj$date))

    if ("projected_lsfo_bbl" %in% names(long_term_proj)) {
      futures_base <- futures_base |>
        left_join(
          long_term_proj |> select(contract_month, projected_lsfo_bbl),
          by = "contract_month"
        ) |>
        mutate(r3_lsfo_bbl = coalesce(projected_lsfo_bbl, r3_fn(brent_bbl))) |>
        select(-projected_lsfo_bbl)
    } else {
      futures_base <- futures_base |> mutate(r3_lsfo_bbl = r3_fn(brent_bbl))
    }

    futures_comparison <- futures_base |>
      mutate(
        r1_cf_bbl          = r1_fn(brent_bbl),
        r3_lsfo_mmbtu      = r3_lsfo_bbl  / LSFO_MMBTU_PER_BBL,
        r1_cf_mmbtu        = r1_cf_bbl    / LSFO_MMBTU_PER_BBL,
        savings_mmbtu      = r1_cf_mmbtu - r3_lsfo_mmbtu,
        lng_allin_04_mmbtu = LNG_SLOPE * brent_bbl + LNG_INTERCEPT + LNG_REGAS_04,
        lng_allin_10_mmbtu = LNG_SLOPE * brent_bbl + LNG_INTERCEPT + LNG_REGAS_10
      )
  }
}

# ── AEO/STEO alternative Brent path (if available) ────────────────────────────

aeo_path <- file.path(base_dir, "brent_nominal_real_path_adjusted_to_steo2026.csv")
aeo_comparison <- NULL

if (file.exists(aeo_path)) {
  aeo <- fread(aeo_path) |>
    as_tibble() |>
    mutate(date = as.Date(paste0(year, "-01-01"))) |>
    filter(!is.na(year))

  # Column is "adjusted_nominal_brent" in current aeo_brent_adjust.R output
  brent_col <- intersect(
    c("adjusted_nominal_brent", "adjusted_brent_nominal", "adjusted_brent_real"),
    names(aeo)
  )[1]
  if (is.na(brent_col)) brent_col <- names(aeo)[2]
  r1_fn <- approxfun(long_run_curve$p, long_run_curve$r1_lsfo_bbl, rule = 2)
  r3_fn <- approxfun(long_run_curve$p, long_run_curve$r3_lsfo_bbl, rule = 2)

  aeo_comparison <- aeo |>
    mutate(
      brent_bbl          = as.numeric(.data[[brent_col]]),
      r3_lsfo_mmbtu      = r3_fn(brent_bbl) / LSFO_MMBTU_PER_BBL,
      r1_cf_mmbtu        = r1_fn(brent_bbl) / LSFO_MMBTU_PER_BBL,
      lng_allin_04_mmbtu = LNG_SLOPE * brent_bbl + LNG_INTERCEPT + LNG_REGAS_04,
      lng_allin_10_mmbtu = LNG_SLOPE * brent_bbl + LNG_INTERCEPT + LNG_REGAS_10
    )
}

# ── FRED LNG regression alternative (if available) ────────────────────────────

lng_ma_path <- file.path(base_dir, "lng_brent_optimal_ma_coefficients.csv")
lng_fn_04 <- NULL
lng_fn_10 <- NULL

if (file.exists(lng_ma_path)) {
  lng_ma <- fread(lng_ma_path) |> as_tibble()
  # Expect columns: sample, intercept, slope (from lng_brent_fred_regression_test.R)
  # Use the "excluding_spike" sample as the more robust estimate.
  row <- lng_ma[grep("exclud", tolower(lng_ma$sample)), ][1, ]
  # Column is slope_ma (moving-average slope on Brent) and intercept
  if (nrow(row) && !is.na(row$slope_ma) && !is.na(row$intercept)) {
    lng_intercept_ma <- as.numeric(row$intercept)
    lng_slope_ma     <- as.numeric(row$slope_ma)
    # Convert from 2024$ real to nominal: use a simple deflator multiplier
    # (the FRED regression was in 2024$ real; for nominal comparison apply ~1.0
    # since recent years are near 2024 price level).
    # Commodity-only function (not shown directly; used for adder calculations).
    lng_fn_commodity <- function(brent_nominal) lng_intercept_ma + lng_slope_ma * brent_nominal
    # All-in functions including regasification adders (the only realistic comparison).
    lng_fn_04 <- function(brent_nominal) lng_fn_commodity(brent_nominal) + LNG_REGAS_04
    lng_fn_10 <- function(brent_nominal) lng_fn_commodity(brent_nominal) + LNG_REGAS_10
  }
}

# ── Q3: Spot comparison at current Brent ──────────────────────────────────────

spot_brent <- latest_brent

# LSFO under current contract at spot Brent (from long-run curve, steady state).
r3_fn    <- approxfun(long_run_curve$p, long_run_curve$r3_lsfo_bbl, rule = 2)
r1_fn    <- approxfun(long_run_curve$p, long_run_curve$r1_lsfo_bbl, rule = 2)

spot_comparison <- tibble(
  scenario = c(
    "LSFO – current contract (R3)",
    "LSFO – old contract (R1 counterfactual)",
    "LNG – HSEO/FGE all-in 0.4 mtpa",
    "LNG – HSEO/FGE all-in 1.0 mtpa"
  ),
  brent_bbl  = spot_brent,
  price_mmbtu = c(
    r3_fn(spot_brent) / LSFO_MMBTU_PER_BBL,
    r1_fn(spot_brent) / LSFO_MMBTU_PER_BBL,
    LNG_SLOPE * spot_brent + LNG_INTERCEPT + LNG_REGAS_04,
    LNG_SLOPE * spot_brent + LNG_INTERCEPT + LNG_REGAS_10
  )
)

if (!is.null(lng_fn_04)) {
  spot_comparison <- bind_rows(
    spot_comparison,
    tibble(
      scenario    = c("LNG – FRED MA all-in 0.4 mtpa (ex. 2022 spike)",
                      "LNG – FRED MA all-in 1.0 mtpa (ex. 2022 spike)"),
      brent_bbl   = spot_brent,
      price_mmbtu = c(lng_fn_04(spot_brent), lng_fn_10(spot_brent))
    )
  )
}

# ── Print summaries ────────────────────────────────────────────────────────────

cat("\n=== Q1: Historical savings since contract change ===\n")
cat(sprintf(
  "Period: %s to %s\n",
  format(min(historical_savings$date), "%b %Y"),
  format(max(historical_savings$date), "%b %Y")
))
cat(sprintf(
  "Total cumulative savings (post-break): $%.1fM\n",
  tail(historical_savings$cumulative_savings_usd, 1) / 1e6
))
cat(sprintf(
  "Average monthly savings: $%.2fM / month\n",
  mean(historical_savings$monthly_savings_usd, na.rm = TRUE) / 1e6
))
cat(sprintf(
  "Average savings per barrel: $%.2f/bbl  (= $%.2f/MMBtu)\n",
  mean(historical_savings$savings_per_bbl, na.rm = TRUE),
  mean(historical_savings$savings_per_mmbtu, na.rm = TRUE)
))

pre_inv_effect <- sum(pre_break_effect$inventory_effect_usd, na.rm = TRUE)
cat(sprintf(
  "\nPre-break inventory wind-down effect (R1 actual vs R1 steady-state w2=66):\n"
))
cat(sprintf(
  "  Total cumulative effect: $%.1fM  (negative = wind-down SAVED customers money)\n",
  pre_inv_effect / 1e6
))

cat("\n=== Q2: Forward savings (next 12 months, flat crude) ===\n")
cat(sprintf(
  "Brent used in LSFO projection: $%.2f/bbl%s\n",
  proj_brent,
  if (abs(proj_brent - latest_brent) > 5)
    sprintf(" (NOTE: current FRED Brent = $%.2f — re-run heco_lsfo_crude_projection.R to refresh)", latest_brent)
  else ""
))
cat(sprintf(
  "Projected total savings vs old contract: $%.1fM over %d months\n",
  sum(forward_comparison$savings_mmbtu * LSFO_MMBTU_PER_BBL *
        mean(fuel_use$estimated_barrels[fuel_use$date >= as.Date("2024-01-01")], na.rm = TRUE),
      na.rm = TRUE) / 1e6,
  nrow(forward_comparison)
))

cat("\n=== Q3: Spot price comparison at Brent = $", round(spot_brent, 1), "/bbl ===\n")
print(spot_comparison |> mutate(price_mmbtu = round(price_mmbtu, 2)))

# ── Save outputs ───────────────────────────────────────────────────────────────

hist_out_path    <- file.path(base_dir, "policy_historical_savings.csv")
fwd_out_path     <- file.path(base_dir, "policy_forward_comparison.csv")
spot_out_path    <- file.path(base_dir, "policy_spot_comparison.csv")

fwrite(as.data.table(historical_savings), hist_out_path)
fwrite(as.data.table(forward_comparison), fwd_out_path)
fwrite(as.data.table(spot_comparison),    spot_out_path)

cat("Saved:", hist_out_path, "\n")
cat("Saved:", fwd_out_path,  "\n")
cat("Saved:", spot_out_path, "\n")

# ── Plots ──────────────────────────────────────────────────────────────────────

plot_and_save <- function(p, path, width = 10, height = 6) {
  ggsave(path, plot = p, width = width, height = height, dpi = 160)
  cat("Saved:", path, "\n")
}

# -- Plot 1: Cumulative historical savings ------------------------------------

p_savings <- ggplot(historical_savings, aes(x = date, y = cumulative_savings_usd / 1e6)) +
  geom_area(fill = "#1b7837", alpha = 0.25) +
  geom_line(color = "#1b7837", linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = label_dollar(suffix = "M")) +
  labs(
    title    = "Cumulative HECO customer savings from 2024 contract renegotiation",
    subtitle = sprintf(
      "Comparison: actual prices vs. old contract (R1 parameters, w2 = %d days) applied to same VLSFO market\nThrough %s",
      inv_break1,
      format(max(historical_savings$date), "%B %Y")
    ),
    x = NULL,
    y = "Cumulative savings (USD millions)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 9))

plot_and_save(p_savings, file.path(base_dir, "policy_historical_savings.png"))

# -- Plot 2: Monthly savings with range (historical) --------------------------

p_monthly <- ggplot(historical_savings, aes(x = date)) +
  geom_col(aes(y = savings_per_mmbtu), fill = "#1b7837", alpha = 0.7, width = 25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title    = "Monthly savings per MMBtu: current vs. old contract",
    subtitle = "Positive = current contract cheaper than old-contract counterfactual",
    x = NULL,
    y = "Savings ($/MMBtu)"
  ) +
  theme_minimal(base_size = 12)

plot_and_save(p_monthly, file.path(base_dir, "policy_monthly_savings.png"))

# -- Plot 3: Forward price comparison ($/MMBtu) --------------------------------

fwd_base_cols <- c("date", "r3_lsfo_mmbtu", "r1_cf_mmbtu",
                   "lng_allin_04_mmbtu", "lng_allin_10_mmbtu")

fwd_compare_aug <- forward_comparison
if (!is.null(lng_fn_04)) {
  fwd_compare_aug <- fwd_compare_aug |>
    mutate(
      lng_fred_04_mmbtu = lng_fn_04(brent_for_lng),
      lng_fred_10_mmbtu = lng_fn_10(brent_for_lng)
    )
  fwd_base_cols <- c(fwd_base_cols, "lng_fred_04_mmbtu", "lng_fred_10_mmbtu")
}

fwd_long <- fwd_compare_aug |>
  select(all_of(fwd_base_cols)) |>
  pivot_longer(-date, names_to = "series", values_to = "price_mmbtu") |>
  mutate(series = recode(series,
    "r3_lsfo_mmbtu"       = "LSFO – current contract",
    "r1_cf_mmbtu"         = "LSFO – old contract (R1)",
    "lng_allin_04_mmbtu"  = "LNG – HSEO all-in 0.4 mtpa",
    "lng_allin_10_mmbtu"  = "LNG – HSEO all-in 1.0 mtpa",
    "lng_fred_04_mmbtu"   = "LNG – FRED MA all-in 0.4 mtpa",
    "lng_fred_10_mmbtu"   = "LNG – FRED MA all-in 1.0 mtpa"
  )) |>
  filter(!is.na(price_mmbtu))

series_colors <- c(
  "LSFO – current contract"        = "#1b7837",
  "LSFO – old contract (R1)"       = "#74c476",
  "LNG – HSEO all-in 0.4 mtpa"    = "#6a3d9a",
  "LNG – HSEO all-in 1.0 mtpa"    = "#9e9ac8",
  "LNG – FRED MA all-in 0.4 mtpa" = "#d95f02",
  "LNG – FRED MA all-in 1.0 mtpa" = "#f4a442"
)
series_lty <- c(
  "LSFO – current contract"        = "solid",
  "LSFO – old contract (R1)"       = "dashed",
  "LNG – HSEO all-in 0.4 mtpa"    = "solid",
  "LNG – HSEO all-in 1.0 mtpa"    = "solid",
  "LNG – FRED MA all-in 0.4 mtpa" = "dashed",
  "LNG – FRED MA all-in 1.0 mtpa" = "dashed"
)

p_fwd <- ggplot(fwd_long, aes(x = date, y = price_mmbtu, color = series, linetype = series)) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = series_colors) +
  scale_linetype_manual(values = series_lty) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title    = "12-month forward fuel cost comparison (flat crude scenario)",
    subtitle = sprintf(
      "Brent = $%.2f/bbl (flat-crude assumption used in LSFO projection); WTI = Brent assumed",
      proj_brent
    ),
    x = NULL,
    y = "All-in cost ($/MMBtu)",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

plot_and_save(p_fwd, file.path(base_dir, "policy_forward_comparison.png"), width = 11)

# -- Plot 4: Long-run curve comparison (LSFO vs LNG over Brent range) ----------

brent_grid <- seq(40, 160, by = 1)
lr_compare <- tibble(
  brent_bbl     = brent_grid,
  r3_lsfo_mmbtu = r3_fn(brent_grid) / LSFO_MMBTU_PER_BBL,
  r1_cf_mmbtu   = r1_fn(brent_grid) / LSFO_MMBTU_PER_BBL,
  lng_allin_04  = LNG_SLOPE * brent_grid + LNG_INTERCEPT + LNG_REGAS_04,
  lng_allin_10  = LNG_SLOPE * brent_grid + LNG_INTERCEPT + LNG_REGAS_10
)

if (!is.null(lng_fn_04)) {
  lr_compare$lng_fred_04 <- lng_fn_04(brent_grid)
  lr_compare$lng_fred_10 <- lng_fn_10(brent_grid)
}

lr_long <- lr_compare |>
  pivot_longer(-brent_bbl, names_to = "series", values_to = "price_mmbtu") |>
  mutate(series = recode(series,
    "r3_lsfo_mmbtu" = "LSFO – current contract",
    "r1_cf_mmbtu"   = "LSFO – old contract (R1)",
    "lng_allin_04"  = "LNG – HSEO all-in 0.4 mtpa",
    "lng_allin_10"  = "LNG – HSEO all-in 1.0 mtpa",
    "lng_fred_04"   = "LNG – FRED MA all-in 0.4 mtpa",
    "lng_fred_10"   = "LNG – FRED MA all-in 1.0 mtpa"
  ))

colors_lr <- c(
  "LSFO – current contract"    = "#1b7837",
  "LSFO – old contract (R1)"   = "#74c476",
  "LNG – HSEO all-in 0.4 mtpa" = "#6a3d9a",
  "LNG – HSEO all-in 1.0 mtpa" = "#9e9ac8",
  "LNG – FRED MA all-in 0.4 mtpa" = "#d95f02",
  "LNG – FRED MA all-in 1.0 mtpa" = "#f4a442"
)

p_lr <- ggplot(lr_long, aes(x = brent_bbl, y = price_mmbtu, color = series)) +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = spot_brent, linetype = "dotted", color = "grey40") +
  annotate("text", x = spot_brent + 1, y = max(lr_long$price_mmbtu, na.rm = TRUE) * 0.97,
           label = sprintf("Current\nBrent\n$%.0f", spot_brent),
           hjust = 0, size = 3, color = "grey40") +
  scale_color_manual(values = colors_lr, na.value = "grey70") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title    = "Long-run fuel cost: LSFO (current contract) vs. LNG scenarios",
    subtitle = "LSFO via Brent→VLSFO→HECO contract model (steady state, WTI = Brent)\nLNG: HSEO/FGE indicative contract (Aug 2024) ± regasification adders",
    x = "Brent crude ($/bbl)",
    y = "All-in fuel cost ($/MMBtu)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

plot_and_save(p_lr, file.path(base_dir, "policy_lsfo_vs_lng_longrun.png"), width = 11)

# -- Plot 5: Spot comparison bar chart -----------------------------------------

p_spot <- ggplot(spot_comparison,
                 aes(x = reorder(scenario, -price_mmbtu), y = price_mmbtu,
                     fill = grepl("LSFO", scenario))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("$%.2f", price_mmbtu)),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1b7837", "FALSE" = "#6a3d9a")) +
  scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "All-in fuel cost comparison at current Brent price",
    subtitle = sprintf("Brent = $%.2f/bbl (latest daily close)", spot_brent),
    x = NULL,
    y = "$/MMBtu"
  ) +
  theme_minimal(base_size = 12)

plot_and_save(p_spot, file.path(base_dir, "policy_spot_comparison.png"), width = 10, height = 5)

cat("\nPolicy analysis complete.\n")
