## pv_savings_sketch.R
## Present-value sketch of fuel cost savings under LNG vs. LSFO
## for the UHERO brief.  Uses HSEO generation mix scenarios and the
## AEO-adjusted Brent price path already in this directory.
##
## Assumptions:
##   Heat rate – existing oil steam turbines:    11 MMBtu/MWh  (conservative)
##   Heat rate – new LNG CCGT:                    6.5 MMBtu/MWh
##   LNG price (Brent-indexed, 0.4 mtpa):        0.118 * Brent + 0.60 + 3.93
##   LNG price (HH-indexed, AEO HH path):        1.15 * HH    + 2.75 + 1.50 + 3.93
##   LSFO price:                                 long-run R3 curve ($/bbl) / 6.22
##   Discount rate:                              5% (real, from 2026)
##   Contract operational window:               2029–2044

library(data.table)
library(dplyr)

base_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) character(0)
)
if (length(base_dir) == 0 || !nchar(base_dir)) base_dir <- getwd()

# ── 1. Brent price path ────────────────────────────────────────────────────
brent_path <- fread(file.path(base_dir, "brent_nominal_real_path_adjusted_to_steo2026.csv")) |>
  as_tibble() |>
  transmute(year, brent = adjusted_nominal_brent)

# AEO Henry Hub: EIA AEO 2026 reference case (approximate; no local file).
# Values from EIA AEO 2026 Table A13 (nominal $/MMBtu).
hh_aeo <- tibble(
  year = 2024:2050,
  hh   = c(2.22, 3.29, 3.44, 3.56, 3.69, 3.83, 3.93, 4.03, 4.14, 4.25,
            4.36, 4.47, 4.57, 4.68, 4.80, 4.93, 5.06, 5.19, 5.32, 5.44,
            5.57, 5.70, 5.83, 5.95, 6.07, 6.19, 6.31)
)

# ── 2. Long-run LSFO curve ─────────────────────────────────────────────────
curve <- fread(file.path(base_dir, "heco_lsfo_long_run_curve.csv")) |>
  as_tibble() |>
  transmute(brent_p = p, lsfo_bbl = heco_lsfo_bbl)

lsfo_fn <- approxfun(curve$brent_p, curve$lsfo_bbl, rule = 2)

MMBtu_per_bbl_lsfo <- 6.22   # residual fuel oil energy content

# ── 3. LNG price functions ($/MMBtu) ─────────────────────────────────────
REGAS_04   <- 3.93          # 0.4 mtpa regasification adder
SHIP       <- 1.50          # approximate shipping cost
LIQUEF     <- 2.75          # typical liquefaction fee
LNG_SLOPE_BRENT <- 0.118
LNG_INT_BRENT   <- 0.60

lng_brent_fn <- function(brent) LNG_SLOPE_BRENT * brent + LNG_INT_BRENT + REGAS_04
lng_hh_fn    <- function(hh)    1.15 * hh + LIQUEF + SHIP + REGAS_04

# ── 4. HSEO generation scenarios (GWh) ────────────────────────────────────
hseo_lng <- fread(file.path(base_dir, "hseo_lng.csv")) |>
  as_tibble() |>
  rename_with(trimws) |>
  transmute(year = Year, oil_gwh = `Oil`, lng_gwh = `LNG`)

hseo_oil_raw <- fread(file.path(base_dir, "hseo_oil.csv")) |>
  as_tibble() |>
  rename_with(trimws)
hseo_oil <- hseo_oil_raw |>
  transmute(year = as.integer(hseo_oil_raw[[1]]), oil_gwh_base = `Oil`)

# ── 5. Build annual fuel-cost comparison ──────────────────────────────────
HR_OIL <- 11.0   # MMBtu/MWh, existing steam turbines
HR_LNG <-  6.5   # MMBtu/MWh, new CCGT

analysis <- hseo_lng |>
  inner_join(hseo_oil,  by = "year") |>
  inner_join(brent_path, by = "year") |>
  left_join(hh_aeo,     by = "year") |>
  filter(year >= 2029, year <= 2044) |>
  mutate(
    lsfo_mmbtu  = lsfo_fn(brent) / MMBtu_per_bbl_lsfo,   # $/MMBtu

    # Cost per MWh ($/MWh)
    oil_cost_mwh  = lsfo_mmbtu * HR_OIL,
    lng_cost_mwh_brent = lng_brent_fn(brent) * HR_LNG,
    lng_cost_mwh_hh    = lng_hh_fn(hh)       * HR_LNG,

    # GWh replaced: difference in oil generation between scenarios
    oil_replaced_gwh = oil_gwh_base - oil_gwh,

    # Annual fuel cost — base (all oil)
    fuel_cost_base_M = oil_gwh_base * 1e3 * oil_cost_mwh / 1e6,

    # Annual fuel cost — LNG scenario (remaining oil + LNG)
    fuel_cost_lng_brent_M = (oil_gwh * 1e3 * oil_cost_mwh +
                              lng_gwh * 1e3 * lng_cost_mwh_brent) / 1e6,
    fuel_cost_lng_hh_M    = (oil_gwh * 1e3 * oil_cost_mwh +
                              lng_gwh * 1e3 * lng_cost_mwh_hh)    / 1e6,

    # Annual savings (positive = LNG saves money)
    savings_brent_M = fuel_cost_base_M - fuel_cost_lng_brent_M,
    savings_hh_M    = fuel_cost_base_M - fuel_cost_lng_hh_M,

    # Discount factor (from end of 2026)
    disc = 1 / (1.05 ^ (year - 2026)),

    # Present-value savings
    pv_brent_M = savings_brent_M * disc,
    pv_hh_M    = savings_hh_M    * disc
  )

# ── 6. Summary table ───────────────────────────────────────────────────────
print(analysis |>
  select(year, brent, oil_replaced_gwh, lng_gwh,
         lsfo_mmbtu, fuel_cost_base_M,
         savings_brent_M, savings_hh_M,
         pv_brent_M, pv_hh_M),
  n = 16)

cat("\n── Present-value totals (2026 base, 5% discount) ──\n")
cat(sprintf("  PV of fuel savings – Brent-indexed LNG:    $%.1f billion\n",
            sum(analysis$pv_brent_M) / 1e3))
cat(sprintf("  PV of fuel savings – HH-indexed LNG:       $%.1f billion\n",
            sum(analysis$pv_hh_M) / 1e3))
cat(sprintf("  Undiscounted total – Brent-indexed:        $%.1f billion\n",
            sum(analysis$savings_brent_M) / 1e3))
cat(sprintf("  Undiscounted total – HH-indexed:           $%.1f billion\n",
            sum(analysis$savings_hh_M) / 1e3))
cat(sprintf("  Total investment (JERA proposal):          $2.0 billion\n"))
cat(sprintf("  Net PV (Brent-indexed):                    $%.1f billion\n",
            (sum(analysis$pv_brent_M) - 2000) / 1e3))
cat(sprintf("  Net PV (HH-indexed):                       $%.1f billion\n",
            (sum(analysis$pv_hh_M) - 2000) / 1e3))

# ── 7. IGP scaling ─────────────────────────────────────────────────────────
# Oil+LNG averages from oil_lng_avg_oil_plus_lng_load_1931_2044.csv:
#   igp_pref: 207 MW average  = 1,813 GWh/year
#   igp_lc:   308 MW average  = 2,698 GWh/year
# HSEO LNG scenario average thermal (2029-2044):
hseo_avg_thermal <- mean(analysis$oil_replaced_gwh + analysis$lng_gwh)
cat(sprintf("\nHSEO LNG avg thermal replaced (2029-2044): %.0f GWh/yr\n", hseo_avg_thermal))

igp_pref_gwh <- 207 * 8.76    # 1,813 GWh
igp_lc_gwh   <- 308 * 8.76    # 2,698 GWh

scale_pref <- igp_pref_gwh / hseo_avg_thermal
scale_lc   <- igp_lc_gwh   / hseo_avg_thermal

cat(sprintf("IGP preferred scale factor vs HSEO:        %.2f\n", scale_pref))
cat(sprintf("IGP low-carbon scale factor vs HSEO:       %.2f\n", scale_lc))
cat(sprintf("PV savings – IGP preferred (Brent):        $%.1f billion\n",
            sum(analysis$pv_brent_M) * scale_pref / 1e3))
cat(sprintf("PV savings – IGP low-carbon (Brent):       $%.1f billion\n",
            sum(analysis$pv_brent_M) * scale_lc / 1e3))

# ── 8. Sensitivity: Brent stays near current ($100) vs AEO path ───────────
analysis_high_brent <- analysis |>
  mutate(
    brent = 100,
    lsfo_mmbtu = lsfo_fn(100) / MMBtu_per_bbl_lsfo,
    oil_cost_mwh = lsfo_mmbtu * HR_OIL,
    lng_cost_mwh_brent = lng_brent_fn(100) * HR_LNG,
    fuel_cost_base_M = oil_gwh_base * 1e3 * oil_cost_mwh / 1e6,
    fuel_cost_lng_brent_M = (oil_gwh * 1e3 * oil_cost_mwh +
                              lng_gwh * 1e3 * lng_cost_mwh_brent) / 1e6,
    savings_brent_M = fuel_cost_base_M - fuel_cost_lng_brent_M,
    pv_brent_M = savings_brent_M * disc
  )

analysis_low_brent <- analysis |>
  mutate(
    brent = 70,
    lsfo_mmbtu = lsfo_fn(70) / MMBtu_per_bbl_lsfo,
    oil_cost_mwh = lsfo_mmbtu * HR_OIL,
    lng_cost_mwh_brent = lng_brent_fn(70) * HR_LNG,
    fuel_cost_base_M = oil_gwh_base * 1e3 * oil_cost_mwh / 1e6,
    fuel_cost_lng_brent_M = (oil_gwh * 1e3 * oil_cost_mwh +
                              lng_gwh * 1e3 * lng_cost_mwh_brent) / 1e6,
    savings_brent_M = fuel_cost_base_M - fuel_cost_lng_brent_M,
    pv_brent_M = savings_brent_M * disc
  )

cat("\n── Brent sensitivity (HSEO volumes, Brent-indexed LNG) ──\n")
cat(sprintf("  Flat Brent $70/bbl:    PV = $%.1f billion\n",
            sum(analysis_low_brent$pv_brent_M) / 1e3))
cat(sprintf("  AEO reference path:    PV = $%.1f billion\n",
            sum(analysis$pv_brent_M) / 1e3))
cat(sprintf("  Flat Brent $100/bbl:   PV = $%.1f billion\n",
            sum(analysis_high_brent$pv_brent_M) / 1e3))

# ── 9. Futures-based Brent path (Investing.com, March 31, 2026) ────────────
# Annual averages: 2026 uses realized FRED daily (Jan $66.60, Feb $70.89, Mar $98.00)
# + futures (Apr $112.85, May-Dec from curve). 2027-2033 from futures annual avg.
# 2033 partial (Jan-Mar): ~$69.2. Hold flat at $69 for 2033-2044.
brent_futures_annual <- tibble(
  year  = 2026:2044,
  brent = c(
    92.50,  # 2026: realized Jan-Mar + futures Apr-Dec avg
    78.62,  # 2027
    75.06,  # 2028
    73.38,  # 2029
    71.90,  # 2030
    70.74,  # 2031
    69.77,  # 2032
    rep(69.0, 12)  # 2033-2044: extrapolated flat at long-run futures level
  )
)

pv_recompute <- function(brent_path, data_base) {
  data_base |>
    select(year, oil_gwh, lng_gwh, oil_gwh_base, hh) |>
    inner_join(brent_path, by = "year") |>
    mutate(
      lsfo_mmbtu         = lsfo_fn(brent) / MMBtu_per_bbl_lsfo,
      oil_cost_mwh       = lsfo_mmbtu * HR_OIL,
      lng_cost_mwh_brent = lng_brent_fn(brent) * HR_LNG,
      fuel_cost_base_M   = oil_gwh_base * 1e3 * oil_cost_mwh / 1e6,
      fuel_cost_lng_M    = (oil_gwh * 1e3 * oil_cost_mwh +
                             lng_gwh * 1e3 * lng_cost_mwh_brent) / 1e6,
      savings_M          = fuel_cost_base_M - fuel_cost_lng_M,
      disc               = 1 / (1.05 ^ (year - 2026)),
      pv_M               = savings_M * disc
    )
}

analysis_futures <- pv_recompute(brent_futures_annual, analysis)

cat("\n── Futures-implied Brent path (Investing.com March 31, 2026) ──\n")
cat(sprintf("  Futures annual Brent 2029-2044 (avg):   $%.1f/bbl\n",
            mean(brent_futures_annual$brent[brent_futures_annual$year >= 2029])))
cat(sprintf("  PV of fuel savings – Brent-indexed LNG: $%.2f billion\n",
            sum(analysis_futures$pv_M) / 1e3))
cat(sprintf("  Net PV vs $2B investment:               $%.2f billion\n",
            (sum(analysis_futures$pv_M) - 2000) / 1e3))
cat(sprintf("  IGP preferred volumes (futures path):   $%.2f billion\n",
            sum(analysis_futures$pv_M) * scale_pref / 1e3))

cat("\n── Futures path by year (2029-2044) ──\n")
print(analysis_futures |>
  select(year, brent, lsfo_mmbtu, savings_M, disc, pv_M),
  n = 16)
