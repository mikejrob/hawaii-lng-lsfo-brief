# Hawaii LNG vs. LSFO Fuel Cost Analysis

Replication code and data for:

> Roberts, M.J. (2026). *Hawaii's Fuel Cost Problem: What the LSFO–LNG Price Comparison Really Shows.* UHERO Brief, University of Hawaiʻi Economic Research Organization.

The brief analyzes Hawaiian Electric's low-sulfur fuel oil (LSFO) supply contract — including a consequential 2024 renegotiation — and compares long-run LSFO costs against LNG alternatives and solar+battery under the two HECO Integrated Grid Plan scenarios.

---

## Repository Contents

```
.
├── data inputs (CSV)          raw price series and scenario files
├── R scripts                  analysis and figure-generation code
├── brief_figures/             all figures in the published brief (PNG + PDF)
├── graphs/                    exploratory plots from earlier analysis stages
├── old/                       earlier draft scripts and figures
├── PLEXOS_24-10-29 .../       HSEO/HECO generation-mix scenario exports
├── uhero_brief_lng_lsfo.md    the brief (Markdown source)
└── uhero_brief_lng_lsfo.pdf   compiled PDF
```

---

## Data Sources

| File | Source | Notes |
|------|--------|-------|
| `fred_poilbreusdm_brent_global.csv` | [FRED](https://fred.stlouisfed.org/series/POILBREUSDM) | Monthly Brent crude ($/bbl) |
| `fred_dcoilbrenteu_daily_brent.csv` | [FRED](https://fred.stlouisfed.org/series/DCOILBRENTEU) | Daily Brent crude ($/bbl) |
| `fred_dcoilwtico_daily_wti.csv` | [FRED](https://fred.stlouisfed.org/series/DCOILWTICO) | Daily WTI crude ($/bbl) |
| `fred_mhhngsp_henry_hub.csv` | [FRED](https://fred.stlouisfed.org/series/MHHNGSP) | Monthly Henry Hub ($/MMBtu) |
| `fred_pngasjpusdm_lng_japan.csv` | [FRED](https://fred.stlouisfed.org/series/PNGASJPUSDM) | Japan LNG import price ($/MMBtu) |
| `fred_cpiaucsl.csv` | [FRED](https://fred.stlouisfed.org/series/CPIAUCSL) | CPI for real-price conversions |
| `vlsfo_prices.csv` | Ship & Bunker | Singapore VLSFO spot price ($/MT) |
| `brent_futures_curve.csv` | Investing.com | Brent futures curve, March 31, 2026 |
| `aeo_ref_brent_prices_to_2050.csv` | [EIA AEO 2026](https://www.eia.gov/outlooks/aeo/) | Reference-case Brent path |
| `brent_nominal_real_path_adjusted_to_steo2026.csv` | UHERO/EIA | AEO Brent recalibrated to STEO Feb 2026 |
| `heco_lsfo_long_run_curve.csv` | UHERO model | Steady-state LSFO cost vs. Brent (R3 contract) |
| `heco_lsfo_model_params.csv` | UHERO model | Estimated R1/R3 contract regime parameters |
| `policy_historical_savings.csv` | UHERO model | Monthly R3 vs. R1 savings (from EIA Form 923) |
| `policy_forward_comparison.csv` | UHERO model | Six-month forward LSFO vs. LNG cost path |
| `hseo_lng.csv` | HECO IGP / HSEO | Annual LNG + oil generation, LNG scenario (GWh) |
| `hseo_oil.csv` | HECO IGP / HSEO | Annual oil generation, baseline scenario (GWh) |
| `igp_oahu_net_demand_2020_2050_8760.csv` | HECO IGP | Hourly Oahu net demand, 2020–2050 |
| `oil_lng_avg_oil_plus_lng_load_1931_2044.csv` | UHERO | Average IGP oil+LNG dispatch, 2019–2044 |
| `PLEXOS_24-10-29 cost calculations MF/` | HECO / PLEXOS | Generation scenario tables (heat rates, costs) |

HECO LSFO purchase prices are drawn from EIA Form 923 public filings. The VLSFO-to-HECO-price relationship is estimated statistically from public data; exact contract terms are not disclosed.

---

## Scripts

Run scripts in the order listed. Each script's key outputs become inputs for later scripts.

| # | Script | What it does | Key outputs |
|---|--------|-------------|-------------|
| 1 | `aeo_brent_adjust.R` | Recalibrates AEO Brent price path to match STEO Feb 2026 | `brent_nominal_real_path_adjusted_to_steo2026.csv` |
| 2 | `vlsfo_brent_daily_link.R` | Fits VLSFO–Brent daily relationship (lag + MA structure) | `brent_vlsfo_daily_merged.csv`, model summaries |
| 3 | `heco_lsfo_vlsfo.R` | Estimates R1/R3 LSFO contract parameters; builds long-run curve | `heco_lsfo_model_params.csv`, `heco_lsfo_long_run_curve.csv` |
| 4 | `heco_lsfo_crude_projection.R` | Projects near-term LSFO costs from Brent futures | `heco_lsfo_short_term_monthly_projection.csv` |
| 5 | `lng_brent_fred_regression_test.R` | Tests LNG–Brent price relationships from FRED data | Regression summaries (`.txt`, `.csv`) |
| 6 | `policy_analysis.R` | Main policy analysis: R3 vs. R1 savings, forward projection | `policy_historical_savings.csv`, `policy_forward_comparison.csv` |
| 7 | `pv_savings_sketch.R` | Present-value fuel savings under IGP scenarios | Console output (PV tables); see Table 1 in brief |
| 8 | `oil_lng.R` | Generation mix plots for HSEO/IGP scenarios | `oahu_generation_mix_*.png` |
| 9 | `generate_brief_figures.R` | Generates Figures 1–6 for the brief | `brief_figures/fig1_*` … `fig6_*` (.png + .pdf) |
| 10 | `fig_fuel_cost_comparison.R` | Mixed-efficiency fuel cost comparison figure | `brief_figures/fig_fuel_cost_comparison.*` |
| 11 | `fig_fuel_cost_bands.R` | Two-panel fuel cost comparison — **Figure 7** in brief. Left panel: CCGT efficiency; right panel: CT efficiency. Each panel shows LSFO at steam and matched heat rates alongside LNG scenarios. Also generates single-panel band variants (opt1, opt2). | `brief_figures/fig_fuel_cost_twopanel_doc.pdf` |

`plot_vlsfo_prices.R` and `lng_brent_fred_regression_test.R` are exploratory scripts and do not need to be run to reproduce the brief's main results.

---

## Physical Constants Used Throughout

| Quantity | Value | Source |
|----------|-------|--------|
| LSFO energy content | 6.22 MMBtu/bbl | Residual fuel oil standard |
| HECO steam turbine heat rate | 11.09 MMBtu/MWh | HECO reported (existing Kahe/Waiau units) |
| CCGT heat rate | 6.5 MMBtu/MWh | NREL ATB 2024 (new combined-cycle) |
| CT heat rate | 9.5 MMBtu/MWh | NREL ATB 2024 (simple-cycle combustion turbine) |
| LNG Brent-indexed formula | 0.118 × Brent + $0.60/MMBtu | HSEO/FGE August 2024 indicative contract |
| FSRU regasification (0.4 mtpa, full) | $3.93/MMBtu | HSEO/FGE August 2024 |
| FSRU regasification (1.0 mtpa, full) | $1.68/MMBtu | HSEO/FGE August 2024 |

---

## R Package Requirements

```r
install.packages(c("data.table", "dplyr", "ggplot2", "tidyr",
                   "lubridate", "scales", "zoo"))
```

---

## License

MIT License. See `LICENSE` for details.

The brief itself (`.md` and `.pdf`) is © 2026 UHERO, University of Hawaiʻi. All rights reserved.
