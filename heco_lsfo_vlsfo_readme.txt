What heco_lsfo_vlsfo.R Does

This script reconstructs HECO's monthly LSFO purchase price using public VLSFO market indices. The working idea is that HECO's reported price behaves like a nested moving average:

- a contract-price average over recent published VLSFO quotes
- combined with an inventory-like carryover window that delays how market prices appear in HECO's reported cost

The contract appears to have changed in 2024. The script therefore explores models with separate pre-change, transition, and post-change regimes, then ends with a streamlined final specification that is fast to run and easy to interpret.

Core interpretation

HECO does not buy fuel at the spot price of a single day. Instead, the reported monthly price appears to reflect:

- lagged contract averaging over market quotes
- cost carryover through inventory
- a contract reset in 2024 that changed both the effective index and the speed of pass-through

The script is trying to reverse-engineer those mechanics from public data.

Part 1 — Grid Search for the Best Predictor
Input data:

heco_lsfo.csv — HECO's actual reported monthly LSFO price ($/BBL), starting Feb 2022
vlsfo_prices.csv — daily VLSFO spot prices from Singapore, Houston, and a global 20-port average
The build_predictor() function constructs a two-layer nested moving average:

Layer 1 (w1 days): smooth the daily spot price into a "contract price" series
Layer 2 (w2 days): average that contract price over an inventory accumulation window, anchored to the 24th of the prior month (the presumed contract pricing date)
A grid search over w1 (1–90 days), w2 (60–150 days), and three price series (Singapore, global average, Singapore+Houston average) fits a simple OLS regression for each combination and ranks by R².

Part 2 — Residual Analysis (H1–H3)
Three hypotheses are tested to explain remaining error in the best model:

Model	Idea
H1	Add month-over-month change in predictor (rate-of-change effect)
H2	Allow a structural break — different intercept & slope before/after a searched break date
H3	Allow a structural break in the inventory window (w2 changes)
Part 3 — Blended Transition Model (H4)
Recognizing that HECO likely renegotiated its pricing contract around 2024 (switching from Singapore-only pricing to a Singapore+Houston average), the script models a gradual transition between regimes. Because inventory is a rolling average, the switch isn't instantaneous — each month's reported price is a blend of old- and new-regime pricing, weighted by how much of the inventory window falls before vs. after the contract change date. A grid search finds the optimal contract date.

Part 4 — Three-Regime Model (H12)
An important intermediate model divides the data into three regimes separated by two break dates (April 2024 and November 2024):

Regime	Period	Description
R1 Pre-contract	Jun 2023 – Mar 2024	Original contract, longer inventory window (w2=40)
R2 Transition	Apr 2024 – Oct 2024	New contract pricing, very short inventory window (w2=15)
R3 Post-contract	Nov 2024 – present	Settled new regime, medium inventory window (w2=35)
Each regime gets its own OLS coefficients. This materially improves fit and motivates the later local refinements.

Part 5 — R3 Timing And Inventory Recovery (H13–H14)
After H12, the script probes the remaining post-contract residual structure in two steps:

H13 tests whether the R3 misfit is partly a calendar-timing problem. It holds R1 and R2 fixed, then searches the R3 anchor day (prior-month day 1–31, prior-month end, and the first few days of the current month), along with a local refinement of w1 and w2. This checks whether HECO's actual billing/pricing anchor differs from the default 24th-of-prior-month assumption.

H14 then keeps R1 and R2 fixed, takes the H13-best R3 timing setup, and replaces the fixed R3 inventory window with a rebuilding path. Instead of one post-break w2, the model lets w2 start short immediately after the November 2024 break and rise linearly toward a longer steady-state level over several months. This is intended to capture inventory depletion before the contract reset and gradual inventory rebuilding afterward.

Part 6 — Pre- And Mid-Transition Inventory Drawdown (H15–H16)
The script then extends the same physical idea backward into the earlier regimes.

H15 revisits the pre-contract regime (R1). Rather than treating R1 as a single fixed-window process, it searches over the underlying index (including Singapore and alternative public proxies), the contract averaging length w1, and a linearly declining inventory window from the start of R1 to the end of R1. This is designed to test the hypothesis that HECO was still on a Singapore-linked contract before 2024, but was already drawing inventory down as the renegotiation approached.

H16 applies the same structure to the transition regime (R2). Holding the refined R1 and the H14-style R3 rebuild fixed, it searches for the best R2 index, w1, and a declining w2 path across the transition months. The result is a fully dynamic three-regime specification: inventory drawdown in R1, continued drawdown in R2, and rebuilding in R3.

Part 7 — Joint Local Refinement And Boundary Checks (H17)
After the first-pass regime-by-regime searches, the script performs a smaller joint search around the selected parameter values. This keeps the chosen index for each regime fixed, narrows the search to local windows around the first-pass w1 and w2 path endpoints, and jointly optimizes all three regimes at once. It then reports whether any winning parameter lies on the edge of its local search range. Those boundary flags indicate where the next refinement should widen the search window rather than simply tightening it further.

Part 8 — Singapore-Only Joint Refinement With Shared Inventory Handoffs (H18)
H18 is a physical-consistency check on H17. It forces all three regimes onto the Singapore series and replaces the regime-by-regime endpoint specification with a shared four-level inventory path:

pre-contract high inventory -> inventory at break1 -> inventory at break2 -> post-contract rebuilt inventory

Because the ending inventory of one regime is the starting inventory of the next, H18 rules out the implausible case where inventory appears to disappear at a break date. It is specifically designed to test whether the apparent R1-to-R2 inventory jump in H17 was an artifact of using the wrong market index rather than a real physical change in stock levels.

Part 9 — Transition Cost-Layer Release Model (H19)
H19 addresses a different possibility: the sharp statistical break may reflect a change in cost recognition rather than a sudden change in physical barrels. It keeps inventory physically continuous across regimes, but models R2 as a blend of two cost layers:

legacy layer = the R1 pricing rule carried forward into transition months
new layer    = the Singapore-based post-contract rule

The share on the legacy layer decays over a searched number of transition months, allowing the reported fuel-cost series to move faster than physical inventory alone would permit. H19 also broadens the R1 index choice, trying four raw candidates: global, Singapore, Houston, and Singapore/Houston average.

Part 10 — Smooth Inventory With Repricing Shocks (H20)
H20 moves away from the gradual release interpretation and tests a sharper accounting-reset story. Physical inventory is still forced to evolve smoothly across the regime boundaries, but the reported price is allowed to absorb transient repricing shocks at the break dates. The post-contract index is fixed to the Singapore/Houston average, while the pre-contract index is searched across the four candidate raw series. This lets the model separate two effects:

physical inventory carryover
administrative or accounting repricing of the recoverable fuel-cost pool

If H20 outperforms H19 materially, that supports the idea that the observed sharp regime breaks are driven more by reporting or contract-cost resets than by implausibly fast physical inventory turnover.

Final Consolidated Tail
The end of the script contains a consolidated "production" section intended to be the normal endpoint after the exploratory work. It:

- compares a small benchmark set of candidate models
- automatically selects the lowest-RMSE available final H20 candidate
- prints a compact coefficient table and a short interpretation of the old contract, transition regime, and current contract
- plots actual vs. fitted prices, the implied inventory path, residuals, and the refined R1/R3 contract-relationship figure with confidence bands
- projects future HECO-equivalent prices under flat Singapore/Houston prices while holding inventory constant at the final model's steady-state level
- constructs an "old contract continues" counterfactual by repricing the transition and new-contract periods with the R1 relationship while holding the fitted inventory path fixed
- estimates historical and forward fuel-expense savings by multiplying the contract-price difference ($/bbl) by a monthly Oahu oil-use proxy

This tail is intended to be the clean summary layer even if the earlier exploratory model blocks remain in the script for documentation and replication.

Default Run Mode
The script now defaults to a fast run mode (`RUN_EXPLORATORY <- FALSE`). In that mode it skips the long exploratory searches and directly reconstructs the selected final H20 model from its chosen parameter values. It then produces:

- a benchmark comparison table
- fitted-vs-actual, residual, and inventory-path diagnostics
- the refined old-contract vs current-contract relationship plot with uncertainty bands
- a forward projection under flat Singapore/Houston prices and constant inventory
- historical and projected "old contract vs selected contract" savings estimates
- a forward-risk table that holds the projected price path fixed and uses coefficient uncertainty in the old-vs-new contract relationships to bound projected savings

Fuel-use proxy for savings calculations
The script uses the compact Honolulu monthly oil-generation file in `processed/honolulu_fuel_generation_mwh_and_avg_mw.csv` and converts MWh to barrels with a fixed calibration derived from Hawaiian Electric Co Inc.'s HI oil units in the detailed EIA-923 generation panel. This keeps the default run fast while still grounding the savings calculation in local EIA generation data rather than an arbitrary constant-MW assumption.

To replicate the intermediate searches, set `RUN_EXPLORATORY <- TRUE`. The exploratory blocks remain in the script and are still the authoritative record of how the final model was reached.

Outputs
Throughout each part, the script prints model comparison tables and produces diagnostic plots: heatmaps of R² across the parameter grid, time-series of actual vs. fitted prices, residual plots, ACF of residuals, regime-colored scatter plots, and for the dynamic-regime sections plots of the month-by-month inventory window paths.
