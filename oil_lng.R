
library(data.table)
library(lubridate)
library(ggplot2)

# IGP projected net demand (excludes DR)
setwd("~/EIA/lng_vs_oil/")

demand <- fread("igp_oahu_net_demand_2020_2050_8760.csv")
demand$date <- as.Date(demand$Dt, format="%m/%d/%y")
demand$year <- as.integer(substr(demand$date, 1, 4))
demand$day <- apply(demand[,2:25], 1, mean)

yearly_demand <- data.frame(
  year = unique(demand$year),
  net_load = tapply(demand$day, demand$year, mean)
)

# IGP generation mixes from Figure 2-3 in the final report
igp_pref <- data.frame(
  year = c(2022, 2030, 2035, 2040, 2045),
  fossil = c( 71.8, 23.1, 6.7, 3.2, 0),
  solar = c(5.7, 51.1, 43.2, 47.9, 52.6),
  biomass = c(4.9, 5.4, 5.1, 4.8, 4.4),
  biofuel = c(0.2, 0, 1.0, 3.2, 3.3),
  der = c(14.1, 17.1, 18.0, 17.9, 17.1),
  onshore_wind = c(3.3, 3.2, 2.3, 0, 0.9),
  offshore_wind = c(0, 0, 23.5, 23.0, 21.7)
)

igp_lc <- data.frame(
  year = c(2022, 2030, 2035, 2040, 2045),
  fossil = c( 71.8, 45.0, 17.2, 12.1, 0),
  solar = c(5.7, 26.4, 24.9, 23.5, 20.7),
  biomass = c(4.9, 5.5, 5.2, 4.9, 4.5),
  biofuel = c(0.2, 2.3, 4.6, 10.5, 9.7),
  der = c(14.1, 17.5, 18.3, 18.2, 37.9),
  onshore_wind = c(3.3, 3.3, 4.0, 6.3, 5.7),
  offshore_wind = c(0, 0, 25.8, 24.4, 21.6)
)

# Preserve raw IGP anchor-year shares for bar-plot comparison.
igp_pref_raw <- as.data.table(igp_pref)
igp_lc_raw <- as.data.table(igp_lc)
  
# Adjust for rounding
ysums <- apply(igp_pref[,2:8],1,sum)
for(i in 1:5) igp_pref[i,2:8] <- igp_pref[i,2:8]*100/ysums[i]
ysums <- apply(igp_lc[,2:8],1,sum)
for(i in 1:5) igp_lc[i,2:8] <- igp_lc[i,2:8]*100/ysums[i]

# Function to expand reported planning years to all years using linear interpolation
interp_yearly_shares <- function(df, year_col = "year", sum_to = 100, fix_last_col = TRUE) {
  stopifnot(is.data.frame(df))
  stopifnot(year_col %in% names(df))
  
  yrs <- df[[year_col]]
  if (anyNA(yrs)) stop("Year column contains NA.")
  if (!is.numeric(yrs)) stop("Year column must be numeric.")
  if (any(duplicated(yrs))) stop("Year column must not contain duplicates.")
  if (length(yrs) < 2) stop("Need at least 2 rows (years) to interpolate.")
  
  # sort by year (approx expects increasing x)
  df <- df[order(df[[year_col]]), , drop = FALSE]
  
  vars <- setdiff(names(df), year_col)
  if (length(vars) == 0) stop("No columns to interpolate (only year column found).")
  
  # ensure interpolated columns are numeric
  nonnum <- vars[!vapply(df[vars], is.numeric, logical(1))]
  if (length(nonnum) > 0) {
    stop("These columns are not numeric and cannot be interpolated: ",
         paste(nonnum, collapse = ", "))
  }
  
  # expanded year sequence (integer years)
  years_out <- seq(min(df[[year_col]]), max(df[[year_col]]), by = 1)
  out <- data.frame(year = years_out)
  names(out)[1] <- year_col
  
  # interpolate each variable
  for (v in vars) {
    out[[v]] <- approx(
      x = df[[year_col]],
      y = df[[v]],
      xout = years_out,
      method = "linear",
      rule = 2
    )$y
  }
  
  # renormalize row-wise to sum_to
  row_sums <- rowSums(out[vars])
  if (any(row_sums == 0)) stop("At least one interpolated year sums to 0; cannot renormalize.")
  out[vars] <- out[vars] / row_sums * sum_to
  out
}

igp_pref <- interp_yearly_shares(igp_pref)
igp_lc <- interp_yearly_shares(igp_lc)

igp_pref <- merge(igp_pref, yearly_demand, by = "year")
igp_lc <- merge(igp_lc, yearly_demand, by = "year")

igp_pref_mwh <- igp_pref
igp_pref_mwh[, 2:8] <- igp_pref_mwh[,9]*igp_pref_mwh[, 2:8]/(100-igp_pref[,"der"])
igp_pref_mwh$gross_load <- igp_pref_mwh$net_load + igp_pref_mwh$der

igp_lc_mwh <- igp_lc
igp_lc_mwh[, 2:8] <- igp_lc_mwh[,9]*igp_lc_mwh[, 2:8]/(100-igp_lc[,"der"])
igp_lc_mwh$gross_load <- igp_lc_mwh$net_load + igp_lc_mwh$der


###### HSEO plan ########
hseo_lng <- fread("hseo_lng.csv")
hseo_oil <- fread("hseo_oil.csv")
colnames(hseo_lng) <- c("year","biofuel","solar","der","onshore_wind","offshore_wind","refuse","oil","lng","hydrogen")
colnames(hseo_oil) <- c("year","oil","lng","biofuel","solar","der","onshore_wind","offshore_wind","refuse")

# ------------------------------------------------------------------------------
# Stacked bar plot (raw anchor years only), all plans side-by-side
# ------------------------------------------------------------------------------

to_plan_long <- function(dt, plan_name, id_col = "year") {
  x <- as.data.table(dt)
  stopifnot(id_col %in% names(x))
  value_cols <- setdiff(names(x), id_col)
  long <- melt(
    x,
    id.vars = id_col,
    measure.vars = value_cols,
    variable.name = "resource",
    value.name = "value"
  )
  setnames(long, id_col, "year")
  long[, plan := plan_name]
  long[]
}

# Build IGP plan values from raw anchor-year shares.
# Shares include DER, so gross_load = net_load / (1 - der_share).
igp_pref_plot <- merge(copy(igp_pref_raw), yearly_demand, by = "year", all.x = TRUE)
igp_lc_plot <- merge(copy(igp_lc_raw), yearly_demand, by = "year", all.x = TRUE)
igp_pref_plot[, gross_load := net_load / (1 - der / 100)]
# Apply IGP-LC shares to gross totals implied by IGP preferred DER shares.
pref_der_lookup <- igp_pref_plot[, .(year, der_pref = der)]
igp_lc_plot <- merge(igp_lc_plot, pref_der_lookup, by = "year", all.x = TRUE)
igp_lc_plot[, gross_load := net_load / (1 - der_pref / 100)]
for (v in setdiff(names(igp_pref_plot), c("year", "net_load", "gross_load"))) {
  igp_pref_plot[, (v) := gross_load * get(v) / 100]
}
for (v in setdiff(names(igp_lc_plot), c("year", "net_load", "gross_load", "der_pref"))) {
  igp_lc_plot[, (v) := gross_load * get(v) / 100]
}
igp_pref_plot[, c("net_load", "gross_load") := NULL]
igp_lc_plot[, c("net_load", "gross_load", "der_pref") := NULL]

hseo_lng_plot <- as.data.table(hseo_lng)
hseo_oil_plot <- as.data.table(hseo_oil)
# HSEO inputs are annual GWh; convert to average MW to align with IGP series.
hseo_cols_lng <- setdiff(names(hseo_lng_plot), "year")
hseo_cols_oil <- setdiff(names(hseo_oil_plot), "year")
hseo_lng_plot[, (hseo_cols_lng) := lapply(.SD, function(x) x * 1000 / 8760), .SDcols = hseo_cols_lng]
hseo_oil_plot[, (hseo_cols_oil) := lapply(.SD, function(x) x * 1000 / 8760), .SDcols = hseo_cols_oil]

plans_long <- rbindlist(list(
  to_plan_long(igp_pref_plot, "igp_pref"),
  to_plan_long(igp_lc_plot, "igp_lc"),
  to_plan_long(hseo_lng_plot, "hseo_lng"),
  to_plan_long(hseo_oil_plot, "hseo_oil")
), use.names = TRUE, fill = TRUE)

plans_long <- plans_long[!is.na(value)]
plans_long[, resource := gsub("_", " ", resource)]
# Harmonize IGP naming with HSEO categories.
plans_long[plan %in% c("igp_pref", "igp_lc") & resource == "biomass", resource := "refuse"]
plans_long[plan %in% c("igp_pref", "igp_lc") & resource == "fossil", resource := "oil"]
# If remapping created duplicate plan-year-resource rows, combine them.
plans_long <- plans_long[, .(value = sum(value, na.rm = TRUE)), by = .(plan, year, resource)]

years_keep <- c(2022L, 2030L, 2035L, 2040L, 2045L)
plans_long <- plans_long[year %in% years_keep]
stack_order <- c(
  "refuse",
  "oil",
  "lng",
  "biofuel",
  "hydrogen",
  "onshore wind",
  "offshore wind",
  "solar",
  "der"
)
extra_resources <- sort(setdiff(unique(plans_long$resource), stack_order))
all_levels <- c(stack_order, extra_resources)
plans_long[, resource := factor(resource, levels = all_levels)]
plan_levels <- c("igp_pref", "igp_lc", "hseo_lng", "hseo_oil")
plan_labels <- c(
  igp_pref = "IGP Base",
  igp_lc = "IGP Land Constrained",
  hseo_lng = "HSEO LNG",
  hseo_oil = "HSEO Oil"
)
plans_long[, plan := factor(plan, levels = plan_levels)]

fill_colors <- c(
  "solar" = "#c28f00",         # dark yellow
  "der" = "#ffe082",           # light yellow
  "oil" = "#4d4d4d",           # dark gray
  "lng" = "#bdbdbd",           # light gray
  "offshore wind" = "#0b3c8c", # dark blue
  "onshore wind" = "#7fb3ff",  # light blue
  "refuse" = "#66a61e",
  "biofuel" = "#e67e22",
  "biomass" = "#1b9e77",
  "fossil" = "#8c564b",
  "hydrogen" = "#8e44ad"
)

out_dir <- getwd()
plans_long[, year_f := factor(year, levels = years_keep)]

g <- ggplot(plans_long, aes(x = year_f, y = value, fill = resource)) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.78,
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(values = fill_colors, breaks = rev(all_levels), drop = FALSE) +
  facet_grid(. ~ plan, labeller = labeller(plan = plan_labels), scales = "fixed") +
  labs(
    title = "Oahu Generation Mix by Plan",
    x = "Year",
    y = "Average generation (MW)",
    fill = "Resource"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

out_file <- file.path(out_dir, "oahu_generation_mix_all_plans_stacked_bars.png")
ggsave(out_file, g, width = 15, height = 7.2, dpi = 170)
cat("Saved:", out_file, "\n")

# ------------------------------------------------------------------------------
# Average (oil + lng) load by plan for requested year window
# ------------------------------------------------------------------------------
range_start <- 1931L
range_end <- 2044L

igp_pref_avg <- copy(as.data.table(igp_pref))
igp_lc_avg <- copy(as.data.table(igp_lc))
igp_pref_avg[, gross_load := net_load / (1 - der / 100)]

# Apply IGP-LC shares to preferred-derived gross totals for consistency.
igp_lc_avg <- merge(igp_lc_avg, igp_pref_avg[, .(year, der_pref = der)], by = "year", all.x = TRUE)
igp_lc_avg[, gross_load := net_load / (1 - der_pref / 100)]

igp_pref_avg[, oil_lng_mw := gross_load * fossil / 100]
igp_lc_avg[, oil_lng_mw := gross_load * fossil / 100]

hseo_lng_avg <- copy(hseo_lng_plot)
hseo_oil_avg <- copy(hseo_oil_plot)
hseo_lng_avg[, oil_lng_mw := oil + lng]
hseo_oil_avg[, oil_lng_mw := oil + lng]

summ_avg <- function(dt, plan_name, y0, y1) {
  sub <- dt[year >= y0 & year <= y1 & !is.na(oil_lng_mw), .(year, oil_lng_mw)]
  data.table(
    plan = plan_name,
    requested_start_year = y0,
    requested_end_year = y1,
    available_start_year = if (nrow(sub) > 0) min(sub$year) else NA_integer_,
    available_end_year = if (nrow(sub) > 0) max(sub$year) else NA_integer_,
    n_years_used = nrow(sub),
    avg_oil_plus_lng_mw = if (nrow(sub) > 0) mean(sub$oil_lng_mw) else NA_real_
  )
}

avg_oil_lng <- rbindlist(list(
  summ_avg(igp_pref_avg, "igp_pref", range_start, range_end),
  summ_avg(igp_lc_avg, "igp_lc", range_start, range_end),
  summ_avg(hseo_lng_avg, "hseo_lng", range_start, range_end),
  summ_avg(hseo_oil_avg, "hseo_oil", range_start, range_end)
), use.names = TRUE, fill = TRUE)

out_avg <- file.path(out_dir, "oil_lng_avg_oil_plus_lng_load_1931_2044.csv")
fwrite(avg_oil_lng, out_avg)
cat("Saved:", out_avg, "\n")
