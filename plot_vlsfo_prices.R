library(data.table)
library(ggplot2)

get_script_dir <- function() {
  if (!is.null(sys.frame(1)$ofile)) {
    dirname(normalizePath(sys.frame(1)$ofile))
  } else {
    getwd()
  }
}

base_dir <- get_script_dir()
input_path <- file.path(base_dir, "vlsfo_prices.csv")
out_dir <- file.path(base_dir, "processed")
out_path <- file.path(out_dir, "vlsfo_prices_timeseries.png")

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path)
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

prices <- fread(input_path)
required_cols <- c("date", "singapore_vlsfo_usd_mt", "global_20ports_vlsfo_usd_mt")
missing_cols <- setdiff(required_cols, names(prices))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

prices[, date := as.Date(date)]
prices[, singapore_vlsfo_usd_mt := as.numeric(singapore_vlsfo_usd_mt)]
prices[, global_20ports_vlsfo_usd_mt := as.numeric(global_20ports_vlsfo_usd_mt)]
prices <- prices[!is.na(date)]

plot_dt <- melt(
  prices,
  id.vars = "date",
  measure.vars = c("singapore_vlsfo_usd_mt", "global_20ports_vlsfo_usd_mt"),
  variable.name = "series",
  value.name = "price_usd_mt"
)

plot_dt[, series := fifelse(
  series == "singapore_vlsfo_usd_mt",
  "Singapore VLSFO",
  "Global 20 Ports VLSFO"
)]

p <- ggplot(plot_dt[!is.na(price_usd_mt)], aes(x = date, y = price_usd_mt, color = series)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Singapore VLSFO" = "#0B6E4F", "Global 20 Ports VLSFO" = "#C84C09")) +
  labs(
    title = "VLSFO Price Time Series",
    x = NULL,
    y = "USD per metric ton",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(out_path, p, width = 10, height = 5.5, dpi = 300)
cat("Saved:", out_path, "\n")
