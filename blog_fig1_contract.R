## blog_fig1_contract.R
## Blog Figure 1: Old vs. new HECO fuel supply contract
##
## Shows the two pricing regimes as lines over the VLSFO price range,
## with a shaded "savings zone" above the crossover (~$83/bbl).
## Model parameters from heco_lsfo_model_params.csv.
## Output: brief_figures/blog_fig1_contract.png  (and .pdf)

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

base_dir <- "/Users/mike/Library/CloudStorage/GoogleDrive-michael@ae4cast.com/Other computers/My Mac (1)/EIA/lng_vs_oil"
out_dir  <- file.path(base_dir, "brief_figures")
dir.create(out_dir, showWarnings = FALSE)

# ── Model parameters ─────────────────────────────────────────────────────────
params_raw <- fread(file.path(base_dir, "heco_lsfo_model_params.csv"))
params_num <- params_raw[suppressWarnings(!is.na(as.numeric(value)))]
params <- setNames(as.numeric(params_num$value), params_num$parameter)

r1_slope     <- params["r1_slope"]      # ~1.90
r1_intercept <- params["r1_intercept"]  # ~-59.5
r3_slope     <- params["r3_slope"]      # ~0.74
r3_intercept <- params["r3_intercept"]  # ~37.3

# Crossover: r1_intercept + r1_slope*v = r3_intercept + r3_slope*v
crossover <- (r3_intercept - r1_intercept) / (r1_slope - r3_slope)
# ~ $83/bbl VLSFO

CURRENT_VLSFO <- 145   # approximate mid-March 2026 VLSFO ($145-150/bbl)

# ── Build curve data ──────────────────────────────────────────────────────────
vlsfo_seq <- seq(65, 155, by = 0.5)

df_lines <- data.frame(
  vlsfo   = rep(vlsfo_seq, 2),
  price   = c(
    r1_intercept + r1_slope * vlsfo_seq,   # old contract
    r3_intercept + r3_slope * vlsfo_seq    # new contract
  ),
  contract = rep(c("Old contract (pre-2024)", "Current contract (2024–)"),
                 each = length(vlsfo_seq))
)
df_lines$contract <- factor(df_lines$contract,
                             levels = c("Old contract (pre-2024)",
                                        "Current contract (2024–)"))

# Savings ribbon: above crossover, gap between the two lines
df_ribbon <- data.frame(
  vlsfo = vlsfo_seq[vlsfo_seq >= crossover],
  lo    = r3_intercept + r3_slope * vlsfo_seq[vlsfo_seq >= crossover],
  hi    = r1_intercept + r1_slope * vlsfo_seq[vlsfo_seq >= crossover]
)

# ── Annotation values ─────────────────────────────────────────────────────────
r1_at_current <- r1_intercept + r1_slope * CURRENT_VLSFO
r3_at_current <- r3_intercept + r3_slope * CURRENT_VLSFO
saving_at_current <- r1_at_current - r3_at_current  # ~$74/bbl

# ── Theme ─────────────────────────────────────────────────────────────────────
blog_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_line(colour = "grey90"),
    axis.title            = element_text(size = 11),
    legend.position       = "none",
    plot.margin           = margin(t = 8, r = 12, b = 8, l = 8, unit = "pt"),
    plot.caption          = element_text(size = 7, colour = "grey40",
                                         hjust = 0, margin = margin(t = 6)),
    plot.caption.position = "plot"
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +
  # Savings zone shading
  geom_ribbon(data = df_ribbon,
              aes(x = vlsfo, ymin = lo, ymax = hi),
              fill = "#27ae60", alpha = 0.12) +
  # Contract lines
  geom_line(data = df_lines,
            aes(x = vlsfo, y = price, colour = contract, linewidth = contract)) +
  # Crossover annotation
  geom_vline(xintercept = crossover, colour = "grey55", linewidth = 0.5,
             linetype = "dashed") +
  annotate("text", x = crossover + 1, y = 78,
           label = sprintf("Contracts equal\nat ~$%.0f/bbl", crossover),
           colour = "grey40", size = 3, hjust = 0, lineheight = 0.9) +
  # Current VLSFO reference
  geom_vline(xintercept = CURRENT_VLSFO, colour = "#c0392b", linewidth = 0.7) +
  annotate("text", x = CURRENT_VLSFO - 1.5, y = 215,
           label = sprintf("Current\n~$%d/bbl", CURRENT_VLSFO),
           colour = "#c0392b", size = 3, hjust = 1, fontface = "bold",
           lineheight = 0.9) +
  # Savings callout at current price
  annotate("segment",
           x = CURRENT_VLSFO + 2, xend = CURRENT_VLSFO + 2,
           y = r3_at_current, yend = r1_at_current,
           arrow = arrow(ends = "both", length = unit(0.1, "cm")),
           colour = "#27ae60", linewidth = 0.7) +
  annotate("text",
           x = CURRENT_VLSFO + 3.5, y = (r1_at_current + r3_at_current) / 2,
           label = sprintf("~$%.0f/bbl\nsaved", saving_at_current),
           colour = "#27ae60", size = 3, hjust = 0, lineheight = 0.9,
           fontface = "bold") +
  # Direct line labels — placed mid-range where lines are well separated
  annotate("text", x = 100, y = r1_intercept + r1_slope * 100 + 6,
           label = "Old contract (pre-2024)\nrises $1.90 for every\n$1 rise in oil",
           colour = "#2c3e6b", size = 3, hjust = 0.5, vjust = 0, lineheight = 0.9,
           fontface = "bold") +
  annotate("text", x = 110, y = r3_intercept + r3_slope * 110 - 6,
           label = "Current contract (2024–)\nrises only $0.74 for every\n$1 rise in oil",
           colour = "#c0392b", size = 3, hjust = 0.5, vjust = 1, lineheight = 0.9,
           fontface = "bold") +
  # Scales
  scale_colour_manual(values = c("Old contract (pre-2024)" = "#2c3e6b",
                                  "Current contract (2024–)"  = "#c0392b")) +
  scale_linewidth_manual(values = c("Old contract (pre-2024)" = 1.4,
                                     "Current contract (2024–)"  = 1.4)) +
  scale_x_continuous(
    name   = "Global marine fuel price ($/barrel)",
    labels = dollar_format(prefix = "$", suffix = "/bbl"),
    breaks = seq(70, 150, by = 10),
    limits = c(65, 160),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    name   = "Hawaiian Electric fuel cost ($/barrel)",
    labels = dollar_format(prefix = "$"),
    breaks = seq(80, 240, by = 20),
    expand = expansion(mult = 0)
  ) +
  coord_cartesian(ylim = c(70, 235), xlim = c(65, 160)) +
  labs(
    title   = "How Hawaiian Electric's fuel contract was restructured in 2024",
    caption = paste0(
      "The old contract (R1 regime, pre-2024) had a steep pricing formula: ",
      "HECO's cost rose ~$1.90 for every $1 increase in the VLSFO marine fuel benchmark. ",
      "The current contract (R3 regime) has a much flatter slope (~$0.74/$1), ",
      "with a higher floor when oil is cheap. The green shaded zone shows where ",
      "the current contract saves money relative to the old terms. ",
      "At ~$145/bbl VLSFO (mid-March 2026), the saving is roughly $",
      sprintf("%.0f", saving_at_current),
      "/barrel.\n",
      "Contract parameters estimated from HECO monthly LSFO prices (EIA Form 923) ",
      "and Singapore/Houston VLSFO spot prices (Ship & Bunker). UHERO model."
    )
  ) +
  blog_theme +
  theme(plot.title = element_text(size = 13, face = "bold",
                                   margin = margin(b = 8)))

# ── Save ──────────────────────────────────────────────────────────────────────
quartz_pdf <- function(filename, width, height, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = "white")
}

ggsave(file.path(out_dir, "blog_fig1_contract.png"),
       p, width = 8, height = 5.5, dpi = 200, bg = "white")
ggsave(file.path(out_dir, "blog_fig1_contract.pdf"),
       p, width = 8, height = 5.5, device = quartz_pdf)
message("Saved: blog_fig1_contract (.png / .pdf)")
