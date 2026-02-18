# scripts/51_fig05.R
# Figure 5 — Climatological temperature
#
# Input comes from scripts/11_run_sims_fig02.R outputs:
#   - output/sims/fig02_df_all.rds
#
# Output:
#   - output/figures/fig05_climatological_temperature.png
#   - output/figures/fig05_climatological_temperature.pdf

source(here::here("scripts", "01_load_inputs.R"))  # for helper fcts + constants

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# load data (explicitly from Fig02 products)
# ----------------------------
df_all <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))

# ----------------------------
# settings
# ----------------------------
if (!exists("ylab_left"))  ylab_left  <- "Temperature (°C)"
if (!exists("year_ticks")) year_ticks <- 5

colors <- c(
  "T4M" = "black",
  "ERA5 t2m" = "firebrick",
  "AWS9 t2m climatology precip ERA" = "steelblue"
)

# ----------------------------
# annual aggregation (derived here)
# ----------------------------
# Assumption: df_all has columns date + value + source (as in Fig02)
# Keep full years only (optional; remove filter if you want partial years)
df_annual <- df_all %>%
  mutate(year = year(date)) %>%
  group_by(source, year) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(paste0(year, "-07-01"))) %>%  # mid-year anchor
  select(date, source, value)

# ----------------------------
# plots
# ----------------------------
g.aws.clim <- make_plot_dualaxis(
  df_all,
  source_left  = "T4M",
  source_right = "AWS9 t2m climatology precip ERA",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = "based on AWS t2m climatology (°C)",
  year_ticks   = year_ticks,
  show_year_labels = TRUE
)

g.aws.clim.annual <- make_plot_annual_dualaxis(
  df_annual,
  sources    = c("T4M", "AWS9 t2m climatology precip ERA"),
  colors     = colors[c("T4M", "AWS9 t2m climatology precip ERA")],
  ylab_left  = ylab_left,
  ylab_right = "based on AWS t2m climatology (°C)"
)

# ----------------------------
# enforce identical y-limits across both panels
# ----------------------------
y_range <- range(
  ggplot_build(g.aws.clim)$layout$panel_params[[1]]$y.range,
  ggplot_build(g.aws.clim.annual)$layout$panel_params[[1]]$y.range
)

g.aws.clim <- g.aws.clim + coord_cartesian(ylim = y_range, expand = FALSE)
g.aws.clim.annual <- g.aws.clim.annual + coord_cartesian(ylim = y_range, expand = FALSE)

# ----------------------------
# layout + export
# ----------------------------
p_fig05 <- (g.aws.clim | g.aws.clim.annual) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

outfile_base <- here::here("output", "figures", "fig05_climatological_temperature")
ggsave(paste0(outfile_base, ".png"), p_fig05, width = 12, height = 5, dpi = 300)
ggsave(paste0(outfile_base, ".pdf"), p_fig05, width = 12, height = 5, device = cairo_pdf)

p_fig05