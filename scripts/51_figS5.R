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
ylab_left <- expression("T19" ~~ delta^{18}*O ~ "(" * "‰" * ")")
# years to label on the emulated top axis (must be a vector of years, not a count;
# every-year tick marks are drawn regardless, only these get number labels)
year_ticks <- c(2000, 2005, 2010, 2015)

colors <- c(
  "T19" = "black",
  "ERA5 t2m" = "firebrick",
  "AWS9 t2m climatology precip ERA" = "steelblue"
)

# ----------------------------
# annual aggregation (derived here)
# ----------------------------
# Assumption: df_all has columns date + value + source (as in Fig02)
# Keep full years only (optional; remove filter if you want partial years)

df_annual <- df_all %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(source, year) %>%
  summarise(
    d18O  = mean(d18O,  na.rm = TRUE),
    proxy = mean(proxy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(year), year < 2019)


# ----------------------------
# plots
# ----------------------------
g.aws.clim <- make_plot_dualaxis(
  df_all,
  source_left  = "T19",
  source_right = "AWS9 t2m climatology precip ERA",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = "based on AWS t2m climatology (°C)",
  year_ticks   = year_ticks,
  show_year_labels = TRUE
)




g.aws.clim.annual <- make_plot_annual_dualaxis(
  df_annual,
  source_left  = "T19",
  source_right = "AWS9 t2m climatology precip ERA",
  colors     = colors[c("T19", "AWS9 t2m climatology precip ERA")],
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

# the two panels carry identical series -> keep only one legend
g.aws.clim.annual <- g.aws.clim.annual + ggplot2::guides(colour = "none", color = "none")

# identical axis colouring on both panels:
# left axis (T19) black, right axis (AWS) blue incl. ticks
axis_theme <- ggplot2::theme(
  axis.title.y       = ggplot2::element_text(color = colors[["T19"]]),
  axis.text.y        = ggplot2::element_text(color = colors[["T19"]]),
  axis.ticks.y       = ggplot2::element_line(color = colors[["T19"]]),
  axis.title.y.right = ggplot2::element_text(color = colors[["AWS9 t2m climatology precip ERA"]]),
  axis.text.y.right  = ggplot2::element_text(color = colors[["AWS9 t2m climatology precip ERA"]]),
  axis.ticks.y.right = ggplot2::element_line(color = colors[["AWS9 t2m climatology precip ERA"]])
)
g.aws.clim        <- g.aws.clim + axis_theme
g.aws.clim.annual <- g.aws.clim.annual + axis_theme

# ----------------------------
# layout + export
# ----------------------------
p_fig05 <- (g.aws.clim | g.aws.clim.annual) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

outfile_base <- here::here("output", "figures", "FigureS5_climatological_temperature")
ggsave(paste0(outfile_base, ".png"), p_fig05, width = 12, height = 5, dpi = 300)
ggsave(paste0(outfile_base, ".pdf"), p_fig05, width = 12, height = 5, device = cairo_pdf)

p_fig05


# ============================================================
# Correlation – full time series (depth-mean per date, as plotted)
# ============================================================

df_full_ts <- df_all %>%
  filter(source %in% c("T19", "AWS9 t2m climatology precip ERA")) %>%
  group_by(source, depth) %>%
  summarise(
    d18O  = mean(d18O,  na.rm = TRUE),
    proxy = mean(proxy, na.rm = TRUE),
    .groups = "drop"
  )

df_wide_full <- df_full_ts %>%
  tidyr::pivot_wider(names_from = source, values_from = c(d18O, proxy))

dat_full <- df_wide_full %>%
  transmute(
    t4m = d18O_T19,
    aws = `proxy_AWS9 t2m climatology precip ERA`
  ) %>%
  drop_na()

test_full <- cor.test(dat_full$t4m, dat_full$aws, method = "pearson")

cat(sprintf(
  "\nFull time series (T19 vs AWS clim): r = %.3f, p = %.4g, n = %d\n",
  unname(test_full$estimate),
  test_full$p.value,
  nrow(dat_full)
))



##
# ============================================================
# Correlation – annual means (as plotted in g.aws.clim.annual)
# ============================================================

df_wide_ann <- df_annual %>%
  filter(source %in% c("T19", "AWS9 t2m climatology precip ERA")) %>%
  select(year, source, d18O, proxy) %>%
  tidyr::pivot_wider(names_from = source, values_from = c(d18O, proxy))

dat_ann <- df_wide_ann %>%
  transmute(
    t4m = d18O_T19,
    aws = `proxy_AWS9 t2m climatology precip ERA`
  ) %>%
  drop_na()

test_ann <- cor.test(dat_ann$t4m, dat_ann$aws, method = "pearson")

cat(sprintf(
  "Annual means (T19 vs AWS clim): r = %.3f, p = %.4g, n = %d\n",
  test_ann$estimate,
  test_ann$p.value,
  nrow(dat_ann)
))
