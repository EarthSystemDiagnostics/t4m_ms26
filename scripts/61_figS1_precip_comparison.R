# scripts/S1_figS1_precip_comparison.R
# Supplement Figure: AWS9 accumulation vs ERA5 precipitation
# AWS acc is net snow height change (m), converted to mm w.e. via rho_snow = 330 kg/m3
# ERA5 tp is in mm w.e./day

source(here::here("scripts", "00_setup.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

RHO_SNOW  <- 330        # kg/m3, surface density used in simulations
JUMP_YEARS <- 2009:2011 # ultrasonic sensor artifact period

load(here::here("data", "processed", "AWS9_daily.RData"))
era5 <- read.csv(here::here("data", "processed", "era5_t2m_tp_daily_1940_2024_kohnen_6h.csv"))
era5$date <- as.Date(era5$date)

# ---- daily data: overlapping period ----
t_start <- max(min(daily$day), min(era5$date))
t_end   <- min(max(daily$day), max(era5$date))

aws_daily <- daily %>%
  filter(day >= t_start, day <= t_end) %>%
  mutate(acc_mm_we = acc * RHO_SNOW, date = day)

era5_daily <- era5 %>%
  filter(date >= t_start, date <= t_end)

# ---- annual sums ----
aws_ann <- aws_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(acc_mm = sum(acc_mm_we, na.rm = TRUE), n = n()) %>%
  filter(n >= 300) %>%
  mutate(source = "AWS9")

era5_ann <- era5_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(acc_mm = sum(tp, na.rm = TRUE), n = n()) %>%
  filter(n >= 300) %>%
  mutate(source = "ERA5")

common_years <- intersect(aws_ann$year, era5_ann$year)
ann <- bind_rows(aws_ann, era5_ann) %>% filter(year %in% common_years)

aws_vec  <- ann$acc_mm[ann$source == "AWS9"]
era5_vec <- ann$acc_mm[ann$source == "ERA5"]
yrs      <- ann$year[ann$source == "AWS9"]

r_all  <- cor(aws_vec, era5_vec)
r_excl <- cor(aws_vec[!yrs %in% JUMP_YEARS], era5_vec[!yrs %in% JUMP_YEARS])

# ---- cumulative (daily) ----
start_year <- min(common_years)

cum_aws <- aws_daily %>%
  filter(year(date) >= start_year) %>%
  arrange(date) %>%
  mutate(cum_mm = cumsum(replace_na(acc_mm_we, 0)), source = "AWS9")

cum_era5 <- era5_daily %>%
  filter(year(date) >= start_year) %>%
  arrange(date) %>%
  mutate(cum_mm = cumsum(replace_na(tp, 0)), source = "ERA5")

cum <- bind_rows(
  cum_aws  %>% select(date, cum_mm, source),
  cum_era5 %>% select(date, cum_mm, source)
)

# jump period for shading
jump_start <- as.Date(paste0(min(JUMP_YEARS), "-01-01"))
jump_end   <- as.Date(paste0(max(JUMP_YEARS), "-12-31"))

# ---- monthly values for example years ----
# chosen to span the range of intra-annual agreement:
# 2012 (r = -0.44, worst), 2007 (r = 0.53, mid), 2018 (r = 0.71, best; trench year)
EXAMPLE_YEARS <- c(2012, 2007, 2018)

aws_mon_ex <- aws_daily %>%
  filter(year(date) %in% EXAMPLE_YEARS) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(acc_mm = sum(acc_mm_we, na.rm = TRUE), source = "AWS9", .groups = "drop")

era5_mon_ex <- era5_daily %>%
  filter(year(date) %in% EXAMPLE_YEARS) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(acc_mm = sum(tp, na.rm = TRUE), source = "ERA5", .groups = "drop")

mon_ex <- bind_rows(aws_mon_ex, era5_mon_ex) %>%
  mutate(
    month_lbl = factor(month.abb[month], levels = month.abb),
    year_lbl  = factor(year, levels = EXAMPLE_YEARS,
                       labels = sprintf("%d", EXAMPLE_YEARS))
  )

# per-year r for facet labels
r_by_year <- mon_ex %>%
  tidyr::pivot_wider(names_from = source, values_from = acc_mm) %>%
  group_by(year_lbl) %>%
  summarise(r = cor(AWS9, ERA5, use = "complete.obs"), .groups = "drop") %>%
  mutate(label = sprintf("r = %.2f", r))

# ---- colors ----
cols <- c("AWS9" = "steelblue", "ERA5" = "forestgreen")

# ---- panel A: cumulative ----
pA <- ggplot(cum, aes(x = date, y = cum_mm, colour = source)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = cols) +
  labs(x = NULL, y = "Cumulative accumulation (mm w.e.)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

# ---- panel C: example years monthly ----
pC <- ggplot(mon_ex, aes(x = month_lbl, y = acc_mm, colour = source, group = source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  geom_text(data = r_by_year,
            aes(x = Inf, y = Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = 1.5, size = 3.2) +
  scale_colour_manual(values = cols) +
  facet_wrap(~year_lbl, ncol = 3) +
  labs(x = "Month", y = "Monthly accumulation (mm w.e.)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "none",
    strip.text        = element_text(face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1)
  )

# ---- assemble ----
p <- (pA / pC) +
  plot_layout(ncol = 1, heights = c(1.2, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 11, face = "plain"))

ggsave(here::here("output", "figures", "FigureS1_precip_comparison.pdf"),
       p, width = 7, height = 8.5, units = "in", device = cairo_pdf)
ggsave(here::here("output", "figures", "FigureS1_precip_comparison.png"),
       p, width = 7, height = 8.5, units = "in", dpi = 200)

cat("r(AWS9, ERA5) annual          =", round(r_all,  3), "\n")
cat("r(AWS9, ERA5) excl. jump years =", round(r_excl, 3), "\n")
cat("Mean AWS9:", round(mean(aws_ann$acc_mm), 1), "mm w.e./yr\n")
cat("Mean ERA5:", round(mean(era5_ann$acc_mm), 1), "mm w.e./yr\n")
