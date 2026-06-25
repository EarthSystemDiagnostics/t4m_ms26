# scripts/41_fig04.R
source(here::here("scripts", "01_load_inputs.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(patchwork)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

t2m_shift <- 10  # Shift for visual purposes

colors <- c(
  "Diffused precip.-weighted T" = "steelblue",
  "Annual mean t2m"             = "darkorange3",
  "T19 (scaled)"                = "black"
)

# ----------------------------
# load data
# ----------------------------
sims_raw <- readRDS(here::here("output", "sims", "fig02_sims_raw.rds"))
df_all   <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))

# simulated diffused precip.-weighted T (°C)
sim_era5 <- sims_raw$era5 %>%
  filter(time <= endDate)

ann_sim <- sim_era5 %>%
  mutate(year = year(time)) %>%
  group_by(year) %>%
  summarise(signal = mean(signal, na.rm = TRUE), .groups = "drop")

# ERA5 annual mean t2m (°C)
ann_t2m <- era5 %>%
  filter(date <= endDate) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(t2m = mean(t2m, na.rm = TRUE), .groups = "drop")

# T19 annual mean d18O (‰)
ann_t4m <- df_all %>%
  filter(source == "T19") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(t4m_d18O = mean(d18O, na.rm = TRUE), .groups = "drop")

# base df
df <- ann_sim %>%
  full_join(ann_t2m, by = "year") %>%
  full_join(ann_t4m, by = "year") %>%
  arrange(year)

# ----------------------------
# 5-year centered running means
# ----------------------------
df <- df %>%
  mutate(
    signal_5y = zoo::rollmean(signal,   k = 5, fill = NA, align = "center"),
    t2m_5y    = zoo::rollmean(t2m,      k = 5, fill = NA, align = "center"),
    t4m_5y    = zoo::rollmean(t4m_d18O, k = 5, fill = NA, align = "center")
  )

# ----------------------------
# scale T19 to match mean+sd of signal
# ----------------------------
overlap <- df %>%
  filter(!is.na(signal), !is.na(t4m_d18O))

mu_sig <- mean(overlap$signal, na.rm = TRUE)
sd_sig <- sd(overlap$signal, na.rm = TRUE)

mu_t4m <- mean(overlap$t4m_d18O, na.rm = TRUE)
sd_t4m <- sd(overlap$t4m_d18O, na.rm = TRUE)

df <- df %>%
  mutate(
    t4m_scaled = ifelse(
      is.na(t4m_d18O),
      NA_real_,
      mu_sig + (t4m_d18O - mu_t4m) * (sd_sig / sd_t4m)
    ),
    t4m_scaled_5y = ifelse(
      is.na(t4m_5y),
      NA_real_,
      mu_sig + (t4m_5y - mu_t4m) * (sd_sig / sd_t4m)
    )
  )

# ----------------------------
# correlations
# ----------------------------
detrend <- function(y, year) {
  resid(lm(y ~ year))
}

ann_common <- df %>%
  filter(!is.na(signal), !is.na(t2m))

cor_ann_raw <- cor(ann_common$signal, ann_common$t2m, use = "complete.obs")
cor_ann_det <- cor(
  detrend(ann_common$signal, ann_common$year),
  detrend(ann_common$t2m,    ann_common$year),
  use = "complete.obs"
)

mean5_common <- df %>%
  filter(!is.na(signal_5y), !is.na(t2m_5y))

cor_5y_raw <- cor(mean5_common$signal_5y, mean5_common$t2m_5y, use = "complete.obs")
cor_5y_det <- cor(
  detrend(mean5_common$signal_5y, mean5_common$year),
  detrend(mean5_common$t2m_5y,    mean5_common$year),
  use = "complete.obs"
)

message("Correlation (signal vs ERA5 t2m):")
message("  Annual (raw):        ", round(cor_ann_raw, 3))
message("  Annual (detrended):  ", round(cor_ann_det, 3))
message("  5-year (raw):        ", round(cor_5y_raw, 3))
message("  5-year (detrended):  ", round(cor_5y_det, 3))

# ----------------------------
# left panel
# ----------------------------
p_left <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = signal, color = "Diffused precip.-weighted T"), linewidth = 0.9) +
  geom_line(aes(y = t2m + t2m_shift, color = "Annual mean t2m"), linewidth = 0.9) +
  geom_line(aes(y = t4m_scaled, color = "T19 (scaled)"), linewidth = 0.9) +
  scale_y_continuous(
    name = "Simulated firn record (°C)",
    sec.axis = sec_axis(~ . - t2m_shift, name = "Annual mean t2m (°C)")
  ) +
  scale_color_manual(values = colors) +
  labs(x = "Year", color = "", tag = "a") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    axis.title.y.left  = element_text(color = colors["Diffused precip.-weighted T"]),
    axis.text.y.left   = element_text(color = colors["Diffused precip.-weighted T"]),
    axis.title.y.right = element_text(color = colors["Annual mean t2m"]),
    axis.text.y.right  = element_text(color = colors["Annual mean t2m"]),
    axis.ticks.y.left  = element_line(color = colors["Diffused precip.-weighted T"]),
    axis.ticks.y.right = element_line(color = colors["Annual mean t2m"])
  )

# ----------------------------
# right panel
# ----------------------------
sc_ann <- df %>%
  filter(!is.na(signal), !is.na(t2m))

b_ann <- mean(sc_ann$signal, na.rm = TRUE) -
  mean(sc_ann$t2m, na.rm = TRUE)

p_right <- ggplot(sc_ann, aes(x = t2m, y = signal)) +
  geom_point(size = 1.8) +
  geom_abline(slope = 1, intercept = b_ann, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Annual mean t2m (°C)",
    y = "Simulated firn record (°C)",
    tag = "b"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

# ----------------------------
# combine panels
# ----------------------------
p_combined <- p_left + p_right +
  plot_layout(widths = c(5, 3)) &
  theme(
    plot.tag = element_text(size = 11, face = "plain"),
    plot.tag.position = c(0.01, 0.98)
  )

ggsave(
  here::here("output", "figures", "Figure4.pdf"),
  p_combined, width = 8, height = 3, units = "in",
  device = cairo_pdf
)

ggsave(
  here::here("output", "figures", "Figure4.svg"),
  p_combined, width = 8, height = 3, units = "in"
)