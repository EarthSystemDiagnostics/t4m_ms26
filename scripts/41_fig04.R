# scripts/41_fig04.R
source(here::here("scripts", "01_load_inputs.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# settings
# ----------------------------
endDate   <- as.Date("2018-12-27")
t2m_shift <- 10  # K (same increment as °C), visual offset for clarity

# ----------------------------
# load data
# ----------------------------
sims_raw <- readRDS(here::here("output", "sims", "fig02_sims_raw.rds"))
df_all   <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))

# ----------------------------
# annual means (full years shown)
# ----------------------------

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

# T4M annual mean d18O (‰) from df_all (only where available)
ann_t4m <- df_all %>%
  filter(source == "T4M") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(t4m_d18O = mean(d18O, na.rm = TRUE), .groups = "drop")

# base df: all years from simulation/ERA5 (show full time range)
df <- ann_sim %>%
  full_join(ann_t2m, by = "year") %>%
  full_join(ann_t4m, by = "year") %>%
  arrange(year)

# ----------------------------
# 5-year centered running means
# ----------------------------
df <- df %>%
  mutate(
    signal_5y = zoo::rollmean(signal, k = 5, fill = NA, align = "center"),
    t2m_5y    = zoo::rollmean(t2m,    k = 5, fill = NA, align = "center"),
    t4m_5y    = zoo::rollmean(t4m_d18O, k = 5, fill = NA, align = "center")
  )

# ----------------------------
# scale T4M to match mean+sd of signal (fit on overlapping years only)
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
# plot (dual axis with identical scaling; ERA5 shifted by +10 K)
# ----------------------------
p <- ggplot(df, aes(x = year)) +
  
  # annual means (thin)
  geom_line(aes(y = signal, color = "Diffused precip.-weighted T (annual)"), linewidth = 0.7) +
  geom_line(aes(y = t2m + t2m_shift, color = "ERA5 t2m (annual)"), linewidth = 0.7) +
  geom_line(aes(y = t4m_scaled, color = "T4M (scaled; annual)"), linewidth = 0.7) +
  
  # 5-year means (thick)
  geom_line(aes(y = signal_5y, color = "Diffused precip.-weighted T (5-year)"), linewidth = 1.2) +
  geom_line(aes(y = t2m_5y + t2m_shift, color = "ERA5 t2m (5-year)"), linewidth = 1.2) +
  geom_line(aes(y = t4m_scaled_5y, color = "T4M (scaled; 5-year)"), linewidth = 1.2) +
  
  scale_y_continuous(
    name = "Diffused precipitation-weighted T (°C)",
    sec.axis = sec_axis(~ . - t2m_shift, name = "ERA5 annual mean t2m (°C)")
  ) +
  
  theme_minimal(base_size = 13) +
  labs(x = "Year", color = NULL) +
  theme(legend.position = "top")

ggsave(
  here::here("output", "figures", "Figure4.pdf"),
  p, width = 7.2, height = 3.9, units = "in"
)

# ----------------------------
# correlations (raw and detrended)
# ----------------------------

# helper: detrend by year (returns residuals)
detrend <- function(y, year) {
  resid(lm(y ~ year))
}

# ---- annual correlations (common years) ----
ann_common <- df %>%
  filter(!is.na(signal), !is.na(t2m))

cor_ann_raw <- cor(ann_common$signal, ann_common$t2m, use = "complete.obs")
cor_ann_det <- cor(detrend(ann_common$signal, ann_common$year),
                   detrend(ann_common$t2m,    ann_common$year),
                   use = "complete.obs")

# ---- 5-year correlations (common years) ----
mean5_common <- df %>%
  filter(!is.na(signal_5y), !is.na(t2m_5y))

cor_5y_raw <- cor(mean5_common$signal_5y, mean5_common$t2m_5y, use = "complete.obs")
cor_5y_det <- cor(detrend(mean5_common$signal_5y, mean5_common$year),
                  detrend(mean5_common$t2m_5y,    mean5_common$year),
                  use = "complete.obs")

message("Correlation (signal vs ERA5 t2m):")
message("  Annual (raw):        ", round(cor_ann_raw, 3))
message("  Annual (detrended):  ", round(cor_ann_det, 3))
message("  5-year (raw):        ", round(cor_5y_raw, 3))
message("  5-year (detrended):  ", round(cor_5y_det, 3))

# optional: also report T4M vs signal (raw & detrended), where available
t4m_common <- df %>%
  filter(!is.na(t4m_scaled), !is.na(signal))

if (nrow(t4m_common) > 3) {
  cor_t4m_raw <- cor(t4m_common$t4m_scaled, t4m_common$signal, use = "complete.obs")
  cor_t4m_det <- cor(detrend(t4m_common$t4m_scaled, t4m_common$year),
                     detrend(t4m_common$signal,     t4m_common$year),
                     use = "complete.obs")
  
  message("Correlation (T4M scaled vs signal):")
  message("  Annual (raw):        ", round(cor_t4m_raw, 3))
  message("  Annual (detrended):  ", round(cor_t4m_det, 3))
}

# optional diagnostics (console)
message("T4M scaling fitted on overlapping years only:")
message("  overlap years: ", min(overlap$year), "–", max(overlap$year),
        " (n=", nrow(overlap), ")")
message("  mu(signal) = ", round(mu_sig, 3), ", sd(signal) = ", round(sd_sig, 3))
message("  mu(T4M)    = ", round(mu_t4m, 3), ", sd(T4M)    = ", round(sd_t4m, 3))






# ---- 1) annual scatter (common years) ----
sc_ann <- df %>%
  filter(!is.na(signal), !is.na(t2m))
b_ann <- mean(sc_ann$signal, na.rm = TRUE) -
  mean(sc_ann$t2m,    na.rm = TRUE)

p_sc_ann <- ggplot(sc_ann, aes(x = t2m, y = signal)) +
  geom_point() +
  geom_abline(slope = 1, intercept = b_ann, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  theme_minimal(base_size = 13) +
  labs(
    x = "ERA5 annual mean t2m (°C)",
    y = "Diffused precipitation-weighted T (°C)",
    title = "Annual: simulated signal vs ERA5 t2m"
  )

ggsave(
  here::here("output", "figures", "Figure4_scatter_annual.pdf"),
  p_sc_ann, width = 5.6, height = 4.6, units = "in"
)

# ---- 2) 5-year NON-overlapping means ----
# define 5y blocks based on first available year in common data
miny <- min(sc_ann$year, na.rm = TRUE)

sc_5y_nonoverlap <- sc_ann %>%
  mutate(block_start = miny + 5 * ((year - miny) %/% 5)) %>%
  group_by(block_start) %>%
  summarise(
    year_mid = block_start + 2,  # just for labeling if needed
    signal_5y = mean(signal, na.rm = TRUE),
    t2m_5y    = mean(t2m, na.rm = TRUE),
    n_years   = n(),
    .groups = "drop"
  ) %>%
  # keep only complete 5-year blocks (optional but usually desired)
  filter(n_years == 5)

b_5y <- mean(sc_5y_nonoverlap$signal_5y, na.rm = TRUE) -
  mean(sc_5y_nonoverlap$t2m_5y,    na.rm = TRUE)

p_sc_5y <- ggplot(sc_5y_nonoverlap, aes(x = t2m_5y, y = signal_5y)) +
  geom_point() +
  geom_abline(slope = 1, intercept = b_5y, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  coord_equal() +
  theme_minimal(base_size = 13) +
  labs(
    x = "ERA5 5-year mean t2m (°C) (non-overlapping)",
    y = "Diffused precipitation-weighted T (°C) (non-overlapping)",
    title = "5-year non-overlapping: simulated signal vs ERA5 t2m"
  )

ggsave(
  here::here("output", "figures", "Figure4_scatter_5y_nonoverlap.pdf"),
  p_sc_5y, width = 5.6, height = 4.6, units = "in"
)

# optional: print correlations for these scatter datasets
message("Scatter correlations:")
message("  Annual (raw): ", round(cor(sc_ann$signal, sc_ann$t2m, use = "complete.obs"), 3))
message("  5y non-overlap (raw): ",
        round(cor(sc_5y_nonoverlap$signal_5y, sc_5y_nonoverlap$t2m_5y, use = "complete.obs"), 3))