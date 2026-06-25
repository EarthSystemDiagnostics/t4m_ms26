# scripts/01_build_inputs.R
source(here::here("scripts", "00_setup.R"))

library(dplyr)



# ---- load processed inputs  ----
load(
  here("data", "processed", "model_data.rda"),
  verbose = TRUE
)  # expects: era5, echam6, obs, mod

load(
  here("data", "processed", "t4m.rda"),
  verbose = TRUE
)  # expects: t4m

agemodel <- read.csv(
  here("data", "processed", "t4m_depth_age.csv")
)


load(
  here("data", "processed", "AWS9_daily.RData"),
  verbose = TRUE
)   # expects: daily

era5 <- read.csv(here("data", "processed", "era5_t2m_tp_daily_1940_2024_kohnen_6h.csv"))

# Convert date column to Date class
era5$date <- as.Date(era5$date)

aws <- data.frame(
  date = daily$day,
  t2m  = daily$Traw
)

# join ERA5 precip onto aws
aws <- aws %>%
  left_join(
    era5 %>% select(date, tp) %>% rename(tp.era5 = tp),
    by = "date"
  )


# ---- climatologies (ERA5 + AWS) ----
era5 <- era5 %>%
  mutate(md = format(date, "%m-%d")) %>%
  group_by(md) %>%
  mutate(
    t2m.climatological = mean(t2m, na.rm = TRUE),
    tp.climatological  = mean(tp,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-md)

aws <- aws %>%
  mutate(md = format(date, "%m-%d")) %>%
  group_by(md) %>%
  mutate(t2m.climatological = mean(t2m, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-md)

## Convert all into m
t4m$depth <- t4m$depth / 100
agemodel$depth <- agemodel$depth / 100

# ---- dated trench profile ----
t4m_dated <- TrenchWithDates(t4m, agemodel, source = "T19")

inputs <- list(
  aws = aws,
  era5 = era5,
  echam6 = echam6,
  t4m = t4m,
  t4m_dated = t4m_dated,
  agemodel = agemodel
)

saveRDS(inputs, here("data", "processed", "inputs.rds"))

