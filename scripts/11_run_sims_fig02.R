# scripts/11_run_sims_fig02.R
source(here::here("scripts", "01_load_inputs.R"))

library(FirnR)
library(dplyr)
library(zoo)

dir.create(here::here("output", "sims"), recursive = TRUE, showWarnings = FALSE)

# --- helper: simulate only (raw FirnR profile) ---
run_sim <- function(met.time, met.tp, met.signal, source,
                    diffuse = TRUE, rho.surface = 330, dz.out = 1/1000) {
  
  idx <- met.time <= endDate
  
  sim <- FirnR::SimProfile(
    time        = met.time[idx],
    precip      = met.tp[idx],
    temperature = met.signal[idx],
    diffuse     = diffuse,
    rho.surface = rho.surface,
    dz.out      = dz.out
  ) |>
    RenameFirnRSignal()
  
  # source direkt dran, damit du es später immer hast
  sim$source <- source
  sim
}

# --- helper: raw sim -> dated -> binned (Fig02-format) ---
date_bin <- function(sim, source) {
  sim_dated <- DateSimByAgeModel(sim, agemodel)
  
  out <- BinDatedSimToT4M(sim_dated, t4m, value_col = "signal") %>%
    mutate(source = source)
  
  out
}

# --- special: ERA5 tp mapped onto ECHAM6 time axis ---
era5_tp_on_echam6 <- left_join(
  echam6 %>% select(date),
  era5  %>% select(date, tp),
  by = "date"
)$tp

# --- 1) run all RAW simulations ---
sims_raw <- list(
  era5 = run_sim(
    met.time   = era5$date,
    met.tp     = era5$tp,
    met.signal = era5$t2m,
    source     = "ERA5 t2m",
    rho.surface = rho.surface
  ),
  
  aws9_t2m_clim = run_sim(
    met.time   = aws$date,
    met.tp     = aws$tp.era5,
    met.signal = aws$t2m.climatological,
    source     = "AWS9 t2m climatology precip ERA",
    rho.surface = rho.surface
  ),
  
  aws9 = run_sim(
    met.time   = aws$date,
    met.tp     = aws$tp.era5,
    met.signal = aws$t2m,
    source     = "AWS9 t2m precip ERA",
    rho.surface = rho.surface
  ),
  
  era5_t2m_clim = run_sim(
    met.time   = era5$date,
    met.tp     = era5$tp,
    met.signal = era5$t2m.climatological,
    source     = "ERA5 t2m climatology",
    rho.surface = rho.surface
  ),
  
  era5_tp_clim = run_sim(
    met.time   = era5$date,
    met.tp     = era5$tp.climatological,
    met.signal = era5$t2m,
    source     = "ERA5 t2m (tp climatology)",
    rho.surface = rho.surface
  ),
  
  echam6_d18O = run_sim(
    met.time   = echam6$date,
    met.tp     = echam6$tp,
    met.signal = zoo::na.approx(echam6$oxy, rule = 2),
    source     = "ECHAM6 d18O",
    rho.surface = rho.surface
  ),
  
  echam6_d18O_precipERA = run_sim(
    met.time   = echam6$date,
    met.tp     = era5_tp_on_echam6,
    met.signal = zoo::na.approx(echam6$oxy, rule = 2),
    source     = "ECHAM6 d18O precip ERA",
    rho.surface = rho.surface
  ),
  
  echam6_t2m = run_sim(
    met.time   = echam6$date,
    met.tp     = echam6$tp,
    met.signal = echam6$t2m,
    source     = "ECHAM6 t2m",
    rho.surface = rho.surface
  )
)

# --- 2) create dated+binned versions (as before) ---
sims_binned <- bind_rows(lapply(names(sims_raw), function(nm) {
  date_bin(sims_raw[[nm]], source = sims_raw[[nm]]$source[1])
}))

# --- 3) combine trench data with binned sims (Fig02) ---
df_all <- bind_rows(
  t4m_dated,
  sims_binned
)

# --- save both products ---
saveRDS(df_all,      here::here("output", "sims", "fig02_df_all.rds"))
saveRDS(sims_raw,    here::here("output", "sims", "fig02_sims_raw.rds"))
# optional: auch binned separat
saveRDS(sims_binned, here::here("output", "sims", "fig02_sims_binned.rds"))