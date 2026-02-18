# scripts/10_run_sims_fig01.R
source(here::here("scripts", "01_load_inputs.R"))
library(FirnR)

dir.create(here("output", "sims"), recursive = TRUE, showWarnings = FALSE)

met.time <- aws$date
idx      <- met.time <= endDate
met.tp   <- aws$tp.era5
met.proxy<- aws$t2m  # temperature in Fig1 (units preserved)

# sim1: constant accumulation
sim1 <- FirnR::SimProfile(
  time        = met.time[idx],
  precip      = rep(mean(met.tp[idx]), sum(idx)),
  temperature = met.proxy[idx],
  diffuse     = FALSE,
  rho.surface = rho.surface,
  dz.out      = 1/1000
) |> RenameFirnRSignal()

# sim2: variable accumulation (no diffusion)
sim2 <- FirnR::SimProfile(
  time        = met.time[idx],
  precip      = met.tp[idx],
  temperature = met.proxy[idx],
  diffuse     = FALSE,
  rho.surface = rho.surface,
  dz.out      = 1/1000
) |> RenameFirnRSignal()

# sim3: variable accumulation + diffusion
sim3 <- FirnR::SimProfile(
  time        = met.time[idx],
  precip      = met.tp[idx],
  temperature = met.proxy[idx],
  diffuse     = TRUE,
  rho.surface = rho.surface,
  dz.out      = 1/1000
) |> RenameFirnRSignal()

# sim4: sim3 dated + binned to T4M resolution
sim3_dated <- DateSimByAgeModel(sim3, agemodel)
sim4 <- BinDatedSimToT4M(sim3_dated, t4m, value_col = "signal")

saveRDS(
  list(sim1 = sim1, sim2 = sim2, sim3 = sim3, sim4 = sim4),
  here("output", "sims", "fig01_sims.rds")
)
