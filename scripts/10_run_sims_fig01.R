# scripts/10_run_sims_fig01.R
source(here::here("scripts", "01_load_inputs.R"))
library(FirnR)

dir.create(here("output", "sims"), recursive = TRUE, showWarnings = FALSE)

met.time <- aws$date
idx      <- met.time <= endDate
met.tp   <- aws$tp.era5
met.proxy<- aws$t2m  

# sim1: constant accumulation 3cm binning
sim1 <- FirnR::SimProfile(
  time        = met.time[idx],
  precip      = rep(mean(met.tp[idx]), sum(idx)),
  temperature = met.proxy[idx],
  diffuse     = FALSE,
  rho.surface = rho.surface,
  dz.out      = 3/100
) |> RenameFirnRSignal()

# sim2: variable accumulation (no diffusion), 3cm binning
sim2 <- FirnR::SimProfile(
  time        = met.time[idx],
  precip      = met.tp[idx],
  temperature = met.proxy[idx],
  diffuse     = FALSE,
  rho.surface = rho.surface,
  dz.out      = 3/100
) |> RenameFirnRSignal()

# sim3: variable accumulation + diffusion, 3cm binning
sim3 <- FirnR::SimProfile(
  time        = met.time[idx],
  precip      = met.tp[idx],
  temperature = met.proxy[idx],
  diffuse     = TRUE,
  rho.surface = rho.surface,
  dz.out      = 3/100
) |> RenameFirnRSignal()

saveRDS(
  list(sim1 = sim1, sim2 = sim2, sim3 = sim3),
  here("output", "sims", "fig01_sims.rds")
)
