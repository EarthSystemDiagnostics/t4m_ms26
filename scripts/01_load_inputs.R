# scripts/01_load_inputs.R
source(here::here("scripts", "00_setup.R"))

inputs <- readRDS(here::here("data", "processed", "inputs.rds"))

# ---- unpack ----
aws        <- inputs$aws
era5       <- inputs$era5
echam6     <- inputs$echam6
t4m        <- inputs$t4m
t4m_dated  <- inputs$t4m_dated
agemodel   <- inputs$agemodel

endDate <- as.Date("2018-12-27") #Date of trench sampling
rho.surface <- 330 #
#from Münch, T., Kipfstuhl, S., Freitag, J., Meyer, H., & Laepple, T. (2017). Constraints on post-depositional isotope modifications in East Antarctic firn from analysing temporal changes of isotope profiles. The Cryosphere, 11(5), 2175–2188. https://doi.org/10.5194/tc-11-2175-2017
# / Laepple et al., 2016 The average firn density in the first metre is ∼ 330 kg m−3.