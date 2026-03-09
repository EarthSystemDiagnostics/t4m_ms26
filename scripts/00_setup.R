# scripts/00_setup.R
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(MethComp)

# load functions
source(here("R", "binning_profiles.R"))
source(here("R", "dating_profiles.R"))
source(here("R", "firnr_helpers.R"))
source(here("R", "plot_dualaxis_helpers.R"))
