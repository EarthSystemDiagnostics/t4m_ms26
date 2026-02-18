# scripts/00_setup.R
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lubridate)

# load functions
source(here("R", "binning_profiles.R"))
source(here("R", "dating_profiles.R"))
source(here("R", "firnr_helpers.R"))
