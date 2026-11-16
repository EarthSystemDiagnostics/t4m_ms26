# t4m_ms26

Scripts and data for the manuscript 2026GL123309, Even in low-accumulation Dronning Maud Land, Antarctica, stable water isotopes record interannual temperature variability, accepted for publication in Geophysical Research Letters.

Thomas Laepple¹,²*, Thomas Münch¹*, Johannes Freitag³, Maria Hörhold³, Martin Werner³, Melanie Behrens³, Nora Hirsch¹ *These authors contributed equally to this work

¹ Alfred-Wegener-Institut Helmholtz-Zentrum für Polar- und Meeresforschung, Potsdam, Germany ² MARUM—Center for Marine Environmental Sciences and Faculty of Geosciences, University of Bremen, Bremen, Germany ³ Alfred-Wegener-Institut Helmholtz-Zentrum für Polar- und Meeresforschung, Bremerhaven, Germany Corresponding author: Thomas Laepple (thomas.laepple@awi.de)

## Data, software and license

- T19 trench δ¹⁸O data (trench profiles on a common depth scale, stack, raw samples): PANGAEA, https://doi.org/10.1594/PANGAEA.998360
- FirnR R package (v0.2.0), used for the simulations: https://doi.org/10.5281/zenodo.24272335 (source: https://github.com/EarthSystemDiagnostics/FirnR)
- This repository is archived on Zenodo with each release.

Code: GPL-3.0-or-later. Data under `data/`: CC-BY-4.0. See `LICENSE`.


- Raw data: `data/raw/`
- Derived/intermediate: `data/interim/`
- Analysis-ready: `data/processed/`
- Reproducible scripts: `scripts/`
- Functions that are used in multiple scripts: `R/`
- Explorations: `experiments/`
- Generated outputs: `output/`


## Reproducing the analysis

1. Clone the repository
2. Run scripts in the following order:

scripts/00_setup.R
scripts/01_build_inputs.R
scripts/01_load_inputs.R

Then generate simulations:

scripts/10_run_sims_fig01.R
scripts/11_run_sims_fig02.R

Finally generate figures:

scripts/20_fig01.R
scripts/21_fig02.R
scripts/22_fig03.R
scripts/41_fig04.R
scripts/51_figS5.R
scripts/61_figS1_precip_comparison.R

All figures will be written to `output/figures/`.

## Depth–age scale of the T19 record

The depth–age scale of the T19 record is provided as `data/processed/t4m_depth_age.csv` and used as input by the scripts above.

