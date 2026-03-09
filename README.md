# t4m_ms26

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
scripts/51_fig05.R

All figures will be written to `output/figures/`.

