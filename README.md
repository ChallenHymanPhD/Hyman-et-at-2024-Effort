# Modeling Recreational Reef Fish Effort on Florida's Gulf Coast
 
Data and R code to reproduce the analysis in:
 
> Hyman, A. C., Chagaris, D., Drexler, M., & Frazer, T. K. (2024). Modeling effort in a multispecies recreational fishery; influence of species-specific temporal closures, relative abundance, and seasonality on monthly angler-trips. *Fisheries Research*, 279, 107136. https://doi.org/10.1016/j.fishres.2024.107136
 
## Overview
 
This study modeled monthly recreational reef fish effort (angler-trips) along the west coast of Florida as a function of species-specific temporal management for gag (*Mycteroperca microlepis*), red snapper (*Lutjanus campechanus*), and red grouper (*Epinephelus morio*). Other predictors were relative abundance, seasonality, angler participation, economic conditions, and weather. Separate effects were estimated for the Panhandle and Peninsula regions.
 
Main findings:
 
- Red snapper and gag temporal management directly influenced recreational effort.
- Effort increased with the fraction of each month open to harvest for both species.
- Shorter red snapper seasons were associated with higher effort (effort compression), particularly in the Panhandle.
- Gag season length was not associated with effort, so there was no evidence yet of effort compression for gag.
## Data and model
 
**Response.** Monthly angler-trips, 2004 to 2023, from the NMFS Marine Recreational Information Program (MRIP) Access Point Angler Intercept Survey (APAIS), weighted by Fishing Effort Survey (FES) sample weights. The analysis kept nearshore and offshore private-vessel and charter trips that targeted or caught at least one of 31 reef fish species. Data were aggregated by month for two regions:
 
- **Panhandle** (MRIP Florida region 1)
- **Peninsula** (MRIP Florida region 2)
**Model.** Bayesian Gamma regression fit with [`brms`](https://paul-buerkner.github.io/brms/). Both the mean (log link) and the shape parameter are modeled, with region-specific coefficients for every term:
 
$$
\begin{aligned}
\text{Trips}_{r,t} &\sim \text{Gamma}(\mu_{r,t},\, k_{r,t})\\
\log(\mu_{r,t}) &= \mathbf{x}_{r,t}^\top \boldsymbol{\beta}_r\\
\log(k_{r,t}) &= \mathbf{h}_{t}^\top \boldsymbol{\delta}_r
\end{aligned}
$$
 
where $r$ indexes region, $t$ indexes month, and $\mathbf{h}_t$ contains the harmonic terms. Priors on all coefficients are $\text{Normal}(0, 10)$.
 
**Predictors of mean effort** (variable names in the model dataset):
 
| Variable | Description |
|---|---|
| `M_Gag`, `M_RS` | Fraction of the month open to harvest (gag, red snapper) |
| `S_GagL`, `S_RSL` | Log season length, scaled by the fraction of the month open |
| `CPUE_Gag_lag`, `CPUE_RG_lag`, `CPUE_RS_lag` | Prior-year catch-per-unit-effort for the same month and region, scaled by the fraction of the month open |
| `Sales` | Florida saltwater fishing license sales |
| `FIR` | Ratio of inflation-adjusted median household income to gasoline price |
| `Wind` | Monthly mean wind speed from NOAA buoys |
| `sin1`, `cos1`, `sin2`, `cos2` | Annual and semi-annual harmonic terms (also used for the shape parameter) |
 
Red grouper management terms were left out because closures covered too few days to estimate their effects.
 
**Validation.** Fifty randomly selected monthly estimates were withheld and predicted out of sample.
 
## Repository structure
 
Files are split across three branches:
 
| Branch | Contents |
|---|---|
| [`main`](https://github.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/tree/main) | This README |
| [`R-files`](https://github.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/tree/R-files) | Analysis scripts, numbered in the order they are run |
| [`Data-Files`](https://github.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/tree/Data-Files) | Input data, the aggregated model dataset, and the fitted model |
 
### Scripts (`R-files`)
 
| Script | Purpose | Output |
|---|---|---|
| `1) Hyman et al 2024 APAIS Data Aggregation.R` | Reads raw MRIP catch and trip files (2003–2023), subsets to Florida Gulf reef fish trips, and assigns counties to MRIP regions | `APAIS.csv` |
| `2)_Fishable_days.R` | Downloads daily NOAA buoy wind and wave data for 16 Gulf buoys (2004–2023) and aggregates by region | `Fishable_days.csv` |
| `3)_Hyman_et_al_2024_Dataset_Aggregation.R` | Merges APAIS effort and catch with management, economic, license, buoy, and juvenile-index data; builds lagged CPUE and harmonic terms | `MRIP Effort and Catch data.csv` |
| `4)_Hyman_et_al_2024_Effort_Analysis.R` | Fits (or loads) the effort model, runs diagnostics and out-of-sample checks, and produces all figures and tables | Figures 3, 4, 6, S3, S4; parameter tables |
 
### Data (`Data-Files`)
 
| File | Description |
|---|---|
| `MRIP Effort and Catch data.csv` | Aggregated monthly model dataset (output of script 3; input to script 4) |
| `Effort_Model.rds` | Fitted `brms` model object |
| `Seasons.csv` | Daily open/closed status by species and region |
| `Bag limits.csv` | Bag limits by species and year |
| `Licsense sales.csv` | Florida saltwater fishing license sales |
| `Florida gas.csv` | Florida gasoline prices |
| `Income.csv` | Florida median household income |
| `Consumer price index.csv`, `Inflation rate.csv` | Inflation adjustment |
| `NOAABuoyData.csv` | Daily NOAA buoy wind and wave data by region |
| `Gag juvenile indices.csv` | Gag juvenile abundance indices |
| `MRIP-Survey-Variables-for-Web.xls` | MRIP variable definitions |
 
## Reproducing the analysis
 
### Requirements
 
- R (≥ 4.1) with a working C++ toolchain for Stan
- CRAN packages:
```r
install.packages(c("dplyr", "tidyr", "tidyverse", "lubridate", "ggplot2",
                   "ggrepel", "ggpubr", "RColorBrewer", "xtable", "readxl",
                   "rstan", "brms", "forecast", "mapview", "sf"))
```
 
- [`cmdstanr`](https://mc-stan.org/cmdstanr/) (loaded by script 4)
- [`rnoaa`](https://github.com/ropensci/rnoaa) (script 2 only). It has been retired from CRAN, so install it from the CRAN archive or GitHub.
### Quick start: model results only
 
Script 4 can be run without the upstream steps. Download `MRIP Effort and Catch data.csv` and `Effort_Model.rds` from the `Data-Files` branch into your working directory, then run:
 
```r
source("4)_Hyman_et_al_2024_Effort_Analysis.R")
```
 
With `run_model <- F` (the default), the script loads the saved model instead of refitting it. Set `run_model <- T` to refit from scratch.
 
### Full pipeline
 
1. **Raw MRIP data.** Download MRIP APAIS `catch_*.csv` and `trip_*.csv` files for 2003–2023 from [NOAA Fisheries](https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads) into one folder, and set `DIRECTORY` in script 1 to that folder.
2. Run scripts 1 through 4 in order. Scripts 3 and 4 read most supporting data directly from the `Data-Files` branch by URL; `APAIS.csv` from script 1 must be in the working directory when running script 3.
## Citation
 
If you use this code or data, please cite:
 
```bibtex
@article{Hyman2024Effort,
  author  = {Hyman, A. Challen and Chagaris, David and Drexler, Michael and
             Frazer, Thomas K.},
  title   = {Modeling effort in a multispecies recreational fishery; influence of
             species-specific temporal closures, relative abundance, and
             seasonality on monthly angler-trips},
  journal = {Fisheries Research},
  year    = {2024},
  volume  = {279},
  pages   = {107136},
  doi     = {10.1016/j.fishres.2024.107136}
}
```
 
## Contact
 
A. Challen Hyman, PhD
University of South Florida, College of Marine Science / FWC Fish and Wildlife Research Institute
 
