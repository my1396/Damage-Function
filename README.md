# Interactive Effects of Temperature and Precipitation on Global Economic Growth

Country-level panel estimates of how temperature, precipitation, and their
interaction affect GDP per-capita growth, and projections of the resulting
economic impact to 2100 under the SSP scenarios.

The repository holds two snapshots of the analysis:

| Folder | Status |
|---|---|
| [`Revision_2026Aug/`](Revision_2026Aug) | Current revision. All new work goes here. |
| [`ERL_2026Feb/`](ERL_2026Feb) | Version submitted to *Environmental Research Letters* (ERL), February 2026. Frozen. |


Shared across both: [`data/`](data) (inputs), [`helper_function/`](helper_function),
and `figures/` (root-level, untracked).

---

## `Revision_2026Aug/` — current revision

Four things changed relative to the ERL version:

1. **Interactive fixed effects** (Bai 2009) added alongside additive fixed
   effects, to address cross-sectional dependence that survives year effects.
2. **Distributed lags in climate** ($L = 0\ldots5$) to test the growth-effects
   assumption directly rather than assume it.
3. **Global aggregation corrected.** The ERL version averaged country
   *growth rates* with population weights and then cumulated/compounded. 
   The average-and-compound approach overweighs countries with large losses. Now we compound each country’s growth rate and then take the population-weighted average for the global damage projection.
   This change alone moves the 2100 impact from about −65% (ERL documented) to −26% (AFE) / −37% (IFE).
4. **Bootstrapped confidence intervals** for the impact projections.

### Scripts, in run order

Numbering reflects dependencies: scripts run in numeric order. Stata do-files are prefixed with `S`.

| Script | Purpose |
|---|---|
| `_projection_common.R` | Shared projection machinery — inputs, $\eta_{i,t}$, and the global aggregation. Sourced by 5, 7, 8, 9, 90. Single source of truth; do not duplicate the aggregation elsewhere. |
| `1_four_model_comparison.R` | AFE vs IFE × direct ($M=4$) vs interactive ($M=8$), contemporaneous only |
| `2_merge_DK.R` | Merges Driscoll-Kraay standard errors from `ERL_2026Feb/S2-1_FE.do` into the table from 1 |
| `3_lagged_climate.R` | Estimates the **24 models**: 2 specs × 6 lag lengths × 2 estimators. Slow (~20 min). Writes `lagged_climate_fits.rds` with coefficients *and* covariances. |
| `4_lag_coefficient_table.R` | Inverts the reparameterization to recover $\beta_0\ldots\beta_L$ with delta-method SEs |
| `5_projection_lagged.R` | Country and global impacts, $L = 0\ldots5$, all four SSPs |
| `6_plot_lag_diagnostics.R` | Whether the lag structure converges (it does not beyond $L=2$) |
| `7_bootstrap_lagged_projection.R` | 1,000-draw distribution of the 2100 impact and of the full path |
| `8_plot_global_path.R` | Global damage pathways, plus the bootstrap fan chart |
| `9_decompose_IE_effects.R` | Splits the impact into direct and interactive contributions |
| `90_diagnostic_aggregation_comparison.R` | **Diagnostic, not a pipeline stage.** Documents why the aggregation was changed. Runs after 4. |
| `S1_dynamic_lagged_y.do` | Stata: lagged $y$ via within and Arellano-Bond GMM, stationary and full samples, with and without collapsed instruments |

Outputs go to `Revision_2026Aug/output/` (tables and CSVs) and
`Revision_2026Aug/figures/` (PNGs).

### Key results

- Cumulative climate effects attenuate with lag length but stay negative under
  IFE; beyond $L=2$ the lag polynomial is **not identified** (sign-flipped
  projections, a quarter of AFE bootstrap draws positive).
- 2100 global impact under SSP585, interactive specification:
  **−25.9% (AFE)** and **−36.6% (IFE)** at $L=0$; bootstrap 90% interval for IFE roughly $[-59\%, +7\%]$.
- Temperature-precipitation interactions **mitigate** damages by about 14% of the direct-only impact.

---

## `ERL_2026Feb/` — outdated version

Frozen snapshot; retained for reference and reproducibility of the submission to ERL.

Scripts run in numeric order (`1-1` → `7-4`), with the Stata do-files (`S1`–`S4`, `CCE.do`, `country_time_trend.do`) supplying the dynamic-panel estimates that the `4-2-*` R scripts read back in. `fun_script.R` holds user-defined helpers.

Main issues (fixed in the `Revision_2026Aug`): the global aggregation described above. 

---

## `data/`


| Path | Used by |
|---|---|
| `GDP_reg_panelData_V2.csv` / `.dta` | Revision estimation (122 stationary countries, 1961–2019) |
| `GDP_reg_panelData.dta` | Stationary sample, Stata |
| `GDP_reg_panelData_full_sample.dta` | Full sample (170 countries), Stata |
| `cntry_ann_climate_gdpKD_1961to2019.csv` | Baseline GDP per capita for the aggregation |
| `SSP_Population_weight.csv` | Population shares by scenario |
| `baseline_growth/SSP{1,2,3,5}_GrowthProjections.csv` | Counterfactual growth |
| `SSP{126,245,370,585}/climate_trend/climate_trend_{tas,pr}.csv` | Projected climate trends |

---

## Running

R 4.3.1+ with `tidyverse`, `plm`, `plyr`, `lmtest`, `sandwich`, `SMUT`.
Stata 18+ with `reghdfe`, `xtabond2`, `estout`, `ftools`.

```bash
cd "Shared folder"
for f in Revision_2026Aug/{1,2,3,4,5,6,7,8,9}_*.R; do Rscript "$f"; done
```

Script 3 dominates the runtime. Scripts 6, 8, and 9 only need 3–5 and can be
re-run cheaply while iterating on figures. The Stata do-file is run separately:

```bash
stata-se -b do Revision_2026Aug/S1_dynamic_lagged_y.do
```

Absolute paths are set once at the top of each script via `root_dir`; change that
line if the repository moves.
