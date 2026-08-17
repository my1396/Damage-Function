use "data/GDP_reg_panelData_V2.dta", clear
xtset country_id year_id, yearly

local X1  tmp tmp2 pre pre2
local X2  tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2
local ABS i.year_id i.country_id##c.(year_id year_id2)

**# FE within estimator
* Baseline estimator, used as robustness check
* One-way country clustering: reference point for the SE comparison
reghdfe logd_gdp `X1', absorb(`ABS') vce(cluster country_id)
estimates store D_clu

reghdfe logd_gdp `X2', absorb(`ABS') vce(cluster country_id)
estimates store I_clu

**# Two-way cluster (country and year)
reghdfe logd_gdp tmp tmp2 pre pre2, ///
    absorb(i.year_id i.country_id##c.(year_id year_id2)) ///
    vce(cluster country_id year_id)
estimates store D_2way

reghdfe logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2, ///
    absorb(i.year_id i.country_id##c.(year_id year_id2)) ///
    vce(cluster country_id year_id)
estimates store I_2way

**# Driscoll-Kraay SE
reghdfe logd_gdp tmp tmp2 pre pre2, ///
    absorb(i.year_id i.country_id##c.(year_id year_id2)) ///
    vce(dkraay 3)
estimates store D_dk

reghdfe logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2, ///
    absorb(i.year_id i.country_id##c.(year_id year_id2)) ///
    vce(dkraay 3)
estimates store I_dk


**# ------------------------------------------------------------------------ #
**# Side-by-side comparison
**# ------------------------------------------------------------------------ #
* Point estimates are identical within each specification; only the SEs change.

local MODELS D_clu D_2way D_dk I_clu I_2way I_dk

local VLAB varlabels(      ///
    tmp       "T"          ///
    tmp2      "T^2"        ///
    pre       "P"          ///
    pre2      "P^2"        ///
    tmp_pre   "T x P"      ///
    tmp2_pre  "T^2 x P"    ///
    tmp_pre2  "T x P^2"    ///
    tmp2_pre2 "T^2 x P^2" )

local TABOPT keep(`X2') b(%9.5f) se(%9.5f)                                    ///
    star(* 0.10 ** 0.05 *** 0.01)                                             ///
    mgroups("Direct" "Interactive", pattern(1 0 0 1 0 0))                     ///
    mtitles("Cluster" "Two-way" "DK(3)" "Cluster" "Two-way" "DK(3)")          ///
    stats(N r2, labels("Observations" "R-squared") fmt(%9.0fc %9.4f))         ///
    `VLAB' nonumbers

di _n(2) "===== p-values ====="
estimates table `MODELS', keep(`X2') b(%10.6f) p(%8.4f)

di _n(2) "===== Side-by-side ====="
esttab `MODELS', `TABOPT'

capture mkdir "Revision_2026Aug/output"
esttab `MODELS' using "Revision_2026Aug/output/FE_SE_comparison.txt", `TABOPT' fixed replace

di _n(2) "===== Written to Revision_2026Aug/output/FE_SE_comparison.txt ====="
