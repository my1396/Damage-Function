*! Dynamic panel: lagged dependent variable added to the M = 8 interactive model.
*!
*! Four columns:
*!   (1) Stationary sample, within estimator (AFE) with L.logd_gdp
*!   (2) Stationary sample, Arellano-Bond difference GMM (xtabond2)
*!   (3) Full sample,       within estimator (AFE) with L.logd_gdp
*!   (4) Full sample,       Arellano-Bond difference GMM (xtabond2)
*!
*! GMM specification follows S4-1_AB_estimator_xtabond2.do exactly:
*!   gmm(L.logd_gdp)  -- lagged y instrumented by its own deeper lags
*!   iv(climate i.year) -- weather treated as strictly exogenous
*!   nolevel robust     -- difference GMM, robust SE
*! Both samples get year FE and country-specific quadratic trends, as in the
*! static specifications.
*!
*! All three estimators within a sample are run on the SAME observations --
*! e(sample) from the difference GMM, which is the binding constraint.
*!
*! Out: Revision_2026Aug/output/dynamic_lagged_y.txt / .csv / .tex

clear all
set more off
set linesize 200
cd "/Users/menghan/Documents/GDP/Shared folder"
capture mkdir "Revision_2026Aug/output"

local X8   tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2
local KEEP L.logd_gdp `X8'

* Scratch lines for stepping through the loop body interactively -- set one of
* these by hand, then run the loop contents. They must stay commented out: in
* batch mode the second gen aborts the do-file with r(110).
// gen smp = "stat"
// gen smp = "full"

*==============================================================================*
**# Loop over the two samples
*==============================================================================*
foreach smp in stat full {

    if "`smp'" == "stat" {
        use "data/GDP_reg_panelData.dta", clear
    }
    else {
        use "data/GDP_reg_panelData_full_sample.dta", clear
        capture drop if iso == "KIR"
    }

    xtset country_id year

    * trend variables on the calendar year, as in S4-1
    capture drop trend trend2
    sort country_id year
    bys country_id (year): gen trend = year - 1961
    gen trend2 = trend^2

    *--- (a) Arellano-Bond difference GMM ------------------------------------*
    xtabond2 logd_gdp L.logd_gdp `X8' ///
        i.year c.trend#i.country_id c.trend2#i.country_id, ///
        gmm(L.logd_gdp) ///
        iv(`X8' i.year) ///
        nolevel robust
    * xtabond2 already stores e(ar1p), e(ar2p), e(hansenp), e(j) -- no estadd needed
    estimates store GMM_`smp'

    * COMMON ESTIMATION SAMPLE. Difference GMM loses one further period per
    * country than the within estimator: the differenced equation at t needs
    * y_t and y_{t-1}, and L.logd_gdp is itself a lag, so the first two usable
    * years are consumed. Left unrestricted, AFE would be fitted on 122 (170)
    * more observations than the GMM columns and the comparison would confound
    * estimator with sample. All three columns are therefore run on e(sample)
    * from the GMM above.
    capture drop esamp
    gen byte esamp = e(sample)

    *--- (b) AB GMM with COLLAPSED instruments --------------------------------*
    * The uncollapsed spec generates ~1,660 moment conditions against 122-170
    * countries. Hansen p = 1.000 is then a symptom of a test with no power and
    * an overfitted weight matrix, not evidence of valid instruments
    * (Roodman 2009). `collapse` reduces the gmm() block to one instrument per
    * lag distance instead of one per lag-by-period.
    * The deterministic country trends must also be moved into iv(): they are
    * strictly exogenous by construction, and without them the collapsed model
    * is underidentified (244 trend regressors, no moment conditions). Note the
    * uncollapsed column above leaves them out of iv() and relies on the
    * oversized gmm() block to identify them -- itself questionable.
    xtabond2 logd_gdp L.logd_gdp `X8' ///
        i.year c.trend#i.country_id c.trend2#i.country_id, ///
        gmm(L.logd_gdp, collapse) ///
        iv(`X8' i.year c.trend#i.country_id c.trend2#i.country_id) ///
        nolevel robust
    estimates store GMMc_`smp'

    * the collapsed variant must land on the same observations
    count if e(sample) != esamp
    if r(N) > 0 {
        di as error "[`smp'] collapsed GMM sample differs in " r(N) " obs"
    }

    *--- (c) AFE within estimator with lagged y, on the GMM sample 
    reghdfe logd_gdp L.logd_gdp `X8' if esamp, ///
        absorb(i.year c.trend#i.country_id c.trend2#i.country_id) ///
        vce(cluster country_id)
    estimates store AFE_`smp'
}

*==============================================================================*
**# Side-by-side table
*==============================================================================*
local MODELS AFE_stat GMM_stat GMMc_stat AFE_full GMM_full GMMc_full

local VLAB varlabels(               ///
    L.logd_gdp "L.dlnGDP"           ///
    tmp        "T"                  ///
    tmp2       "T^2"                ///
    pre        "P"                  ///
    pre2       "P^2"                ///
    tmp_pre    "T x P"              ///
    tmp2_pre   "T^2 x P"            ///
    tmp_pre2   "T x P^2"            ///
    tmp2_pre2  "T^2 x P^2" )

local TABOPT keep(`KEEP') order(`KEEP') b(%9.5f) se(%9.5f)                    ///
    star(* 0.10 ** 0.05 *** 0.01)                                             ///
    mgroups("Stationary sample" "Full sample", pattern(1 0 0 1 0 0))              ///
    mtitles("AFE" "GMM" "GMM coll." "AFE" "GMM" "GMM coll.")                                    ///
    stats(N N_g ar1p ar2p hansenp j,                                             ///
          labels("Observations" "Countries" "AR(1) p" "AR(2) p" "Hansen p" "Instruments") ///
          fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f %9.0fc))                              ///
    `VLAB' nonumbers

di _n(2) "===== Side-by-side: dynamic models with lagged y ====="
esttab `MODELS', `TABOPT'

esttab `MODELS' using "Revision_2026Aug/output/dynamic_lagged_y.txt", ///
    `TABOPT' fixed replace

esttab `MODELS' using "Revision_2026Aug/output/dynamic_lagged_y.csv", ///
    `TABOPT' csv replace

esttab `MODELS' using "Revision_2026Aug/output/dynamic_lagged_y.tex", ///
    keep(`KEEP') order(`KEEP') b(%9.5f) se(%9.5f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mgroups("Stationary sample" "Full sample", pattern(1 0 0 1 0 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    mtitles("AFE" "GMM" "GMM coll." "AFE" "GMM" "GMM coll.") ///
    stats(N N_g ar1p ar2p hansenp j, ///
          labels("Observations" "Countries" "AR(1) $p$" "AR(2) $p$" "Hansen $p$" "Instruments") ///
          fmt(%9.0fc %9.0fc %9.3f %9.3f %9.3f %9.0fc)) ///
    varlabels(L.logd_gdp "$\Delta\ln GDP_{t-1}$" ///
        tmp "$T$" tmp2 "$T^2$" pre "$P$" pre2 "$P^2$" ///
        tmp_pre "$T \times P$" tmp2_pre "$T^2 \times P$" ///
        tmp_pre2 "$T \times P^2$" tmp2_pre2 "$T^2 \times P^2$") ///
    nonumbers booktabs replace

di _n(2) "===== p-values ====="
estimates table `MODELS', keep(`KEEP') b(%10.6f) p(%8.4f)

di _n(2) "===== Written to Revision_2026Aug/output/dynamic_lagged_y.{txt,csv,tex} ====="
exit, clear STATA
