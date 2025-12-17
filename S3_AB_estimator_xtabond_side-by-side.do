use "data/GDP_reg_panelData.dta", clear
//# xtabond2 by David Roodman
xtset country_id year

sort country_id year
bys country_id (year): gen trend = year - 1961   // or starting year
gen trend2 = trend^2

// instrument reduction (enable diagnostic tests)
xtabond2 logd_gdp L.logd_gdp tmp tmp2 pre pre2 ///
    tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
    i.year c.trend#i.country_id c.trend2#i.country_id, ///
    gmm(L.logd_gdp) ///
    iv(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 i.year) ///
	nolevel robust
	
// Store estimates
estimates store dynamic_model_xtabond2


//# Arellano–Bond Estimator
xi: xtabond logd_gdp tmp tmp2 pre pre2 ///
	tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	i.year i.iso|year_id i.iso|year_id2, ///
	lags(1) vce(robust) noconstant
estimates store dynamic_model_xtabond


// Compare climate coefficients
estimates table dynamic_model_xtabond dynamic_model_xtabond2, keep(L.logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) star


//# Save to csv, side-by-side comparison
esttab dynamic_model_xtabond dynamic_model_xtabond2 ///
	using "data/stata/xtabond2_comparison.csv", ///
    keep(L.logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
    mgroups("xtabond" "xtabond2", pattern(1 1)) ///
    csv replace
