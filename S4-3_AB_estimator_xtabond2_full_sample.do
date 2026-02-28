use "data/GDP_reg_panelData_full_sample.dta", clear

drop if iso == "KIR"
//# Arellano–Bond Estimator
// robust SE -> Use this one
xi: xtabond logd_gdp tmp tmp2 pre pre2 ///
	tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	i.year i.iso|year_id i.iso|year_id2, ///
	lags(1) vce(robust) noconstant

estimates store dynamic_model_xtabond
