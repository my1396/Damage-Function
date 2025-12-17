use "data/GDP_reg_panelData.dta", clear

xtset country_id year_id, yearly

//# dynamic CCE: short-run
xtdcce2 logd_gdp L.logd_gdp tmp tmp2 pre pre2 ///
	tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	year_id year_id2, ///
	cr(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) ///
	reportc

// pooled coefficients
xtdcce2 logd_gdp L.logd_gdp tmp tmp2 pre pre2 ///
	tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	year_id year_id2, ///
	cr(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) ///
	pooled(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) ///
	reportc

//# dynamic CCE: long-run

