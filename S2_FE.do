use "data/GDP_reg_panelData.dta", clear

//# FE within  estimator
xtreg logd_gdp tmp tmp2 pre pre2 ///
	  tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	  i.country_id#c.year_id i.country_id#c.year_id#c.year_id, ///
	  fe vce(robust)
	  
