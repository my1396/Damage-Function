use "data/GDP_reg_panelData.dta", clear
//# xtabond2 by David Roodman
xtset country_id year

sort country_id year
bys country_id (year): gen trend = year - 1961   // or starting year
gen trend2 = trend^2


xtabond2 logd_gdp L.logd_gdp tmp tmp2 pre pre2 ///
    tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
    i.year c.trend#i.country_id c.trend2#i.country_id, ///
    gmm(L.logd_gdp) ///
    iv(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 i.year) ///
	nolevel robust
	
// Store estimates
estimates store dynamic_model_xtabond2



//# with Distributed Lagg terms
xtabond2 logd_gdp L.logd_gdp tmp tmp2 pre pre2 ///
    tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	L.tmp L.tmp2 L.pre L.pre2 ///
    i.year c.trend#i.country_id c.trend2#i.country_id, ///
    gmm(L.logd_gdp) ///
    iv(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 i.year) ///
	nolevel robust


gen tmplag_pre = tmp * L.pre
//# lagged independent variables not significant
xtabond2 logd_gdp L.logd_gdp tmp tmp2 pre pre2 ///
    tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 tmplag_pre ///
    i.year c.trend#i.country_id c.trend2#i.country_id, ///
    gmm(L.logd_gdp) ///
    iv(tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 i.year) ///
	nolevel robust



