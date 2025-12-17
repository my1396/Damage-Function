// Test Whether Country-Specific Trends Are Needed

// Model 1: No country-specific trends
xi: xtabond logd_gdp tmp tmp2 pre pre2 ///
    tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
    i.year year_id year_id2, ///
    lags(1) maxldep(3) vce(gmm) noconstant
estimates store global_trend

// Model 2: Country-specific trends only
xi: xtabond logd_gdp tmp tmp2 pre pre2 ///
    tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
    i.year i.iso|year_id i.iso|year_id2, ///
    lags(1) maxldep(3) vce(gmm) noconstant
estimates store country_trend

// Compare climate coefficients
estimates table global_trend country_trend, keep(L.logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) star
