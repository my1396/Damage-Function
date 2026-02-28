use "data/GDP_reg_panelData.dta", clear

//# Arellano–Bond Estimator
// xtabond is NOT used in the final version as it does not have diagnostic test results
// We choose to use xtabond2 by David Roodman in the end.

// robust SE -> Use this one
xi: xtabond logd_gdp tmp tmp2 pre pre2 ///
	tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	i.year i.iso|year_id i.iso|year_id2, ///
	lags(1) vce(robust) noconstant

estimates store dynamic_model_xtabond

xi: xtabond logd_gdp tmp tmp2 pre pre2 ///
	tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	i.year i.iso|year_id i.iso|year_id2, ///
	lags(1) vce(gmm) noconstant


estat sargan
estat abond, artests(4)

// coef vector
matrix b = e(b)
matrix list b

* Show only the first 9 columns
matrix b9 = b[1,1..9]
matrix list b9

matrix V = e(V)
matrix list V
 
mat2txt, matrix(b) saving("data/stata/coef_vector.txt") replace
mat2txt, matrix(V) saving("data/stata/variance_matrix.txt") replace

* Estimation table with stars
estimates table, star keep(L1.logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2)

* Save results to csv for selected variables with SE, t, and p
esttab using "data/stata/xtabond_results.csv", ///
    cells(b se t p) ///
    keep(L.logd_gdp tmp tmp2 pre pre2 tmp_pre tmp2_pre tmp_pre2 tmp2_pre2) ///
    replace
	
