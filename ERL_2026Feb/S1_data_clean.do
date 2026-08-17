**# clean dataset for Stata 
import delimited "/Users/menghan/Documents/GDP/Shared folder/data/GDP_reg_panelData_V2.csv", clear

destring logd_gdp-rad, replace ignore(`"NA"')
egen float country_id = group(iso)
egen float year_id = group(year)
xtset country_id year, yearly

generate tmp_pre = tmp*pre
generate tmp2_pre = tmp^2*pre
generate tmp_pre2 = tmp*pre^2
generate tmp2_pre2 = tmp^2*pre^2
generate float year_id2 = year_id^2

**# FE
xtreg logd_gdp tmp tmp2 pre pre2 ///
	  tmp_pre tmp2_pre tmp_pre2 tmp2_pre2 ///
	  i.country_id#c.year_id i.country_id#c.year_id#c.year_id, ///
	  fe vce(robust)
	  
save "data/GDP_reg_panelData_V2.dta", replace
