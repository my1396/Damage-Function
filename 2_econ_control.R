## Add economic controls; check alternative specifications
##      1. rich-poor; agricultural-nonagricultural countries;
##      2. oil-nonoil producers;
##      3. conventional economic drivers in the production function;
##      4. check precipitation volatility;
source("fun_script.R")

library(tidyverse)
library(plm) 
library(lmtest)
library(broom) 

Pdata <- readRDS("data/Pdata.RDS")
Pdata %>% head(5) %>% as.data.frame()
colnames(Pdata)
regressor_t <- colnames(Pdata)[startsWith(colnames(Pdata), "T")]
regressor_t

## rich-poor; agr-nonagr dummy ## ----------------------------------------------
# significant difference between rich and poor w.r.t. precip. sensitivity
# no significant difference between agricultural and non-agri. countries
econ_df <- read_csv("data/rich-agr.csv")
Pdata <- Pdata %>% 
    left_join(econ_df[,c("ISO_C3", "year", "poor", "agr")], 
              by=c("iso"="ISO_C3", "year"="year") )
Pdata <- Pdata %>% 
    mutate(tmp_poor = tmp*poor,
           tmp2_poor = tmp2*poor,
           pre_poor = pre*poor,
           pre2_poor = pre2*poor
    )
Pdata <- Pdata %>% 
    mutate(tmp_agr = tmp*agr,
           tmp2_agr = tmp2*agr,
           pre_agr = pre*agr,
           pre2_agr = pre2*agr)
colnames(Pdata)[!str_starts(colnames(Pdata), "T[0-9]_")]

## rich-poor control
regressor_v_econ <- c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2", "tmp_poor", "tmp2_poor", "pre_poor", "pre2_poor")
## agricultural control
regressor_v_econ <- c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2", "tmp_agr", "tmp2_agr", "pre_agr", "pre2_agr")

reg_f.gdp.econ <- formula(paste("logD_gdp ~ ", 
                                paste(c(regressor_v_econ, regressor_t), collapse=" + ")))
# reg_f.gdp.econ 
ml.gdp.econ <- plm(reg_f.gdp.econ, data=Pdata, index=c("iso", "year"), effect="twoways", model="within")
coef.gdp.econ <- coeftest(ml.gdp.econ, vcovHC(ml.gdp.econ, type = 'HC0', cluster = 'group'))
econ_result <- tidy_coeftest(coef.gdp.econ, 20)
econ_result

## control for oil producers ## ------------------------------------------------
# non significant difference between oil and non-oil producers
oil_df <- read_csv("data/fuel_export_avg.csv")
Pdata <- Pdata %>% 
    left_join(oil_df %>% select(ISO_C3, oil_rent_dummy), 
              by=c("iso" = "ISO_C3") )
Pdata <- Pdata %>% 
    mutate(tmp_oil = tmp*oil_rent_dummy,
           tmp2_oil = tmp2*oil_rent_dummy,
           pre_oil = pre*oil_rent_dummy,
           pre2_oil = pre2*oil_rent_dummy)
regressor_v_oil <- c("tmp", "tmp2", "pre", "pre2", 
                     "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
                     "tmp_oil", "tmp2_oil", "pre_oil", "pre2_oil")
reg_f.gdp.oil <- formula(paste("logD_gdp ~ ", 
                               paste(c(regressor_v_oil, regressor_t), collapse=" + ")))
Pdataml.gdp.oil <- plm(reg_f.gdp.oil, data=Pdata, index=c("iso", "year"), effect="twoways", model="within")
coef.gdp.oil <- coeftest(Pdataml.gdp.oil, vcovHC(Pdataml.gdp.oil, type = 'HC0', cluster = 'group'))
oil_result <- tidy_coeftest(coef.gdp.oil, 20)
oil_result

## WDI political, institutional, investment factors ----------------------------
#       World Development Indicators from World Bank
#       Gross capital formation (% of GDP) -> 100*log
#       Tax revenue (% of GDP) -> 100*log
#       Adjusted net enrollment rate, primary (% of primary school age children) -> 100*log
#       Mortality rate, infant (per 1,000 live births) -> 100*log
#       Total population growth -> 100*Δlog
econ_control <- read_csv("data/WDI_WB_econ.csv")
econ_control

Pdata <- Pdata %>% 
    left_join(econ_control %>% select(-ISO_N3, -cntry.name), by=c("iso"="ISO_C3", "year"="year"))

econ_control_var <- c("gross_invest_log", "tax_rev_log", "edu_log", "infant_mortality_log", "total_population_gr")
econ_control_var

# specify which econ var to include
regressor_v_econ <- c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2", econ_control_var[c(1, 2)])
cat (sprintf("Econ controls: %s", paste(regressor_v_econ[regressor_v_econ %ni% regressor_v1], collapse = "; ") ) )

reg_f.gdp.econ <- formula(paste("logD_gdp ~ ", 
                                     paste(c(regressor_v_econ, regressor_t), collapse=" + ")))
ml.gdp.interact.econ <- plm(reg_f.gdp.econ, data=Pdata, index=c("iso", "year"), effect="twoways", model="within")
nobs(ml.gdp.interact.econ) # size of data sample change wrt econ var included

coef.gdp.interact.econ <- coeftest(ml.gdp.interact.econ, vcovHC(ml.gdp.interact.econ, type = 'HC0', cluster = 'group'))
tidy_coeftest(coef.gdp.interact.econ, 15)

## use econ controls as dependent variable -------------------------------------
## econ control as dependent var 
#   If economic controls are themselves functions of climate variables;
#   it will be problematic to include them in the model specification.
#   The coefficient of climate variables can no longer capture the partial effects. 
the_var <- econ_control_var[1]
the_var
reg_f.gdp.econDep <- formula(paste(the_var, "~", 
                                            paste(c(regressor_v1, regressor_t), collapse=" + ")))

ml.gdp.econDep <- plm(reg_f.gdp.econDep, data=Pdata, index=c("iso", "year"), effect="twoways", model="within")
nobs(ml.gdp.econDep) # no.obs

coef.gdp.econDep <- coeftest(ml.gdp.econDep, vcovHC(ml.gdp.econDep, type = 'HC0', cluster = 'group'))
tidy_coeftest(coef.gdp.econDep, 10)

## Precipitation volatility ## -------------------------------------------------
## check if precip. volatility makes a difference to precip. volatility
# precipitation trend, volatility per country 
# no significant effects
climate_df_pre <- read_csv("data/climate/climate_trend_pre.csv")
climate_df_pre

Pdata <- Pdata %>% 
    left_join(climate_df_pre[,c("ISO_C3", "avg_vol_btwYr", "avg_vol_withinYr")], 
              by=c("iso"="ISO_C3"))

Pdata <- Pdata %>% 
    mutate(Pavg_vol_btwYr = pre*avg_vol_btwYr,
           P2avg_vol_btwYr = pre2*avg_vol_btwYr,
           Pavg_vol_withinYr = pre*avg_vol_withinYr,
           P2avg_vol_withinYr = pre2*avg_vol_withinYr
           )
regressor_vol <- c("tmp", "tmp2", "pre", "pre2", 
                   "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
                   "Pavg_vol_btwYr", "P2avg_vol_btwYr", 
                   "Pavg_vol_withinYr","P2avg_vol_withinYr")
reg_f.gdp.vol <- formula(paste("logD_gdp ~ ", 
                               paste(c(regressor_vol, regressor_t), collapse=" + ")))
ml.gdp.stat.vol <- plm(reg_f.gdp.vol, data=Pdata, index=c("iso", "year"), effect="twoways", model="within")
# summary(ml.gdp.stat.vol)
coef.gdp.stat.vol <- coeftest(ml.gdp.stat.vol, vcovHC(ml.gdp.stat.vol, type = 'HC0', cluster = 'group'))
vol_result <- tidy_coeftest(coef.gdp.stat.vol, 15)
vol_result


## test for small changes ##


