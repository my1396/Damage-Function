## Additive fixed effects model
## Dynamic models: with lagged dependent and independent variables
## ARDL(1,1) was estimated 
## M0: static
## M1 to M4: first lagged dependent variable
##      M1 (pgmm) and M2 (pdynmc): no time trend
##      M3: linear time trend
##      M4: quadratic time trend
## M5 to M7: first lagged independent variables
##      M5: no time trend
##      M6: linear time trend
##      M7: quadratic time trend


source("fun_script.R")

library(tidyverse)
library(plm)
library(lmtest)
library(broom)
library(pdynmc)
library(modelsummary)


## data processing ##
## -------------------------------------------------------------------------- ##
f_name <- "data/GDP_reg_panelData.csv"
f_name

Pdata <- read_csv(f_name)
Pdata <- Pdata %>%
  mutate(
      tmp_pre = tmp * pre, 
      tmp2_pre = tmp2 * pre, 
      pre2_tmp = pre2 * tmp, 
      tmp2_pre2 = tmp2 * pre2
      )
Pdata %>%
  head(5) %>%
  as.data.frame()
colnames(Pdata)

target_v <- "logD_gdp"
obs <- sum(!is.na(Pdata[[target_v]]))  ## non-NA obs.
obs

N.obs <- Pdata$iso %>%
  n_distinct()
T.obs <- Pdata$year %>%
  n_distinct()
cat(sprintf("No.sample: %s \nNo.time: %s \n", N.obs, T.obs))

## create country specific trend variables: `regressor_t`
## kronecker product %x%
ttrend <- diag(N.obs) %x% matrix(1:T.obs, ncol = 1)
colnames(ttrend) <- paste("T1", 1:N.obs, sep = "_")

ttrend2 <- diag(N.obs) %x% matrix((1:T.obs)^2, ncol = 1)
colnames(ttrend2) <- paste("T2", 1:N.obs, sep = "_")

Pdata <- cbind(Pdata, ttrend, ttrend2)
regressor_t <- colnames(Pdata)[startsWith(colnames(Pdata), "T")]
regressor_t

## -------------------------------------------------------------------------- ##
## Static ----------------------------------------------------------------------
## Burke baseline ##
regressor_v0 <- c("tmp", "tmp2", "pre", "pre2")

reg_f.gdp <- formula(paste("logD_gdp ~ ", paste(c(regressor_v0,
  regressor_t), collapse = " + ")))
ml.gdp <- plm(reg_f.gdp, data = Pdata, index = c("iso", "year"),
  effect = "twoways", model = "within")
tidy(ml.gdp)
coef.gdp <- coeftest(ml.gdp, vcovHC(ml.gdp, type = "HC0", cluster = "group"))
tidy_coeftest(coef.gdp, 10)


## AFE with interactive terms ##
## -------------------------------------------------------------------------- ##
regressor_v1 <- c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre",
  "pre2_tmp", "tmp2_pre2")
reg_f.gdp.interact <- formula(paste("logD_gdp ~ ", paste(c(regressor_v1,
  regressor_t), collapse = " + ")))

ml.gdp.interact <- plm(reg_f.gdp.interact, data = Pdata, index = c("iso",
  "year"), effect = "twoways", model = "within")
coef.gdp.interact <- coeftest(ml.gdp.interact, vcovHC(ml.gdp.interact,
  type = "HC0", cluster = "group"))
tidy_coeftest(coef.gdp.interact, 10)


## Dynamic ---------------------------------------------------------------------
## Lagged dependent ##
## -------------------------------------------------------------------------- ##


# remove NA values
Pdata_nomissing <- Pdata %>%
  na.omit()
Pdata_nomissing %>%
  colnames()
Pdata_nomissing %>%
  head()

### no time trend --------------------------------------------------------------
reg_f.gdp.interact.AR1 <- pgmm(logD_gdp ~ lag(logD_gdp) + tmp +
  tmp2 + pre + pre2 + tmp_pre + tmp2_pre + pre2_tmp + tmp2_pre2 |
  lag(logD_gdp, 2:3), data = Pdata_nomissing, index = c("iso",
  "year"), effect = "twoways", model = "onestep", collapse = TRUE)
summary(reg_f.gdp.interact.AR1)
summary(reg_f.gdp.interact.AR1)$coef

# summary(reg_f.gdp.interact.AR1)$vcov
# reg_f.gdp.interact.AR1$W[[3]]
# reg_f.gdp.interact.AR1$W[[3]][, c(-1, -2)] %>%
#   ncol()

# with pdynmc
# lagged depend. variable
reg_f.gdp.interact.AR1_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE, varname.reg.fur = c("tmp", "tmp2", "pre",
    "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2"),
    lagTerms.reg.fur = rep(0, 8), include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", opt.meth = "none"
    )
summary(reg_f.gdp.interact.AR1_pdynmc)
summary(reg_f.gdp.interact.AR1_pdynmc)$coef %>%
  head(9)

summary(reg_f.gdp.interact.AR1_pdynmc)$coef %>%
  head(9) %>%
  rownames()

# ARDL(1,1)
# lagged depend. + indep. variable
reg_f.gdp.interact.ARDL1_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE,
    varname.reg.fur = c(
        "tmp", "tmp2", "pre", "pre2", 
        "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2"),
    lagTerms.reg.fur = rep(1, 8), include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", opt.meth = "none"
    )
summary(reg_f.gdp.interact.ARDL1_pdynmc)
summary(reg_f.gdp.interact.ARDL1_pdynmc)$coef %>%
    head(17)

summary(reg_f.gdp.interact.ARDL1_pdynmc)$coef %>%
    head(17) %>% 
    rownames()

### linear time trend ----------------------------------------------------------
# lagged depend. variable
reg_f.gdp.interact.AR1_time_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE, 
    varname.reg.fur = c(
        "tmp", "tmp2", "pre", "pre2", 
        "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
        colnames(ttrend)),
    lagTerms.reg.fur = rep(0, 122+8), 
    include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", opt.meth = "none"
    )
summary(reg_f.gdp.interact.AR1_time_pdynmc)
summary(reg_f.gdp.interact.AR1_time_pdynmc)$coef %>%
    head(9)

# lagged depend. + indep. variable
reg_f.gdp.interact.ARDL1_time_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE, 
    varname.reg.fur = c(
        "tmp", "tmp2", "pre", "pre2", 
        "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
        colnames(ttrend)),
    lagTerms.reg.fur = c(rep(1, 8), rep(0, 122)), 
    include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", opt.meth = "none"
)
summary(reg_f.gdp.interact.ARDL1_time_pdynmc)
summary(reg_f.gdp.interact.ARDL1_time_pdynmc)$coef %>%
    head(17)

### quadratic time trend -------------------------------------------------------
# lagged depend. variable
reg_f.gdp.interact.AR1_time2_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE, 
    varname.reg.fur = c(
        "tmp", "tmp2", "pre", "pre2", 
        "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
        colnames(ttrend), colnames(ttrend2)),
    lagTerms.reg.fur = rep(0, 122*2+8), 
    include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", opt.meth = "none"
)
summary(reg_f.gdp.interact.AR1_time2_pdynmc)
summary(reg_f.gdp.interact.AR1_time2_pdynmc)$coef %>%
    head(9)

# lagged depend. + indep. variable
reg_f.gdp.interact.ARDL1_time2_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE, 
    varname.reg.fur = c(
        "tmp", "tmp2", "pre", "pre2", 
        "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
        colnames(ttrend), colnames(ttrend2)),
    lagTerms.reg.fur = c(rep(1, 8), rep(0, 122*2)), 
    include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", opt.meth = "none"
)
summary(reg_f.gdp.interact.ARDL1_time2_pdynmc)
summary(reg_f.gdp.interact.ARDL1_time2_pdynmc)$coef %>%
    head(17)

## -------------------------------------------------------------------------- ##
# Model summary ----------------------------------------------------------------

# static
M0 <- tidy_coeftest(coef.gdp.interact, 8)
M0$term <- c("L0.tmp", "L0.tmp2", "L0.pre", "L0.pre2", "L0.tmp_pre",
  "L0.tmp2_pre", "L0.pre2_tmp", "L0.tmp2_pre2")
M0

# dynamic tables
M1 <- summary(reg_f.gdp.interact.AR1)$coef %>%
  as_tibble()
colnames(M1) <- c("estimate", "std.error", "statistic", "p.value")
M1 <- M1 %>%
  add_column(term = c("L1.logD_gdp", "L0.tmp", "L0.tmp2", "L0.pre",
    "L0.pre2", "L0.tmp_pre", "L0.tmp2_pre", "L0.pre2_tmp",
    "L0.tmp2_pre2"), .before = 1)
M1

M2 <- summary(reg_f.gdp.interact.AR1_pdynmc)$coef %>%
  head(9)
colnames(M2) <- c("estimate", "std.error", "statistic", "p.value")
M2 <- M2 %>%
  as.data.frame() %>%
  rownames_to_column(var = "term")
M2


# linear time trend
M3 <- summary(reg_f.gdp.interact.AR1_time_pdynmc)$coef %>%
    head(9)
colnames(M3) <- c("estimate", "std.error", "statistic", "p.value")
M3 <- M3 %>%
    as.data.frame() %>%
    rownames_to_column(var = "term")
M3

# quadratic time trend
M4 <- summary(reg_f.gdp.interact.AR1_time2_pdynmc)$coef %>%
    head(9)
colnames(M4) <- c("estimate", "std.error", "statistic", "p.value")
M4 <- M4 %>%
    as.data.frame() %>%
    rownames_to_column(var = "term")
M4

# ARDL
M5 <- summary(reg_f.gdp.interact.ARDL1_pdynmc)$coef %>%
    head(17)
colnames(M5) <- c("estimate", "std.error", "statistic", "p.value")
M5 <- M5 %>%
    as.data.frame() %>%
    rownames_to_column(var = "term")
M5


M6 <- summary(reg_f.gdp.interact.ARDL1_time_pdynmc)$coef %>%
    head(17)
colnames(M6) <- c("estimate", "std.error", "statistic", "p.value")
M6 <- M6 %>%
    as.data.frame() %>%
    rownames_to_column(var = "term")
M6

M7 <- summary(reg_f.gdp.interact.ARDL1_time2_pdynmc)$coef %>%
    head(17)
colnames(M7) <- c("estimate", "std.error", "statistic", "p.value")
M7 <- M7 %>%
    as.data.frame() %>%
    rownames_to_column(var = "term")
M7

M0 <- list(tidy = M0, glance = data.frame(`Time FE` = "Y",
                                          Trends = "YQ"))
M1 <- list(tidy = M1, glance = data.frame(`Time FE` = "Y",
                                          Trends = "–"))
M2 <- list(tidy = M2, glance = data.frame(`Time FE` = "Y",
                                          Trends = "–"))
M3 <- list(tidy = M3, glance = data.frame(`Time FE` = "Y",
                                          Trends = "Y"))
M4 <- list(tidy = M4, glance = data.frame(`Time FE` = "Y",
                                          Trends = "YQ"))
M5 <- list(tidy = M5, glance = data.frame(`Time FE` = "Y",
                                          Trends = "–"))
M6 <- list(tidy = M6, glance = data.frame(`Time FE` = "Y",
                                          Trends = "Y"))
M7 <- list(tidy = M7, glance = data.frame(`Time FE` = "Y",
                                          Trends = "YQ"))

# Model class
class(M0) <- "modelsummary_list"
class(M1) <- "modelsummary_list"
class(M2) <- "modelsummary_list"
class(M3) <- "modelsummary_list"
class(M4) <- "modelsummary_list"
class(M5) <- "modelsummary_list"
class(M6) <- "modelsummary_list"
class(M7) <- "modelsummary_list"

get_estimates(M1)
modelsummary(M0, stars = TRUE)

# created named list
models <- list()
models[["static"]] <- M0
models[["pgmm"]] <- M1
models[["pdynmc (1)"]] <- M2
models[["pdynmc (2)"]] <- M3
models[["pdynmc (3)"]] <- M4
models[["pdynmc (4)"]] <- M5
models[["pdynmc (5)"]] <- M6
models[["pdynmc (6)"]] <- M7

modelsummary(models, stars = TRUE, fmt = 4, 
             coef_map = c(
                 "L1.logD_gdp", "L0.tmp", "L0.tmp2", "L0.pre", "L0.pre2", 
                  "L0.tmp_pre", "L0.tmp2_pre", "L0.pre2_tmp", "L0.tmp2_pre2"), 
             estimate = "estimate", std.error = "std.error",
             statistic = "statistic", p.value = "p.value", )

modelsummary(models[-2], stars = TRUE, fmt = 6, 
             coef_map = c(
                 "L1.logD_gdp", "L0.tmp", "L1.tmp", "L0.tmp2", "L1.tmp2", 
                 "L0.pre", "L1.pre", "L0.pre2", "L1.pre2", 
                 "L0.tmp_pre", "L1.tmp_pre", "L0.tmp2_pre", "L1.tmp2_pre", 
                 "L0.pre2_tmp", "L1.pre2_tmp", "L0.tmp2_pre2", "L1.tmp2_pre2"), 
             estimate = "estimate", std.error = "std.error",
             statistic = "statistic", p.value = "p.value", )

## save to local
AFE_dynamic <- map(list(M0, M1, M2, M3, M4, M5, M6, M7), 1) %>% 
    bind_rows(.id = "model")
AFE_dynamic <- AFE_dynamic %>% mutate(pval.symbol = addPval.symbol(p.value))
date <- "250811" # file suffix to indicate version
f_name <- sprintf("data/AFE_dynmc_%s.csv", date)
# write_csv(AFE_dynamic, f_name)






