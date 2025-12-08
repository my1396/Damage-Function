## bootstrap for dynamic model

library(tidyverse)
library(plm)
library(lmtest)
library(broom)
library(pdynmc)


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
obs <- sum(!is.na(Pdata[[target_v]])) ## non-NA obs.
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

Pdata_nomissing <- Pdata %>%
    na.omit()
reg_f.gdp.interact.ARDL1_time2_pdynmc <- pdynmc(
    dat = Pdata_nomissing,
    varname.i = "iso", varname.t = "year", use.mc.diff = TRUE,
    use.mc.lev = FALSE, use.mc.nonlin = FALSE, include.y = TRUE,
    varname.y = "logD_gdp", lagTerms.y = 1, fur.con = TRUE, fur.con.diff = TRUE,
    fur.con.lev = FALSE,
    varname.reg.fur = c(
        "tmp", "tmp2", "pre", "pre2",
        "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
        colnames(ttrend), colnames(ttrend2)
    ),
    lagTerms.reg.fur = c(rep(1, 8), rep(0, 280)),
    include.dum = TRUE, dum.diff = TRUE,
    dum.lev = FALSE, varname.dum = "year", w.mat = "iid.err",
    std.err = "corrected", estimation = "onestep", 
    opt.meth = "none"
)
summary(reg_f.gdp.interact.ARDL1_time2_pdynmc)
summary(reg_f.gdp.interact.ARDL1_time2_pdynmc)$coef %>%
    head(17)