## Forecast economic growth under dynamic models, estimated by R
##    1. Convert ARDL coefficients to MA representation
##    2. Project future growth using climate projections
##          2.1 Impact of climate change on GDP growth (eta), two scenarios:
##              a. with interactive terms;
##              b. without interactive terms;
##          2.2 Project GDP pathway from 2020 to 2100, two scenarios:
##              a. with climate change -> GDP_cc;
##              b. without climate change (baseline growth) -> GDP_nCC;
##          2.3 Cumulative impacts until 2100, GDP_cc/GDP_nCC-1 (delta)

library(tidyverse)
library(data.table)
source("fun_script.R")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

date <- "250811" # file suffix to indicate version
f_name <- sprintf("data/AFE_dynmc_%s.csv", date)
AFE_dynamic <- read_csv(f_name)
AFE_dynamic$model %>% unique()

# choose model
model_id <- "AFE_AR1"
model_id <- "AFE_AR1-time1"
model_id <- "AFE_AR1-time2"

model_id <- "AFE_ARDL"
model_id <- "AFE_ARDL-time1"
model_id <- "AFE_ARDL-time2"


switch(model_id,
    # no DL
    "AFE_AR1" = { # no time trend
        beta_hat <- AFE_dynamic %>% filter(model == 3)
        DL <- FALSE # FALSE: ignore lagged independent variables
    },
    "AFE_AR1-time1" = { # linear time trend
        beta_hat <- AFE_dynamic %>% filter(model == 4)
        DL <- FALSE # FALSE: ignore lagged independent variables
    },
    "AFE_AR1-time2" = { # quadratic time trend
        beta_hat <- AFE_dynamic %>% filter(model == 5)
        DL <- FALSE # FALSE: ignore lagged independent variables
    },
    # with DL
    "AFE_ARDL" = { # no time trend
        beta_hat <- AFE_dynamic %>% filter(model == 6)
        DL <- TRUE # TRUE: include lagged independent variables
    },
    "AFE_ARDL-time1" = { # linear time trend
        beta_hat <- AFE_dynamic %>% filter(model == 7)
        DL <- TRUE # TRUE: include lagged independent variables
    },
    "AFE_ARDL-time2" = { # quadratic time trend
        beta_hat <- AFE_dynamic %>% filter(model == 8)
        DL <- TRUE # TRUE: include lagged independent variables
    }
)

beta_hat

# ============================================================================ #
# ARDL > MA representation -----------------------------------------------------



# get MA coefficients for each regressor
# initialize containers
var_list <- c("T", "T2", "P", "P2", "T*P", "T2*P", "T*P2", "T2*P2") # x-axis names
psi_df <- list() # L x 8, each col is rho(L)^(-1) * beta_j(L), j = 1,...,8
mean_lag <- numeric(length(var_list))
# plot <- TRUE # whether to plot MA coefficients
plot <- FALSE


j <- 1

for (j in 1:8){
    # get MA coefficients and plot bar plot
    if (j == 1) {
        cat(sprintf("Plot: %s \n", plot))
        cat(sprintf("Distributed lag: %s \n", DL))
    }
    var <- var_list[j]
    rho <- beta_hat$estimate[1]
    if (DL) {
        idx <- j * 2
        beta_j <- beta_hat$estimate[idx:(idx + 1)]
        psi_coef <- lag_poly_solution(rho1 = rho, beta0 = beta_j[1], beta1 = beta_j[2], n_terms = 20)
    } else {
        idx  <- j + 1
        beta_j <- beta_hat$estimate[idx]
        psi_coef <- lag_poly_solution(rho1 = rho, beta0 = beta_j, beta1 = 0, n_terms = 20)
    }
    psi_coef
    psi_df[[var]] <- psi_coef
    
    mean_lag[j] <- sum(0:20 * psi_coef)/sum(psi_coef)
    
    # plot lag polynomial coefficients, psi0, psi1, ..., psi20
    if (plot) {
        title <- sprintf("Lag Polynomial Coefficients. Regressor: %s", var)
        par <- sprintf("beta0:%.2e, beta1:%.2e, rho:%.2f", beta_j[1], beta_j[2], rho)
        print(title)
        print(par)

        plot_df <- tibble(
            lag = 1:21,
            coef = psi_coef
        )
        p <- ggplot(plot_df, aes(x = lag, y = coef)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            labs(x = "Lag", 
                 y = expression(psi[L]), 
                 title = title,
                 subtitle = par)
        p
        f_name <- sprintf("figures/lag_polynomial/%1$s_dynamic_lag_poly_%2$s_%3$s.png", model_id, var, date)
        f_name
        plot_png(p, f_name, 9.26, 6.27)
    }
}

psi_df <- do.call(cbind, psi_df) %>% 
    as.data.frame() %>% 
    as_tibble(rownames = "lag")
psi_df %>% print(n=Inf)
psi_df %>% str()
f_name <- sprintf("data/%s_dynmc_lag_poly_%s.csv", model_id, date)
f_name
# write_csv(psi_df, f_name)

# Plot mean lag for each variable ----------------------------------------------
mean_lag
plot_data <- tibble(
    variable = factor(var_list, levels = var_list),
    mean_lag = mean_lag
)
p <- ggplot(plot_data, aes(x = variable, y = mean_lag)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
        x = "Variable",
        y = "Mean Lag (years)",
        title = "Mean Lag of Lag Polynomial Coefficients",
        subtitle = "AFE Dynamic Model"
    )
p
f_name <- sprintf("figures/lag_polynomial/AFE_dynamic_lag_poly_mean_lag_%s.png", date)
# plot_png(p, f_name, 9.29, 5.25)


# ============================================================================ #
# Projection -------------------------------------------------------------------

# choose ssp scenario: one of "SSP126", "SSP245", "SSP370", "SSP585"
# ssp <- "SSP126"
# ssp <- "SSP245"
# ssp <- "SSP370"
ssp <- "SSP585"


## Prepare regressor matrix ----------------------------------------------------
## load climate projections
tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))
# convert to annual total precipitation
pre_df <- pre_df %>% 
    mutate_at(c("start", "end", "avg", "trend_annual"), ~.*12/1000)
# calculate country-specific pathways
temp_path_df <- tmp_df %>%
    crossing(year = 1:80) %>%                 # Repeat each country for 80 years
    mutate(tmp = start + year * trend_annual) # Compute temperature pathway
pre_path_df <- pre_df %>%
    crossing(year = 1:80) %>%
    mutate(pre = start + year * trend_annual)
regressor_df <- temp_path_df[, c("ISO_C3", "year", "tmp")] %>% 
    left_join(pre_path_df[, c("ISO_C3", "year", "pre")], 
              by = c("ISO_C3", "year"))

# contemporaneous regressors
regressor_df <- regressor_df %>% 
    mutate(tmp2 = tmp^2,
           pre2 = pre^2,
           tmp_pre = tmp * pre, 
           tmp2_pre = tmp2 * pre, 
           pre2_tmp = pre2 * tmp,
           tmp2_pre2 = tmp2 * pre2)

# calculate the increment since BOP
regressor_df <- regressor_df %>%
    group_by(ISO_C3) %>%
    mutate(
        across(
            .cols = -year,
            .fns = ~ .x - first(.x)
        )
    ) %>%
    ungroup()

# lagged regressors, up to lag 4
regressor_df_lags <- regressor_df %>%
    group_by(ISO_C3) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(across(
        .cols = -c(year), 
        .fns = list(
            L1 = ~lag(.x, 1),
            L2 = ~lag(.x, 2),
            L3 = ~lag(.x, 3),
            L4 = ~lag(.x, 4)
        ),
        .names = "{fn}.{col}"
    )) %>%
    ungroup()
regressor_df_lags

# reorder columns by variable groups
ordered_cols <- c(
  "ISO_C3", "year", 
  "tmp", "L1.tmp", "L2.tmp", "L3.tmp", "L4.tmp",
  "pre", "L1.pre", "L2.pre", "L3.pre", "L4.pre",
  "tmp2", "L1.tmp2", "L2.tmp2", "L3.tmp2", "L4.tmp2",
  "pre2", "L1.pre2", "L2.pre2", "L3.pre2", "L4.pre2",
  "tmp_pre", "L1.tmp_pre", "L2.tmp_pre", "L3.tmp_pre", "L4.tmp_pre",
  "tmp2_pre", "L1.tmp2_pre", "L2.tmp2_pre", "L3.tmp2_pre", "L4.tmp2_pre",
  "pre2_tmp", "L1.pre2_tmp", "L2.pre2_tmp", "L3.pre2_tmp", "L4.pre2_tmp",
  "tmp2_pre2", "L1.tmp2_pre2", "L2.tmp2_pre2", "L3.tmp2_pre2", "L4.tmp2_pre2"
  )
regressor_df_lags <- regressor_df_lags[ordered_cols]
regressor_df_lags


## Prepare regression coefficients -----------------------------------------------
# Retain only the first 5 lags 
psi_df %>%
    select(-lag) %>%
    head(5)

# whether to include interactive terms in the model
interactive_terms <- TRUE
interactive_terms <- FALSE
if (interactive_terms) {
    # with interactive terms
    psi_vec <- psi_df %>%
        select(-lag) %>%
        head(5) %>% # first 5 lags
        as.matrix() %>%
        as.vector()
} else {
    # without interactive terms
    psi_vec <- psi_df %>%
        select(-lag) %>%
        head(5) %>%
        mutate(across(5:8, ~0)) %>% # make interactive terms zero
        as.matrix() %>%
        as.vector()
}
psi_vec # preview the MA coefficients

## Predict expected GDP growth -------------------------------------------------
X <- as.matrix(regressor_df_lags[, 3:42])
y_hat <- X %*% psi_vec
y_hat <- regressor_df_lags[, 1:2] %>% 
    cbind(y_hat)
y_hat %>% dim()
y_hat %>% data.table()

Delta_all_df <- y_hat %>% 
    pivot_wider(
        names_from = "year",
        values_from = "y_hat"
    )
colnames(Delta_all_df)[-1] <- seq(2021, 2100)
Delta_all_df
f_name <- ifelse(interactive_terms, 
       sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_inter_%3$s.csv", ssp, model_id, date),
       sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_no_inter_%3$s.csv", ssp, model_id, date))
f_name
write_csv(Delta_all_df, f_name)


## Calculate GDP pathway with and without CC -> cumulative effects -------------
## baseline growth, growth without CC
f_name <- sprintf("data/baseline_growth/%s_GrowthProjections.csv", substr(ssp, 1, 4))
f_name
gdp_SSP <- read_csv(f_name)
gdp_SSP
colnames(gdp_SSP)[-1] <- seq(2021, 2100)
gdp_SSP[, 70:81]
Delta_all_df <- Delta_all_df %>% 
    left_join(
        gdp_SSP, 
        by = c("ISO_C3" = "Region"), 
        suffix = c(".deltaAll", ".baseline")
    )
Delta_all_df %>% dim()

# growth without CC
gdp_nCC <- Delta_all_df[, 82:161]
baseline_country <- apply(gdp_nCC+1, 1, cumprod) %>% t() 
# growth with CC
gdp_CC <- Delta_all_df[,2:81] + gdp_nCC
CC_country <- apply(
    gdp_CC+1, 1, 
    function(x) cumprod(replace(x, is.na(x), 1))
    ) %>% 
    t()
# cumulative effects
pct_impact_country <- CC_country/baseline_country - 1
pct_impact_country <- pct_impact_country %>% 
    as_tibble() %>%
    add_column(ISO_C3 = Delta_all_df$ISO_C3, .before = 1)

dim(pct_impact_country)
pct_impact_country[1:5, 1:10]
pct_impact_country[1:5, c(1, 72:81)]

f_name <- ifelse(interactive_terms, 
       sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_%3$s.csv", ssp, model_id, date),
       sprintf("data/%1$s/%1$s_country_all_impact_nointer_%2$s_dynmc_%3$s.csv", ssp, model_id, date))
f_name
write_csv(pct_impact_country, f_name)





