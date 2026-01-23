## Project CC impacts with stata xtabond2 estimates
## Input: coefficient estimates from stata xtabond2 dynamic panel model
## Method: Inverting the AR(1).
## Output: 
##      - country-level projected CC impacts on GDP growth rate per year (eta)
##      - country-level projected cumulative CC impacts per year
## "interactive_terms" (BOOL): whether to include interactive terms in the projection
## Depending on interactive_terms, the output files are saved accordingly.
## Date: 2025-12-16

library(tidyverse)
library(data.table)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("fun_script.R")

f_name <- "data/stata/xtabond2_coef_vector.txt"
beta_hat <- read_delim(f_name, delim = "\t")

beta_hat <- t(beta_hat)
beta_hat <- beta_hat %>% tail(-1)
beta_hat <- beta_hat %>% head(-1)
beta_hat %>% tail()
beta_hat %>% str()
beta_hat <- beta_hat %>%
    as_tibble(rownames = "variable")
colnames(beta_hat)[2] <- "estimate"
beta_hat <- beta_hat %>%
    mutate(estimate = as.numeric(estimate))
beta_hat

# if exists distributed lag (DL) terms
DL <- FALSE

# get MA coefficients for each regressor
# initialize containers
var_list <- c("T", "T2", "P", "P2", "T*P", "T2*P", "T*P2", "T2*P2") # x-axis names
psi_df <- list() # L x 8, each col is rho(L)^(-1) * beta_j(L), j = 1,...,8
mean_lag <- numeric(length(var_list))
plot <- TRUE # whether to plot MA coefficients
# plot <- FALSE

model_id <- "stata_AFE_AR1-time2_xtabond2"
date <- "251216"
j <- 1

for (j in 1:8) {
    # get MA coefficients and plot bar plot
    if (j == 1) {
        cat(sprintf("Plot: %s \n", plot))
        cat(sprintf("Distributed lag: %s \n", DL))
    }
    var <- var_list[j]
    rho <- beta_hat$estimate[1]
    if (DL) {
        # with distributed lag terms
        idx <- j * 2
        beta_j <- beta_hat$estimate[idx:(idx + 1)]
        psi_coef <- lag_poly_solution(rho1 = rho, beta0 = beta_j[1], beta1 = beta_j[2], n_terms = 20)
    } else {
        # without distributed lag terms
        idx <- j + 1
        beta_j <- beta_hat$estimate[idx]
        psi_coef <- lag_poly_solution(rho1 = rho, beta0 = beta_j, beta1 = 0, n_terms = 20)
    }
    psi_coef
    psi_df[[var]] <- psi_coef

    mean_lag[j] <- sum(0:20 * psi_coef) / sum(psi_coef)

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
            labs(
                x = "Lag",
                y = expression(psi[L]),
                title = title,
                subtitle = par
            )
        p
        f_name <- sprintf("figures/lag_polynomial/%3$s/%1$s_dynamic_lag_poly_%2$s_%3$s.png", model_id, var, date)
        f_name
        plot_png(p, f_name, 9.26, 6.27)
    }
}

psi_df <- do.call(cbind, psi_df) %>%
    as.data.frame() %>%
    as_tibble(rownames = "lag")
psi_df %>% print(n = Inf)
f_name <- sprintf("data/%s_dynmc_lag_poly_%s.csv", model_id, date)
f_name
# write_csv(psi_df, f_name)

mean_lag <- setNames(mean_lag, var_list)
mean_lag

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
    mutate_at(c("start", "end", "avg", "trend_annual"), ~ . * 12 / 1000)
# calculate country-specific pathways
temp_path_df <- tmp_df %>%
    crossing(year = 1:80) %>% # Repeat each country for 80 years
    mutate(tmp = start + year * trend_annual) # Compute temperature pathway
pre_path_df <- pre_df %>%
    crossing(year = 1:80) %>%
    mutate(pre = start + year * trend_annual)
regressor_df <- temp_path_df[, c("ISO_C3", "year", "tmp")] %>%
    left_join(pre_path_df[, c("ISO_C3", "year", "pre")],
        by = c("ISO_C3", "year")
    )

# contemporaneous regressors
regressor_df <- regressor_df %>%
    mutate(
        tmp2 = tmp^2,
        pre2 = pre^2,
        tmp_pre = tmp * pre,
        tmp2_pre = tmp2 * pre,
        pre2_tmp = pre2 * tmp,
        tmp2_pre2 = tmp2 * pre2
    )

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


## h-step ahead forecast -------------------------------------------------------
# Model: y_t = rho1*y_{t-1} + X_t*beta + epsilon_t
# h-step forecast: y_{t+h} = sum_{j=0}^h psi_j * X_{t+h-j}
# where psi(L) = (1 - rho1*L)^(-1) = sum_{k=0}^inf rho1^k * L^k

# Get AR(1) coefficient
rho <- beta_hat$estimate[1]

# Calculate MA coefficients up to h=80 lags
h_max <- 80
psi_all <- lag_poly_solution(rho1 = rho, beta0 = 1, beta1 = 0, n_terms = h_max)

# Extract beta coefficients for regressors (excluding AR coefficient)
beta_vec <- beta_hat$estimate[2:9] # 8 regressors
names(beta_vec) <- c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")

# interactive_terms <- TRUE
interactive_terms <- FALSE
if (!interactive_terms) {
    # zero out interaction term coefficients
    beta_vec[c("tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")] <- 0
}
beta_vec

# Initialize forecast matrix: countries x forecast horizons
countries <- unique(regressor_df$ISO_C3)
n_countries <- length(countries)
Delta_all_df <- tibble(
    ISO_C3 = rep(countries, each = h_max),
    horizon = rep(1:h_max, times = n_countries),
    y_forecast = NA_real_
)

# Compute h-step forecasts for each country
# ctry <- countries[1]  # for testing
for (ctry in countries) {
    # Extract regressor data for this country
    ctry_data <- regressor_df %>%
        filter(ISO_C3 == ctry) %>%
        arrange(year)

    # Get regressor matrix (years x 8 variables)
    X_matrix <- ctry_data %>%
        select(tmp, tmp2, pre, pre2, tmp_pre, tmp2_pre, pre2_tmp, tmp2_pre2) %>%
        as.matrix()

    # Calculate h-step forecasts
    for (h in 1:h_max) {
        # y_{t+h} = sum_{j=0}^h psi_j * (beta' * X_{t+h-j})
        forecast_h <- 0

        for (j in 0:h) {
            # Time index for X_{t+h-j}
            time_idx <- h - j + 1 # +1 because R is 1-indexed

            if (time_idx >= 1 && time_idx <= nrow(X_matrix)) {
                # X_{t+h-j}' * beta
                X_contribution <- sum(X_matrix[time_idx, ] * beta_vec)

                # Multiply by MA coefficient psi_j
                forecast_h <- forecast_h + psi_all[j + 1] * X_contribution
            }
        }

        # Store forecast
        Delta_all_df$y_forecast[Delta_all_df$ISO_C3 == ctry & Delta_all_df$horizon == h] <- forecast_h
    } # end of h-loop

    # Debug: print forecasts for this country
    # Delta_all_df %>%
    #     filter(ISO_C3 == ctry) %>%
    #     print(n = Inf)

    if (match(ctry, countries) %% 20 == 0) {
        cat(sprintf(
            "Completed forecasts for %d/%d countries\n",
            match(ctry, countries), n_countries
        ))
    }
}

# View results
Delta_all_df %>%
    group_by(horizon) %>%
    summarise(
        mean_forecast = mean(y_forecast, na.rm = TRUE),
        median_forecast = median(y_forecast, na.rm = TRUE),
        sd_forecast = sd(y_forecast, na.rm = TRUE)
    ) %>%
    print(n = 20)

# Reshape to wide format: each year as a separate column
Delta_all_df <- Delta_all_df %>%
    pivot_wider(
        names_from = "horizon",
        values_from = "y_forecast"
    )
# Save forecast results
colnames(Delta_all_df)[-1] <- seq(2021, 2100)
Delta_all_df %>% select(ISO_C3, `2021`:`2024`, `2096`:`2100`)

f_name <- ifelse(interactive_terms,
    sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_inter_%3$s.csv", ssp, model_id, date),
    sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_no_inter_%3$s.csv", ssp, model_id, date)
)
f_name
# write_csv(Delta_all_df, f_name)

## --------------------------------- ##

## Calculate GDP pathway with and without CC -> cumulative effects -------------
interactive_terms <- TRUE
interactive_terms <- FALSE
f_name <- ifelse(interactive_terms,
    sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_inter_%3$s.csv", ssp, model_id, date),
    sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_no_inter_%3$s.csv", ssp, model_id, date)
)
f_name
Delta_all_df <- read_csv(f_name)

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
baseline_country <- apply(gdp_nCC + 1, 1, cumprod) %>% t()
# growth with CC
gdp_CC <- Delta_all_df[, 2:81] + gdp_nCC
CC_country <- apply(
    gdp_CC + 1, 1,
    function(x) cumprod(replace(x, is.na(x), 1))
) %>%
    t()


# cumulative effects
pct_impact_country <- CC_country / baseline_country - 1
pct_impact_country <- pct_impact_country %>%
    as_tibble() %>%
    add_column(ISO_C3 = Delta_all_df$ISO_C3, .before = 1)

dim(pct_impact_country)
pct_impact_country[1:5, 1:10]
pct_impact_country[1:5, c(1, 72:81)]

f_name <- ifelse(interactive_terms,
    sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_%3$s.csv", ssp, model_id, date),
    sprintf("data/%1$s/%1$s_country_all_impact_nointer_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
    )
f_name
# write_csv(pct_impact_country, f_name)
