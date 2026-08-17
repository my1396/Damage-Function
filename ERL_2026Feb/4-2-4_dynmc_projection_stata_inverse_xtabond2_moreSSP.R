## This script generates dynamic projections of GDP growth under climate change under alternative SSPs.
## These results are used in the Online Supplement of the paper submitted to EctJ.
##
## Three figures are generated:
## 1) Global map of cumulative GDP impacts in 2100
## 2) Histogram of country-level cumulative GDP impacts in 2100
## 3) Global average GDP pathway with and without CC, showing all effects vs direct effects only
##
## Update: 2025-02-26

library(tidyverse)
library(glue)
library(data.table)
library(scales)
library(rnaturalearth)

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


# ssp <- "SSP126"
# ssp <- "SSP245"
ssp <- "SSP370"
# ssp <- "SSP585"
date <- "260226"

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
write_csv(Delta_all_df, f_name)

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
write_csv(pct_impact_country, f_name)

## --------------------------------- ##

## Plot cumulative effects map -------------------------------------------------

legend_low <- -1
legend_high <- 1
c <- 10 * .Machine$double.eps
"%ni%" <- Negate("%in%")
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
f_name
pct_impact_country <- read_csv(f_name)
plot_data <- pct_impact_country %>%
    select(1, 81) %>%
    mutate(V80_c = squish(`2100.deltaAll`, range = c(legend_low + c, legend_high))) # set out of boundary values to limits
plot_data

# world.map
world.map <- ne_countries(scale = "medium", returnclass = "sf")
sum(plot_data$ISO_C3 %ni% world.map$iso_a3_eh)
world.map <- world.map %>%
    left_join(plot_data, by = c("iso_a3_eh" = "ISO_C3"))

cold <- colorRampPalette(c("#000033", "#00007F", "#7AACED", "white"))(7) # from -7
warm <- c("#FFD4D4", "#FFB2B2", "#FF9090", "#FF6D6D", "#FF4B4B", "#FF2A2A")
myColors <- c(cold, warm)

title <- sprintf("Climate change impacts until 2100, %s", ssp)
unit <- "%"
step <- 0.2

p_map <- ggplot(data = world.map %>%
    filter((continent != "Antarctica") & (name_en != "Greenland"))) +
    geom_sf(aes(fill = V80_c), colour = "gray50", lwd = 0.2) +
    scale_fill_stepsn(
        limits = c(legend_low - c, legend_high + c),
        breaks = c(legend_low - c, seq(legend_low + step, legend_high - step, step), legend_high + c),
        labels = function(x) {
            sprintf("%.1f", x * 100)
        },
        show.limits = TRUE,
        right = FALSE, # include right bin, (low, up]
        colours = myColors,
        name = TeX(unit)
    ) +
    coord_sf(datum = NA) +
    guides(pattern = guide_legend(title = element_blank())) +
    labs(title = title) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.1),
        legend.title = element_text(hjust = 0.85),
        legend.position = c(0.1, 0.3)
    )
p_map
# Note: South Sudan (SSD) missing base GDP growth, hence missing impact values in the map
f_name <- paste0("/Users/menghan/Documents/GDP/Shared folder/figures/", ssp, sprintf("/%1$s_climate_impacts_map_%2$s_cumulative_%3$s.png", ssp, "all", model_id))
f_name
plot_png(p_map, f_name, width = 9.89, height = 3.93)

## ========================================================================== ##

## Historgram of country-level impacts -----------------------------------------
p_hist <- ggplot(pct_impact_country, aes(x = `2100.deltaAll`)) +
    geom_histogram(
        aes(y = after_stat(density)),
        fill = "#BDBCBC",
        color = "black",
        binwidth = 0.1,
        boundary = 0
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    labs(
        x = "Cumulative GDP Impact in 2100",
        y = "Density",
        title = ssp,
    ) +
    my_theme
# x11()
p_hist

f_name <- sprintf("figures/%1$s/%1$s_cumu_impacts_2100_histogram_%2$s.png", ssp, model_id)
f_name
plot_png(p_hist, f_name, 7.83, 5.19)


## Global average GDP pathways ---------------------------------------------------------------
## Two series: 1) with all effects, 2) with direct effects only (no IE-effects)

## Load population weights
# population distribution are more even, not densely concentrated in China or India any more
# SSP585: 5.48 Celsius global warming by 2100, weighted by pop in 2100
f_name <- "data/SSP_Population_weight.csv"
pop_weight_df <- read_csv(f_name)
pop_weight <- pop_weight_df %>% filter(Scenario == substr(ssp, 1, 4))
pop_weight
pop_weight <- pop_weight[, c(-1, -2, -4, -5)]
pop_weight %>% dim()
pop_weight %>% select(1:6, 77:81)

## GDP growth in absence of CC, baseline growth
# SSP projected GDP growth
f_name <- sprintf("data/baseline_growth/%s_GrowthProjections.csv", substr(ssp, 1, 4))
f_name
gdp_SSP <- read_csv(f_name)
gdp_SSP

# avg of growth rates across countries, weighted by population ----------------
## All effects ================================================================
f_name <- sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_inter_%3$s.csv", ssp, model_id, date)
f_name
Delta_all_df <- read_csv(f_name)
Delta_all_df <- Delta_all_df %>%
    left_join(gdp_SSP,
        by = c("ISO_C3" = "Region"),
        suffix = c(".deltaAll", ".baseline")
    ) %>%
    drop_na()
Delta_all_df %>% dim()

# growth without CC
gdp_nCC <- Delta_all_df[, 82:161]
# growth with CC
gdp_CC <- Delta_all_df[, 2:81] + gdp_nCC
gdp_CC %>% dim()

## Aggregate to global, weighted by SSP projected population
Delta_gdp_cc <- gdp_CC %>%
    as_tibble() %>%
    mutate(ISO_C3 = Delta_all_df$ISO_C3) %>%
    left_join(pop_weight, by = c("ISO_C3" = "Region")) %>%
    drop_na()
Delta_gdp_cc %>% dim()
colnames(Delta_gdp_cc)
global_impact <- t(Delta_gdp_cc[, 1:80]) %*% as.matrix(Delta_gdp_cc[, 82:161])
global_impact %>% dim()
diag(global_impact)
global_impact_cum <- cumprod(diag(global_impact) + 1)
global_impact_cum

## GDP growth in absence of CC, baseline ---------------------------------------
## glbal_gdp_cum can be reused as it does not concern eta
Delta_gdp_ncc <- gdp_nCC %>%
    as_tibble() %>%
    mutate(ISO_C3 = Delta_all_df$ISO_C3) %>%
    left_join(pop_weight, by = c("ISO_C3" = "Region")) %>%
    drop_na()
Delta_gdp_ncc %>% dim()

Delta_gdp_ncc[, 75:80]
Delta_gdp_ncc[, 81:82]
global_gdp <- t(Delta_gdp_ncc[, 1:80]) %*% as.matrix(Delta_gdp_ncc[, 82:161])
global_gdp %>% dim()
diag(global_gdp)
global_gdp_cum <- cumprod(diag(global_gdp) + 1)
global_gdp_cum

pct_impact_interactive <- global_impact_cum / global_gdp_cum - 1
pct_impact_interactive

sprintf("Global GDP per capita impact in 2100 under %s (all effects): %.2f%%, weighted by population", ssp, pct_impact_interactive[80] * 100)

# =========================================================================== #

## Diect effects only ----------------------------------------------------------

f_name <- sprintf("data/%1$s/%1$s_country_eta_%2$s_dynmc_no_inter_%3$s.csv", ssp, model_id, date)
Delta_all_df <- read_csv(f_name)
Delta_all_df <- Delta_all_df %>%
    left_join(gdp_SSP,
        by = c("ISO_C3" = "Region"),
        suffix = c(".deltaAll", ".baseline")
    ) %>%
    drop_na()
Delta_all_df %>% dim()
colnames(Delta_all_df) %>% tail(5)

gdp_CC <- Delta_all_df[, 2:81] + gdp_nCC
gdp_CC %>% dim()

# Aggregate to global, weighted by population
Delta_gdp_cc <- gdp_CC %>%
    as_tibble() %>%
    mutate(ISO_C3 = Delta_all_df$ISO_C3) %>%
    left_join(pop_weight, by = c("ISO_C3" = "Region")) %>%
    drop_na()
Delta_gdp_cc

global_impact_dir <- t(Delta_gdp_cc[, 1:80]) %*% as.matrix(Delta_gdp_cc[, 82:161]) # dynamic weight
global_impact_dir %>% dim()
diag(global_impact_dir)
global_impact_dir_cum <- cumprod(diag(global_impact_dir) + 1)
global_impact_dir_cum

pct_impact_dir <- global_impact_dir_cum / global_gdp_cum - 1
pct_impact_dir

glue(
    "Global GDP per capita impact in 2100 under {ssp}:
     - All effects (population weighted): {round(pct_impact_interactive[80] * 100, 2)}%
     - Direct effects only (population weighted): {round(pct_impact_dir[80] * 100, 2)}%
     - Indirect effects (damage mitigation): {round((pct_impact_interactive[80] - pct_impact_dir[80]) * 100, 2)}%",
    .trim = FALSE
)


# Two-in-one plot: all effects vs direct effects -------------------------------
time_vec <- ymd("2020-12-31") %m+% years(1:80)
time_vec
plot_data <- tibble(
    time = rep(time_vec, 2),
    impact = c(pct_impact_interactive * 100, pct_impact_dir * 100),
    type = rep(c("All effects", "Direct effects only"), each = length(time_vec))
)
breaks <- c("All effects", "Direct effects only")
colors <- setNames(c("red", "blue"), breaks)
p_pathway <- plot_data %>%
    ggplot(aes(x = time, y = impact, color = type)) +
    geom_line() +
    scale_color_manual(values = colors) +
    geom_point() +
    labs(
        y = "Percentage change in GDPpc",
        title = ssp
        ) +
    scale_x_date(limits = c(ymd("2021-12-31"), ymd("2099-12-31"))) +
    my_theme +
    theme(
        axis.title.x = element_blank(),
        legend.position = c(0.8, 0.8)
    )
p_pathway

f_name <- sprintf("figures/%1$s/%1$s_persistent_impacts_pathway_oneModel_%2$s.png", ssp, model_id)
f_name
plot_png(p_pathway, f_name, 8.05, 5.5)
