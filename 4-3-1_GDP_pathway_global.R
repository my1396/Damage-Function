# Excerpt from 4-3_GDP_pathway_global.R
#
# Global average GDP pathways, weighted by population
#   1. all effects
#   2. direct effects only
# Plot the global GDP pathway impacts: two series in one plot (refer to "6-5_Fig_projection-2100.R" and "6-5_Fig3.R")
#
# Update: 2026-01-23

library(tidyverse)
library(lubridate)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("fun_script.R")
## Parameter initialization
# ssp <- "SSP126"
# ssp <- "SSP245"
# ssp <- "SSP370"
ssp <- "SSP585"
model_id <- "stata_AFE_AR1-time2_xtabond2"
date <- "251216"

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

# Plot the global GDP pathway impact; all effects including direct and IE-effects
time_vec <- ymd("2020-12-31") %m+% years(1:80)
time_vec
plot_data <- tibble(time = time_vec, impact = pct_impact_interactive * 100)
ggplot(plot_data, aes(x = time, y = impact)) +
    geom_line() +
    labs(y = "Percentage change in GDPpc", title = "All effects")

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

sprintf("Global GDP per capita impact in 2100 under %s (direct effects only): %.2f%%, weighted by population", ssp, pct_impact_dir[80] * 100)

sprintf("IE-effects in 2100 under %s (mitigate damage by): %.2f%%", ssp, (pct_impact_interactive[80] - pct_impact_dir[80]) * 100)

# Plot direct effects only, no IE-effects
plot_data <- tibble(time = time_vec, impact = pct_impact_dir * 100)
ggplot(plot_data, aes(x = time, y = impact)) +
    geom_line() +
    labs(y = "Percentage change in GDPpc", title = "Direct effects only, no IE-effects")

# Two-in-one plot: all effects vs direct effects -------------------------------
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
    labs(y = "Percentage change in GDPpc", color = "Disentangled effects") +
    scale_x_date(limits = c(ymd("2021-12-31"), ymd("2099-12-31"))) +
    my_theme +
    theme(axis.title.x = element_blank(),
          legend.position = c(0.8, 0.8)
          )

f_name <- sprintf("figures/%1$s/%1$s_persistent_impacts_pathway_oneModel_%2$s_%3$s.png", ssp, model_id, date)
f_name
# plot_png(p_pathway, f_name, 8.05, 5.5)

