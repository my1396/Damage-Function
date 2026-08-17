# Excerpt from 5-3_cumu_impacts_2100_histogram.R
#
# Country-level cumulative impacts in 2100 for a single SSP scenario
#   1. summary statistics
#   2. histogram plot
# Update: 2026-01-14

library(tidyverse)

# set working dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("fun_script.R")

ssp <- "SSP585"
model_id <- "stata_AFE_AR1-time2_xtabond2"
date <- "251216"

f_name <- sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_%3$s.csv", ssp, model_id, date)

pct_impact_country <- read_csv(f_name)
pct_impact_country[, c("ISO_C3", "2100.deltaAll")] %>% drop_na()

# summary statistics removed extreme values ------------------------------------
impact_2100 <- pct_impact_country[, "2100.deltaAll"] %>%
    filter(between(`2100.deltaAll`, -1, 1)) %>%
    pull()

impact_2100 %>% summary()

sprintf("Proportion of countries with cumulative GDP impact less than -70%% in 2100: %.2f%%", sum(impact_2100 < -.7) / length(impact_2100) * 100)

sprintf("Proportion of countries with positive cumulative GDP impact in 2100: %.2f%%", sum(impact_2100 > 0) / length(impact_2100) * 100)

moments::skewness(impact_2100)
moments::kurtosis(impact_2100)

# histogram --------------------------------------------------------------------
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

f_name <- sprintf("figures/%1$s/%1$s_cumu_impacts_2100_histogram_%2$s_dynmc_%3$s.png", ssp, model_id, date)
f_name
# plot_png(p_hist, f_name, 7.83, 5.19)