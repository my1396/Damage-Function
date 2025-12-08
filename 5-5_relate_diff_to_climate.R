## Relate diff to climate trend in order to understand the interactive effects 
## Note: no obvious relationship found 

library(ggsci)

# choose ssp scenario: one of "SSP126", "SSP245", "SSP370", "SSP585"
ssp <- "SSP126"
ssp <- "SSP245"
ssp <- "SSP370"
ssp <- "SSP585"

f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_with-without-IE_%2$s_dynmc_250729.csv", ssp, model_id)
diff_df <- read_csv(f_name)

tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))
# convert to annual total precipitation
pre_df <- pre_df %>%
    mutate_at(c("start", "end", "avg", "trend_annual"), ~ . * 12 / 1000)

# drying indicator, TRUE/FALSE
climate_df <- pre_df %>%
    mutate(drying = ifelse(trend_annual < 0, TRUE, FALSE))
climate_df <- climate_df %>% 
    left_join(
        tmp_df, 
        by = "ISO_C3", 
        suffix = c(".pre", ".tmp")) %>% 
    left_join(country_name[c("ISO_C3", "cntry.name", "region")], by = "ISO_C3") 
climate_df

diff_climate <- diff_df %>%
    left_join(climate_df, by = "ISO_C3") %>%
        mutate(diff_sign = ifelse(diff >= 0, "positive", "negative"))
diff_climate


# diff ~ trend_annual.pre
p_diff_pre <- diff_climate %>% 
    drop_na(region) %>%
    # filter(Pvalue.pre < 0.05) %>%  # filter significant trend
    ggplot(aes(trend_annual.pre, diff, color = region)) +
    geom_point() +
    geom_smooth(
        method = "lm", color = "#D62728FF",
        fill = "#D62728FF", alpha = 0.2
    ) +
    # facet_wrap(~diff_sign, ncol=1) + 
    scale_color_npg() +
    labs(title = sprintf("%s", ssp)) 
p_diff_pre
f_name <- sprintf("figures/diff_climate/%s_diff_trend_annual_pre.png", ssp)
f_name
plot_png(p_diff_pre, f_name, 7.53, 5.53)

# trend_annual.pre ~ start.pre
p_pre <- diff_climate %>%
    drop_na(region) %>%
    # filter(Pvalue.pre < 0.05) %>%  # filter significant trend
    ggplot(aes(x = start.pre, y = trend_annual.pre, color = region)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_color_npg() +
    labs(title = ssp)
p_pre
f_name <- sprintf("figures/diff_climate/%s_trend_annual_pre_start_pre.png", ssp)
f_name
plot_png(p_pre, f_name, 7.53, 5.53)


# diff ~ trend_annual.tmp
p_diff_tmp <- diff_climate %>%
    drop_na(region) %>%
    ggplot(aes(trend_annual.tmp, diff, color = region)) +
    geom_point() +
    geom_smooth(
        method = "lm", color = "#D62728FF",
        fill = "#D62728FF", alpha = 0.2
    ) +
    # facet_wrap(~diff_sign) + 
    scale_color_npg() +
    labs(title = sprintf("%s", ssp))
p_diff_tmp
f_name <- sprintf("figures/diff_climate/%s_diff_trend_annual_tmp.png", ssp)
f_name
plot_png(p_diff_tmp, f_name, 7.53, 5.53)

# trend_annual.tmp ~ start.tmp
p_tmp <- diff_climate %>%
    drop_na(region) %>%
    ggplot(aes(x = start.tmp, y = trend_annual.tmp, color = region)) +
    geom_point() +
    scale_color_npg() +
    labs(title = ssp)
p_tmp



# trend_annual.pre ~ trend_annual.tmp
p_climate_trend <- diff_climate %>%
    drop_na(region) %>%
    ggplot(aes(x = trend_annual.tmp, y = trend_annual.pre, color = region)) +
    geom_point() +
    scale_color_npg() +
    labs(title = ssp)
p_climate_trend
