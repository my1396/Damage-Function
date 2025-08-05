## Further analysis of differences: with and without interactive terms
#   Key Analyses:
#       1. Overall distribution of interactive effects by SSP scenario (violin plots)
#       2. Summary statistics showing predominantly negative effects (interactive
#          terms reduce climate damages)
#       3. Investigation of extreme positive outliers (especially in SSP370)
#       4. Regional comparison of interactive effects across world regions
#       5. Statistical testing (ANOVA) for significant regional and scenario differences

country_name <- read_csv("data/region_category.csv")

model_id <- "AFE"
f_name <- sprintf("data/%s_dynmc_cumu_effect_2100_diff_with-without-IE.csv", model_id)
f_name
impact_2100_wide <- read_csv(f_name)
impact_2100_wide <- impact_2100_wide %>%
    left_join(
        country_name[c("ISO_C3", "cntry.name", "region")], 
        by = "ISO_C3")
impact_2100_wide

# Reshape data for visualization
impact_2100_long <- impact_2100_wide %>%
    pivot_longer(cols = starts_with("SSP"), 
                 names_to = "SSP", 
                 values_to = "diff") %>%
    filter(!is.na(diff))
impact_2100_long

# 1. Violin plots to show density distribution ---------------------------------
p_ssp <- ggplot(impact_2100_long, aes(x = SSP, y = diff, fill = SSP)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.1, alpha = 0.8) +
    labs(
        title = "Density Distribution of Interactive Effects",
        subtitle = TeX("$\\delta^{IE} - \\delta^{no\\, IE}$"),
        y = "Impact Difference"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
    )
p_ssp

f_name <- sprintf("figures/diff_with-without-IE/interactive_effects_violin_%s_dynmc.png", model_id)
f_name
plot_png(p_ssp, f_name, 7.59, 6.09)

# 2. Summary statistics table --------------------------------------------------
ssp_stats <- impact_2100_long %>%
    group_by(SSP) %>%
    summarise(
        n = n(),
        mean = round(mean(diff, na.rm = TRUE), 4),
        median = round(median(diff, na.rm = TRUE), 4),
        sd = round(sd(diff, na.rm = TRUE), 4),
        min = round(min(diff, na.rm = TRUE), 4),
        max = round(max(diff, na.rm = TRUE), 4),
        negative_pct = round(100 * sum(diff < 0, na.rm = TRUE) / n(), 1)
    )
print(ssp_stats)

# 3. Investigate extreme observations in SSP370 -------------------------------
top_5_ssp370 <- impact_2100_wide %>%
    arrange(desc(SSP370)) %>%
    slice_head(n = 5)
top_5_ssp370


# 4. Summary by region (across all SSPs) ---------------------------------------
#  use first n as number of countries
regional_stats <- impact_2100_long %>%
    group_by(region, SSP) %>%
    summarise(
        n = n(),
        mean = round(mean(diff, na.rm = TRUE), 4),
        median = round(median(diff, na.rm = TRUE), 4),
        sd = round(sd(diff, na.rm = TRUE), 4),
        negative_pct = round(100 * sum(diff < 0, na.rm = TRUE) / n(), 1),
        .groups = "drop"
    ) %>% 
    drop_na()

regional_stats %>% print(n = Inf)

# 5. Regional boxplot comparison -----------------------------------------------
region_labels <- regional_stats %>%
    distinct(region, .keep_all = TRUE) %>%
    mutate(label = paste0(substring(region, 2), "\n(n=", n, ")")) %>%
    select(region, label)

p_regional_box <- impact_2100_long %>%
    left_join(region_labels, by = "region") %>% 
    drop_na(label) %>%   # drop rows without region labels
    ggplot(aes(x = label, y = diff, fill = region)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~SSP, scales = "free_y") +
    labs(
        title = "Regional Differences in Interactive Effects by SSP Scenario",
        subtitle = TeX("$\\delta^{IE} - \\delta^{no\\, IE}$"),
        x = "Region",
        y = "Impact Difference"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
    )

p_regional_box
f_name <- sprintf("figures/diff_with-without-IE/regional_boxplot_%s_dynmc.png", model_id)
f_name
plot_png(p_regional_box, f_name, 9.59, 8.39)

# Note: regional violin plots approximate to flat lines; too few data points
#       hence boxplots are more informative

# 6. Test for statistical differences between regions --------------------------
library(broom)
anova_result <- aov(diff ~ region * SSP, data = drop_na(impact_2100_long, region))
print("ANOVA results for regional and SSP differences:")
print(tidy(anova_result))
