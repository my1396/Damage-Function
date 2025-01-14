# Diff and precip trends, check whether there is a positive correlation


# G124 [√], relate diff to pre trend, significant positive correlation
plot_data <- end_climate[,c("ISO_C3", "diff", "group")] %>% 
    filter(group==124) %>% 
    left_join(pre_df[,c("ISO_C3", "trend_annual", "Pvalue")], by="ISO_C3") %>% 
    mutate(trend_annual=trend_annual*10)
plot_data
lm.diff <- lm(diff~trend_annual, data=plot_data)
summary(lm.diff)
country_vec <- c("USA", "AUS", "MEX", "BRA")
p <- ggplot(plot_data, 
       aes(trend_annual, diff)) +
    geom_point(data=plot_data %>% filter(!ISO_C3 %in% country_vec), color="#1F77B4FF", shape=1, size=2) +
    geom_point(data=plot_data %>% filter(ISO_C3 %in% country_vec), color="#1F77B4FF", shape=16, size=2) +
    geom_text(data=plot_data %>% filter(ISO_C3 %in% country_vec),
              aes(label=ISO_C3),
              color="#1F77B4FF",
              hjust=-0.3, vjust=-0.1, 
              size=3.5, fontface="bold",
              show.legend = FALSE
              ) +
    geom_smooth(method="lm", color="#D62728FF", 
                fill="#D62728FF", alpha = 0.2) +
    labs(x = TeX("Projected precipitation trend [Meters dec$^{-1}$]"), 
         y = "Interactive effects on GDP",
         title = sprintf("%s, group 124", ssp) ) +     
    scale_x_continuous(limits=c(-0.05, 0.05)) +
    scale_y_continuous(limits=c(-0.02, 0.25)) +
    my_theme
p
f_name <- sprintf("figures/%s/diff-pre.png", ssp)
# plot_png(p, f_name, 8.13, 5.74)

# all countries
plot_data <- end_climate[,c("ISO_C3", "diff", "group")] %>% 
    filter(!ISO_C3=="MNG") %>% 
    left_join(pre_df[,c("ISO_C3", "trend_annual")], by="ISO_C3")
plot_data
lm.diff <- lm(diff~trend_annual, data=plot_data)
summary(lm.diff)

ggplot(plot_data, aes(trend_annual, diff, color=group)) +
    geom_point() +
    scale_color_manual(values=c("6"="#008B45FF", "11"="#D62728FF", "15"="#FF7F0EFF", "124"="#1F77B4FF"), 
                       labels=c("6"="6", "11"="10", "15"="15", "124"="124"))

# G11, related diff to tmp trend, no obvious pattern
plot_data <- end_climate[,c("ISO_C3", "diff", "group")] %>% 
    filter((!ISO_C3=="MNG") & group==11) %>% 
    left_join(tmp_df[,c("ISO_C3", "trend_annual")], by="ISO_C3")
plot_data
lm.diff <- lm(diff~trend_annual, data=plot_data)
summary(lm.diff)
ggplot(plot_data, aes(trend_annual, diff)) +
    geom_point()
