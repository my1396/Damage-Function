# Plot eta pathways per country

no_inter_eta <- read_csv(sprintf("data/%s/country_eta_nointer.csv", ssp))
inter_eta <- read_csv(sprintf("data/%s/country_eta_inter.csv", ssp))
end_climate$group %>% unique()
country_vec <- end_climate %>% filter(group==6) %>% pull(ISO_C3)

for (the_country in country_vec){
    # plot time series of eta
    trend_T <- tmp_df %>% filter(ISO_C3==the_country) %>% select(trend_annual) %>% unlist()
    trend_P <- pre_df %>% filter(ISO_C3==the_country) %>% select(trend_annual) %>% unlist()
    eta_country <-  bind_rows(no_inter_eta %>% filter(ISO_C3==the_country),
                              inter_eta %>% filter(ISO_C3==the_country)) %>% 
        select(-1) %>% 
        t()
    colnames(eta_country) <- c("noInter", "Inter")
    
    plot_data <- eta_country %>% 
        as_tibble(rownames="year") %>% 
        mutate(year=as.numeric(year)) %>% 
        gather("key", "value", -year)
    p <- ggplot(plot_data, aes(year, value, color=key)) +
        geom_line() +
        scale_color_manual(values=c("Inter"="#EE0000FF", "noInter"="#3B4992FF")) +
        labs(y=TeX("\\eta"), title = TeX(sprintf("%s, $\\Delta T$=%.2f, $\\Delta P$=%.4f", the_country, trend_T, trend_P))) + 
        my_theme +
        theme(axis.title.x = element_blank())
    f_name <- sprintf("/Users/menghan/Documents/GDP/figures/%s_projection/group6/eta_%s.png", ssp, the_country)
    plot_png(p, f_name, 8.43, 5.99)
}

