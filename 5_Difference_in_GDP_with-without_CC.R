# Difference in CC impacts on GDP with and without interactive terms

ssp <- "SSP585"
no_inter <- read_csv(sprintf("data/%s/country_all_impact_nointer_250106.csv", ssp))
no_inter$`2100.deltaAll`
inter <- read_csv(sprintf("data/%s/country_all_impact_inter_250106.csv", ssp))
inter$`2100.deltaAll`

## calculate the difference, inter - no_inter 
diff_df <- inter[,c(1, 81)] %>% 
    left_join(no_inter[,c(1, 81)], by="ISO_C3", suffix = c(".Inter", ".noInter"))
diff_df <- diff_df %>% mutate(diff =  .[[2]] - .[[3]])
diff_df %>% nrow()
diff_df[,-1] %>% summary()

## histogram of diff
p <- ggplot() +
    geom_histogram(aes(x=diff_df$diff, y=after_stat(density)),
                   fill="#BDBCBC", color="black", binwidth=0.1, boundary=0) +
    scale_x_continuous(limits=c(-1,1)) +
    labs(x = TeX("$\\delta^{no\\, IE} - \\delta^{IE}$"),
         y = "Density",
         title = ssp )
p

# 124 countries
diff_df %>% 
    filter(diff>0 & `2100.deltaAll.noInter`<0 & `2100.deltaAll.Inter`<0 ) %>% 
    as.data.frame() %>% 
    add_country_name()
cast_124 <- with(diff_df, diff>0 & `2100.deltaAll.noInter`<0 & `2100.deltaAll.Inter`<0)

# 6 countries
diff_df %>% 
    filter(diff<0 & `2100.deltaAll.noInter`>0 & `2100.deltaAll.Inter`<0 ) %>% 
    as.data.frame() %>% 
    arrange(diff) %>% 
    add_country_name()
cast_6 <- with(diff_df, diff<0 & `2100.deltaAll.noInter`>0 & `2100.deltaAll.Inter`<0)

# diff<0, NEG + NEG: 15 countries
diff_df %>% 
    filter(diff<0 & `2100.deltaAll.noInter`<0 & `2100.deltaAll.Inter`<0 ) %>% 
    as.data.frame() %>% 
    arrange(diff) %>% 
    add_country_name()
cast_15 <- with(diff_df, diff<0 & `2100.deltaAll.noInter`<0 & `2100.deltaAll.Inter`<0)

# 11 countries
diff_df %>% 
    filter(diff<0 & `2100.deltaAll.noInter`>0 & `2100.deltaAll.Inter`>0 ) %>% 
    as.data.frame() %>% 
    arrange(diff) %>% 
    add_country_name()
cast_11 <- with(diff_df, diff<0 & `2100.deltaAll.noInter`>0 & `2100.deltaAll.Inter`>0)


diff_df[cast_124, "group"] <- 124
diff_df[cast_6, "group"] <- 6
diff_df[cast_15, "group"] <- 15
diff_df[cast_11, "group"] <- 11
diff_df <- diff_df %>% mutate(group=as.factor(group))



