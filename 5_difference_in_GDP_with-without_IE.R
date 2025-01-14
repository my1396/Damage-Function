# Difference in CC impacts on GDP with and without interactive terms

ssp <- "SSP585"
no_inter <- read_csv(sprintf("data/%s/country_all_impact_nointer_250106.csv", ssp))
no_inter$`2100.deltaAll`
inter <- read_csv(sprintf("data/%s/country_all_impact_inter_250106.csv", ssp))
inter$`2100.deltaAll`
## country time series of cumulative effects
the_country <- "HND"


delta_country <- bind_rows(no_inter %>% filter(ISO_C3==the_country),
          inter %>% filter(ISO_C3==the_country)) %>% 
    select(-1) %>% 
    t()
colnames(delta_country) <- c("noInter", "Inter")
delta_country


{ # difference in eta
    T0 <- tmp_df %>% filter(ISO_C3==the_country) %>% select(start) %>% unlist()
    P0 <- pre_df %>% filter(ISO_C3==the_country) %>% select(start) %>% unlist()
    h0_noIE <- sum(c(T0, T0^2, P0, P0^2, rep(0,4)) * beta_hat_IFE)
    h0_IE <- sum(c(T0, T0^2, P0, P0^2, T0*P0, T0^2*P0, T0*P0^2, T0^2*P0^2) * beta_hat_IFE)
    
    t <- 80
    trend_T <- tmp_df %>% filter(ISO_C3==the_country) %>% select(trend_annual) %>% unlist()
    trend_P <- pre_df %>% filter(ISO_C3==the_country) %>% select(trend_annual) %>% unlist()
    T1 <- T0 + trend_T*t
    P1 <- P0 + trend_P*t
    h1_noIE <- sum(c(T1, T1^2, P1, P1^2, rep(0,4)) * beta_hat_IFE)
    h1_IE <- sum(c(T1, T1^2, P1, P1^2, T1*P1, T1^2*P1, T1*P1^2, T1^2*P1^2) * beta_hat_IFE)
    eta_noIE <- h1_noIE - h0_noIE
    eta_IE <- h1_IE - h0_IE
    list("country"=the_country, "eta"=c("pre_trend"=trend_P, "eta_IE"=eta_IE, "eta_noIE"=eta_noIE, "eta_diff"=eta_IE-eta_noIE))
}



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
sum(cast_124)/(nrow(diff_df)-1)

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


f_name <- sprintf("data/%1$s/country_all_impact_diff_with-without-IE_%1$s.csv", ssp)
f_name
# write_csv(diff_df, f_name)
## Examine countries ===========================================================
tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))
# convert to annual total precipitation
pre_df <- pre_df %>% 
    mutate_at(c("start", "end", "avg", "trend_annual"), ~.*12/1000)
the_country <- "MEX"
{
    start_pre <- pre_df %>% filter(ISO_C3==the_country) %>% pull(start)
    avg_pre <- pre_df %>% filter(ISO_C3==the_country) %>% pull(avg)
    end_pre <- pre_df %>% filter(ISO_C3==the_country) %>% pull(end)
    start_tmp <- tmp_df %>% filter(ISO_C3==the_country) %>% pull(start)
    avg_tmp <- tmp_df %>% filter(ISO_C3==the_country) %>% pull(avg)
    end_tmp <- tmp_df %>% filter(ISO_C3==the_country) %>% pull(end)
    
    cat(sprintf("country: %s", the_country),
        sprintf("start pre — opt pre*: %.4f — %.4f;", start_pre, opt_pre(start_tmp, beta_hat=beta_hat_IFE)),
        sprintf("avg pre — opt pre*: %.4f — %.4f;", avg_pre, opt_pre(avg_tmp, beta_hat=beta_hat_IFE)),
        sprintf("end pre — opt pre*: %.4f — %.4f", end_pre, opt_pre(end_tmp, beta_hat=beta_hat_IFE)),
        sep="\n")
    
    cat(sprintf("start tmp — opt tmp*: %.4f — %.4f;", start_tmp, opt_tmp(start_pre, beta_hat=beta_hat_IFE)),
        sprintf("avg tmp — opt tmp*: %.4f — %.4f;", avg_tmp, opt_tmp(avg_pre, beta_hat=beta_hat_IFE)),
        sprintf("end tmp — opt tmp*: %.4f — %.4f", end_tmp, opt_tmp(end_pre, beta_hat=beta_hat_IFE)),
        sep="\n")
}








