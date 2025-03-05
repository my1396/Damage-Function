## Based on bootstrapped regression coefficients, calculate projected economic impacts
## per bootstrap.

## Load data -------------------------------------------------------------------
f_name <- "data//bootstrap58_coef.csv" 
f_name
IEres_1000 <- read_csv(f_name)
IEres_1000
dim(IEres_1000)

## Load population weights
f_name <- paste0(data_dir, "population/SSP_Population_weight.csv") 
f_name
pop_weight_df <- read_csv(f_name)

## projected GDP growth from SSP scenarios
ssp <- "SSP126"
ssp <- "SSP245"
ssp <- "SSP370"
ssp <- "SSP585"

pop_weight <- pop_weight_df %>% filter(Scenario == substr(ssp, 1, 4))
pop_weight
pop_weight <- pop_weight[,c(-1,-2,-4,-5)]

## GDP growth in absence of CC, baseline growth 
f_name <- sprintf("data/baseline_growth/%s_GrowthProjections.csv", substr(ssp, 1, 4))
f_name
gdp_SSP <- read_csv(f_name)
gdp_SSP

## impacts at each year --------------------------------------------------------
tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))
# convert to annual total precipitation
pre_df <- pre_df %>% 
    mutate_at(c("start", "end", "avg", "trend_annual"), ~.*12/1000)
tmp_df
pre_df

time_vec <- ymd("2020-12-31") %m+% years(1:80) 
time_vec

beta <- matrix(c(0.0104108555998223, -0.000406755949195648), ncol=1)
beta
Delta_tmp_df <- data.frame(matrix(nrow = nrow(tmp_df), ncol = 80)) 
for (t in 1:80){
    # loop through time periods, annually
    print (time_vec[t])
    tmp_df_t <- tmp_df %>% 
        mutate(mid_0 = start,
               mid_1 = start+trend_annual*t,
               tmp1_change = mid_1-mid_0, 
               tmp2_change = mid_1^2-mid_0^2)
    tmp_df_t %>% head() %>% data.frame()
    tmp2_change <- tmp_df_t %>% 
        pull(tmp2_change)
    tmp1_change <- tmp_df_t %>% 
        pull(tmp1_change)
    Delta_tmp_df[,t] <- cbind(tmp1_change, tmp2_change) %*% beta %>% as.vector()
    
}

## GDP growth in absence of CC, baseline growth 
# SSP projected GDP growth
Delta_tmp_df <- Delta_tmp_df %>% 
    as_tibble() %>% 
    add_column(ISO_C3=tmp_df$ISO_C3, .before = 1) %>% 
    left_join(gdp_SSP, by=c("ISO_C3"="Region")) %>% 
    drop_na() 
gdp_nCC <- Delta_tmp_df[,82:161]
gdp_CC <- Delta_tmp_df[,2:81] + gdp_nCC

# aggregate to global, weighted by population
Delta_gdp_ncc <- gdp_nCC %>% 
    as_tibble() %>% 
    mutate(ISO_C3 = Delta_tmp_df$ISO_C3) %>% 
    left_join(pop_weight, by=c("ISO_C3"="Region") ) %>% 
    drop_na() 

global_gdp <- t(Delta_gdp_ncc[, 1:80]) %*% as.matrix(Delta_gdp_ncc[, 82:161])
global_gdp_cum <- cumprod(diag(global_gdp)+1)
global_gdp_cum


## start looping through bootstraps --------------------------------------------
bt <- 1
pct_impact_interactive_df <- data.frame(matrix(nrow = 80, ncol = 0)) 
for (bt in 2:58){
    # loop through bootstraps #
    print (bt)
    beta_hat <- IEres_1000 %>%      # assign regression coef.
        filter(bootstrap==bt) %>% 
        pull(estimate) %>% 
        head(8)
    beta_hat
    Delta_all_df <- data.frame(matrix(nrow = nrow(tmp_df), ncol = 0)) 
    for (t in 1:80){
        # loop through time periods, annually #
        tmp_df_t <- tmp_df %>% 
            mutate(mid_0 = start,
                   mid_1 = start+trend_annual*t )
        pre_df_t <- pre_df %>% 
            mutate(mid_0 = start,
                   mid_1 = start+trend_annual*t )
        
        # X regressor at t-1
        x_df0 <- tmp_df_t %>% 
            select(ISO_C3, mid_0) %>% 
            rename(tmp = mid_0)
        temp <- pre_df_t %>% 
            select(ISO_C3, mid_0) %>% 
            rename(pre = mid_0)
        x_df0 <- x_df0 %>% 
            left_join(temp, by="ISO_C3") %>% 
            mutate(tmp2 = tmp^2,
                   pre2 = pre^2,
                   tmp_pre = tmp*pre, 
                   tmp2_pre = tmp2*pre, 
                   pre2_tmp = pre2*tmp,
                   tmp2_pre2 = tmp2*pre2 ) %>% 
            select(c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2"))
        
        
        # X regressor at t
        x_df1 <- tmp_df_t %>% 
            select(ISO_C3, mid_1) %>% 
            rename(tmp = mid_1)
        temp <- pre_df_t %>% 
            select(ISO_C3, mid_1) %>% 
            rename(pre = mid_1)
        x_df1 <- x_df1 %>% 
            left_join(temp, by="ISO_C3") %>% 
            mutate(tmp2 = tmp^2,
                   pre2 = pre^2,
                   tmp_pre = tmp*pre, 
                   tmp2_pre = tmp2*pre, 
                   pre2_tmp = pre2*tmp,
                   tmp2_pre2 = tmp2*pre2 ) %>% 
            select(c("tmp", "tmp2", "pre", "pre2", "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2"))
        
        y_t0 <- tcrossprod(x_df0 %>% as.matrix(), beta_hat %>% matrix(nrow=1))
        y_t1 <- tcrossprod(x_df1 %>% as.matrix(), beta_hat %>% matrix(nrow=1))
        Delta_all_df[, t] <- (y_t1-y_t0) %>% as.vector()
    } # end of for loop over time T=80
    
    Delta_all_df  <- Delta_all_df %>% 
        as_tibble() %>% 
        add_column(ISO_C3=tmp_df$ISO_C3, .before = 1) 
    Delta_all_df %>% dim()
    
    Delta_all_df  <- Delta_all_df %>% 
        left_join(gdp_SSP, by=c("ISO_C3"="Region")) %>% 
        drop_na() 
    Delta_all_df %>% dim()
    
    gdp_nCC <- Delta_all_df[,82:161]
    gdp_CC <- Delta_all_df[,2:81] + gdp_nCC
    
    # global impact of CC #
    x_df <- gdp_CC %>% 
        as_tibble() %>% 
        mutate(ISO_C3 = Delta_all_df$ISO_C3) %>% 
        left_join(pop_weight, by=c("ISO_C3"="Region") )%>% 
        drop_na() 
    x_df
    
    global_impact_interactive <- t(x_df[, 1:80]) %*% as.matrix(x_df[, 82:161]) # dynamic weight
    global_impact_interactive_cum <- cumprod(diag(global_impact_interactive)+1)
    global_impact_interactive_cum
    
    pct_impact_interactive <- global_impact_interactive_cum/global_gdp_cum-1
    pct_impact_interactive
    pct_impact_interactive_df[,bt] <- pct_impact_interactive
}

pct_impact_interactive_df %>% dim() # 80*1000
pct_impact_interactive_df <- pct_impact_interactive_df %>%
    add_column(time=time_vec, .before=1)

f_name <- paste0(data_dir, sprintf("reg_result/bootstrap/%s/global_impact_bootstrap1000_growth.csv", ssp))
f_name 
write_csv(pct_impact_interactive_df, f_name)
