# Country level GDP pathway from 2020 to 2100 per SSP scenario
#       Two models:
#           1. without interactive terms;
#           2. with interactive terms;
library(tidyverse)
library(scales)
c <- 10 * .Machine$double.eps

ssp <- "SSP585"
## load climate projections
tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))

# convert to annual total precipitation
pre_df <- pre_df %>% 
    mutate_at(c("start", "end", "avg", "trend_annual"), ~.*12/1000)
tmp_df
pre_df

time_vec <- ymd("2020-12-31") %m+% years(1:80) 
time_vec

## No interactive terms ========================================================
beta_hat <- read_csv("data/IFE_result.csv")
beta_hat <- beta_hat$estimate[1:8] %>% 
    head(4)
beta_hat

# initialize impact data frame, NxT
Delta_all_df <- data.frame(matrix(nrow = nrow(tmp_df), ncol = 0)) 
for (t in 1:80){
    # loop through time periods, annually
    if (t%%5 == 0) print (time_vec[t])
    tmp_df_t <- tmp_df %>% 
        mutate(mid_0 = start,
               mid_1 = start+trend_annual*t )
    tmp_df_t %>% head() %>% data.frame()
    
    pre_df_t <- pre_df %>% 
        mutate(mid_0 = start,
               mid_1 = start+trend_annual*t )
    pre_df_t %>% head() %>% data.frame()
    
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
               pre2 = pre^2) %>% 
        select(c("tmp", "tmp2", "pre", "pre2"))
    
    
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
               pre2 = pre^2) %>% 
        select(c("tmp", "tmp2", "pre", "pre2"))
    
    y_t0 <- tcrossprod(x_df0 %>% as.matrix(), beta_hat %>% matrix(nrow=1))
    y_t1 <- tcrossprod(x_df1 %>% as.matrix(), beta_hat %>% matrix(nrow=1))
    Delta_all_df[, t] <- (y_t1-y_t0) %>% as.vector()
} # end of for loop

colnames(Delta_all_df) <- seq(2021, 2100)
Delta_all_df  <- Delta_all_df %>% 
    add_column(ISO_C3 = tmp_df$ISO_C3, .before = 1)  %>% 
    as_tibble()
f_name <- sprintf("data/baseline_growth/%s_GrowthProjections.csv", substr(ssp, 1, 4))
f_name
gdp_SSP <- read_csv(f_name)
colnames(gdp_SSP)[-1] <- seq(2021, 2100)
Delta_all_df <- Delta_all_df %>% 
    left_join(gdp_SSP, by=c("ISO_C3"="Region"), 
              suffix = c(".deltaAll", ".baseline")) %>% 
    drop_na() 
Delta_all_df %>% dim()

# growth without CC
gdp_nCC <- Delta_all_df[,82:161]
baseline_country <- apply(gdp_nCC+1, 1, cumprod) %>% t() 
# growth with CC
gdp_CC <- Delta_all_df[,2:81] + gdp_nCC
CC_country <- apply(gdp_CC+1, 1, cumprod) %>% t()
pct_impact_country <- CC_country/baseline_country - 1
pct_impact_country[1:5,1:10]
pct_impact_country[1:5,71:80]

pct_impact_country <- pct_impact_country %>% 
    as_tibble() %>%
    add_column(ISO_C3=Delta_all_df$ISO_C3, .before = 1)

f_name <- sprintf("data/%s/country_all_impact_nointer_250106.csv", ssp)
f_name
# write_csv(pct_impact_country, f_name)


## With interactive terms ======================================================
beta_hat <- read_csv("data/IFE_result.csv")
beta_hat <- beta_hat$estimate[1:8]
beta_hat


# initialize impact data frame
Delta_all_df <- data.frame(matrix(nrow = nrow(tmp_df), ncol = 0)) 
for (t in 1:80){
    # loop through time periods, annually
    if (t%%5 == 0) print (time_vec[t])
    tmp_df_t <- tmp_df %>% 
        mutate(mid_0 = start,
               mid_1 = start+trend_annual*t )
    tmp_df_t %>% head() %>% data.frame()
    
    pre_df_t <- pre_df %>% 
        mutate(mid_0 = start,
               mid_1 = start+trend_annual*t )
    pre_df_t %>% head() %>% data.frame()
    
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
} # end of for loop

colnames(Delta_all_df) <- seq(2021, 2100)
Delta_all_df  <- Delta_all_df %>% 
    add_column(ISO_C3 = tmp_df$ISO_C3, .before = 1) %>% 
    as_tibble()
Delta_all_df  <- Delta_all_df %>% 
    left_join(gdp_SSP, by=c("ISO_C3"="Region"), 
              suffix = c(".deltaAll", ".baseline")) %>% 
    drop_na() 
Delta_all_df %>% dim()
colnames(Delta_all_df) %>% tail(5)

gdp_CC <- Delta_all_df[,2:81] + gdp_nCC
CC_country <- apply(gdp_CC+1, 1, cumprod) %>% t()
pct_impact_country <- CC_country/baseline_country - 1
pct_impact_country[1:5,1:10]
pct_impact_country[1:5,71:80]

pct_impact_country <- pct_impact_country %>% 
    as_tibble() %>%
    add_column(ISO_C3=Delta_all_df$ISO_C3, .before = 1)

f_name <- sprintf("data/%s/country_all_impact_inter_250106.csv", ssp)
f_name
write_csv(pct_impact_country, f_name)



















