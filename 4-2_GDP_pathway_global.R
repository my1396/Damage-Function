# Global GDP pathways in Fig 2 (a)

f_name <- paste0(data_dir, "population/SSP_Population_weight.csv") 
f_name
pop_weight_df <- read_csv(f_name)

ssp <- "SSP126"
## Load population weights
# population distribution are more even, not densely concentrated in China or India any more
# SSP585: 5.48 Celsius global warming by 2100, weighted by pop in 2100
pop_weight <- pop_weight_df %>% filter(Scenario == substr(ssp, 1, 4))
pop_weight
pop_weight <- pop_weight[,c(-1,-2,-4,-5)]


## GDP growth in absence of CC, baseline growth 
# SSP projected GDP growth
f_name <- sprintf("data/baseline_growth/%s_GrowthProjections.csv", substr(ssp, 1, 4))
f_name
gdp_SSP <- read_csv(f_name)
gdp_SSP

## Temp effects ================================================================
f_name <- sprintf("data/%s/country_eta_tmp.csv", ssp)
Delta_tmp_df <- read_csv(f_name)
Delta_tmp_df %>% dim()
setdiff(Delta_tmp_df$ISO_C3, gdp_SSP$Region)

# Merge with baseline GDP
Delta_tmp_df <- Delta_tmp_df %>% 
    left_join(gdp_SSP, by=c("ISO_C3"="Region")) %>% 
    drop_na() 
Delta_tmp_df
# growth without CC
gdp_nCC <- Delta_tmp_df[,82:161]
# growth with CC
gdp_CC <- Delta_tmp_df[,2:81] + gdp_nCC

## Aggregate to global, weighted by SSP projected population 
Delta_gdp_cc <- gdp_CC %>% 
    as_tibble() %>% 
    mutate(ISO_C3 = Delta_tmp_df$ISO_C3) %>% 
    left_join(pop_weight, by=c("ISO_C3"="Region") )%>% 
    drop_na() 
Delta_gdp_cc %>% dim()
colnames(Delta_gdp_cc)
global_impact <- t(Delta_gdp_cc[, 1:80]) %*% as.matrix(Delta_gdp_cc[, 82:161]) 
global_impact %>% dim()
diag(global_impact)
global_impact_cum <- cumprod(diag(global_impact)+1)
global_impact_cum


# GDP growth in absence of CC, baseline
Delta_gdp_ncc <- gdp_nCC %>% 
    as_tibble() %>% 
    mutate(ISO_C3 = Delta_tmp_df$ISO_C3) %>% 
    left_join(pop_weight, by=c("ISO_C3"="Region") ) %>% 
    drop_na() 
Delta_gdp_ncc %>% dim()

Delta_gdp_ncc[, 75:80]
Delta_gdp_ncc[, 81:82]
global_gdp <- t(Delta_gdp_ncc[, 1:80]) %*% as.matrix(Delta_gdp_ncc[, 82:161])
global_gdp %>% dim()
diag(global_gdp)
global_gdp_cum <- cumprod(diag(global_gdp)+1)
global_gdp_cum

pct_impact <- global_impact_cum/global_gdp_cum-1
pct_impact

plot_data <- tibble(time=time_vec, impact=pct_impact*100)
ggplot(plot_data, aes(x=time, y=impact)) +
    geom_line() +
    labs(y="Percentage change in GDPpc", title="Temperature effects")



## No interactive terms ========================================================
f_name <- sprintf("data/%s/country_eta_nointer.csv", ssp)
f_name
Delta_all_df <- read_csv(f_name)
Delta_all_df  <- Delta_all_df %>% 
    left_join(gdp_SSP, by=c("ISO_C3"="Region"), 
              suffix = c(".deltaAll", ".baseline")) %>% 
    drop_na() 
Delta_all_df %>% dim()
colnames(Delta_all_df) %>% tail(5)

gdp_CC <- Delta_all_df[,2:81] + gdp_nCC
gdp_CC %>% dim()

# Aggregate to global, weighted by population
Delta_gdp_cc <- gdp_CC %>% 
    as_tibble() %>% 
    mutate(ISO_C3 = Delta_all_df$ISO_C3) %>% 
    left_join(pop_weight, by=c("ISO_C3"="Region") )%>% 
    drop_na() 
Delta_gdp_cc

global_impact_all <- t(Delta_gdp_cc[, 1:80]) %*% as.matrix(Delta_gdp_cc[, 82:161]) # dynamic weight
global_impact_all %>% dim()
diag(global_impact_all)
global_impact_all_cum <- cumprod(diag(global_impact_all)+1)
global_impact_all_cum

pct_impact_all <- global_impact_all_cum/global_gdp_cum-1
pct_impact_all

plot_data <- tibble(time=time_vec, impact=pct_impact_all*100)
ggplot(plot_data, aes(x=time, y=impact)) +
    geom_line() +
    labs(y="Percentage change in GDPpc", title="Temp + Precip effects, no interactive terms")
#

## With interactive terms ======================================================
f_name <- sprintf("data/%s/country_eta_inter.csv", ssp)
Delta_all_df <- read_csv(f_name)

Delta_all_df  <- Delta_all_df %>% 
    left_join(gdp_SSP, by=c("ISO_C3"="Region")) %>% 
    drop_na() 
Delta_all_df %>% dim()

gdp_CC <- Delta_all_df[,2:81] + gdp_nCC

# Aggregate to global, weighted by population
Delta_gdp_cc <- gdp_CC %>% 
    as_tibble() %>% 
    mutate(ISO_C3 = Delta_all_df$ISO_C3) %>% 
    left_join(pop_weight, by=c("ISO_C3"="Region") )%>% 
    drop_na() 
Delta_gdp_cc

global_impact_interactive <- t(Delta_gdp_cc[, 1:80]) %*% as.matrix(Delta_gdp_cc[, 82:161]) # dynamic weight
global_impact_interactive %>% dim()
diag(global_impact_interactive)
global_impact_interactive_cum <- cumprod(diag(global_impact_interactive)+1)
global_impact_interactive_cum

pct_impact_interactive <- global_impact_interactive_cum/global_gdp_cum-1
pct_impact_interactive

plot_data <- tibble(time=time_vec, impact=pct_impact_interactive*100)
ggplot(plot_data, aes(x=time, y=impact)) +
    geom_line() +
    labs(y="Percentage change in GDPpc", title="Temp + Precip effects, with interactive effects")


## Merge 3-scenario in one =====================================================
impact_df <- tibble(time = time_vec,
                    tmp_impact = pct_impact,
                    all_impact_nointer = pct_impact_all,
                    all_impact_inter = pct_impact_interactive)
impact_df
impact_df %>% tail()
f_name <- paste0("data/", sprintf("%s/global_impact_projections.csv", ssp))
f_name
write_csv(impact_df, f_name)

plot_data <- impact_df %>% 
    mutate_at(-1, ~.*100)
long_format <- plot_data %>% 
    gather(key, value, -time)
long_format <- long_format %>% 
    mutate(key=factor(key, 
                      levels=c("tmp_impact", "all_impact_nointer", "all_impact_inter")) )
labels <- c("tmp_impact"="tmp", 
            "all_impact_nointer"="tmp+pre, w/o interactive", 
            "all_impact_inter"="tmp+pre, w/ interactive")
colors <-  c("tmp_impact"="blue", 
             "all_impact_nointer"="black", 
             "all_impact_inter"="red")
p <- ggplot(long_format, aes(x = time, y = value, color = key)) +
    geom_line() +
    scale_color_manual(values = colors, labels = labels) +
    labs(y="Percentage change in GDPpc", title=ssp) +
    theme(legend.position = c(0.8, 0.9),
          legend.margin = margin(t=0, b=0, unit="mm"),
          legend.title = element_blank(),
          axis.title.x = element_blank())
p




