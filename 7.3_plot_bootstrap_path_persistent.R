## Plot uncertainty band for economic losses

# set source location as working dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# Generating uncertainty band for projected impacts through bootstrap
ssp <- "SSP585"
data_dir <- "~/Documents/GDP/data/"
f_name <- paste0(data_dir, sprintf("reg_result/bootstrap/%s/global_impact_bootstrap1000_growth.csv", ssp))
f_name 
pct_impact_interactive_df <- read_csv(f_name)

## summary statistics 
pct_impact_interactive_df %>% dim()
Delta_all_summary <- pct_impact_interactive_df[,30:59] %>% 
    apply(1, get_stat, q_list=c(.05, .95))
Delta_all_summary <- Delta_all_summary %>% 
    t() %>% 
    as_tibble() %>% 
    add_column(time=time_vec, .before=1)
Delta_all_summary

plot_data <- Delta_all_summary %>% 
    select(Mean, `5%`, `95%`)

plot_data <- plot_data*100
plot_data <- plot_data %>% 
    add_column(date=Delta_all_summary$time, .before = 1)

long_format <- plot_data %>% 
    gather(key="key", value="value", -date)
p_global <- long_format %>% 
    ggplot(aes(x=date, y=value, color=key)) +
    geom_line() +
    labs(title=sprintf("%s, %s", "Global", ssp), y="Percentage change in GDPpc") +
    theme(axis.title.x = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.title = element_blank())
p_global
