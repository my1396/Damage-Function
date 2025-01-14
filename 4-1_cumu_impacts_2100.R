# Cumulative impacts until 2100 

ssp <- "SSP585"
inter <- read_csv(sprintf("data/%s/country_all_impact_inter_250106.csv", ssp))
st_2100 <- inter %>% select(1, 81)
st_2100

st_2100$`2100.deltaAll` %>% summary()

# histogram
p <- ggplot(st_2100) +
    geom_histogram(aes(x=`2100.deltaAll`, y=after_stat(density)), 
                   fill="#BDBCBC", color="black", binwidth=0.1, boundary=0) +
    scale_x_continuous(limits=c(-1,1)) +
    labs(x="Climate change impact until 2100",
         y="Density",
         title=sprintf("%s, with interactive terms", ssp) )
f_name <- sprintf("figures/%1$s/%1$s_persistent_impacts_histogram_with-IE.png", ssp)
# plot_png(p, f_name, 6.42, 4.21)

