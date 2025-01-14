# Plot Fig. 1

library(plotly)

f_name <- "data/IFE_result.csv"
beta_hat <- read_csv(f_name)
beta_hat <- beta_hat$estimate[1:8]
beta_hat

## panel (a) ===================================================================
## pre-load parameters ##
tmp_vec <- seq(0, 30, length.out=50)
pre_vec <- seq(0.00, 3, length.out=length(tmp_vec) )
plot_df <- predict_response_interact(beta_hat)
legend_low <- -0.4
legend_high <- 0.12

m <- list(l = 10, r = 10, t = 0, b = 10) # margins; pad: space between subplots or between the plotting area and the axis lines;
camera <- list(eye = list(x = 1.6, y = 1.6, z = 1.25)) # camera angle
opacity <- 0.7 # marker transparency
fig <- plot_ly(data=plot_df, 
                x=~tmp, y=~pre, z=~z, 
                type="scatter3d", mode="markers",
                marker = list(color=~z, 
                              colorscale = 'Jet', 
                              opacity = opacity,
                              showscale = TRUE, 
                              size = 2, # point size;
                              cmin = legend_low,
                              cmax = legend_high,
                              # `colorbar`: x:rel position; len: length;
                              colorbar = list(x=0.8, len=0.5),
                              cliponaxis = FALSE
                ), 
                scene='scene3')  %>% 
    layout(scene3 = list(xaxis = list(title = 'tmp', 
                                      titlefont = list(size=20), 
                                      tickfont = list(size = 15) ), # axis title
                         yaxis = list(title = 'pre', 
                                      titlefont = list(size=20),
                                      tickfont = list(size = 15),
                                      tickmode = "array", 
                                      tickvals = seq(0,3,0.5), 
                                      ticktext = c("", "0.5", "1", "1.5", "2", "2.5", "3") 
                                      ),
                         zaxis = list(title = '∆ln(GDPpc)', 
                                      titlefont = list(size=20),
                                      tickfont = list(size = 15),
                                      range = list(legend_low, legend_high) 
                                      ) 
                         ), 
           margin = m,
           camera = camera
           ) 
fig

save_image(fig, "figures/surface-plot2.png")

# panel (b) ====================================================================
# fix pre, 2D curve to tmp
base_tmp <- ggplot() +
    xlim(0, 30) 
y_tmp <- function(x, pre=0.9773, 
                  beta_hat=c(0.0143, -0.0005, 0.1193, -0.0833, 
                             -0.0107, 0.0003, 0.0074, -0.0002)
                  ){
    ## Response to tmp change, given fixed precip levels;
    #     @x: tmp (of variable levels);
    #     @pre: fixed pre levels;
    y <- beta_hat[1]*x + beta_hat[2]*x^2 + beta_hat[3]*pre + beta_hat[4]*pre^2 + beta_hat[5]*x*pre + beta_hat[6]*x^2*pre + beta_hat[7]*pre^2*x + beta_hat[8]*x^2*pre^2
    y <- y-mean(y)
    return (y)
}


f_name <- paste0("~/Documents/GDP/data/climate_trend/", "climate_summary_stat.csv")
climate_summary <- read_csv(f_name)

pre_list <- climate_summary[7:15, "pre"] %>% pull() # quantiles 10n
p_tmp <- base_tmp + 
    geom_function(aes(color="Q10"), fun = y_tmp, args=list(pre=pre_list[1], beta_hat=beta_hat)) +
    geom_function(aes(color="Q20"), fun = y_tmp, args=list(pre=pre_list[2], beta_hat=beta_hat)) +
    geom_function(aes(color="Q30"), fun = y_tmp, args=list(pre=pre_list[3], beta_hat=beta_hat)) +
    geom_function(aes(color="Q40"), fun = y_tmp, args=list(pre=pre_list[4], beta_hat=beta_hat)) +
    geom_function(aes(color="Q50"), fun = y_tmp, args=list(pre=pre_list[5], beta_hat=beta_hat)) +
    geom_function(aes(color="Q60"), fun = y_tmp, args=list(pre=pre_list[6], beta_hat=beta_hat)) +
    geom_function(aes(color="Q70"), fun = y_tmp, args=list(pre=pre_list[7], beta_hat=beta_hat)) +
    geom_function(aes(color="Q80"), fun = y_tmp, args=list(pre=pre_list[8], beta_hat=beta_hat)) +
    geom_function(aes(color="Q90"), fun = y_tmp, args=list(pre=pre_list[9], beta_hat=beta_hat)) +
    scale_color_viridis_d() + 
    labs(x="Temperature [ºC]", y=TeX("$\\Delta \\ln(GDPpc)$") ) +
    theme(legend.title = element_blank() )
p_tmp


# panel (c) ====================================================================
# fix tmp, 2D curve to pre

base_pre <- ggplot() +
    xlim(0, 3)
y_pre <- function(x, tmp=18.77, 
                  beta_hat=c(0.0143, -0.0005, 0.1193, -0.0833, 
                             -0.0107, 0.0003, 0.0074, -0.0002)
                  ){
    ## Response to precip change, given fixed levels of tmp ##
    #     @x: precip (of variable levels);
    #     @tmp: fixed tmp levels;
    y <- beta_hat[1]*tmp + beta_hat[2]*tmp^2 + beta_hat[3]*x + beta_hat[4]*x^2 + beta_hat[5]*tmp*x + beta_hat[6]*tmp^2*x + beta_hat[7]*x^2*tmp + beta_hat[8]*tmp^2*x^2
    y <- y-mean(y)
    return (y)
}
tmp_list <- climate_summary[7:15, "tmp"] %>% pull() # quantiles 10n
p_pre <- base_pre + 
    geom_function(aes(color="Q10"), fun = y_pre, args=list(tmp=tmp_list[1], beta_hat=beta_hat)) +
    geom_function(aes(color="Q20"), fun = y_pre, args=list(tmp=tmp_list[2], beta_hat=beta_hat)) +
    geom_function(aes(color="Q30"), fun = y_pre, args=list(tmp=tmp_list[3], beta_hat=beta_hat)) +
    geom_function(aes(color="Q40"), fun = y_pre, args=list(tmp=tmp_list[4], beta_hat=beta_hat)) +
    geom_function(aes(color="Q50"), fun = y_pre, args=list(tmp=tmp_list[5], beta_hat=beta_hat)) +
    geom_function(aes(color="Q60"), fun = y_pre, args=list(tmp=tmp_list[6], beta_hat=beta_hat)) +
    geom_function(aes(color="Q70"), fun = y_pre, args=list(tmp=tmp_list[7], beta_hat=beta_hat)) +
    geom_function(aes(color="Q80"), fun = y_pre, args=list(tmp=tmp_list[8], beta_hat=beta_hat)) +
    geom_function(aes(color="Q90"), fun = y_pre, args=list(tmp=tmp_list[9], beta_hat=beta_hat)) +
    scale_color_viridis_d() + 
    labs(x="Annual Precipitation [Meters]", y=TeX("$\\Delta \\ln(GDPpc)$")) +
    theme(legend.title = element_blank() )
p_pre

p <- plot_grid(p_tmp + theme(legend.position = "none"), 
          p_pre, 
          nrow=1,
          labels=c("(b)", "(c)"),
          rel_widths = c(.85,1),
          hjust=-1, label_size=12)
f_name <- "figures/response_surface_2D_Q10-Q90.png"
# ggsave(f_name)
# plot_png(p, f_name, 11.2, 5.21)

# panel (d) ====================================================================
## historical impacts map
pre_df <- read_csv("~/Documents/GDP/data/climate_trend/climate_trend_pre.csv")
tmp_df <- read_csv("~/Documents/GDP/data/climate_trend/climate_trend_tmp.csv")

tmp1_change <- tmp_df$end-tmp_df$start
tmp2_change <- tmp_df$end^2-tmp_df$start^2

Delta_tmp <- cbind(1, pre_df$avg, pre_df$avg^2) %*% matrix(c(beta_hat[1], beta_hat[5], beta_hat[7]), ncol=1) * tmp1_change + 
    cbind(1, pre_df$avg, pre_df$avg^2) %*% matrix(c(beta_hat[2], beta_hat[6], beta_hat[8]), ncol=1) * tmp2_change

Delta_tmp <- cbind("ISO_C3"=pre_df$ISO_C3, as_tibble_col(Delta_tmp, column_name = "tmp_impact"))
Delta_tmp$tmp_impact %>% summary()
quantile(Delta_tmp$tmp_impact, c(0.05, 0.95))

pre1_change <- pre_df$end-pre_df$start
pre2_change <- pre_df$end^2-pre_df$start^2

Delta_pre <- cbind(1, tmp_df$avg, tmp_df$avg^2) %*% matrix(c(beta_hat[3], beta_hat[5], beta_hat[6]), ncol=1) * pre1_change + 
    cbind(1, tmp_df$avg, tmp_df$avg^2) %*% matrix(c(beta_hat[4], beta_hat[7], beta_hat[8]), ncol=1) * pre2_change

Delta_pre <- cbind("ISO_C3"=pre_df$ISO_C3, as_tibble_col(Delta_pre, column_name = "pre_impact"))
Delta_pre %>% summary()
quantile(Delta_pre$pre_impact, c(0.05, 0.95))

# total impact
Delta_all <- Delta_tmp %>% left_join(Delta_pre, by="ISO_C3")
Delta_all <- Delta_all %>% 
    mutate(all_impact = tmp_impact+pre_impact)
head(Delta_all)

Delta_all$all_impact %>% summary()
quantile(Delta_all$all_impact, c(0.05, 0.95))

legend_low <- -0.02
legend_high <- 0.02
c <- 10 * .Machine$double.eps
Delta_all <- Delta_all %>% 
    mutate(all_impact_c = squish(all_impact, range = c(legend_low+c, legend_high) ) )

f_name <- "data/historical_impact.csv"
f_name
# write.csv(Delta_all, f_name, row.names = FALSE)

title <- "All impacts"
unit <- "%"
step <- 0.005

library(rnaturalearth)
world.map <- ne_countries(scale = "medium", returnclass = "sf")
world.map <- world.map %>% left_join(Delta_all, by=c("iso_a3_eh"="ISO_C3"))
colnames(world.map)
world.map$iso_a3_eh %>% unique() %>% sort()
cold <- colorRampPalette(c("#000033","#00007F", "#7AACED", "white"))(7) # from -7
warm <- c("#FFD4D4", "#FFB2B2", "#FF9090", "#FF6D6D", "#FF4B4B", "#FF2A2A")
myColors <- c(cold, warm)

p_map <- ggplot(data = world.map %>% filter(continent!="Antarctica")) +
    geom_sf(aes(fill=all_impact_c), colour='gray50', lwd=0.3 ) +
    scale_fill_stepsn(limits = c(legend_low, legend_high), 
                      breaks = c(legend_low-c, seq(legend_low+step, legend_high-step, step), legend_high+c ), 
                      labels = function(x) {sprintf("%.1f", x*100)},
                      show.limits = TRUE, 
                      right = FALSE, # include right bin, (low, up]
                      colours = myColors,
                      name = TeX(unit)
    ) +
    coord_sf(datum = NA) +
    guides(pattern = guide_legend(title=element_blank()) ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.1),
          legend.title = element_text(hjust=0.85) )
p_map

f_name <- "figures/historical_impacts_map.png"
# ggsave(f_name)
# plot_png(p_map, f_name, 11.2, 5.21)

