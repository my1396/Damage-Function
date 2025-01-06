# Plot optimal curves with current climate in 2019

country_points <- tmp_df %>% 
    select(ISO_C3, start) %>% 
    left_join(pre_df[, c(1,2)], by="ISO_C3", suffix = c(".tmp", ".pre"))  
colnames(country_points)[-1] <- c("end.tmp", "end.pre")

ssp <- "SSP585"
tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))
# convert to annual total precipitation
pre_df <- pre_df %>% 
    mutate_at(c("start", "end", "avg", "trend_annual"), ~.*12/1000)
country_points <- tmp_df %>% 
    select(ISO_C3, start) %>% 
    left_join(pre_df[, c(1, 2)], by="ISO_C3", suffix = c(".tmp", ".pre"))  
colnames(country_points)[-1] <- c("end.tmp", "end.pre")


# drying indicator, TRUE/FALSE
climate_df <- pre_df %>% 
    mutate(drying = ifelse(trend_annual<0, TRUE, FALSE))
end_climate <- diff_df %>% 
    left_join(country_points, by="ISO_C3") %>% # join climate 2019
    left_join(climate_df[,c("ISO_C3", "drying")], by="ISO_C3") # join dry/wet
country_vec <- c("USA", "AUS", "SWE", "TJK", "MEX") # select country labels

colors <- c("6"="#008B45FF", "11"="#D62728FF", "15"="#FF7F0EFF", "124"="#1F77B4FF")
shapes <- c("TRUE"=4, "FALSE"=19) # drying: cross; wetting: dot

p_opt_tmp <- ggplot(end_climate, aes(x=end.pre, y=end.tmp, color=group)) +
    geom_point(aes(shape=drying), size=2.5) +
    geom_text(data=end_climate %>% filter(ISO_C3 %in% country_vec),
              aes(label=ISO_C3),
              hjust=-0.3, vjust=-0.1, 
              size=3.5, fontface="bold",
              show.legend = FALSE
    ) + 
    geom_function(linetype="solid", color="black", fun = opt_tmp, args=list(beta_hat=beta_hat_IFE), linewidth=0.8) + 
    geom_hline(aes(yintercept=12.80), color="black", linetype="dashed", linewidth=0.8) +
    scale_color_manual(values=colors) +
    scale_shape_manual(values=shapes, labels=c("TRUE"="Drying", "FALSE"="Wetting")) +
    labs(x="Annual total precipitation [Meters]", 
         y="Optimal temperature [ºC]",
         color="Group",
         shape="Precip. trend",
         title=ssp) +
    # scale_x_continuous(limits = c(0,3)) +
    my_theme +
    theme(legend.position = c(0.8, 0.2),
          legend.box = "horizontal")
p_opt_tmp


breaks <- c("IFE", "Burke")
linetypes <- setNames(c("solid", "dashed"), breaks)
colors <- setNames(c("black", "black"), breaks)
linewidths <- setNames(c(0.8, 0.8), breaks)
labels <- setNames(c("IFE", TeX("Burke ($P^*$=1.73 m)")), breaks)
g <- guide_legend("title", nrow=2) # define legend aes)

p_opt_pre <- base_tmp +
    geom_function(fun = opt_pre, 
                  aes(color="IFE", linetype="IFE", linewidth="IFE"),
                  args = list(beta_hat=beta_hat_IFE)
    ) +
    geom_hline(aes(yintercept=1.734513, color="Burke", linetype="Burke", linewidth="Burke")) +
    labs(x="Annual Average temperature [ºC]", y="Optimal precipitation [Meters]") +
    scale_color_manual(values=colors, breaks=breaks, labels = labels) +
    scale_linetype_manual(values=linetypes, breaks=breaks, labels = labels) +
    scale_linewidth_manual(values=linewidths, breaks=breaks, labels = labels) +
    guides(size = FALSE, colour=FALSE, linewidth=FALSE, linetype=g) +
    my_theme +
    theme(legend.title = element_blank(),
          legend.background = element_rect(colour = NA,
                                           fill=alpha('white', 0.4) ), # box around legend
          legend.direction = "horizontal",
          legend.spacing.y = unit(0.2,"cm"),
          legend.text.align = 0,
          legend.margin = margin(l = 10, r = 10, t = 5, b = 5,),
          legend.position = c(0.15, 0.82) ) 
p_opt_pre


p_opt_all <- plot_grid(p_opt_tmp , 
                       p_opt_pre + theme(legend.position = c(0.25, 0.9) ),
                       nrow=1, align="vh", 
                       labels = sprintf("(%s)", letters[1:2]), 
                       vjust = 1.2,
                       hjust=-1 
)
p_opt_all

f_name <- sprintf("figures/climate_2019_opt-tmp-pre_%s.png", ssp)
# ggsave(f_name)
# plot_png(p_opt_all, f_name, 13.8, 7.7 )




