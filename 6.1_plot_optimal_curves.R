# Plot optimal curves with current climate in 2019
source("fun_script.R")
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
f_name <- sprintf("data/%1$s/country_all_impact_diff_with-without-IE_%1$s.csv", ssp)
diff_df <- read_csv(f_name)
diff_df <- diff_df %>% mutate(group=factor(group))

end_climate <- diff_df %>% 
    left_join(country_points, by="ISO_C3") %>% # join climate 2019
    left_join(climate_df[,c("ISO_C3", "drying")], by="ISO_C3") # join dry/wet
country_vec <- c("USA", "AUS", "SWE", "TJK", "MEX", "BRA", "GBR") # select country labels
end_climate %>% 
    add_country_name() %>% 
    filter(group==124) %>% view()
# load regression coefficients
beta_hat_IFE <- read_csv("data/IFE_result.csv")
beta_hat_IFE <- beta_hat_IFE$estimate[1:8]
beta_hat_IFE

## Optimal tmp =================================================================
colors <- c("6"="#008B45FF", "11"="#D62728FF", "15"="#FF7F0EFF", "124"="#1F77B4FF")
shapes <- c("TRUE"=4, "FALSE"=19) # drying: cross; wetting: dot
breaks <- c("IFE", "Burke")
linetypes <- setNames(c("solid", "dashed"), breaks)
linewidths <- setNames(c(0.8, 0.8), breaks)
labels <- c("IFE"="IFE", "Burke"=TeX("Burke ($T^*$=12.8 ºC)") )
# process outliers
# remove "MNG" because it has unusually low temperature (0.0851ºC)
#     ISO_C3 `2100.deltaAll.Inter` `2100.deltaAll.noInter`  diff group end.tmp end.pre drying
#     <chr>                  <dbl>                   <dbl> <dbl> <fct>   <dbl>   <dbl> <lgl> 
# 1    MNG                     6.11                    17.8 -11.7 11     0.0851   0.427 FALSE 
p_opt_tmp <- ggplot(end_climate %>% filter(!ISO_C3=="MNG"), aes(x=end.pre, y=end.tmp, color=group)) +
    geom_point(aes(shape=drying), size=2.5) +
    geom_text(data=end_climate %>% filter(ISO_C3 %in% country_vec),
              aes(label=ISO_C3),
              hjust=-0.3, vjust=-0.1, 
              size=3.5, fontface="bold",
              show.legend = FALSE
    ) + 
    geom_function(aes(linetype="IFE",  linewidth="IFE"), color="black", fun = opt_tmp, args=list(beta_hat=beta_hat_IFE)) + 
    geom_hline(aes(yintercept=12.80, linetype="Burke", linewidth="Burke"), color="black") +
    scale_color_manual(values=colors, labels=c("6"="6", "11"="10", "15"="15", "124"="124")) +
    scale_shape_manual(values=shapes, labels=c("TRUE"="Drying", "FALSE"="Wetting")) +
    scale_linetype_manual(values=linetypes, breaks=breaks, labels=labels )  + 
    scale_linewidth_manual(values=linewidths, breaks=breaks) +
    labs(x="Annual total precipitation [Meters]", 
         y="Optimal temperature [ºC]",
         color="Group",
         shape="Precip. trend",
         title=ssp) +
    guides(linewidth=FALSE, 
           color=guide_legend(order=1),
           shape=guide_legend(order=2),
           linetype=guide_legend(order=3)
           ) +
    scale_x_continuous(limits = c(0,3)) +
    my_theme +
    theme(legend.position = c(0.77, 0.12),
          axis.text = element_text(size=rel(1.5)),
          axis.title = element_text(size=rel(1.5)),
          legend.box = "horizontal",
          legend.margin = margin(t=0, b=0, l=0, r=0, unit="mm"), # margins around legend box
          legend.text.align = 0)
p_opt_tmp

## Optimal pre =================================================================
breaks <- c("IFE", "Burke")
linetypes <- setNames(c("solid", "dashed"), breaks)
colors <- setNames(c("black", "black"), breaks)
linewidths <- setNames(c(0.8, 0.8), breaks)
labels <- setNames(c("IFE", TeX("Burke ($P^*$=1.73 m)")), breaks)
g <- guide_legend("title", nrow=2) # define legend aes

p_opt_pre <- base_tmp +
    geom_function(fun = opt_pre, 
                  aes(linetype="IFE", linewidth="IFE"),
                  color="black", 
                  args = list(beta_hat=beta_hat_IFE)
    ) +
    geom_hline(aes(yintercept=1.734513, linetype="Burke", linewidth="Burke"), color="black") +
    # add country points
    # geom_point(data=end_climate %>% filter(ISO_C3 %in% country_vec), aes(x=end.tmp, y=end.pre), size=2.5) + 
    # geom_text(data=end_climate %>% filter(ISO_C3 %in% country_vec),
    #           aes(x=end.tmp, y=end.pre, label=ISO_C3),
    #           hjust=-0.3, vjust=-0.1, 
    #           size=3.5, fontface="bold",
    #           show.legend = FALSE
    #         ) + 
    labs(x="Annual Average temperature [ºC]", y="Optimal precipitation [Meters]") +
    scale_linetype_manual(values=linetypes, breaks=breaks, labels = labels) +
    scale_linewidth_manual(values=linewidths, breaks=breaks, labels = labels) +
    guides(linewidth=FALSE, linetype=g) +
    my_theme +
    theme(legend.title = element_blank(),
          axis.text = element_text(size=rel(1.5)),
          axis.title = element_text(size=rel(1.5)),
          legend.background = element_rect(colour = NA,
                                           fill=alpha('white', 0.4) ), # box around legend
          legend.direction = "horizontal",
          legend.spacing.y = unit(0.2,"cm"),
          legend.text.align = 0,
          legend.margin = margin(l = 10, r = 10, t = 5, b = 5,),
          legend.position = c(0.15, 0.82) ) 
p_opt_pre


p_opt_all <- plot_grid(p_opt_tmp, 
                       p_opt_pre + theme(legend.position = c(0.25, 0.9) ),
                       nrow=1, align="vh", 
                       labels = sprintf("(%s)", letters[1:2]), 
                       vjust = 1.2,
                       hjust=-1 
)
p_opt_all

f_name <- sprintf("figures/climate_2019_opt-tmp-pre_%s_2.png", ssp)
# ggsave(f_name)
plot_png(p_opt_all, f_name, 18.7, 7.59 )


# optimal precip with country points
base_tmp +
    geom_function(fun = opt_pre, 
                  color="black", linetype="solid",
                  args = list(beta_hat=beta_hat_IFE)
    ) +
    geom_hline(aes(yintercept=1.734513), color="black", linetype="dashed") +
    geom_point(data=end_climate, aes(x=end.tmp, y=end.pre, color=group, shape=drying), size=2.5) +
    geom_text(data=end_climate %>% filter(ISO_C3 %in% country_vec),
              aes(x=end.tmp, y=end.pre, label=ISO_C3, color=group),
              hjust=-0.3, vjust=-0.1, 
              size=3.5, fontface="bold") +
    scale_shape_manual(values=c("TRUE"=4, "FALSE"=19)) +
    scale_color_manual(values=c("6"="#008B45FF", "11"="#D62728FF", "15"="#FF7F0EFF", "124"="#1F77B4FF")) +
    scale_y_continuous(limits = c(0,3))





