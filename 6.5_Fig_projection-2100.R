# Fig. 3: projections until 2100
# (a) Global pathways, disentangled effects
# (b) Histogram SSP126 and 585
# (c) Map of country-level climate change impacts in 2100, SSP585
# (d) Map of country-level interactive effects, SSP585.

# global pathways ==============================================================
## disentangle into 3 effects
ssp1 <- read_csv(paste0("data/", sprintf("%s/global_impact_projections.csv", "SSP126")))
ssp5 <- read_csv(paste0("data/", sprintf("%s/global_impact_projections.csv", "SSP585")))
plot_data <- ssp1 %>% 
    left_join(ssp5, by="time", suffix=c("_126", "_585"))
plot_data <- plot_data %>% mutate(year=year(time))
long_format <- plot_data %>% 
    select(-time) %>% 
    gather(key="key", value="value", -year) %>% 
    mutate(series = str_sub(key, start=1, end=-5),
           ssp = str_sub(key, start=-3))
long_format <- long_format %>% 
    mutate(series = factor(series, 
                           levels=c("tmp_impact", "all_impact_nointer", "all_impact_inter")) )
long_format

labels <- c("tmp_impact"="tmp", 
            "all_impact_nointer"="tmp+pre, w/o interactive", 
            "all_impact_inter"="tmp+pre, w/ interactive")
colors <-  c("tmp_impact"="blue", 
             "all_impact_nointer"="black", 
             "all_impact_inter"="red")
shapes <- c("126"=16, "585"=17)

p_pathway <- long_format %>% 
    ggplot(aes(year, value, col=series, shape=ssp)) +
    geom_line() +
    scale_color_manual(values = colors, labels = labels) +
    geom_point() +
    scale_shape_manual(values = shapes) +
    labs(y="Change in GDPpc", shape="SSP", color="Disentangled effects") +
    guides(
        shape = guide_legend(order = 1),
        colour = guide_legend(order = 2),
    ) +
    theme(axis.title.x = element_blank())

p_pathway

f_name <- "figures/persistent_impacts_pathway_oneModel.png"
f_name
# plot_png(p_pathway, f_name, 11.3, 7.8)


# histogram: Country-level impacts in 2100 =====================================
ssp1 <- read_csv("/Users/menghan/Documents/GDP/Shared folder/data/SSP126/country_all_impact_inter_250106.csv")
ssp5 <- read_csv("/Users/menghan/Documents/GDP/Shared folder/data/SSP585/country_all_impact_inter_250106.csv")
colnames(ssp1)[81] <- "V80"
colnames(ssp5)[81] <- "V80"
ssp1[,c(1,81)] %>% arrange(desc(V80) )
ssp5[,c(1,81)] %>% arrange(desc(V80) )

plot_data <- ssp1[,c("ISO_C3", "V80")] %>% 
    left_join(ssp5[,c("ISO_C3", "V80")], by="ISO_C3", suffix=c("_126", "_585"))
long_format <- plot_data %>% gather("key", "value", -ISO_C3)
long_format
legend_low <- -1
legend_high <- 1
step <- 0.1
fills <- c("V80_126"="#F8766D", "V80_585"="#00BFC4")
labels <-  c("V80_126"="SSP126", "V80_585"="SSP585")
p_hist <- ggplot(data=long_format, aes(value, fill=key) ) + 
    geom_histogram(aes(y=after_stat(density)), position='identity', binwidth = step, boundary=-1, alpha=0.5) +
    scale_x_continuous(breaks=seq(-1,1,0.2),
                       limits=c(legend_low, legend_high)) +
    scale_fill_manual(values=fills, labels = labels) +
    labs(x="Climate change impact until 2100") +
    theme(axis.title.y=element_blank(),
          legend.position = c(0.8, 0.8),
          legend.margin = margin(t=0, b=0, unit="mm"),
          legend.title = element_blank(),)
p_hist
f_name <- "figures/country_pct_impact_interactive_histogram.png"
# ggsave(f_name)
plot_png(p_hist, f_name, 10.1, 6.65)


# Map: Country-level impacts in 2100 ===========================================
ssp <- "SSP585"
f_name <- sprintf("data/%s/country_all_impact_inter_250106.csv", ssp)
f_name
pct_impact_country <- read_csv(f_name)
colnames(pct_impact_country)
legend_low <- -1
legend_high <- 1
plot_data <- pct_impact_country %>% 
    select(1, 81) %>% 
    mutate(V80_c = squish(`2100.deltaAll`, range = c(legend_low+c, legend_high) ) ) # set out of boundary values to limits

library(rnaturalearth)
world.map <- ne_countries(scale = "medium", returnclass = "sf")
sum(plot_data$ISO_C3 %ni% world.map$iso_a3_eh)
world.map <- world.map %>% 
    left_join(plot_data, by=c("iso_a3_eh"="ISO_C3"))
# world.map
cold <- colorRampPalette(c("#000033","#00007F", "#7AACED", "white"))(7) # from -7
warm <- c("#FFD4D4", "#FFB2B2", "#FF9090", "#FF6D6D", "#FF4B4B", "#FF2A2A")
myColors <- c(cold, warm)

title <- sprintf("Climate change impacts until 2100, %s", ssp)
unit <- "%"
step <- 0.2

p_map <- ggplot(data = world.map %>% filter(continent!="Antarctica") ) +
    geom_sf(aes(fill=V80_c), colour='gray50', lwd=0.2 ) +
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
    labs(title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.1),
          legend.title = element_text(hjust=0.85),
          legend.position = c(0.1,0.3))
p_map

# x11()
f_name <- paste0(fig_dir, sprintf("%1$s_projection/%1$s_climate_impacts_map_%2$s_cumulative.png", ssp, "all"))
f_name
plot_png(p_map, f_name, width=9.89, height=3.93)
# ggsave(f_name)


# Map: Interactive effects =====================================================
plot_data <- end_climate[,c("ISO_C3", "diff")]
plot_data$diff %>% quantile(c(.025, .975))
legend_low <- -1
legend_high <- 1
step <- 0.1
plot_data <- plot_data %>% mutate(diff_c = squish(diff, range = c(legend_low+c, legend_high) ))
library(rnaturalearth)
world.map <- ne_countries(scale = "medium", returnclass = "sf")
world.map <- world.map %>% left_join(plot_data, by=c("iso_a3_eh"="ISO_C3"))

p_map_diff <- ggplot(data = world.map %>% filter(continent!="Antarctica")) +
    geom_sf(aes(fill=diff_c), colour='gray50', lwd=0.3 ) +
    scale_fill_stepsn(limits = c(legend_low, legend_high), 
                      breaks = c(legend_low-c, seq(legend_low+step, legend_high-step, step), legend_high+c ),
                      # labels = function(x) {sprintf("%.1f", x*100)},
                      show.limits = TRUE, 
                      right = FALSE, # include right bin, (low, up]
                      colours = myColors,
                      name = TeX(unit)
    ) +
    labs(title = "Differences with and without interactive effects") +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.1),
          # legend.title = element_text(hjust=0.85),
          legend.position = "none")
p_map_diff

f_name <- "figures/diff_map.png"
# ggsave(f_name)
# plot_png(p_map, f_name, 11.2, 5.21)


# Combine in one panel =========================================================
p_all <- plot_grid(plot_grid(p_pathway + theme(plot.margin = margin(0,0,15,15),
                                   legend.text = element_text(size=rel(1.2)),
                                   legend.title = element_text(size=rel(1.2)),
                                   axis.text = element_text(size=rel(1.2)),
                                   axis.title = element_text(size=rel(1.2))
                                   ), NULL, nrow=1, rel_widths = c(1,0.15) ),
                   plot_grid(p_hist + theme(plot.margin = margin(0,0,15,25), 
                                   legend.position = "right",
                                   legend.text = element_text(size=rel(1.2)),
                                   axis.text = element_text(size=rel(1.2)),
                                   axis.title = element_text(size=rel(1.2)) 
                                   ), NULL, nrow=1, rel_widths = c(1,0.3) ),
                   p_map + theme(plot.margin = margin(5,0,0,-50),
                                 title = element_text(size=rel(1.2)),
                                 legend.text = element_text(size=rel(1.1)),
                                 legend.key.height=unit(0.5,"inches"),
                                 legend.position = c(0.1,0.4)
                                 ),
                   p_map_diff + theme(plot.margin = margin(5,0,0,-50),
                                      title = element_text(size=rel(1.2))
                                      ),
                   nrow=2,
                   rel_heights = c(1, 1.2),
                   labels=sprintf("(%s)", letters[1:4]), 
                   label_size=16)
p_all

plot_grid(plot_grid(p_pathway + theme(plot.margin = margin(5,0,25,25)), 
                    p_hist + theme(plot.margin = margin(5,10,15,35), legend.position = c(0.8, 0.85)), 
                    nrow = 1, 
                    rel_widths = c(1.2,1), labels = c("(a)", "(b)")
                    ),
          plot_grid(p_map, p_map_diff, nrow = 2, labels = c("(c)", "(d)"), hjust=-8), 
          rel_heights = c(1,2.5),
          nrow=2)

f_name <- "figures/Fig3.png"
f_name
ggsave(f_name)
# plot_png(p_all, f_name, 26.2, 10.4)
#  













