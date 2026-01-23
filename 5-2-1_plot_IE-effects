
# Plot a map of countries grouped by ALl-, Dir-, and IE-effect signs
# Map of IE-effects sign combinations, does not include magnitude

library(tidyverse)
library(rnaturalearth)

ssp <- "SSP585"
model_id <- "stata_AFE_AR1-time2_xtabond2"
date <- "251216"

f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_with-without-IE_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
diff_df <- read_csv(f_name)

categorized_df %>% select(ISO_C3, ends_with("_sign"))
categorized_df %>% select(ends_with("_sign")) %>% count()

plot_data <- categorized_df %>%
    select(ISO_C3, ends_with("_sign")) %>%
    # Create a unified category column
    unite("category", diff_sign, inter_sign, nointer_sign, sep = "_", remove = FALSE)
plot_data$category %>% count()

existing_cats <- plot_data %>% 
    plyr::count("category") %>% 
    arrange(desc(freq))

# Create a mapping for only the categories that exist
# Label: (IE-effects, All-effects, Dir-effects)
category_mapping <- existing_cats %>%
    mutate(category_label = paste0(
        "Group ", row_number(), ": (",
        gsub("negative", "–", gsub("positive", "+", category)),
        ")"
    )) %>%
    mutate(category_label = gsub("_", ", ", category_label))
category_mapping

# Join the labels back to plot_data
plot_data <- plot_data %>%
    left_join(category_mapping %>% select(category, category_label), by = "category")


# Plot map of categories -------------------------------------------------------
# Load world map
world.map <- ne_countries(scale = "medium", returnclass = "sf")

# Check which countries are missing
missing_countries <- plot_data$ISO_C3[plot_data$ISO_C3 %ni% world.map$iso_a3_eh]
cat("\nCountries in data but not in map:", length(missing_countries), "\n")

# Join with world map
world.map <- world.map %>%
    left_join(plot_data, by = c("iso_a3_eh" = "ISO_C3"))
color_palette <- c("#3B4992FF", "#EE0000FF", "#008B45FF", "#FF8C00", "#8E44AD")
names(color_palette) <- category_mapping$category_label

# Create the map
# title <- "Climate Change Impact Categories by Sign Combinations\n(IE-effects, All-effects, Dir-effects)"

p_map <- ggplot(data = world.map %>%
    filter((continent != "Antarctica") & (name_en != "Greenland"))) +
    geom_sf(aes(fill = category_label), colour = "gray50", lwd = 0.2) +
    scale_fill_manual(
        values = color_palette,
        na.value = "gray60",
        breaks = category_mapping$category_label,
        name = "Category"
    ) +
    coord_sf(datum = NA) +
    # labs(title = title) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right",
        legend.key.size = unit(0.8, "cm")
    )
p_map

# Save the map
f_name <- sprintf("figures/%1$s/%1$s_country_IE-effects_sign-combination_map_%2$s_dynmc_%3$s.png", ssp, model_id, date)
f_name
# plot_png(p_map, f_name, 14, 6.26)

