# Get country bounding box (longmin, longmax, latmin, latmax) for each country

library(tidyverse)
data <- read_csv("/Users/menghan/Documents/GDP/data/MergeDataset/data/cntry_ann_climate_econ_bbox_1961to2019.csv")

country_box <- data %>%
    distinct(ISO_N3, .keep_all = TRUE) %>% 
    select(ISO_N3, ISO_C3, cntry.name, longmin, longmax, latmin, latmax)
f_name <- "/Users/menghan/Documents/GDP/Shared folder/data/cntry_box.csv"
# write_csv(country_box, f_name)

f_name <- "/Users/menghan/Documents/GDP/Shared folder/data/GDP_reg_panelData.csv"
data <- read_csv(f_name)

# Check if all countries in data are in country_box
"%ni%" <- Negate("%in%")
sum(unique(data$iso) %ni% country_box$ISO_C3)

data <- data %>%
    left_join(country_box %>% select(-ISO_N3), by = c("iso" = "ISO_C3")) %>%
    select(iso, cntry.name, longmin, longmax, latmin, latmax, everything())
f_name <- "/Users/menghan/Documents/GDP/Shared folder/data/GDP_reg_panelData_V2.csv"
write_csv(data, f_name)
