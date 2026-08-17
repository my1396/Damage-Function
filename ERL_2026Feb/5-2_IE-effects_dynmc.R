# Dynamic model: Difference in CC impacts on GDP under alternative specifications:
#       - with and 
#       - without interactive terms
# Cross tables of IE-effects in 2100

library(knitr)
library(tidyverse)
library(data.table)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("fun_script.R")

country_name <- read_csv("data/region_category.csv")

model_id <- "AFE_AR1"
model_id <- "AFE_AR1-time1"
model_id <- "AFE_AR1-time2"

model_id <- "AFE_ARDL"
model_id <- "AFE_ARDL-time1"
model_id <- "AFE_ARDL-time2"

model_id <- "stata_AFE_AR1-time2" # xtabond
model_id <- "stata_AFE_AR1-time2_xtabond2" # xtabond2

# choose ssp scenario: one of "SSP126", "SSP245", "SSP370", "SSP585"
# ssp <- "SSP126"
# ssp <- "SSP245"
# ssp <- "SSP370"
ssp <- "SSP585"
# date <- "251208"
date <- "251216"

f_name <- sprintf("data/%1$s/%1$s_country_all_impact_nointer_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
no_inter <- read_csv(f_name)
no_inter$`2100.deltaAll`
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
inter <- read_csv(f_name)
inter$`2100.deltaAll`

# Calculate the difference, inter - no_inter, in 2100 --------------------------
diff_df <- inter[, c(1, 81)] %>% 
    left_join(no_inter[, c(1, 81)], 
              by = "ISO_C3", 
              suffix = c(".Inter", ".noInter")) %>% 
    drop_na()
diff_df <- diff_df %>% mutate(diff = .[[2]] - .[[3]])
diff_df %>% nrow()
diff_df %>% head()
diff_df[, -1] %>% summary()

# Save to file
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_with-without-IE_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
f_name
# write_csv(diff_df, f_name)


# Summary statistics of IE-effects ---------------------------------------------
diff_df <- read_csv(f_name)
diff_df[, -1] %>% summary()


# Scenario count table ---------------------------------------------------------
# Categorize each value as positive or negative
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_with-without-IE_%2$s_dynmc_%3$s.csv", ssp, model_id, date)
diff_df <- read_csv(f_name)

categorized_df <- diff_df %>%
    mutate(
        diff_sign     = if_else(diff >= 0, "positive", "negative"),
        inter_sign    = if_else(`2100.deltaAll.Inter` >= 0, "positive", "negative"),
        nointer_sign  = if_else(`2100.deltaAll.noInter` >= 0, "positive", "negative")
    )
categorized_df <- categorized_df %>% 
    left_join(
        country_name[c("ISO_C3", "cntry.name", "region")],
        by = "ISO_C3"
    )

# frequency table of all possible sign combinations
categorized_df %>% 
    select(ends_with("_sign")) %>% 
    count()

# Investigate the group with positive diff and negative inter & nointer 
# join region info
categorized_df %>%
    filter(
        diff_sign == "positive" &
        inter_sign == "negative" &
        nointer_sign == "negative"
    ) %>% 
    arrange(region) %>%
    data.table() %>% 
    print(topn = 5)

# tabulate region composition of the group
categorized_df %>% 
    filter(
        diff_sign == "positive" & 
        inter_sign == "negative" & 
        nointer_sign == "negative") %>% 
    group_by(region) %>%
    summarise(n = n())

# Display clean output
# see fun "format_cross_tab" in fun_scripts.R
cross_tab_formatted <- format_cross_tab(categorized_df)
cat("\n", paste(cross_tab_formatted, collapse = "\n"), "\n")

# For percentages
cross_tab_formatted_pct <- format_cross_tab(categorized_df, show_pct = TRUE)
cat("\n", paste(cross_tab_formatted_pct, collapse = "\n"), "\n")


# Save to file for even cleaner viewing
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_scenario-count_%2$s_dynmc_%3$s.txt", ssp, model_id, date)
f_name
# writeLines(cross_tab_formatted, f_name)

