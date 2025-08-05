# Dynamic model: Difference in CC impacts on GDP under alternative specifications:
#       - with and 
#       - without interactive terms
# cross tables of scenario counts in 2100

library(knitr)

model_id <- "AFE"
# choose ssp scenario: one of "SSP126", "SSP245", "SSP370", "SSP585"
ssp <- "SSP126"
# ssp <- "SSP245"
# ssp <- "SSP370"
# ssp <- "SSP585"

no_inter <- read_csv(sprintf("data/%1$s/%1$s_country_all_impact_nointer_%2$s_dynmc_250729.csv", ssp, model_id))
no_inter$`2100.deltaAll`
inter <- read_csv(sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_250729.csv", ssp, model_id))
inter$`2100.deltaAll`

# Calculate the difference, inter - no_inter, in 2100 --------------------------
diff_df <- inter[, c(1, 81)] %>% 
    left_join(no_inter[, c(1, 81)], 
              by = "ISO_C3", 
              suffix = c(".Inter", ".noInter")) %>% 
    drop_na()
diff_df <- diff_df %>% mutate(diff = .[[2]] - .[[3]])
diff_df %>% nrow()
diff_df[,-1] %>% summary()

f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_with-without-IE_%2$s_dynmc_250729.csv", ssp, model_id)
f_name
# write_csv(diff_df, f_name)

# Scenario count table ---------------------------------------------------------
# Categorize each value as positive or negative
categorized_df <- diff_df %>%
    mutate(
        diff_sign     = if_else(diff >= 0, "positive", "negative"),
        inter_sign    = if_else(`2100.deltaAll.Inter` >= 0, "positive", "negative"),
        nointer_sign  = if_else(`2100.deltaAll.noInter` >= 0, "positive", "negative")
    )
categorized_df %>% 
    select(ends_with("_sign")) %>% 
    count()

# Display clean output
# see fun "format_cross_tab" in fun_scripts.R
cross_tab_formatted <- format_cross_tab(categorized_df)
cat("\n", paste(cross_tab_formatted, collapse = "\n"), "\n")

# Save to file for even cleaner viewing
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_scenario-count_%2$s_dynmc_250729.txt", ssp, model_id)
f_name
writeLines(cross_tab_formatted, f_name)

