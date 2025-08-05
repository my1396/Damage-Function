# Static model: Difference in CC impacts on GDP with and without interactive terms
# cross tables of scenario counts in 2100

ssp <- "SSP585"
no_inter <- read_csv(sprintf("data/%s/country_all_impact_nointer_250106.csv", ssp))
no_inter$`2100.deltaAll`
inter <- read_csv(sprintf("data/%s/country_all_impact_inter_250106.csv", ssp))
inter$`2100.deltaAll`

## calculate the difference, inter - no_inter
diff_df <- inter[, c(1, 81)] %>%
    left_join(no_inter[, c(1, 81)], by = "ISO_C3", suffix = c(".Inter", ".noInter"))
diff_df <- diff_df %>% mutate(diff = .[[2]] - .[[3]])
diff_df %>% nrow()
diff_df[, -1] %>% summary()
diff_df %>% filter(ISO_C3 == "MNG")

## histogram of diff
p <- ggplot() +
    geom_histogram(aes(x = diff_df$diff, y = after_stat(density)),
        fill = "#BDBCBC", color = "black", binwidth = 0.1, boundary = 0
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    labs(
        x = TeX("$\\delta^{IE} - \\delta^{no\\, IE}$"),
        y = "Density",
        title = ssp
    )
p

# Scenario count table ---------------------------------------------------------
# Categorize each value as positive or negative
categorized_df <- diff_df %>%
    mutate(
        diff_sign     = if_else(diff >= 0, "positive", "negative"),
        inter_sign    = if_else(`2100.deltaAll.Inter` >= 0, "positive", "negative"),
        nointer_sign  = if_else(`2100.deltaAll.noInter` >= 0, "positive", "negative")
    )
categorized_df

categorized_df %>%
    select(ends_with("_sign")) %>%
    count()

# Format as combined table for better readability
cross_tab_formatted <- format_cross_tab(categorized_df)
cat("\n", paste(cross_tab_formatted, collapse = "\n"), "\n")

# Save to file for even cleaner viewing
f_name <- sprintf("data/%1$s/%1$s_country_all_impact_diff_scenario-count_stat_250729.txt", ssp)
f_name
# writeLines(combined_output, f_name)
