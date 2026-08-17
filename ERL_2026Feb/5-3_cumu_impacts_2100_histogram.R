## Plot histograms of country-level cumulative impacts in 2100 under various model specifications:
##    Case 1: with interactive terms
##    Case 2: without interactive terms
##    Case 3: difference between the two cases
##    For each case, the histogram is grouped by SSP scenarios: SSP126, SSP245, SSP370, SSP585.
##    You can do comparison across SSPs based on this file.

plot_cumulative_impacts <- function(
    impact_data,  
    x_var, 
    x_label,
    title
    ) {
    # Create histogram for cumulative impacts in 2100
    # Grouped by SSP
    # @param impact_data: Data frame with cumulative impacts
    #   contains 2 columns: x_var and `SSP`
    # @param x_var: Variable to plot histogram on
    # @param x_label: Label for the x-axis
    # @param title: Title for the plot
    p <- ggplot(impact_data, aes(x = .data[[x_var]])) +
        geom_histogram(
            aes(y = after_stat(density)),
            fill = "#BDBCBC", color = "black",
            binwidth = 0.1, boundary = 0
        ) +
        scale_x_continuous(limits = c(-1, 1)) +
        facet_wrap(~SSP, ncol = 2) +
        labs(
            title = title,
            x = x_label,
            y = "Density"
        ) +
        theme(plot.title = element_text(hjust = 0.5))
    return(p)
}


model_id <- "AFE"
# ============================================================================ #
# Cumulative impacts with interactive terms ------------------------------------
ssps <- c("SSP126", "SSP245", "SSP370", "SSP585")
files <- sprintf("data/%1$s/%1$s_country_all_impact_inter_%2$s_dynmc_250729.csv", ssps, model_id)

names(files) <- ssps

impact_2100_long <- imap_dfr(
    files,
    ~ read_csv(.x, show_col_types = FALSE) %>%
        select(ISO_C3, `2100.deltaAll`) %>%
        mutate(SSP = .y)) 

# Plot histogram for each SSP
plot_title <- "Climate Change Impact Until 2100 (Interactive Terms)"
xaxis_label <- TeX("$\\delta^{IE}$")
p <- plot_cumulative_impacts(
    impact_data = impact_2100_long,
    x_var = "2100.deltaAll",
    x_label = xaxis_label,
    title = plot_title)
p

f_name <- sprintf("figures/cumulative_impacts_histogram_with-IE_%s_dynmc.png", model_id)
f_name
# ggsave(f_name)
plot_png(p, f_name, 9.51, 6.72)

impact_2100_wide <- impact_2100_long %>%
    pivot_wider(
        names_from  = SSP,
        values_from = `2100.deltaAll`
    ) %>%
    arrange(ISO_C3)
impact_2100_wide

f_name <- "data/AFE_dynamic_cumu_effect_2100_with-IE.csv"
f_name
write_csv(impact_2100_wide, f_name)

# ============================================================================ #
# Cumulative impacts without interactive terms ---------------------------------

files <- sprintf("data/%1$s/%1$s_country_all_impact_nointer_%2$s_dynmc_250729.csv", ssps, model_id)
names(files) <- ssps
files

impact_2100_long <- imap_dfr(
    files,
    ~ read_csv(.x, show_col_types = FALSE) %>%
        select(ISO_C3, `2100.deltaAll`) %>%
        mutate(SSP = .y))

# Plot histogram for each SSP 
plot_title <- "Climate Change Impact Until 2100 (No Interactive Terms)"
xaxis_label <- TeX("$\\delta^{no\\, IE}$")
p <- plot_cumulative_impacts(
    impact_data = impact_2100_long,
    x_var = "2100.deltaAll",
    x_label = xaxis_label,
    title = plot_title)
p
f_name <- sprintf("figures/cumulative_impacts_histogram_without-IE_%s_dynmc.png", model_id)
f_name
plot_png(p, f_name, 9.51, 6.72)

impact_2100_wide <- impact_2100_long %>%
    pivot_wider(
        names_from  = SSP,
        values_from = `2100.deltaAll`
    ) %>%
    arrange(ISO_C3)
impact_2100_wide

f_name <- "data/AFE_dynamic_cumu_effect_2100_without-IE.csv"
f_name
write_csv(impact_2100_wide, f_name)

# ============================================================================ #
# Difference with and without interactive terms --------------------------------

files <- sprintf("data/%1$s/%1$s_country_all_impact_diff_with-without-IE_%2$s_dynmc_250729.csv", ssps, model_id)
names(files) <- ssps
files

impact_2100_long <- imap_dfr(
    files,
    ~ read_csv(.x, show_col_types = FALSE) %>%
        select(ISO_C3, `diff`) %>%
        mutate(SSP = .y))


# Plot histogram for each SSP
plot_title <- "Climate Change Impact Attributable to Interactive Terms"
xaxis_label <- TeX("$\\delta^{IE} - \\delta^{no\\, IE}$")
p <- plot_cumulative_impacts(
    impact_data = impact_2100_long,
    x_var = "diff",
    x_label = xaxis_label,
    title = plot_title)
p
f_name <- sprintf("figures/cumulative_impacts_histogram_diff_with-without-IE_%s_dynmc.png", model_id)
f_name
plot_png(p, f_name, 9.51, 6.72)

impact_2100_wide <- impact_2100_long %>%
    pivot_wider(
        names_from  = SSP,
        values_from = `diff`
    ) %>%
    arrange(ISO_C3)
impact_2100_wide

f_name <- sprintf("data/%s_dynmc_cumu_effect_2100_diff_with-without-IE.csv", model_id)
f_name
write_csv(impact_2100_wide, f_name)



