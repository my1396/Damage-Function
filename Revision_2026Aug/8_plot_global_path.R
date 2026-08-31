## ========================================================================== ##
## Global GDP damage pathway 2021-2100 under L = 0, 1, 2, for AFE and IFE,
## for BOTH regressor sets (between-model comparison):
##   Interactive (M = 8): T, T2, P, P2 and the four T x P interactions
##   Direct      (M = 4): T, T2, P, P2 only
##
## delta_t = (population-weighted cumulated growth WITH climate change)
##           / (same WITHOUT climate change) - 1
## aggregated exactly as in 7-2_bootstrap_path_persistent.R.
##
## eta_{i,t} = sum_{j=0}^{L} beta_{.,j}' (x_{i,t-j} - x_{i,0}), lags before the
## base period clamped at x_0.
##
## Out: figures/fig_global_damage_path_interactive.png
##      figures/fig_global_damage_path_direct.png
##      figures/fig_global_damage_path_comparison.png
##      figures/fig_global_damage_fan.png
##      output/global_damage_path.csv   (both specs, long)
##      output/global_damage_between_diff.txt  (M8 - M4, by estimator and L)
## ========================================================================== ##

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

SSP   <- "SSP585"
YEARS <- PROJ_YEARS
LAGS  <- 0:2

## INK / INK_SOFT / INK_MUTED and my_theme come from _fig_theme.R
COL     <- c(AFE = "#2a78d6", IFE = "#eb6834")
SURFACE <- "white"

inp  <- load_projection_inputs(SSP, root_dir)
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"), show_col_types = FALSE)

paths <- map_dfr(c("Interactive", "Direct"), function(sp) {
    regs <- regs_for(sp)
    expand_grid(est = c("AFE", "IFE"), L = LAGS) %>%
        pmap_dfr(function(est, L) {
            B   <- beta_matrix(lagc, sp, est, L)
            eta <- eta_matrix(inp$cl, B, regs)
            tibble(year = YEARS, spec = sp, estimator = est, L = L,
                   delta = global_delta(eta, inp$G, inp$POP, inp$gp0))
        })
}) %>%
    mutate(panel = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)))

write_csv(paths, file.path(out_dir, "global_damage_path.csv"))

## ========================================================================== ##
## 1. Summary of btw-model IE-effects ------------------------------------------
## ========================================================================== ##

f_name <- file.path(out_dir, "global_damage_path.csv")
paths  <- read_csv(f_name, show_col_types = FALSE)

## Interactive (M = 8) minus Direct (M = 4), by estimator and L. This is the
## BETWEEN-model interactive contribution: the difference between the two
## SEPARATELY ESTIMATED models, not the within-model gap of 9_ (which zeroes
## the interaction coefficients inside the M = 8 fit). It is the point estimate
## that 8-1_IE_between_model_uncertainty.R attaches a bootstrap band to, so
## computing it here makes the number available without that script's draws.
## A POSITIVE value means the interactive model projects milder damages.
ie_between <- paths %>%
    select(estimator, L, year, spec, delta) %>%
    pivot_wider(names_from = spec, values_from = delta) %>%
    mutate(IE_between = Interactive - Direct) %>%
    arrange(estimator, L, year)

## the three tables, built once so the same objects go to disk and to console
tab_2100 <- ie_between %>% filter(year == 2100) %>%
    transmute(estimator, L,
              `M8 (%)`  = round(100 * Interactive, 2),
              `M4 (%)`  = round(100 * Direct, 2),
              `IE (pp)` = round(100 * IE_between, 2))

tab_horizon <- ie_between %>% filter(year %in% c(2050, 2075, 2100)) %>%
    mutate(IE = round(100 * IE_between, 2)) %>%
    select(estimator, L, year, IE) %>%
    pivot_wider(names_from = year, values_from = IE, names_prefix = "y")

## where the two specifications diverge most, and when
tab_peak <- ie_between %>% group_by(estimator, L) %>%
    summarise(`max |IE| (pp)` = round(100 * max(abs(IE_between)), 2),
              `in year`       = year[which.max(abs(IE_between))],
              .groups = "drop")

sink(file.path(out_dir, "global_damage_summary.txt"))
cat("BETWEEN-MODEL INTERACTIVE CONTRIBUTION: POINT ESTIMATES\n")
cat("======================================================\n\n")
cat("IE = delta^M8 - delta^M4, the difference between the two SEPARATELY\n")
cat("ESTIMATED models: M = 8 adds the four T x P interactions to the M = 4\n")
cat("direct terms. Contrast 9_decompose_IE_effects.R, which measures the\n")
cat("WITHIN-model gap by zeroing the interaction coefficients inside the M = 8\n")
cat("fit -- a counterfactual built from coefficients never estimated without\n")
cat("the interactions present. The two differ sharply at L = 2.\n")
cat("A POSITIVE value means the interactive model projects milder damages.\n\n")
cat("Scenario:", SSP, "| BHM aggregation | L = 0, 1, 2.\n")
cat("Levels are % of counterfactual GDP; differences are percentage points.\n")
cat("Point estimates only. 8-1_IE_between_model_uncertainty.R attaches the\n")
cat("bootstrap bands and reproduces the IE column below exactly.\n\n")

cat("--- 2100 levels and the between-model difference ---\n\n")
print(as.data.frame(tab_2100), row.names = FALSE)

cat("\n\n--- between-model difference over the horizon (pp) ---\n\n")
print(as.data.frame(tab_horizon), row.names = FALSE)

cat("\n\n--- largest divergence along the path ---\n\n")
cat("The gap compounds, so it is widest at or near the end of the horizon;\n")
cat("2100 is therefore a fair summary rather than an arbitrary slice.\n\n")
print(as.data.frame(tab_peak), row.names = FALSE)
sink()

cat("\nWrote output/global_damage_between_diff.txt\n")
cat("\n=== 2100 levels and the between-model difference ===\n")
print(as.data.frame(tab_2100), row.names = FALSE)
cat("\n=== between-model difference over the horizon (pp) ===\n")
print(as.data.frame(tab_horizon), row.names = FALSE)
cat("\n=== largest divergence along the path ===\n")
print(as.data.frame(tab_peak), row.names = FALSE)

## ========================================================================== ##
## 2. Plot, one figure per specification ---------------------------------------
## ========================================================================== ##
make_plot <- function(sp) {
    d <- paths %>% filter(spec == sp)
    ends <- d %>% group_by(estimator, panel) %>% filter(year == max(year)) %>% ungroup()
    M <- length(regs_for(sp))
    sub <- sprintf("%s, %s specification (M = %d regressors), BHM aggregation (compound then average); L = 0, 1, 2",
                   SSP, tolower(sp), M)

    ggplot(d, aes(year, delta, colour = estimator)) +
        geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
        geom_line(linewidth = 0.7) +
        geom_point(data = ends, aes(fill = estimator), size = 2.2, stroke = 0.9,
                   shape = 21, colour = SURFACE) +
        geom_text(data = ends,
                  aes(label = sprintf("%s  %.0f%%", estimator, 100 * delta)),
                  hjust = 1, nudge_x = -2.5, nudge_y = 0.035, size = 3.0,
                  fontface = "bold", show.legend = FALSE) +
        facet_wrap(~panel) +
        scale_colour_manual(values = COL) +
        scale_fill_manual(values = COL) +
        scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                           expand = expansion(mult = c(0.05, 0.12))) +
        labs(title = "Global GDP impact of climate change, 2021-2100",
             subtitle = sub,
             x = NULL,
             y = expression(delta[t]~"= GDP(CC) / GDP(no CC) - 1"),
             caption = paste0("L = 0 is the contemporaneous-only specification; ",
                              "L = 1, 2 add distributed lags in climate.\n",
                              "Point estimates only -- bootstrap 90% intervals are far wider ",
                              "than the spread between these lines.\n",
                              "Beyond L = 2 the lag polynomial is not identified and paths ",
                              "are not shown.")) +
        my_theme +
        theme(
            plot.background  = element_rect(fill = SURFACE, colour = NA),
            panel.background = element_rect(fill = SURFACE, colour = NA),
            strip.text       = element_text(margin = margin(b = 4)),
            plot.subtitle    = element_text(margin = margin(b = 8)),
            plot.caption     = element_text(size = 8),
            legend.title     = element_blank(),
            legend.text      = element_text(colour = INK_SOFT),
            legend.key.width = unit(18, "pt"))
}

for (sp in c("Interactive", "Direct")) {
    ggsave(file.path(fig_dir, sprintf("fig_global_damage_path_%s.png", tolower(sp))),
           make_plot(sp), width = 8.6, height = 4.4, dpi = 200, bg = SURFACE)
}

cat("Wrote figures/fig_global_damage_path_{interactive,direct}.png\n",
    "and output/global_damage_path.csv\n\n")
print(as.data.frame(
    paths %>% filter(year %in% c(2050, 2075, 2100)) %>%
        mutate(delta = round(100 * delta, 1)) %>%
        select(spec, estimator, L, year, delta) %>%
        pivot_wider(names_from = year, values_from = delta, names_prefix = "y") %>%
        arrange(spec, estimator, L)),
    row.names = FALSE)


## ========================================================================== ##
## 3. Path, two specifications in one panel ------------------------------------
## ========================================================================== ##
gp <- readr::read_csv(
    file.path(out_dir, "global_damage_path.csv"),
    show_col_types = FALSE
)  %>% 
    mutate(
        panel = factor(paste0("L = ", L), levels = paste0("L = ", 0:2)),
        spec = factor(spec, levels = c("Interactive", "Direct"))
    )

p_gp <- ggplot(gp, aes(year, delta, colour = estimator, linetype = spec)) +
    geom_hline(yintercept = 0, colour = "#8a8985", linewidth = 0.3) +
    geom_line(linewidth = 0.75) +
    facet_wrap(~panel) +
    scale_colour_manual(values = c(AFE = "#2a78d6", IFE = "#eb6834")) +
    scale_linetype_manual(
        values = c(Interactive = "solid", Direct = "22"),
        labels = c(Interactive = "M = 8", Direct = "M = 4")
        ) +
    scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        x = NULL, y = expression(delta[t] ~ "= GDP(CC) / GDP(no CC) - 1"),
        colour = "Estimator", linetype = "Specification",
        title = "Estimator and specification comparison, SSP585",
        subtitle = "M = 8 (full climate variables), M = 4 (direct terms only)"
    ) +
    my_theme +
    theme(
        plot.background  = element_rect(fill = SURFACE, colour = NA),
        panel.background = element_rect(fill = SURFACE, colour = NA)
    )
ggsave(file.path(fig_dir, "fig_global_damage_path_comparison.png"), p_gp,
       width = 8.8, height = 4, dpi = 200, bg = SURFACE)

## ========================================================================== ##
## 4. Fan chart: point estimate + bootstrap density bands ----------------------
##    Requires 7_bootstrap_lagged_projection.R (path quantiles).
## ========================================================================== ##
fq_file <- file.path(out_dir, "bootstrap_lagged_path_quantiles.csv")
if (file.exists(fq_file)) {
    fq <- read_csv(fq_file, show_col_types = FALSE) %>%
        mutate(panel = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)))

    ## nested bands, darkest in the middle: 50%, 80%, 95%
    bands <- tribble(
        ~lo,    ~hi,    ~alpha, ~lab,
        "q25",  "q75",  0.34,   "50%",
        "q10",  "q90",  0.20,   "80%",
        "q025", "q975", 0.11,   "95%")

    pf <- ggplot(fq, aes(year))
    for (i in seq_len(nrow(bands))) {
        pf <- pf + geom_ribbon(
            aes(ymin = .data[[bands$lo[i]]], ymax = .data[[bands$hi[i]]],
                fill = estimator),
            alpha = bands$alpha[i], colour = NA)
    }
    pf <- pf +
        geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
        geom_line(aes(y = q50, colour = estimator), linetype = "22", linewidth = 0.45) +
        geom_line(aes(y = point, colour = estimator), linewidth = 0.85) +
        facet_grid(estimator ~ panel) +
        scale_colour_manual(values = COL, guide = "none") +
        scale_fill_manual(values = COL, guide = "none") +
        scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        coord_cartesian(ylim = c(-1, 1)) +
        labs(title = "Global GDP impact with bootstrap uncertainty",
             subtitle = paste0(SSP, ", interactive specification, 1,000 draws. ",
                               "Solid = point estimate, dashed = bootstrap median.\n",
                               "Shading: 50% / 80% / 95% intervals, darkest to lightest."),
             x = NULL, y = expression(delta[t]~"= GDP(CC) / GDP(no CC) - 1")
             ) +
        my_theme +
        theme(plot.background = element_rect(fill = SURFACE, colour = NA),
              panel.background = element_rect(fill = SURFACE, colour = NA),
              strip.text = element_text(margin = margin(b = 4, t = 2)),
              plot.title = element_text(size = rel(1.5)),
              plot.subtitle = element_text(size = rel(1.2), margin = margin(b = 4)),
              plot.caption = element_text(size = 8))
    pf
    ggsave(file.path(fig_dir, "fig_global_damage_fan.png"), pf,
           width = 8.8, height = 5.4, dpi = 200, bg = SURFACE)
    cat("Wrote figures/fig_global_damage_fan.png\n")
} else {
    cat("skip fan chart: run 7_bootstrap_lagged_projection.R first\n")
}
