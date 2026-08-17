## =============================================================================
## Lag-length diagnostics: does the lag structure converge?
##
## Fig 1  cumulative marginal effect  dy/dT  against lag length L  (y = growth of GDP pc)
## Fig 2  projected 2100 GDP impact relative to L=0, against L
##
## If the curves flatten, the distributed lag has been fully captured. If they
## are still moving at L=5, the dynamics are not pinned down by the data.
##
## In : output/lagged_climate_marginal_effects.csv   (3_lagged_climate.R)
##      output/projection_lagged_global.csv          (5_projection_lagged.R)
## Out: figures/fig_lag_marginal_effect.png
##      figures/fig_lag_projection_ratio.png
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")   # PNGs; set to out_dir to keep them together
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

## ---- palette: categorical slots 1 & 2, validated for CVD + contrast --------
COL <- c(AFE = "#2a78d6", IFE = "#eb6834")
INK        <- "#0b0b0b"
INK_SOFT   <- "#52514e"
INK_MUTED  <- "#8a8985"
SURFACE    <- "#fcfcfb"

theme_lag <- function() {
    theme_minimal(base_size = 11) +
        theme(
            plot.background   = element_rect(fill = SURFACE, colour = NA),
            panel.background  = element_rect(fill = SURFACE, colour = NA),
            panel.grid.minor  = element_blank(),
            panel.grid.major  = element_line(colour = "#e6e5e1", linewidth = 0.3),
            panel.spacing     = unit(14, "pt"),
            axis.title        = element_text(colour = INK_SOFT, size = 10),
            axis.text         = element_text(colour = INK_SOFT, size = 9),
            strip.text        = element_text(colour = INK, size = 10, face = "bold",
                                             margin = margin(b = 4)),
            plot.title        = element_text(colour = INK, size = 13, face = "bold"),
            plot.subtitle     = element_text(colour = INK_SOFT, size = 10,
                                             margin = margin(b = 8)),
            plot.caption      = element_text(colour = INK_MUTED, size = 8, hjust = 0,
                                             margin = margin(t = 10)),
            legend.position   = "top",
            legend.title       = element_blank(),
            legend.text       = element_text(colour = INK_SOFT, size = 9),
            legend.key.width  = unit(18, "pt")
        )
}

## =============================================================================
## Fig 1 -- cumulative marginal effect vs L
## =============================================================================
me <- read_csv(file.path(out_dir, "lagged_climate_marginal_effects.csv"),
               show_col_types = FALSE) %>%
    filter(T_pct %in% c("50%", "90%")) %>%
    mutate(lo = dg_dT - 1.96 * se,
           hi = dg_dT + 1.96 * se,
           spec  = factor(spec, levels = c("Direct", "Interactive")),
           panel = factor(ifelse(T_pct == "50%",
                                 "Median T (22.3 C)",
                                 "Hot, 90th pct (27.2 C)"),
                          levels = c("Median T (22.3 C)",
                                     "Hot, 90th pct (27.2 C)")))

lab1 <- me %>% group_by(spec, panel, estimator) %>% filter(L == max(L)) %>% ungroup()

p1 <- ggplot(me, aes(L, dg_dT, colour = estimator, fill = estimator)) +
    geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.13, colour = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.2, stroke = 0.9, shape = 21, colour = SURFACE) +
    geom_text(data = lab1, aes(label = estimator), hjust = -0.25, size = 3.1,
              fontface = "bold", show.legend = FALSE) +
    facet_grid(panel ~ spec) +
    scale_colour_manual(values = COL) +
    scale_fill_manual(values = COL) +
    scale_x_continuous(breaks = 0:5, expand = expansion(mult = c(0.04, 0.13))) +
    labs(title = "Cumulative effect of temperature on GDP per-capita growth (y), by lag length",
         subtitle = "Sum of distributed-lag coefficients; shaded band is the 95% interval",
         x = "Lag length L (years)",
         y = expression(partialdiff*y/partialdiff*T),
         caption = paste0("L = 0 is the contemporaneous-only specification used in the paper. ",
                          "Evaluated at median precipitation (1.085 m/yr).\n",
                          "A flat curve means the lag structure is fully captured; ",
                          "a curve still moving at L = 5 means it is not.")) +
    theme_lag()

ggsave(file.path(fig_dir, "fig_lag_marginal_effect.png"), p1,
       width = 8.2, height = 6.0, dpi = 200, bg = SURFACE)

## =============================================================================
## Fig 2 -- projection ratio vs L
## =============================================================================
gl <- read_csv(file.path(out_dir, "projection_lagged_global.csv"),
               show_col_types = FALSE) %>%
    filter(ssp == "SSP585") %>%
    mutate(spec = factor(spec, levels = c("Direct", "Interactive")))

lab2 <- gl %>% group_by(spec, estimator) %>% filter(L == max(L)) %>% ungroup()

p2 <- ggplot(gl, aes(L, ratio_bhm_vs_L0, colour = estimator)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
             fill = "#e34948", alpha = 0.07) +
    geom_hline(yintercept = 1, colour = INK_MUTED, linewidth = 0.3, linetype = "22") +
    geom_hline(yintercept = 0, colour = "#e34948", linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    geom_point(aes(fill = estimator), size = 2.2, stroke = 0.9,
               shape = 21, colour = SURFACE) +
    geom_text(data = lab2, aes(label = estimator), hjust = -0.25, size = 3.1,
              fontface = "bold", show.legend = FALSE) +
    facet_wrap(~spec) +
    scale_colour_manual(values = COL) +
    scale_fill_manual(values = COL) +
    scale_x_continuous(breaks = 0:5, expand = expansion(mult = c(0.04, 0.16))) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    labs(title = "Projected 2100 GDP impact relative to the contemporaneous model",
         subtitle = "SSP585, population-weighted; dashed line = no change from L = 0",
         x = "Lag length L (years)",
         y = expression(delta[2100]~"/"~delta[2100]~"at L = 0"),
         caption = paste0("Values below 1 mean the lag structure reduces projected damages; ",
                          "above 1 means it increases them.\n",
                          "Shaded region below 0: the projected impact has changed SIGN ",
                          "(climate change becomes beneficial) -- these\nestimates are not ",
                          "credible and indicate the lag polynomial is no longer identified. ",
                          "Ratios rather than\nlevels are plotted so the comparison does not ",
                          "depend on the aggregation choice.")) +
    theme_lag()

ggsave(file.path(fig_dir, "fig_lag_projection_ratio.png"), p2,
       width = 8.2, height = 4.2, dpi = 200, bg = SURFACE)

## ---- table view (accessibility: never color-alone) -------------------------
tbl <- me %>%
    select(spec, estimator, T_pct, L, dg_dT, se) %>%
    arrange(spec, estimator, T_pct, L)
write_csv(tbl, file.path(out_dir, "fig_lag_marginal_effect_data.csv"))
write_csv(gl %>% select(spec, estimator, L, delta_bhm, ratio_bhm_vs_L0),
          file.path(out_dir, "fig_lag_projection_ratio_data.csv"))

cat("Wrote:\n  figures/fig_lag_marginal_effect.png\n",
    " figures/fig_lag_projection_ratio.png\n",
    " + matching *_data.csv table views\n")

## quick console summary of convergence
cat("\n--- cumulative dg/dT at 90th pct temperature ---\n")
print(as.data.frame(
    me %>% filter(T_pct == "90%") %>%
        select(spec, estimator, L, dg_dT) %>%
        pivot_wider(names_from = L, values_from = dg_dT, names_prefix = "L") %>%
        mutate(across(where(is.numeric), ~round(.x, 5)))), row.names = FALSE)

cat("\n--- projection ratio vs L=0 (SSP585, pop-weighted) ---\n")
print(as.data.frame(
    gl %>% select(spec, estimator, L, ratio_bhm_vs_L0) %>%
        pivot_wider(names_from = L, values_from = ratio_bhm_vs_L0, names_prefix = "L") %>%
        mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
