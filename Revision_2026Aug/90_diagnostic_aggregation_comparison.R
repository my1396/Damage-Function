## =============================================================================
## DIAGNOSTIC (not part of the 1-9 production sequence).
##
## Documents why the global aggregation was changed. Compares, on identical
## coefficients and identical climate paths:
##
##   (A) BHM        compound each country's GDP path, THEN sum across countries
##                  -> global_delta()            [used by 5, 7, 8, 9]
##   (B) superseded population-weighted mean GROWTH each year, THEN cumulate
##                  -> global_delta_popgrowth()  [inherited from 7-2]
##
## and, separately, the effect of BHM's out-of-sample cap: they hold the
## response fixed once temperature exceeds 30 C ("so we are not projecting out
## of sample", ComputeMainProjections.R L191). 34% of countries here exceed
## 30 C by 2100 while the estimation sample tops out near 29.7 C.
##
## Depends on: 4_lag_coefficient_table.R (lag_coefficients_long.csv)
## Out: figures/fig_aggregation_comparison.png
##      output/projection_bhm_aggregation.csv
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")

SSP  <- "SSP585"
LAGS <- 0:2
TCAP <- 30

COL <- c(AFE = "#2a78d6", IFE = "#eb6834")
INK <- "#0b0b0b"; INK_SOFT <- "#52514e"; INK_MUTED <- "#8a8985"
SURFACE <- "#fcfcfb"

inp  <- load_projection_inputs(SSP, root_dir)
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"), show_col_types = FALSE)
regs <- REGS_INTERACT

## ---- how far out of sample does the projection go? -------------------------
T2100 <- inp$cl$tmp_start + inp$cl$tmp_trend * PROJ_HORIZ
cat(sprintf("Countries: %d\n", nrow(inp$cl)))
cat(sprintf("2100 temperature: min %.1f, median %.1f, max %.1f C\n",
            min(T2100), median(T2100), max(T2100)))
cat(sprintf("above %d C at 2100: %d (%.0f%%);  hottest country in sample ~%.1f C\n",
            TCAP, sum(T2100 > TCAP), 100 * mean(T2100 > TCAP), max(inp$cl$tmp_start)))

gdp2100 <- (t(apply(inp$G + 1, 1, cumprod)) * inp$gp0)[, PROJ_HORIZ] * inp$POP[, PROJ_HORIZ]
hot <- T2100 >= median(T2100)
cat(sprintf("hottest half hold %.0f%% of 2100 population but %.0f%% of 2100 GDP\n",
            100 * sum(inp$POP[hot, PROJ_HORIZ]) / sum(inp$POP[, PROJ_HORIZ]),
            100 * sum(gdp2100[hot]) / sum(gdp2100)))

## ---- the grid --------------------------------------------------------------
res <- expand_grid(est = c("AFE", "IFE"), L = LAGS, cap = c(Inf, TCAP)) %>%
    pmap_dfr(function(est, L, cap) {
        B   <- beta_matrix(lagc, "Interactive", est, L)
        eta <- eta_matrix(inp$cl, B, regs, cap = cap)
        tibble(year = PROJ_YEARS, estimator = est, L = L,
               cap = ifelse(is.finite(cap), sprintf("%d C cap", TCAP), "no cap"),
               delta_bhm       = global_delta(eta, inp$G, inp$POP, inp$gp0),
               delta_popgrowth = global_delta_popgrowth(eta, inp$G, inp$POP))
    })

write_csv(res, file.path(out_dir, "projection_bhm_aggregation.csv"))

cat("\n=== global GDP impact at 2100, interactive specification ===\n")
print(as.data.frame(
    res %>% filter(year == 2100) %>%
        transmute(estimator, L, cap,
                  `BHM levels (%)`     = round(100 * delta_bhm, 1),
                  `pop-wtd growth (%)` = round(100 * delta_popgrowth, 1),
                  `difference (pp)`    = round(100 * (delta_bhm - delta_popgrowth), 1)) %>%
        arrange(estimator, L, cap)), row.names = FALSE)

## ---- figure ----------------------------------------------------------------
plt <- res %>%
    filter(cap == "no cap") %>%
    pivot_longer(c(delta_bhm, delta_popgrowth),
                 names_to = "method", values_to = "delta") %>%
    mutate(method = recode(method,
                           delta_bhm       = "BHM: compound then average",
                           delta_popgrowth = "Superseded: average then compound"),
           panel = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)))

ends <- plt %>%
    filter(year == max(year), method == "BHM: compound then average")

p <- ggplot(plt, aes(year, delta, colour = estimator, linetype = method)) +
    geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
    geom_line(linewidth = 0.7) +
    geom_text(data = ends, aes(label = sprintf("%.0f%%", 100 * delta)),
              hjust = 1, nudge_x = -1.5, nudge_y = 0.05, size = 2.9,
              fontface = "bold", show.legend = FALSE) +
    facet_grid(estimator ~ panel) +
    scale_colour_manual(values = COL, guide = "none") +
    scale_linetype_manual(values = c("BHM: compound then average"       = "solid",
                                     "Superseded: average then compound" = "22")) +
    scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.06, 0.12))) +
    labs(title = "Global damage depends heavily on how countries are aggregated",
         subtitle = paste0(SSP, ", interactive specification, no out-of-sample cap. ",
                           "Labels give the BHM value at 2100."),
         x = NULL, y = expression(delta[t]~"= GDP(CC) / GDP(no CC) - 1"),
         caption = paste0("BHM compound each country's GDP path before summing, which is ",
                          "algebraically a GDP-share weighted average of country\n",
                          "damages and is what \"world GDP growth\" means. Averaging growth ",
                          "rates with population weights first gives hot poor\n",
                          "countries far more weight and removes the bound that makes the ",
                          "BHM path flatten.")) +
    theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = SURFACE, colour = NA),
          panel.background = element_rect(fill = SURFACE, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
          panel.spacing = unit(14, "pt"),
          axis.title = element_text(colour = INK_SOFT, size = 10),
          axis.text = element_text(colour = INK_SOFT, size = 9),
          strip.text = element_text(colour = INK, size = 10, face = "bold",
                                    margin = margin(b = 4, t = 2)),
          plot.title = element_text(colour = INK, size = 13, face = "bold"),
          plot.subtitle = element_text(colour = INK_SOFT, size = 10, margin = margin(b = 8)),
          plot.caption = element_text(colour = INK_MUTED, size = 8, hjust = 0,
                                      margin = margin(t = 10)),
          legend.position = "top", legend.title = element_blank(),
          legend.text = element_text(colour = INK_SOFT, size = 9),
          legend.key.width = unit(26, "pt"))

ggsave(file.path(fig_dir, "fig_aggregation_comparison.png"), p,
       width = 8.8, height = 5.4, dpi = 200, bg = SURFACE)
cat("\nWrote figures/fig_aggregation_comparison.png\n")
