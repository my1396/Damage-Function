## =============================================================================
## Decomposing the M = 8 projection into DIRECT and INTERACTIVE contributions.
##
##   direct terms      : T, T^2, P, P^2                  (columns 1-4)
##   interactive terms : TP, T^2P, TP^2, T^2P^2          (columns 5-8)
##
## The growth-rate impact is exactly additive, because eta is linear in beta:
##     eta^All_t = eta^Dir_t + eta^IE_t
## The GDP path is NOT, because damages compound. Following eq. (13) of the
## paper, the interactive contribution is therefore defined as the difference
## of two compounded pathways:
##     IE-effects_t = delta^All_t - delta^Dir_t
## where delta^Dir uses the same estimated coefficients but zeroes the four
## interaction terms (identical to the paper's x^Dir construction).
##
## A POSITIVE IE-effect means interactions MITIGATE damages.
##
## Out: figures/fig_IE_decomposition.png
##      output/IE_decomposition.csv
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")

SSP     <- "SSP585"
YEARS   <- PROJ_YEARS
LAGS    <- 0:2
regs    <- REGS_INTERACT
idx_dir <- 1:4

COL_ALL <- "#2a78d6"
COL_DIR <- "#52514e"
FILL_IE <- "#2a78d6"
INK <- "#0b0b0b"; INK_SOFT <- "#52514e"; INK_MUTED <- "#8a8985"
SURFACE <- "#fcfcfb"

inp  <- load_projection_inputs(SSP, root_dir)
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"), show_col_types = FALSE)
POPW <- sweep(inp$POP, 2, colSums(inp$POP), "/")

decompose <- function(est, L) {
    B <- beta_matrix(lagc, "Interactive", est, L)
    Ball <- B
    Bdir <- B; Bdir[, -idx_dir] <- 0        # zero the interaction coefficients
    eta_all <- eta_matrix(inp$cl, Ball, regs)
    eta_dir <- eta_matrix(inp$cl, Bdir, regs)
    eta_ie  <- eta_all - eta_dir            # exact: eta is linear in beta

    d_all <- global_delta(eta_all, inp$G, inp$POP, inp$gp0)
    d_dir <- global_delta(eta_dir, inp$G, inp$POP, inp$gp0)

    tibble(year = YEARS, estimator = est, L = L,
           delta_all = d_all, delta_dir = d_dir,
           IE_effect = d_all - d_dir,
           eta_all = colSums(eta_all * POPW),
           eta_dir = colSums(eta_dir * POPW),
           eta_ie  = colSums(eta_ie  * POPW))
}

dec <- expand_grid(est = c("AFE", "IFE"), L = LAGS) %>%
    pmap_dfr(function(est, L) decompose(est, L)) %>%
    mutate(panel = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)),
           IE_share = IE_effect / abs(delta_dir))

write_csv(dec, file.path(out_dir, "IE_decomposition.csv"))

## =============================================================================
## 3. Plot: delta^All vs delta^Dir, gap = IE-effects
## =============================================================================
ends <- dec %>% group_by(estimator, panel) %>% filter(year == max(year)) %>% ungroup()

p <- ggplot(dec, aes(year)) +
    geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
    geom_ribbon(aes(ymin = delta_dir, ymax = delta_all),
                fill = FILL_IE, alpha = 0.16) +
    geom_line(aes(y = delta_dir, colour = "Direct terms only"), linewidth = 0.7) +
    geom_line(aes(y = delta_all, colour = "All terms"), linewidth = 0.7) +
    geom_text(data = ends,
              aes(y = (delta_all + delta_dir) / 2,
                  label = sprintf("IE +%.0f pp", 100 * IE_effect)),
              x = 2098, hjust = 1, size = 2.9, fontface = "bold",
              colour = COL_ALL) +
    facet_grid(estimator ~ panel) +
    scale_colour_manual(values = c("All terms" = COL_ALL,
                                   "Direct terms only" = COL_DIR),
                        breaks = c("All terms", "Direct terms only")) +
    scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.06, 0.10))) +
    labs(title = "Direct and interactive contributions to projected GDP impact",
         subtitle = paste0(SSP, ", M = 8 specification, population-weighted. ",
                           "BHM aggregation. Shaded gap = interactive contribution."),
         x = NULL,
         y = expression(delta[t]~"= GDP(CC) / GDP(no CC) - 1"),
         caption = paste0("\"Direct terms only\" applies the same estimated coefficients ",
                          "with the four interaction terms set to zero (the paper's x^Dir).\n",
                          "IE-effects = delta^All - delta^Dir (eq. 13). A POSITIVE value ",
                          "means interactions MITIGATE damages.\n",
                          "The growth-rate impact eta decomposes exactly; the GDP path does ",
                          "not, because damages compound.")) +
    theme_minimal(base_size = 11) +
    theme(
        plot.background  = element_rect(fill = SURFACE, colour = NA),
        panel.background = element_rect(fill = SURFACE, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        panel.spacing    = unit(14, "pt"),
        axis.title       = element_text(colour = INK_SOFT, size = 10),
        axis.text        = element_text(colour = INK_SOFT, size = 9),
        strip.text       = element_text(colour = INK, size = 10, face = "bold",
                                        margin = margin(b = 4, t = 2)),
        plot.title       = element_text(colour = INK, size = 13, face = "bold"),
        plot.subtitle    = element_text(colour = INK_SOFT, size = 10,
                                        margin = margin(b = 8)),
        plot.caption     = element_text(colour = INK_MUTED, size = 8, hjust = 0,
                                        margin = margin(t = 10)),
        legend.position  = "top",
        legend.title     = element_blank(),
        legend.text      = element_text(colour = INK_SOFT, size = 9),
        legend.key.width = unit(18, "pt"))

ggsave(file.path(fig_dir, "fig_IE_decomposition.png"), p,
       width = 8.8, height = 5.4, dpi = 200, bg = SURFACE)

## =============================================================================
## 4. Summary
## =============================================================================
cat("Wrote figures/fig_IE_decomposition.png and output/IE_decomposition.csv\n\n")
cat("=== GDP impact at 2100: total, direct-only, and interactive contribution ===\n")
print(as.data.frame(
    dec %>% filter(year == 2100) %>%
        transmute(estimator, L,
                  `delta_All(%)`  = round(100 * delta_all, 1),
                  `delta_Dir(%)`  = round(100 * delta_dir, 1),
                  `IE (pp)`       = round(100 * IE_effect, 1),
                  `IE as % of Dir` = round(100 * IE_share, 1))),
    row.names = FALSE)

cat("\n=== growth-rate impact in 2100 (exact additive split, pop-weighted) ===\n")
print(as.data.frame(
    dec %>% filter(year == 2100) %>%
        transmute(estimator, L,
                  eta_All = round(eta_all, 5),
                  eta_Dir = round(eta_dir, 5),
                  eta_IE  = round(eta_ie, 5),
                  `IE share of eta (%)` = round(100 * eta_ie / eta_all, 1))),
    row.names = FALSE)
