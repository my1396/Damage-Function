## =============================================================================
## Global GDP damage pathway 2021-2100 under L = 0, 1, 2, for AFE and IFE,
## for BOTH regressor sets:
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
##      output/global_damage_path.csv   (both specs, long)
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

SSP   <- "SSP585"
YEARS <- PROJ_YEARS
LAGS  <- 0:2

COL <- c(AFE = "#2a78d6", IFE = "#eb6834")
INK <- "#0b0b0b"
INK_SOFT  <- "#52514e"
INK_MUTED <- "#8a8985"
SURFACE   <- "#fcfcfb"

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

## =============================================================================
## 3. Plot, one figure per specification
## =============================================================================
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
                                            margin = margin(b = 4)),
            plot.title       = element_text(colour = INK, size = 13, face = "bold"),
            plot.subtitle    = element_text(colour = INK_SOFT, size = 10,
                                            margin = margin(b = 8)),
            plot.caption     = element_text(colour = INK_MUTED, size = 8, hjust = 0,
                                            margin = margin(t = 10)),
            legend.position  = "top",
            legend.title     = element_blank(),
            legend.text      = element_text(colour = INK_SOFT, size = 9),
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


## =============================================================================
## 4. Fan chart: point estimate + bootstrap density bands
##    Requires 7_bootstrap_lagged_projection.R (path quantiles).
## =============================================================================
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
             x = NULL, y = expression(delta[t]~"= GDP(CC) / GDP(no CC) - 1"),
             caption = paste0("Coefficients drawn from N(c_hat, V_hat) and mapped to ",
                              "beta_0..beta_L. Distributions are right-skewed because\n",
                              "damages compound, so the median sits below the mean and ",
                              "the point estimate tracks the median, not the mean.\n",
                              "y-axis clipped at +/-100%; the AFE upper tail extends ",
                              "beyond +200% at L = 2.")) +
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
              plot.subtitle = element_text(colour = INK_SOFT, size = 9.5,
                                           margin = margin(b = 8)),
              plot.caption = element_text(colour = INK_MUTED, size = 8, hjust = 0,
                                          margin = margin(t = 10)))

    ggsave(file.path(fig_dir, "fig_global_damage_fan.png"), pf,
           width = 8.8, height = 5.4, dpi = 200, bg = SURFACE)
    cat("Wrote figures/fig_global_damage_fan.png\n")
} else {
    cat("skip fan chart: run 7_bootstrap_lagged_projection.R first\n")
}
