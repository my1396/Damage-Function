## Bootstrap uncertainty for the interactive contribution, IE-effects_t.
##
## 9_decompose_IE_effects.R draws the interactive contribution as the shaded gap
## between the all-terms and the direct-terms-only GDP pathway,
##     IE-effects_t = delta^All_t - delta^Dir_t,
## but reports it as a point estimate only. This script attaches a bootstrap
## distribution to that gap.
##
## The draw mechanism is the one used in 7_bootstrap_lagged_projection.R: the
## Bewley coefficients are drawn from their estimated sampling distribution,
##     c^(b) ~ N(c_hat, V_hat),
## and mapped to beta_0 ... beta_L. Each draw is then pushed through the SAME
## decomposition as 9_: the full beta gives delta^All, the identical beta with
## the four interaction coefficients zeroed gives delta^Dir, and the difference
## is that draw's IE-effect path. Seed, NDRAW and loop order match 7_, so draw b
## here is draw b there and the IE bands are consistent with the delta bands in
## fig_global_damage_fan.png.
##
## Differencing WITHIN a draw matters: delta^All and delta^Dir are built from
## the same direct coefficients, so their sampling errors are strongly
## correlated and only the within-draw difference is meaningful. That does NOT
## make the IE band narrow. Both pathways compound over 80 years, so their
## difference is not bounded by their levels -- at L = 2, where the lag
## polynomial is weakly identified, sd(IE) is 3-5x sd(delta^All).
##
## A POSITIVE IE-effect means interactions MITIGATE damages.
##
## Out: figures/fig_IE_effects_fan.png
##      output/IE_effects_path_quantiles.csv   (All, Dir and IE, long)
##      output/IE_effects_draws_2100.csv
##      output/IE_effects_uncertainty.txt
## ========================================================================== ##

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(20260814)                       # same stream as 7_, so draw b matches
NDRAW   <- 1000
SSP     <- "SSP585"
SPEC    <- "Interactive"
LAGS    <- 0:2
YEARS   <- PROJ_YEARS
regs    <- REGS_INTERACT
idx_dir <- 1:4                           # T, T^2, P, P^2; 5-8 are interactions

COL     <- c(AFE = "#2a78d6", IFE = "#eb6834")
SURFACE <- "#fcfcfb"

fits <- readRDS(file.path(out_dir, "lagged_climate_fits.rds"))
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"),
                 show_col_types = FALSE)
inp  <- load_projection_inputs(SSP, root_dir)
cat(sprintf("Countries: %d | draws: %d | BHM aggregation\n",
            nrow(inp$cl), NDRAW))

## ========================================================================== ##
## 1. Draw coefficients and map Bewley c -> beta_j -----------------------------
## ========================================================================== ##
draw_c <- function(chat, V, n) {
    V <- (V + t(V)) / 2
    ev <- eigen(V, symmetric = TRUE)
    ev$values[ev$values < 0] <- 0                  # PSD repair
    A <- ev$vectors %*% diag(sqrt(ev$values), length(ev$values))
    matrix(chat, n, length(chat), byrow = TRUE) +
        matrix(rnorm(n * length(chat)), n) %*% t(A)
}

## c -> beta_j for one variable's block (c_0, c_1, ..., c_L)
c_to_beta <- function(cc, L) {
    if (L == 0) return(cc)
    b <- numeric(L + 1)
    b[1] <- cc[1] + cc[2]                          # beta_0 = c_0 + c_1
    if (L >= 2) for (j in 1:(L - 1)) b[j + 1] <- -cc[j + 1] + cc[j + 2]
    b[L + 1] <- -cc[L + 1]                         # beta_L = -c_L
    b
}

## assemble the (L+1) x M beta matrix from one named coefficient vector
beta_from_c <- function(cc, L) {
    B <- matrix(0, L + 1, length(regs), dimnames = list(NULL, regs))
    for (v in regs) {
        cn <- c(v, if (L >= 1) paste0("d", seq_len(L), "_", v))
        B[, v] <- c_to_beta(cc[cn], L)
    }
    B
}

## ========================================================================== ##
## 2. IE-effect path implied by one beta matrix --------------------------------
## ========================================================================== ##
## Identical construction to decompose() in 9_: delta^Dir re-uses the estimated
## coefficients with the four interaction terms set to zero (the paper's x^Dir).
ie_path <- function(B) {
    Bdir <- B
    Bdir[, -idx_dir] <- 0
    d_all <- global_delta(eta_matrix(inp$cl, B, regs),
                          inp$G, inp$POP, inp$gp0)
    d_dir <- global_delta(eta_matrix(inp$cl, Bdir, regs),
                          inp$G, inp$POP, inp$gp0)
    list(all = d_all, dir = d_dir, ie = d_all - d_dir)
}

## ========================================================================== ##
## 3. Bootstrap loop -----------------------------------------------------------
## ========================================================================== ##
qs   <- c(.025, .05, .10, .25, .50, .75, .90, .95, .975)
fanq <- list(); res <- list(); draws <- list()

for (est in c("AFE", "IFE")) {
    for (L in LAGS) {
        tag  <- sprintf("%s | L=%d | %s", SPEC, L, est)
        f    <- fits[[tag]]
        chat <- setNames(f$coefs$estimate, f$coefs$term)

        ## point estimate: same beta as 9_decompose_IE_effects.R
        pt <- ie_path(beta_matrix(lagc, SPEC, est, L))

        ## bootstrap draws
        D <- draw_c(chat, f$vcov, NDRAW)
        colnames(D) <- names(chat)
        ## draws in rows, years in columns, one matrix per component
        M <- list(All = NULL, Dir = NULL, IE = NULL)
        for (nm in names(M)) M[[nm]] <- matrix(0, NDRAW, PROJ_HORIZ)
        for (b in seq_len(NDRAW)) {
            pb <- ie_path(beta_from_c(D[b, ], L))
            M$All[b, ] <- pb$all; M$Dir[b, ] <- pb$dir; M$IE[b, ] <- pb$ie
        }
        ie_end <- M$IE[, PROJ_HORIZ]

        fanq[[tag]] <- imap_dfr(M, function(X, nm) {
            Q <- apply(X, 2, quantile, probs = qs)
            tibble(
                ssp = SSP, spec = SPEC, estimator = est, L = L,
                component = nm, year = YEARS,
                point = switch(nm, All = pt$all, Dir = pt$dir, IE = pt$ie),
                q025 = Q["2.5%", ],  q05 = Q["5%", ],  q10 = Q["10%", ],
                q25  = Q["25%", ],   q50 = Q["50%", ], q75 = Q["75%", ],
                q90  = Q["90%", ],   q95 = Q["95%", ], q975 = Q["97.5%", ])
        })

        res[[tag]] <- tibble(
            ssp = SSP, spec = SPEC, estimator = est, L = L,
            point     = tail(pt$ie, 1),
            mean      = mean(ie_end),
            median    = median(ie_end),
            sd        = sd(ie_end),
            q025      = quantile(ie_end, .025),
            q05       = quantile(ie_end, .05),
            q95       = quantile(ie_end, .95),
            q975      = quantile(ie_end, .975),
            p_mitigate = mean(ie_end > 0),          # P(interactions mitigate)
            sd_all    = sd(M$All[, PROJ_HORIZ]))    # for the sd ratio below

        draws[[tag]] <- tibble(spec = SPEC, estimator = est, L = L,
                               draw = seq_len(NDRAW), IE_2100 = ie_end)

        cat(sprintf(
            "  %-28s IE %+7.3f  mean %+7.3f  [%+7.3f, %+7.3f]  P(IE>0) %.2f\n",
                    tag, tail(pt$ie, 1), mean(ie_end),
                    quantile(ie_end, .025), quantile(ie_end, .975),
                    mean(ie_end > 0)))
    }
}

fq   <- bind_rows(fanq)
summ <- bind_rows(res)

write_csv(fq,               file.path(out_dir, "IE_effects_path_quantiles.csv"))
write_csv(bind_rows(draws), file.path(out_dir, "IE_effects_draws_2100.csv"))

## the All component reproduces 7_ exactly if the two draw streams agree
ref_file <- file.path(out_dir, "bootstrap_lagged_path_quantiles.csv")
if (file.exists(ref_file)) {
    ref <- read_csv(ref_file, show_col_types = FALSE)
    chk <- fq %>% filter(component == "All") %>%
        inner_join(ref, by = c("estimator", "L", "year"), suffix = c("", "_7"))
    cat(sprintf("check vs 7_: %d matched rows, max|q50 diff| = %.2e\n",
                nrow(chk), max(abs(chk$q50 - chk$q50_7))))
}

## ========================================================================== ##
## 4. Fan chart: point estimate + bootstrap density bands ----------------------
## ========================================================================== ##
ie <- fq %>%
    filter(component == "IE") %>%
    mutate(panel = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)))

## nested bands, darkest in the middle: 50%, 80%, 95%
bands <- tribble(
    ~lo,    ~hi,    ~alpha, ~lab,
    "q25",  "q75",  0.34,   "50%",
    "q10",  "q90",  0.20,   "80%",
    "q025", "q975", 0.11,   "95%")

lab <- ie %>%
    filter(year == max(year)) %>%
    left_join(summ %>% select(estimator, L, p_mitigate), by = c("estimator", "L")) %>%
    mutate(txt = sprintf("2100: %+.0f pp\n90%% CI [%+.0f, %+.0f]\nP(IE > 0) = %.2f",
                         100 * point, 100 * q05, 100 * q95, p_mitigate))

YLIM   <- c(-1, 1)
n_clip <- sum(ie$q025 < YLIM[1] | ie$q975 > YLIM[2])

p <- ggplot(ie, aes(year))
for (i in seq_len(nrow(bands))) {
    p <- p + geom_ribbon(
        aes(ymin = .data[[bands$lo[i]]], ymax = .data[[bands$hi[i]]],
            fill = estimator),
        alpha = bands$alpha[i], colour = NA)
}
p <- p +
    geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
    geom_line(aes(y = q50, colour = estimator), linetype = "22", linewidth = 0.45) +
    geom_line(aes(y = point, colour = estimator), linewidth = 0.85) +
    geom_text(data = lab, aes(x = -Inf, y = Inf, label = txt, colour = estimator),
              hjust = -0.06, vjust = 1.25, size = 2.7, fontface = "bold",
              lineheight = 0.95, show.legend = FALSE) +
    facet_grid(estimator ~ panel) +
    scale_colour_manual(values = COL, guide = "none") +
    scale_fill_manual(values = COL, guide = "none") +
    scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(ylim = YLIM) +
    labs(
        title = "Interactive contribution with bootstrap uncertainty",
        subtitle = paste0(
            SSP, ", M = 8 specification, ", format(NDRAW, big.mark = ","),
            " draws. Solid = point estimate, dashed = bootstrap median.\n",
            "Shading: 50% / 80% / 95% intervals, darkest to lightest."),
        x = NULL,
        y = expression("IE-effects"[t] ~ "=" ~ delta[t]^All - delta[t]^Dir),
        caption = paste0(
            "IE-effects are the gap shaded in fig_IE_decomposition.png: the ",
            "all-terms pathway minus the pathway that applies the same\n",
            "estimated coefficients with the four interaction terms set to ",
            "zero. A POSITIVE value means interactions MITIGATE damages.\n",
            "Coefficients are drawn from N(c_hat, V_hat) and the two pathways ",
            "are differenced within each draw, which retains the correlation\n",
            "between them. Damages compound, so the difference is not bounded ",
            "by the levels: at L = 2 the IE band is 3-5x wider than the band\n",
            "on delta itself, and the interactive contribution is not ",
            "distinguishable from zero at any lag length.",
            if (n_clip > 0) sprintf(
                "\nVertical range clipped at [%+.0f, %+.0f] pp; the 95%% band at L = 2 runs well beyond it (see output/IE_effects_uncertainty.txt).",
                100 * YLIM[1], 100 * YLIM[2]) else "")) +
    my_theme +
    theme(
        plot.background  = element_rect(fill = SURFACE, colour = NA),
        panel.background = element_rect(fill = SURFACE, colour = NA),
        panel.spacing    = unit(14, "pt"),
        strip.text       = element_text(colour = INK, size = 10, face = "bold",
                                        margin = margin(b = 4, t = 2)),
        plot.title       = element_text(hjust = 0, size = rel(1.35)),
        plot.subtitle    = element_text(size = rel(1.0), margin = margin(b = 4)),
        plot.caption     = element_text(size = rel(0.75), lineheight = 1.1))
p
ggsave(file.path(fig_dir, "fig_IE_effects_fan.png"), p,
       width = 8.8, height = 5.8, dpi = 200, bg = SURFACE)

## ========================================================================== ##
## 5. Summary ------------------------------------------------------------------
## ========================================================================== ##
tab <- summ %>%
    transmute(estimator, L,
              `IE 2100 (pp)`  = round(100 * point, 1),
              `median (pp)`   = round(100 * median, 1),
              `sd (pp)`       = round(100 * sd, 1),
              `90% CI lo`     = round(100 * q05, 1),
              `90% CI hi`     = round(100 * q95, 1),
              `95% CI lo`     = round(100 * q025, 1),
              `95% CI hi`     = round(100 * q975, 1),
              `P(IE > 0)`     = round(p_mitigate, 3),
              `sd(IE)/sd(All)` = round(sd / sd_all, 2))

sink(file.path(out_dir, "IE_effects_uncertainty.txt"))
cat("BOOTSTRAP UNCERTAINTY OF THE INTERACTIVE CONTRIBUTION (IE-effects)\n")
cat("==================================================================\n\n")
cat("Scenario:", SSP, "| spec:", SPEC, "| draws:", NDRAW,
    "| countries:", nrow(inp$cl), "\n")
cat("IE-effects_t = delta^All_t - delta^Dir_t, differenced within each draw.\n")
cat("delta^Dir applies the same drawn coefficients with the four interaction\n")
cat("terms set to zero. A POSITIVE value means interactions MITIGATE damages.\n")
cat("Coefficients drawn from N(c_hat, V_hat) and mapped to beta_0..beta_L.\n")
cat("Units: percentage points of the 2100 GDP ratio.\n\n")
cat("sd(IE)/sd(All) compares the spread of the interactive contribution with\n")
cat("the spread of delta^All itself. It exceeds 1 at L = 2 because the two\n")
cat("compounded pathways diverge there: the difference of two paths is not\n")
cat("bounded by their levels.\n\n")
print(as.data.frame(tab), row.names = FALSE)
sink()

cat("\nWrote figures/fig_IE_effects_fan.png,\n",
    "output/IE_effects_{path_quantiles,draws_2100}.csv and",
    "IE_effects_uncertainty.txt\n\n")
if (n_clip > 0)
    cat(sprintf(
        "note: %d of %d 95%%-band points lie outside the plotted [%g, %g] range\n\n",
        n_clip, nrow(ie), YLIM[1], YLIM[2]))
cat("=== IE-effects at 2100, point estimate and bootstrap spread (pp) ===\n")
print(as.data.frame(tab), row.names = FALSE)
