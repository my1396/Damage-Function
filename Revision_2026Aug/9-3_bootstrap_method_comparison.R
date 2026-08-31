## Country resampling vs coefficient resampling, AFE only.
##
## 7_bootstrap_lagged_projection.R and 9-2_IE_effects_uncertainty.R both draw
## the Bewley coefficients from their estimated sampling distribution,
## c^(b) ~ N(c_hat, V_hat), rather than refitting the model. That is the
## Krinsky-Robb / parametric bootstrap, and 7_ adopted it because the Bai (2009)
## IFE estimator is far too slow to refit 1,000 times. It does, however, impose
## normality on c_hat and takes V_hat as known.
##
## This script runs the alternative for the AFE estimator, where refitting is
## cheap: resample the 122 countries WITH REPLACEMENT, rebuild the panel giving
## each drawn copy its own id (so duplicated countries get their own fixed
## effect and their own trends), refit, and push the refitted coefficients
## through the identical projection and decomposition machinery. This is the
## pairs/cluster bootstrap; it makes no distributional assumption and
## regenerates the clustering by country that vcovHC(cluster = "group") only
## approximates. IFE is NOT run -- a single L = 2 IFE fit takes ~40 s, so
## 1,000 refits would be days of compute.
##
## PAIRING. Both methods difference delta^All and delta^Dir WITHIN a single
## draw: ie_path() takes one beta matrix and derives Bdir from it by zeroing
## four columns, so the two pathways always come from the same coefficient
## vector. There is no second estimate to correlate, which is why the
## within-model contrast cannot suffer the problem 8-1 has to work around --
## there the M = 8 and M = 4 models are separately estimated and the pairing
## must be reconstructed. Section 7 reports what that retained pairing is worth.
##
## The refit uses fixest rather than plm. Identical linear model, but plm
## carries the 244 country-trend dummies as explicit regressors (~1.7 s/fit)
## while fixest absorbs them as varying slopes (~0.03 s/fit). Section 2 asserts
## the two agree on the full sample before any resampling is done.
##
## Out: figures/fig_IE_effects_fan_bootstrap_compare.png
##      figures/fig_global_damage_fan_bootstrap_compare.png
##      output/bootstrap_cluster_afe_path_quantiles.csv
##      output/bootstrap_cluster_afe_draws_2100.csv
##      output/bootstrap_method_comparison.txt
## ========================================================================== ##

suppressMessages({library(tidyverse); library(fixest)})

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")

set.seed(20260814)
NDRAW   <- 1000
SSP     <- "SSP585"
SPEC    <- "Interactive"
EST     <- "AFE"                         # IFE deliberately excluded, see header
LAGS    <- 0:2
YEARS   <- PROJ_YEARS
regs    <- REGS_INTERACT
idx_dir <- 1:4                           # T, T^2, P, P^2; 5-8 are interactions

M_CLU <- "Country resampling"
M_PAR <- "Coefficient resampling"
COL   <- c("#1a8f7a", "#2a78d6")
names(COL) <- c(M_CLU, M_PAR)
SURFACE <- "white"

fits <- readRDS(file.path(out_dir, "lagged_climate_fits.rds"))
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"),
                 show_col_types = FALSE)
inp  <- load_projection_inputs(SSP, root_dir)

## ========================================================================== ##
## 1. Regression panel and the Bewley design -----------------------------------
## ========================================================================== ##
## Same construction as 3_lagged_climate.R. tt is the within-country time index
## the country trends are built on; fixest takes it as a varying slope.
Pdata <- read_csv("data/GDP_reg_panelData_V2.csv", show_col_types = FALSE) %>%
    arrange(iso, year) %>%
    mutate(tmp2 = tmp^2, pre2 = pre^2,
           tmp_pre = tmp*pre, tmp2_pre = tmp^2*pre,
           pre2_tmp = pre^2*tmp, tmp2_pre2 = tmp^2*pre^2)

n_iso <- n_distinct(Pdata$iso); n_year <- n_distinct(Pdata$year)
stopifnot(nrow(Pdata) == n_iso * n_year)
Pdata <- Pdata %>% mutate(tt = rep(seq_len(n_year), times = n_iso), tt2 = tt^2)

## level terms keep their names (their coefficients are the cumulative
## effects); difference terms are named d<j>_<var>.
build_bewley <- function(dat, regs, L) {
    if (L == 0) return(dat)
    d <- dat %>% group_by(iso)
    for (v in regs) d <- d %>% mutate("d1_{v}" := .data[[v]] - dplyr::lag(.data[[v]], 1))
    d <- d %>% ungroup()
    if (L >= 2) {
        d <- d %>% group_by(iso)
        for (j in 2:L) for (v in regs)
            d <- d %>% mutate("d{j}_{v}" := dplyr::lag(.data[[paste0("d1_", v)]], j - 1))
        d <- d %>% ungroup()
    }
    d
}

lag_terms <- function(L) c(regs, if (L >= 1) as.vector(outer(
    paste0("d", seq_len(L), "_"), regs, paste0)))

## ========================================================================== ##
## 2. AFE refit, and a check that it reproduces plm ----------------------------
## ========================================================================== ##
## The country trends enter as varying slopes on tt; `id` is the panel unit, so
## a resampled copy of a country gets its own FE and its own trends.
afe_fit <- function(dat, allregs) {
    f <- as.formula(paste("logD_gdp ~", paste(allregs, collapse = " + "),
                          "| id[tt, tt2] + year"))
    feols(f, data = dat, notes = FALSE, warn = FALSE)
}

design <- list(); blocks <- list()
for (L in LAGS) {
    allregs <- lag_terms(L)
    d <- build_bewley(Pdata, regs, L) %>%
        select(all_of(c("iso", "year", "logD_gdp", "tt", "tt2", allregs))) %>%
        mutate(id = iso)
    design[[as.character(L)]] <- as.data.frame(d)
    blocks[[as.character(L)]] <-
        split(seq_len(nrow(d)), factor(d$iso, levels = unique(d$iso)))

    ref <- fits[[sprintf("%s | L=%d | %s", SPEC, L, EST)]]$coefs
    got <- coef(afe_fit(design[[as.character(L)]], allregs))[ref$term]
    cat(sprintf("L=%d  refit vs stored plm fit: max|diff| = %.2e (%d terms)\n",
                L, max(abs(got - ref$estimate)), length(got)))
    stopifnot(max(abs(got - ref$estimate)) < 1e-8)
}

## ========================================================================== ##
## 3. c -> beta, and the IE decomposition for one coefficient vector -----------
## ========================================================================== ##
## Identical to 7_ and 9-2: the Bewley coefficients map to beta_0 ... beta_L,
## and delta^Dir re-uses the same beta with the four interaction terms zeroed.
c_to_beta <- function(cc, L) {
    if (L == 0) return(cc)
    b <- numeric(L + 1)
    b[1] <- cc[1] + cc[2]                          # beta_0 = c_0 + c_1
    if (L >= 2) for (j in 1:(L - 1)) b[j + 1] <- -cc[j + 1] + cc[j + 2]
    b[L + 1] <- -cc[L + 1]                         # beta_L = -c_L
    b
}

beta_from_c <- function(cc, L) {
    B <- matrix(0, L + 1, length(regs), dimnames = list(NULL, regs))
    for (v in regs) {
        cn <- c(v, if (L >= 1) paste0("d", seq_len(L), "_", v))
        B[, v] <- c_to_beta(cc[cn], L)
    }
    B
}

ie_path <- function(B) {
    Bdir <- B
    Bdir[, -idx_dir] <- 0
    d_all <- global_delta(eta_matrix(inp$cl, B, regs),
                          inp$G, inp$POP, inp$gp0)
    d_dir <- global_delta(eta_matrix(inp$cl, Bdir, regs),
                          inp$G, inp$POP, inp$gp0)
    list(All = d_all, Dir = d_dir, IE = d_all - d_dir)
}

## ========================================================================== ##
## 4. Country-resampling bootstrap ---------------------------------------------
## ========================================================================== ##
qs   <- c(.025, .05, .10, .25, .50, .75, .90, .95, .975)
fanq <- list(); draws <- list(); cdraw <- list(); nfail <- integer(0)

for (L in LAGS) {
    key <- as.character(L)
    D <- design[[key]]; blk <- blocks[[key]]; allregs <- lag_terms(L)
    newid <- rep(seq_len(n_iso), each = n_year)

    pt <- ie_path(beta_matrix(lagc, SPEC, EST, L))     # full-sample point est.

    M <- list(All = matrix(NA_real_, NDRAW, PROJ_HORIZ),
              Dir = matrix(NA_real_, NDRAW, PROJ_HORIZ),
              IE  = matrix(NA_real_, NDRAW, PROJ_HORIZ))
    C <- matrix(NA_real_, NDRAW, length(allregs),
                dimnames = list(NULL, allregs))
    t0 <- Sys.time()
    for (b in seq_len(NDRAW)) {
        sel <- sample.int(n_iso, n_iso, replace = TRUE)
        Db  <- D[unlist(blk[sel], use.names = FALSE), ]
        Db$id <- newid
        cb <- coef(afe_fit(Db, allregs))
        if (!all(allregs %in% names(cb)) || anyNA(cb[allregs])) next  # collinear
        C[b, ] <- cb[allregs]
        pb <- ie_path(beta_from_c(cb, L))
        for (nm in names(M)) M[[nm]][b, ] <- pb[[nm]]
    }
    ok <- !is.na(M$IE[, 1])
    nfail[key] <- sum(!ok)
    for (nm in names(M)) M[[nm]] <- M[[nm]][ok, , drop = FALSE]
    cdraw[[key]] <- as_tibble(C[ok, , drop = FALSE]) %>%
        mutate(L = L, draw = seq_len(sum(ok)), .before = 1)

    fanq[[key]] <- imap_dfr(M, function(X, nm) {
        Q <- apply(X, 2, quantile, probs = qs)
        tibble(ssp = SSP, spec = SPEC, estimator = EST, method = M_CLU, L = L,
               component = nm, year = YEARS, point = pt[[nm]],
               q025 = Q["2.5%", ],  q05 = Q["5%", ],  q10 = Q["10%", ],
               q25  = Q["25%", ],   q50 = Q["50%", ], q75 = Q["75%", ],
               q90  = Q["90%", ],   q95 = Q["95%", ], q975 = Q["97.5%", ])
    })
    draws[[key]] <- tibble(spec = SPEC, estimator = EST, method = M_CLU, L = L,
                           draw = seq_len(sum(ok)),
                           All_2100 = M$All[, PROJ_HORIZ],
                           IE_2100  = M$IE[, PROJ_HORIZ])

    ie_end <- M$IE[, PROJ_HORIZ]
    cat(sprintf("  L=%d  %4d draws (%d dropped, %.0f s)  IE %+7.3f  [%+7.3f, %+7.3f]\n",
                L, sum(ok), sum(!ok),
                as.numeric(difftime(Sys.time(), t0, units = "secs")),
                tail(pt$IE, 1), quantile(ie_end, .025), quantile(ie_end, .975)))
}

clu_q <- bind_rows(fanq)
clu_d <- bind_rows(draws)
clu_c <- bind_rows(cdraw)
write_csv(clu_q, file.path(out_dir, "bootstrap_cluster_afe_path_quantiles.csv"))
write_csv(clu_d, file.path(out_dir, "bootstrap_cluster_afe_draws_2100.csv"))
write_csv(clu_c, file.path(out_dir, "bootstrap_cluster_afe_coefs.csv"))

## ========================================================================== ##
## 5. Pull in the coefficient-resampling bootstrap -----------------------------
## ========================================================================== ##
## Everything below reads from disk, so the figures can be retuned without
## re-running section 4.
clu_q <- read_csv(file.path(out_dir, "bootstrap_cluster_afe_path_quantiles.csv"),
                  show_col_types = FALSE)
clu_d <- read_csv(file.path(out_dir, "bootstrap_cluster_afe_draws_2100.csv"),
                  show_col_types = FALSE)
clu_c <- read_csv(file.path(out_dir, "bootstrap_cluster_afe_coefs.csv"),
                  show_col_types = FALSE)

## coefficient level: cluster sd against the analytic clustered se. A ratio
## near 1 would mean the two methods agree about coefficient uncertainty, so
## any divergence downstream is the nonlinear compounding, not the sampling.
lev_sd <- map_dfr(LAGS, function(L) {
    V <- fits[[sprintf("%s | L=%d | %s", SPEC, L, EST)]]$vcov
    tibble(L = L, term = regs,
           `analytic se` = sqrt(diag(V))[regs],
           `cluster sd`  = apply(clu_c %>% filter(L == !!L) %>%
                                     select(all_of(regs)), 2, sd))
}) %>% mutate(ratio = `cluster sd` / `analytic se`)

par_file <- file.path(out_dir, "IE_effects_path_quantiles.csv")
if (!file.exists(par_file))
    stop("run 9-2_IE_effects_uncertainty.R first: ", par_file, " not found")

par_q <- read_csv(par_file, show_col_types = FALSE) %>%
    filter(estimator == EST, L %in% LAGS) %>%
    mutate(method = M_PAR)
par_d <- read_csv(file.path(out_dir, "IE_effects_draws_2100.csv"),
                  show_col_types = FALSE) %>%
    filter(estimator == EST, L %in% LAGS) %>%
    mutate(method = M_PAR) %>%
    select(spec, estimator, method, L, draw, IE_2100)
par_all <- read_csv(file.path(out_dir, "bootstrap_lagged_draws.csv"),
                    show_col_types = FALSE) %>%
    filter(estimator == EST, L %in% LAGS) %>%
    select(L, draw, All_2100 = delta_2100)
par_d <- par_d %>% left_join(par_all, by = c("L", "draw"))

## 7_ and 9-2 share one RNG stream, so draw b of bootstrap_lagged_draws.csv is
## draw b of IE_effects_draws_2100.csv and delta^Dir can be recovered as their
## difference. Verify that against the Dir quantiles 9-2 computed directly.
ref_dir <- read_csv(par_file, show_col_types = FALSE) %>%
    filter(estimator == EST, component == "Dir", year == max(YEARS)) %>%
    select(L, q05, q95)
chk_dir <- par_d %>% group_by(L) %>%
    summarise(q05 = quantile(All_2100 - IE_2100, .05),
              q95 = quantile(All_2100 - IE_2100, .95), .groups = "drop") %>%
    inner_join(ref_dir, by = "L", suffix = c("", "_ref"))
cat(sprintf("draw pairing check: max|diff| = %.2e\n",
            max(abs(c(chk_dir$q05 - chk_dir$q05_ref,
                      chk_dir$q95 - chk_dir$q95_ref)))))
stopifnot(max(abs(c(chk_dir$q05 - chk_dir$q05_ref,
                    chk_dir$q95 - chk_dir$q95_ref))) < 1e-10)

both_q <- bind_rows(clu_q, par_q) %>%
    mutate(method = factor(method, levels = c(M_CLU, M_PAR)),
           panel  = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)))
both_d <- bind_rows(clu_d, par_d) %>%
    mutate(method = factor(method, levels = c(M_CLU, M_PAR)),
           Dir_2100 = All_2100 - IE_2100)

## 90% width under country resampling relative to coefficient resampling
wr <- both_d %>%
    pivot_longer(c(All_2100, IE_2100), names_to = "component",
                 values_to = "v", names_pattern = "(.*)_2100") %>%
    filter(!is.na(v)) %>%
    group_by(component, method, L) %>%
    summarise(w90 = quantile(v, .95) - quantile(v, .05), .groups = "drop") %>%
    group_by(component, L) %>%
    summarise(r = w90[method == M_CLU] / w90[method == M_PAR], .groups = "drop")

wrange <- function(comp) {
    r <- wr %>% filter(component == comp) %>% pull(r)
    sprintf("%.1f-%.1fx", min(r), max(r))
}

## ========================================================================== ##
## 6. Fan charts: one row per bootstrap method ---------------------------------
## ========================================================================== ##
## nested bands, darkest in the middle: 50%, 80%, 95%
bands <- tribble(
    ~lo,    ~hi,    ~alpha,
    "q25",  "q75",  0.34,
    "q10",  "q90",  0.20,
    "q025", "q975", 0.11)

make_fan <- function(comp, ylim, ylab, ttl, sub, cap) {
    d <- both_q %>% filter(component == comp)
    ## the other method's 95% envelope, for a direct read across rows
    alt <- d %>%
        mutate(method = factor(ifelse(method == M_CLU, M_PAR, M_CLU),
                               levels = c(M_CLU, M_PAR)))
    lab <- d %>% filter(year == max(year)) %>%
        mutate(txt = sprintf("2100: %+.0f pp\n90%% CI [%+.0f, %+.0f]",
                             100 * point, 100 * q05, 100 * q95))
    n_clip <- sum(d$q025 < ylim[1] | d$q975 > ylim[2])

    p <- ggplot(d, aes(year))
    for (i in seq_len(nrow(bands))) {
        p <- p + geom_ribbon(
            aes(ymin = .data[[bands$lo[i]]], ymax = .data[[bands$hi[i]]],
                fill = method),
            alpha = bands$alpha[i], colour = NA)
    }
    p +
        geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
        geom_line(data = alt, aes(y = q025), colour = INK_MUTED,
                  linetype = "22", linewidth = 0.3) +
        geom_line(data = alt, aes(y = q975), colour = INK_MUTED,
                  linetype = "22", linewidth = 0.3) +
        geom_line(aes(y = q50, colour = method), linetype = "22", linewidth = 0.45) +
        geom_line(aes(y = point, colour = method), linewidth = 0.85) +
        geom_text(data = lab, aes(x = -Inf, y = Inf, label = txt, colour = method),
                  hjust = -0.06, vjust = 1.25, size = 2.7, fontface = "bold",
                  lineheight = 0.95, show.legend = FALSE) +
        facet_grid(method ~ panel) +
        scale_colour_manual(values = COL, guide = "none") +
        scale_fill_manual(values = COL, guide = "none") +
        scale_x_continuous(breaks = c(2025, 2050, 2075, 2100)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        coord_cartesian(ylim = ylim) +
        labs(title = ttl, subtitle = sub, x = NULL, y = ylab, caption = cap) +
        my_theme +
        theme(
            plot.background  = element_rect(fill = SURFACE, colour = NA),
            panel.background = element_rect(fill = SURFACE, colour = NA),
            panel.spacing    = unit(14, "pt"),
            strip.text.y     = element_text(colour = INK, size = 9, face = "bold",
                                            margin = margin(l = 4, r = 2)),
            strip.text.x     = element_text(colour = INK, size = 10, face = "bold",
                                            margin = margin(b = 4, t = 2)),
            plot.title       = element_text(hjust = 0, size = rel(1.35)),
            plot.subtitle    = element_text(size = rel(1.0), margin = margin(b = 4)),
            plot.caption     = element_text(size = rel(0.75), lineheight = 1.1))
}

sub_common <- paste0(
    SSP, ", M = 8 specification, AFE only, ", format(NDRAW, big.mark = ","),
    " draws per method. Solid = point estimate, dashed = bootstrap median.\n",
    "Shading: 50% / 80% / 95% intervals, darkest to lightest. ",
    "Grey dashes repeat the OTHER method's 95% envelope.")

cap_boot <- paste0(
    "Country resampling draws 122 countries with replacement, gives each drawn ",
    "copy its own fixed effect and its own linear and quadratic\n",
    "trends, and refits the AFE model on the resampled panel. Coefficient ",
    "resampling instead draws c ~ N(c_hat, V_hat) from the full-sample fit\n",
    "(the method used in 7_ and 9-2) and never refits. Both are pushed through ",
    "the same projection; only the source of coefficient uncertainty differs.\n",
    sprintf(paste0("On the coefficients themselves the country bootstrap is ",
                   "%.1f-%.1fx the analytic clustered se, so vcovHC(cluster = ",
                   "'group') modestly understates them."),
            min(lev_sd$ratio), max(lev_sd$ratio)))

p_ie <- make_fan(
    "IE", c(-1, 1),
    expression("IE-effects"[t] ~ "=" ~ delta[t]^All - delta[t]^Dir),
    "Interactive contribution: two bootstraps compared",
    sub_common,
    NULL)
p_ie
ggsave(file.path(fig_dir, "fig_IE_effects_fan_bootstrap_compare.png"), p_ie,
       width = 8.8, height = 4.8, dpi = 200, bg = SURFACE)

p_all <- make_fan(
    "All", c(-1, 1),
    expression(delta[t] ~ "= GDP(CC) / GDP(no CC) - 1"),
    "Global GDP impact: two bootstraps compared",
    sub_common,
    NULL)
p_all
ggsave(file.path(fig_dir, "fig_global_damage_fan_bootstrap_compare.png"), p_all,
       width = 8.8, height = 4.8, dpi = 200, bg = SURFACE)

## ========================================================================== ##
## 7. Summary ------------------------------------------------------------------
## ========================================================================== ##
## delta^Dir is the unstable object: with the interactions zeroed the remaining
## direct coefficients can imply positive growth effects that compound over 80
## years. Count how often each method lands there, since that -- not the width
## of the coefficient distribution -- drives the difference in the IE bands.
BLOWUP <- 10                            # delta^Dir_2100 > +1000%

summ <- both_d %>%
    pivot_longer(c(All_2100, IE_2100), names_to = "component",
                 values_to = "v", names_pattern = "(.*)_2100") %>%
    filter(!is.na(v)) %>%
    group_by(component, method, L) %>%
    summarise(n = n(), median = median(v),
              q025 = quantile(v, .025), q05 = quantile(v, .05),
              q95 = quantile(v, .95), q975 = quantile(v, .975),
              p_pos = mean(v > 0), .groups = "drop") %>%
    mutate(w90 = q95 - q05)

pt_2100 <- both_q %>%
    filter(method == M_CLU, year == max(year)) %>%
    select(component, L, point)

tab <- summ %>%
    left_join(pt_2100, by = c("component", "L")) %>%
    group_by(component, L) %>%
    mutate(`width ratio` = round(w90 / w90[method == M_PAR], 2)) %>%
    ungroup() %>%
    transmute(component, L, method = as.character(method), draws = n,
              `point (pp)`  = round(100 * point, 1),
              `median (pp)` = round(100 * median, 1),
              `90% lo`      = round(100 * q05, 1),
              `90% hi`      = round(100 * q95, 1),
              `90% width`   = round(100 * w90, 1),
              `width ratio`,
              `P(> 0)`      = round(p_pos, 3)) %>%
    arrange(component, L, desc(method))

blow <- both_d %>%
    group_by(method, L) %>%
    summarise(`n Dir > +1000%` = sum(Dir_2100 > BLOWUP),
              `max Dir (x)`    = round(max(Dir_2100), 1),
              `n IE < -100pp`  = sum(IE_2100 < -1), .groups = "drop") %>%
    arrange(L, desc(method))

lev_tab <- lev_sd %>%
    mutate(ratio = round(ratio, 2),
           across(c(`analytic se`, `cluster sd`), ~signif(.x, 3)))

## ---- the pairing that ie_path() retains, and what it is worth ---------------
wid90 <- function(v) as.numeric(diff(quantile(v, c(.05, .95))))
pairing <- both_d %>%
    group_by(method, L) %>%
    summarise(`cor(All, Dir)` = round(cor(All_2100, Dir_2100), 3),
              `paired w90`    = round(wid90(All_2100 - Dir_2100), 3),
              `shuffled w90`  = round(median(replicate(
                  20, wid90(All_2100 - sample(Dir_2100)))), 3),
              .groups = "drop") %>%
    mutate(`pairing worth` = round(`shuffled w90` / `paired w90`, 2)) %>%
    arrange(L, desc(method))

sink(file.path(out_dir, "bootstrap_method_comparison.txt"))
cat("BOOTSTRAP METHOD COMPARISON -- AFE, INTERACTIVE SPEC, SSP585\n")
cat("============================================================\n\n")
cat("Country resampling : 122 countries drawn with replacement, each drawn\n")
cat("  copy given its own fixed effect and its own linear and quadratic\n")
cat("  trends, AFE refit on the resampled panel. Makes no distributional\n")
cat("  assumption and regenerates the country clustering directly.\n")
cat("Coefficient resampling : c ~ N(c_hat, V_hat) from the full-sample fit,\n")
cat("  no refitting. The method used in 7_ and 9-2, and the only feasible\n")
cat("  one for IFE. Imposes normality and takes V_hat as known.\n\n")
cat("Draws:", NDRAW, "per method. Dropped for collinearity:",
    paste(sprintf("L=%s: %d", names(nfail), nfail), collapse = ", "), "\n")
cat("Both methods share the point estimate; only the bands differ.\n")
cat("Units: percentage points of the 2100 GDP ratio.\n")
cat("component All = delta^All_2100, IE = delta^All_2100 - delta^Dir_2100.\n")
cat("width ratio = 90% width relative to coefficient resampling.\n")
cat("sd is not reported: a handful of explosive draws dominate it (see the\n")
cat("second table), so the quantile-based width is the meaningful comparison.\n\n")
print(as.data.frame(tab), row.names = FALSE)

cat("\n\n--- Where the two methods diverge: the delta^Dir counterfactual ---\n\n")
cat("delta^Dir zeroes the four interaction coefficients and compounds the\n")
cat("remaining direct terms for 80 years. When a draw puts those terms in the\n")
cat("positive-growth region the path explodes, and since IE = All - Dir the\n")
cat("explosion shows up as a large NEGATIVE IE. max Dir (x) is the largest\n")
cat("delta^Dir_2100 in the sample, as a multiple of counterfactual GDP.\n\n")
print(as.data.frame(blow), row.names = FALSE)

cat("\n\n--- The within-draw pairing, and what it is worth ---\n\n")
cat("delta^Dir is not a separate estimate: ie_path() derives it from the SAME\n")
cat("beta as delta^All by zeroing the four interaction columns, so every draw\n")
cat("is paired by construction and no cross-model covariance has to be\n")
cat("recovered. (8-1 does have to recover one, because there the M = 8 and\n")
cat("M = 4 models are separately estimated.) 'shuffled' differences delta^All\n")
cat("against a random permutation of delta^Dir, which leaves both marginals\n")
cat("untouched, so 'pairing worth' is the factor by which the 90% width would\n")
cat("inflate if the pairing were thrown away.\n\n")
print(as.data.frame(pairing), row.names = FALSE)

cat("\n\n--- Coefficient level: cluster sd vs analytic (clustered) se ---\n\n")
cat("Cumulative (level) coefficients only. Ratio near 1 means the two methods\n")
cat("agree about coefficient uncertainty, so any divergence in the projection\n")
cat("bands comes from the nonlinear compounding, not from the sampling scheme.\n\n")
print(as.data.frame(lev_tab), row.names = FALSE)
sink()

cat("\nWrote figures/fig_{IE_effects,global_damage}_fan_bootstrap_compare.png,\n",
    "output/bootstrap_cluster_afe_{path_quantiles,draws_2100,coefs}.csv and",
    "bootstrap_method_comparison.txt\n\n")
print(as.data.frame(tab), row.names = FALSE)
cat("\n")
print(as.data.frame(blow), row.names = FALSE)
cat("\n")
print(as.data.frame(pairing), row.names = FALSE)
