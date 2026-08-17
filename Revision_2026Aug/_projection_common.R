## =============================================================================
## Shared projection machinery. Sourced by 5_, 7_, 8_, 9_.
##
## Single source of truth for the GLOBAL AGGREGATION, which follows
## Burke, Hsiang & Miguel (2015), ComputeMainProjections.R lines 201-202:
##
##     compound each country's GDP path first, THEN sum across countries
##         delta_t = sum_i GDPpc^CC_it * pop_it / sum_i GDPpc^noCC_it * pop_it - 1
##
## which is algebraically a GDP-share weighted average of country damages,
##     delta_t = sum_i s_it * delta_it,   s = share of counterfactual world GDP.
##
## This is what "world GDP growth" means: world growth is the GDP-share weighted
## average of country growth rates, not the population-weighted average. It is
## also bounded -- a country can remove at most its own GDP share -- which is why
## the aggregate path decelerates as damages accumulate.
##
## The superseded approach (population-weighted mean GROWTH each year, then
## cumulated) is kept as global_delta_popgrowth() for comparison only.
## =============================================================================

suppressMessages(library(tidyverse))

PROJ_HORIZ <- 80
PROJ_YEARS <- seq(2021, 2100)
PROJ_TCAP  <- Inf     # set to 30 for the BHM out-of-sample cap

REGS_DIRECT   <- c("tmp", "tmp2", "pre", "pre2")
REGS_INTERACT <- c(REGS_DIRECT, "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")
regs_for <- function(sp) if (sp == "Interactive") REGS_INTERACT else REGS_DIRECT

## -----------------------------------------------------------------------------
## Inputs for one SSP, aligned on a common country set and ordered by ISO.
## Returns cl (climate trends), G (baseline growth), POP (population, millions),
## gp0 (baseline GDP per capita, constant 2010 US$, mean 2015-2019).
## -----------------------------------------------------------------------------
load_projection_inputs <- function(ssp,
                                   root_dir = "/Users/menghan/Documents/GDP/Shared folder") {
    tmp_df <- read_csv(file.path(root_dir, sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp)),
                       show_col_types = FALSE)
    pre_df <- read_csv(file.path(root_dir, sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp)),
                       show_col_types = FALSE) %>%
        mutate_at(c("start", "end", "avg", "trend_annual"), ~ . * 12 / 1000)

    cl <- tmp_df %>%
        select(ISO_C3, tmp_start = start, tmp_trend = trend_annual) %>%
        inner_join(pre_df %>% select(ISO_C3, pre_start = start, pre_trend = trend_annual),
                   by = "ISO_C3")

    g <- read_csv(file.path(root_dir, sprintf("data/baseline_growth/%s_GrowthProjections.csv",
                                              substr(ssp, 1, 4))), show_col_types = FALSE)
    colnames(g)[-1] <- as.character(PROJ_YEARS)

    p <- read_csv(file.path(root_dir, "data/SSP_Population_weight.csv"),
                  show_col_types = FALSE) %>%
        filter(Scenario == substr(ssp, 1, 4))
    py <- setdiff(colnames(p), c("Model", "Scenario", "Region", "Variable", "Unit"))
    p  <- p %>% select(Region, all_of(py))

    gdp0 <- read_csv(file.path(root_dir,
                               "data/cntry_ann_climate_gdpKD_1961to2019.csv"),
                     show_col_types = FALSE, na = "..") %>%
        filter(year %in% 2015:2019, !is.na(NY.GDP.PCAP.KD)) %>%
        group_by(ISO_C3) %>%
        summarise(gdppc0 = mean(NY.GDP.PCAP.KD), .groups = "drop")

    keep <- Reduce(intersect, list(cl$ISO_C3, g$Region, p$Region, gdp0$ISO_C3))
    cl  <- cl %>% filter(ISO_C3 %in% keep) %>% arrange(ISO_C3)
    G   <- g %>% filter(Region %in% keep) %>% arrange(Region) %>%
        select(all_of(as.character(PROJ_YEARS))) %>% as.matrix()
    POP <- p %>% filter(Region %in% keep) %>% arrange(Region) %>%
        select(all_of(py)) %>% as.matrix()
    POP <- POP[, seq_len(PROJ_HORIZ), drop = FALSE]
    gp0 <- gdp0 %>% filter(ISO_C3 %in% keep) %>% arrange(ISO_C3) %>% pull(gdppc0)

    ok <- complete.cases(G) & complete.cases(POP) & !is.na(gp0)
    list(cl = cl[ok, ], G = G[ok, , drop = FALSE],
         POP = POP[ok, , drop = FALSE], gp0 = gp0[ok])
}

## climate design at horizon s (s = 0 is the base period); cap truncates T
x_at <- function(cl, s, regs, cap = PROJ_TCAP) {
    tt <- pmin(cl$tmp_start + cl$tmp_trend * s, cap)
    pp <- cl$pre_start + cl$pre_trend * s
    cbind(tmp = tt, tmp2 = tt^2, pre = pp, pre2 = pp^2,
          tmp_pre = tt * pp, tmp2_pre = tt^2 * pp,
          pre2_tmp = pp^2 * tt, tmp2_pre2 = tt^2 * pp^2)[, regs, drop = FALSE]
}

## beta_0 ... beta_L from lag_coefficients_long.csv
beta_matrix <- function(lagc, sp, est, L) {
    regs <- regs_for(sp)
    d <- lagc %>% filter(spec == sp, estimator == est, L == !!L)
    B <- matrix(0, L + 1, length(regs), dimnames = list(NULL, regs))
    for (j in 0:L) {
        dj <- d %>% filter(coef == paste0("b", j))
        B[j + 1, dj$variable] <- dj$estimate
    }
    B
}

## eta_{i,t} = sum_j beta_j' (x_{t-j} - x_0), lags before the base period
## clamped at x_0 so they contribute nothing
eta_matrix <- function(cl, B, regs, cap = PROJ_TCAP) {
    L   <- nrow(B) - 1
    x0  <- x_at(cl, 0, regs, cap)
    dev <- lapply(0:PROJ_HORIZ, function(s) x_at(cl, s, regs, cap) - x0)
    out <- matrix(0, nrow(cl), PROJ_HORIZ)
    for (t in 1:PROJ_HORIZ) {
        acc <- numeric(nrow(cl))
        for (j in 0:L) acc <- acc + as.vector(dev[[max(t - j, 0) + 1]] %*% B[j + 1, ])
        out[, t] <- acc
    }
    out
}

## country-level GDP path relative to its own counterfactual
country_delta <- function(eta, G) {
    t(apply(G + eta + 1, 1, cumprod)) / t(apply(G + 1, 1, cumprod)) - 1
}

## ---- THE GLOBAL AGGREGATION (BHM): compound within country, then sum --------
global_delta <- function(eta, G, POP, gp0) {
    cc  <- t(apply(G + eta + 1, 1, cumprod)) * gp0
    ncc <- t(apply(G + 1,       1, cumprod)) * gp0
    colSums(cc * POP) / colSums(ncc * POP) - 1
}

## ---- superseded: population-weighted mean growth, then cumulate -------------
## Retained only so the two can be compared; do not use for reported results.
global_delta_popgrowth <- function(eta, G, POP) {
    W <- sweep(POP, 2, colSums(POP), "/")
    cumprod(colSums((G + eta) * W) + 1) / cumprod(colSums(G * W) + 1) - 1
}
