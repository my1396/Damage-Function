## ========================================================================== ##
## Persistence ratio: cumulative vs impact marginal effect -------------------- 
##
## Growth effect vs level effect. In the distributed-lag model
##     y_t = sum_{j=0}^{L} beta_j' x_{t-j} + ...
## the impact effect is beta_0 and the cumulative effect is sum_j beta_j. If the
## two coincide the shock never unwinds (pure GROWTH effect); if the cumulative
## effect is zero the lags exactly offset the impact (pure LEVEL effect). The
## persistence ratio
##     rho = [dg/dT | sum_j beta_j] / [dg/dT | beta_0]
## places the estimate on that line: rho = 1 growth, rho = 0 level.
##
## Both marginal effects are linear in the SAME Bewley coefficient vector c:
##     sum_j beta_j = c_0                (the level-term coefficient)
##     beta_0       = c_0 + c_1          (level + first difference term)
## so with a = the dg/dT gradient placed on the relevant coefficients,
##     rho = (a_cum' c) / (a_imp' c)
## and the delta method gives, with g = a_cum/(a_imp'c) - rho*a_imp/(a_imp'c),
##     se(rho) = sqrt(g' V g).
## Using the full V keeps the (strong) correlation between numerator and
## denominator -- they share c_0 -- so this is NOT se(cum)/|impact|.
##
## NOTE ON PROVENANCE. output/lag_ratio_within_model.csv (2026-08-14) was left
## behind by a script that is no longer in the folder. This file rebuilds it
## from lagged_climate_fits.rds and reproduces every stored number to <1.5e-12,
## which pins down the method above. The default grid here is a superset of the
## original eight rows (both specs, L = 1..5).
##
## In : output/lagged_climate_fits.rds          (written by 3_lagged_climate.R)
##      output/lagged_climate_marginal_effects.csv  (for T and P evaluation pts)
## Out: output/lag_ratio_within_model.csv
##      output/lag_ratio_within_model.txt  (row table + summary statistics)
## ========================================================================== ##

suppressMessages(library(tidyverse))

## ========================================================================== ##
## 1. Configuration ------------------------------------------------------------
## ========================================================================== ##
root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
out_dir  <- file.path(root_dir, "Revision_2026Aug", "output")

SPECS  <- c("Direct", "Interactive")   # specifications to report
ESTS   <- c("AFE", "IFE")
LAGS   <- 1:5                          # L = 0 gives rho == 1 by construction
T_PCTS <- c("50%", "90%")              # temperature points for dg/dT
ZCRIT  <- 1.96                         # 95% normal band, as in the original CSV

fits <- readRDS(file.path(out_dir, "lagged_climate_fits.rds"))
me   <- read_csv(file.path(out_dir, "lagged_climate_marginal_effects.csv"),
                 show_col_types = FALSE)

## evaluation points: same T quantiles and median P that 3_lagged_climate.R used
Tval <- me %>% distinct(T_pct, T_val) %>% deframe()
Pmed <- unique(me$P_val)
stopifnot(length(Pmed) == 1, all(T_PCTS %in% names(Tval)))

## ========================================================================== ##
## 2. Ratio and its delta-method standard error --------------------------------
## ========================================================================== ##
## gradient of d(growth)/dT wrt the climate coefficient vector -- verbatim from
## 3_lagged_climate.R so the marginal effects match what is already reported
grad_T <- function(Tv, Pv, interactive) {
    g <- c(1, 2*Tv, 0, 0)
    if (interactive) g <- c(g, Pv, 2*Tv*Pv, Pv^2, 2*Tv*Pv^2)
    g
}

persistence <- function(f, T_pct) {
    b   <- setNames(f$coefs$estimate, f$coefs$term)
    nm  <- names(b)
    V   <- f$vcov[nm, nm, drop = FALSE]
    lev <- f$level
    g   <- grad_T(Tval[[T_pct]], Pmed, f$interactive)

    ## a_cum loads the level terms (c_0 = sum_j beta_j);
    ## a_imp adds the first-difference terms (beta_0 = c_0 + c_1)
    a_cum <- setNames(rep(0, length(b)), nm)
    a_cum[lev] <- g
    a_imp <- a_cum
    if (f$L >= 1) a_imp[paste0("d1_", lev)] <- g

    cum <- sum(a_cum * b)
    imp <- sum(a_imp * b)
    rho <- cum / imp

    ## delta method on a ratio of two linear forms in the same c
    gr <- a_cum / imp - rho * a_imp / imp
    se <- sqrt(as.numeric(t(gr) %*% V %*% gr))

    tibble(spec = f$spec, estimator = f$estimator, L = f$L, T_pct = T_pct,
           impact_b0 = imp, cumulative = cum, ratio = rho, se_ratio = se,
           lo = rho - ZCRIT * se, hi = rho + ZCRIT * se)
}

## ========================================================================== ##
## 3. Run the grid and export --------------------------------------------------
## ========================================================================== ##
ratio <- expand_grid(spec = SPECS, L = LAGS, estimator = ESTS, T_pct = T_PCTS) %>%
    pmap_dfr(function(spec, L, estimator, T_pct) {
        tag <- sprintf("%s | L=%d | %s", spec, L, estimator)
        if (is.null(fits[[tag]])) stop("no fit stored for ", tag)
        persistence(fits[[tag]], T_pct)
    }) %>%
    arrange(match(spec, SPECS), match(estimator, ESTS), L, T_pct)

write_csv(ratio, file.path(out_dir, "lag_ratio_within_model.csv"))

sink(file.path(out_dir, "lag_ratio_within_model.txt"))
cat("PERSISTENCE RATIO: CUMULATIVE / IMPACT MARGINAL EFFECT\n")
cat("=====================================================\n\n")
cat("rho = [dg/dT at sum_j beta_j] / [dg/dT at beta_0], evaluated at the\n")
cat("median precipitation and the stated temperature percentile.\n\n")
cat("rho = 1  growth effect : the shock never unwinds\n")
cat("rho = 0  level  effect : the lags exactly offset the impact effect\n\n")
cat("se_ratio is the delta method applied to the ratio of two linear forms in\n")
cat("the same Bewley coefficient vector, using the full covariance matrix, so\n")
cat("the correlation between numerator and denominator is carried.\n")
cat(sprintf("lo/hi are rho -/+ %.2f x se_ratio.\n\n", ZCRIT))
print(as.data.frame(ratio %>% mutate(across(where(is.numeric), ~signif(.x, 4)))),
      row.names = FALSE)
sink()

cat(sprintf("\n%d rows -> output/lag_ratio_within_model.csv / .txt\n", nrow(ratio)))
print(as.data.frame(ratio %>% mutate(across(where(is.numeric), ~signif(.x, 3)))),
      row.names = FALSE)

## ========================================================================== ##
## 4. Summary statistics -------------------------------------------------------
## ========================================================================== ##
## The 40 rows are 20 fitted models (2 specs x 2 estimators x L = 1..5) each
## read at two temperature percentiles, so n below is a ROW count, not a number
## of independent estimates -- the two Temperature points share a coefficient vector.
## L = 0 is excluded from the grid because rho = 1 there by construction.
ratio <- read_csv(file.path(out_dir, "lag_ratio_within_model.csv"),
                  show_col_types = FALSE)

stat_block <- function(d, ...) {
    d %>%
        group_by(...) %>%
        summarise(n       = n(),
                  mean    = mean(ratio),
                  median  = median(ratio),
                  sd      = sd(ratio),
                  min     = min(ratio),
                  max     = max(ratio),
                  se_mean = mean(se_ratio),
                  .groups = "drop")
}

by_all  <- stat_block(ratio) %>% mutate(group = "all rows", .before = 1)
by_spec <- stat_block(ratio, spec)
by_est  <- stat_block(ratio, estimator)
by_cell <- stat_block(ratio, spec, estimator)
by_L    <- stat_block(ratio, L)

## What the bands actually rule out. rho = 1 is the growth effect and rho = 0
## the level effect, so a band covering both says nothing about which holds.
cover <- ratio %>%
    group_by(spec, estimator) %>%
    summarise(n = n(),
              covers_1     = sum(lo <= 1 & hi >= 1),
              covers_0     = sum(lo <= 0 & hi >= 0),
              covers_both  = sum(lo <= 0 & hi >= 1),
              excludes_0   = sum(lo > 0),
              .groups = "drop")

i_lo <- which.min(ratio$ratio); i_hi <- which.max(ratio$ratio)
ext <- ratio[c(i_lo, i_hi), ] %>%
    mutate(which = c("lowest rho", "highest rho"), .before = 1) %>%
    select(which, spec, estimator, L, T_pct, ratio, se_ratio, lo, hi)

rnd <- function(d) d %>% mutate(across(where(is.numeric), ~round(.x, 3)))

n_in01  <- sum(ratio$ratio >= 0 & ratio$ratio <= 1)
n_cov1  <- sum(ratio$lo <= 1 & ratio$hi >= 1)
n_cov0  <- sum(ratio$lo <= 0 & ratio$hi >= 0)
n_both  <- sum(ratio$lo <= 0 & ratio$hi >= 1)
n_excl0 <- sum(ratio$lo > 0)
excl_where <- ratio %>% filter(lo > 0) %>% distinct(estimator, L) %>%
    arrange(estimator, L) %>%
    summarise(txt = paste(sprintf("%s L=%d", estimator, L), collapse = ", ")) %>%
    pull(txt)

sink(file.path(out_dir, "lag_ratio_within_model_summary.txt"), append = TRUE)
cat("\n\n")
cat("SUMMARY STATISTICS\n")
cat("==================\n\n")
cat("n is a ROW count. The", nrow(ratio), "rows are", n_distinct(paste(ratio$spec,
    ratio$estimator, ratio$L)), "fitted models read at two temperature\n")
cat("percentiles, so the two readings of a model share one coefficient vector\n")
cat("and are not independent. L = 0 is excluded: rho = 1 there by construction.\n")
cat("se_mean is the mean delta-method standard error of rho in the group.\n\n")

cat("--- All rows ---\n\n")
print(as.data.frame(rnd(by_all)), row.names = FALSE)

cat("\n\n--- By specification ---\n\n")
print(as.data.frame(rnd(by_spec)), row.names = FALSE)

cat("\n\n--- By estimator ---\n\n")
print(as.data.frame(rnd(by_est)), row.names = FALSE)

cat("\n\n--- By specification and estimator ---\n\n")
print(as.data.frame(rnd(by_cell)), row.names = FALSE)

cat("\n\n--- By lag length ---\n\n")
print(as.data.frame(rnd(by_L)), row.names = FALSE)

cat("\n\n--- Extremes ---\n\n")
print(as.data.frame(rnd(ext)), row.names = FALSE)

cat("\n\n--- What the confidence bands rule out ---\n\n")
print(as.data.frame(cover), row.names = FALSE)

cat("\n\n--- Reading ---\n\n")
cat(sprintf("All %d of %d point estimates fall inside [0, 1], between a pure level\n",
            n_in01, nrow(ratio)))
cat(sprintf("effect and a pure growth effect. The median is %.2f and the mean %.2f, so\n",
            median(ratio$ratio), mean(ratio$ratio)))
cat(sprintf("the central estimate is that roughly %.0f%% of the impact effect survives\n",
            100 * median(ratio$ratio)))
cat("once the lags have played out -- closer to a growth effect than to a level\n")
cat("effect, but not cleanly either.\n\n")
cat(sprintf("The bands are too wide to settle it. %d of %d cover rho = 1, %d cover\n",
            n_cov1, nrow(ratio), n_cov0))
cat(sprintf("rho = 0, and %d cover BOTH, which is to say most rows are consistent with\n",
            n_both))
cat("either extreme. Standard errors rise steeply with L (see the by-lag-length\n")
cat("block): adding lag terms buys generality at a real cost in precision.\n\n")
if (n_excl0 > 0) {
    cat(sprintf("Only %d rows rule out a pure level effect (lo > 0): %s.\n",
                n_excl0, excl_where))
    cat("Those are the short-lag IFE fits, the most precisely estimated cells.\n")
} else {
    cat("No row rules out a pure level effect.\n")
}
if (n_cov1 == nrow(ratio))
    cat("No row rules out a pure growth effect.\n")
sink()

cat(sprintf("\nSummary statistics appended to output/lag_ratio_within_model.txt\n\n"))
print(as.data.frame(rnd(by_cell)), row.names = FALSE)
cat("\n")
print(as.data.frame(rnd(by_L)), row.names = FALSE)
