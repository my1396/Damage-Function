## =============================================================================
## Bootstrap distribution of the global 2100 damage, distributed-lag model.
##
## Follows 7-1_bootstrap_regression.R / 7-2_bootstrap_path_persistent.R in
## structure, with two changes:
##   (1) eta_t uses the full distributed lag  sum_j beta_j'(x_{t-j} - x_0)
##   (2) coefficient uncertainty is drawn from the estimated sampling
##       distribution rather than by refitting on resampled countries.
##
## Why (2): 7-1 refits the Bai (2009) PC estimator on each resampled panel.
## At L=2 a single IFE fit takes ~40 s, so 1000 refits x 6 configurations is
## days of compute. Drawing c ~ N(c_hat, V_hat) from the fit already stored in
## lagged_climate_fits.rds propagates the same estimation uncertainty --
## including the covariance between contemporaneous and lag coefficients, which
## is what governs the cumulative effect -- at negligible cost. This is the
## Krinsky-Robb / parametric-bootstrap approach. Set METHOD <- "cluster" to run
## the slower country-resampling version for the AFE models.
##
## GLOBAL AGGREGATION follows 7-2 exactly: population-weighted growth rate in
## each year, cumulated, then compared with the no-climate-change path.
##
## Out: output/bootstrap_lagged_*.csv , output/fig_bootstrap_damage.png
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")

set.seed(20260814)
NDRAW <- 1000
SSP   <- "SSP585"
SPECS <- "Interactive"
LAGS  <- 0:2

fits <- readRDS(file.path(out_dir, "lagged_climate_fits.rds"))
inp  <- load_projection_inputs(SSP, root_dir)
cat(sprintf("Countries: %d | BHM aggregation (compound within country, then sum)\n",
            nrow(inp$cl)))

## =============================================================================
## 2. Draw coefficients and map Bewley c -> beta_j
## =============================================================================
draw_c <- function(chat, V, n) {
    V <- (V + t(V)) / 2
    ev <- eigen(V, symmetric = TRUE)
    ev$values[ev$values < 0] <- 0                 # PSD repair
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

## =============================================================================
## 3. eta and the global path for one beta draw
## =============================================================================
delta_path <- function(B, regs) {
    eta <- eta_matrix(inp$cl, B, regs)
    global_delta(eta, inp$G, inp$POP, inp$gp0)      # full 2021-2100 path
}

## =============================================================================
## 4. Run
## =============================================================================
res <- list(); paths <- list(); fanq <- list()

for (sp in SPECS) {
    regs <- regs_for(sp)
    for (est in c("AFE", "IFE")) {
        for (L in LAGS) {
            tag <- sprintf("%s | L=%d | %s", sp, L, est)
            f <- fits[[tag]]
            chat <- setNames(f$coefs$estimate, f$coefs$term)
            V    <- f$vcov

            ## point estimate
            Bpt <- matrix(0, L + 1, length(regs), dimnames = list(NULL, regs))
            for (v in regs) {
                cn <- c(v, if (L >= 1) paste0("d", seq_len(L), "_", v))
                Bpt[, v] <- c_to_beta(chat[cn], L)
            }
            p_point <- delta_path(Bpt, regs)
            d_point <- tail(p_point, 1)

            ## bootstrap draws
            D <- draw_c(chat, V, NDRAW)
            colnames(D) <- names(chat)
            ## full path per draw: NDRAW x 80
            PM <- vapply(seq_len(NDRAW), function(b) {
                B <- matrix(0, L + 1, length(regs), dimnames = list(NULL, regs))
                for (v in regs) {
                    cn <- c(v, if (L >= 1) paste0("d", seq_len(L), "_", v))
                    B[, v] <- c_to_beta(D[b, cn], L)
                }
                delta_path(B, regs)
            }, numeric(PROJ_HORIZ))
            PM <- t(PM)                       # draws in rows, years in columns
            dd <- PM[, PROJ_HORIZ]

            ## per-year quantiles for the fan chart
            qs <- c(.025, .05, .10, .25, .50, .75, .90, .95, .975)
            Q  <- apply(PM, 2, quantile, probs = qs)
            fanq[[tag]] <- tibble(
                ssp = SSP, spec = sp, estimator = est, L = L,
                year  = PROJ_YEARS,
                point = p_point,
                q025 = Q["2.5%", ],  q05 = Q["5%", ],  q10 = Q["10%", ],
                q25  = Q["25%", ],   q50 = Q["50%", ], q75 = Q["75%", ],
                q90  = Q["90%", ],   q95 = Q["95%", ], q975 = Q["97.5%", ])

            res[[tag]] <- tibble(
                ssp = SSP, spec = sp, estimator = est, L = L,
                point   = d_point,
                mean    = mean(dd),
                median  = median(dd),
                sd      = sd(dd),
                q025    = quantile(dd, .025),
                q05     = quantile(dd, .05),
                q95     = quantile(dd, .95),
                q975    = quantile(dd, .975),
                p_worse_than_50pct = mean(dd < -0.50))
            paths[[tag]] <- tibble(spec = sp, estimator = est, L = L, draw = seq_len(NDRAW),
                                   delta_2100 = dd)
            cat(sprintf("  %-28s point %7.3f  mean %7.3f  [%7.3f, %7.3f]\n",
                        tag, d_point, mean(dd), quantile(dd, .025), quantile(dd, .975)))
        }
    }
}

summ  <- bind_rows(res)
draws <- bind_rows(paths)

write_csv(summ,  file.path(out_dir, "bootstrap_lagged_summary.csv"))
write_csv(draws, file.path(out_dir, "bootstrap_lagged_draws.csv"))
write_csv(bind_rows(fanq), file.path(out_dir, "bootstrap_lagged_path_quantiles.csv"))

sink(file.path(out_dir, "bootstrap_lagged.txt"))
cat("BOOTSTRAP DISTRIBUTION OF GLOBAL 2100 DAMAGE -- DISTRIBUTED-LAG MODEL\n")
cat("=====================================================================\n\n")
cat("Scenario:", SSP, "| draws:", NDRAW, "| countries:", nrow(inp$cl), "\n")
cat("Global aggregation follows BHM: each country's GDP path is compounded\n")
cat("first, then summed across countries (GDP-share weighted damages).\n")
cat("Coefficients drawn from N(c_hat, V_hat) and mapped to beta_0..beta_L.\n\n")
print(as.data.frame(summ %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)
sink()

cat("\nWritten to output/bootstrap_lagged_{summary,draws,path_quantiles}.csv and .txt\n")
