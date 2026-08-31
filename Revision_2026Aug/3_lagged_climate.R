## Lagged climate: growth effects vs level effects
##
## Distributed-lag model   y_t = sum_{j=0}^{L} beta_j' x_{t-j} + ...
## estimated in Bewley form so the CUMULATIVE effect is read off directly:
##
##   sum_j beta_j' x_{t-j}
##     = (sum_j beta_j)' x_t  -  sum_{j=1}^{L} (sum_{k>=j} beta_k)' dx_{t-j+1}
##
## so regressing on x_t and the lagged first differences dx gives the long-run
## coefficient ON THE LEVEL TERM, with its standard error for free -- no delta
## method needed.
##
##   sum_j beta_j ~ beta_0  ->  growth effect (permanent, no rebound)
##   sum_j beta_j ~ 0       ->  level effect  (temporary, full rebound)
##
## Estimators: additive FE (within) and interactive FE (Bai 2009), each with
## the direct-only and the interaction regressor set, for L = 0, 1, 2.
##
## Out: output/lagged_climate_*.csv , output/lagged_climate.txt
## ========================================================================== ##

library(plyr)
suppressMessages(library(tidyverse))
library(plm); library(lmtest); library(sandwich)

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

NFAC      <- 4
IFE_TOL   <- 5e-4
IFE_MAXIT <- 1000
LAGS      <- 0:5

## ========================================================================== ##
## 1. Data
## ========================================================================== ##
Pdata <- read_csv("data/GDP_reg_panelData_V2.csv", show_col_types = FALSE) %>%
    arrange(iso, year) %>%
    mutate(tmp2 = tmp^2, pre2 = pre^2,
           tmp_pre = tmp*pre, tmp2_pre = tmp^2*pre,
           pre2_tmp = pre^2*tmp, tmp2_pre2 = tmp^2*pre^2)

n_iso <- n_distinct(Pdata$iso); n_year <- n_distinct(Pdata$year)
stopifnot(nrow(Pdata) == n_iso * n_year)

ttrend  <- diag(n_iso) %x% matrix(1:n_year, ncol = 1)
ttrend2 <- diag(n_iso) %x% matrix((1:n_year)^2, ncol = 1)
colnames(ttrend)  <- paste("T1", 1:n_iso, sep = "_")
colnames(ttrend2) <- paste("T2", 1:n_iso, sep = "_")
Pdata <- bind_cols(Pdata, as_tibble(ttrend), as_tibble(ttrend2))
extra <- c(colnames(ttrend), colnames(ttrend2))

regs_direct   <- c("tmp", "tmp2", "pre", "pre2")
regs_interact <- c(regs_direct, "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")

## ---- Bewley design ---------------------------------------------------------
## level terms keep their original names (their coefficients are the cumulative
## effects); difference terms are named d<j>_<var>.
build_bewley <- function(dat, regs, L) {
    if (L == 0) return(list(dat = dat, level = regs, diffs = character(0)))
    d <- dat %>% group_by(iso)
    diffs <- character(0)
    for (v in regs) {
        d <- d %>% mutate("d1_{v}" := .data[[v]] - dplyr::lag(.data[[v]], 1))
    }
    d <- d %>% ungroup()
    diffs <- c(diffs, paste0("d1_", regs))
    if (L >= 2) {
        d <- d %>% group_by(iso)
        for (j in 2:L) {
            for (v in regs) {
                d <- d %>% mutate("d{j}_{v}" := dplyr::lag(.data[[paste0("d1_", v)]], j - 1))
            }
            diffs <- c(diffs, paste0("d", j, "_", regs))
        }
        d <- d %>% ungroup()
    }
    list(dat = d, level = regs, diffs = diffs)
}

## ========================================================================== ##
## 2. Estimators
## ========================================================================== ##
run_afe <- function(dat, regs) {
    f <- as.formula(paste("logD_gdp ~", paste(c(regs, extra), collapse = " + ")))
    m <- plm(f, data = dat, index = c("iso", "year"),
             effect = "twoways", model = "within")
    V  <- vcovHC(m, type = "HC0", cluster = "group")
    ct <- coeftest(m, V)
    keep <- rownames(ct) %in% regs
    list(coefs = tibble(term = rownames(ct)[keep], estimate = ct[keep, 1],
                        std.error = ct[keep, 2], statistic = ct[keep, 3],
                        p.value = ct[keep, 4], nobs = nobs(m)),
         vcov = V[regs, regs, drop = FALSE])
}

run_ife <- function(dat, regs, nfac = NFAC, tol = IFE_TOL, maxit = IFE_MAXIT) {
    nvar <- length(regs)
    n <- n_distinct(dat$iso); t <- n_distinct(dat$year)

    M <- as.matrix(dat[, c("logD_gdp", regs, extra)])
    bad <- rowSums(is.na(M)) > 0
    M[bad, ] <- NA
    ok <- !bad; obs <- sum(ok)
    iso_i <- rep(1:n, each = t); year_i <- rep(1:t, times = n)
    tpcs <- as.vector(rowsum(as.numeric(ok), iso_i))

    M0 <- M; M0[bad, ] <- 0
    mean_i <- rowsum(M0, iso_i)  / as.vector(rowsum(as.numeric(ok), iso_i))
    mean_t <- rowsum(M0, year_i) / as.vector(rowsum(as.numeric(ok), year_i))
    mean_g <- colSums(M0) / obs
    A <- M - mean_i[iso_i, , drop = FALSE] - mean_t[year_i, , drop = FALSE] +
        matrix(mean_g, nrow = nrow(M), ncol = ncol(M), byrow = TRUE)

    XData <- A[, -1, drop = FALSE]; XData[is.na(XData)] <- 0
    YData <- A[,  1];               YData[is.na(YData)] <- 0
    k <- ncol(XData)

    Bai1 <- solve(crossprod(XData))
    Bai2 <- Bai1 %*% crossprod(XData, YData)
    resid  <- YData - XData %*% Bai2
    resid0 <- t(matrix(resid, nrow = t))
    eiv    <- eigen(crossprod(resid0) / obs)
    Fmat   <- sqrt(t) * eiv$vectors[, 1:nfac]
    Lambda <- t((t(Fmat) %*% t(resid0)) / t)
    LFact  <- as.vector(Fmat %*% t(Lambda))

    it <- 0
    repeat {
        bit <- Bai2
        YData0 <- YData - LFact; YData0[bad] <- 0
        Bai2   <- Bai1 %*% crossprod(XData, YData0)
        resid  <- YData - XData %*% Bai2
        resid1 <- t(matrix(resid, nrow = t))
        resid1[resid0 == 0] <- LFact[resid0 == 0]
        eiv    <- eigen(crossprod(resid1) / obs)
        Fmat   <- sqrt(t) * eiv$vectors[, 1:nfac]
        Lambda <- t((t(Fmat) %*% t(resid1)) / t)
        LFact  <- as.vector(Fmat %*% t(Lambda))
        it <- it + 1
        if (sqrt(sum((Bai2 - bit)^2)) <= tol || it >= maxit) break
    }

    mfh <- diag(t) - tcrossprod(Fmat) / t
    Aall <- A; Aall[is.na(Aall)] <- 0
    xpcar <- array(Aall[, -1, drop = FALSE], dim = c(t, n, k))
    for (j in seq_len(k)) xpcar[, , j] <- mfh %*% xpcar[, , j]
    aik <- Lambda %*% solve(crossprod(Lambda) / n) %*% t(Lambda)
    for (j in seq_len(k)) xpcar[, , j] <- xpcar[, , j] - (xpcar[, , j] %*% aik) / n
    ZData <- matrix(xpcar, nrow = n * t, ncol = k); ZData[bad, ] <- 0
    rm(xpcar, aik)

    insm <- array(0, dim = c(k, k, n))
    for (i in seq_len(n)) {
        idx <- ((i - 1) * t + 1):(i * t)
        insm[, , i] <- crossprod(ZData[idx, , drop = FALSE])
    }
    d0 <- apply(insm, c(1, 2), sum) / obs
    residf <- as.vector(YData - XData %*% Bai2 - LFact); residf[bad] <- 0
    eps2 <- vapply(seq_len(n), function(i) {
        idx <- ((i - 1) * t + 1):(i * t); sum(residf[idx]^2) }, numeric(1)) / tpcs
    d1 <- matrix(0, k, k)
    for (i in seq_len(n)) d1 <- d1 + insm[, , i] * eps2[i]
    d1 <- d1 / obs
    d0i <- solve(d0)
    Vfull <- d0i %*% d1 %*% d0i / obs
    se <- sqrt(diag(Vfull))
    b  <- as.vector(Bai2)[1:nvar]
    V  <- Vfull[1:nvar, 1:nvar, drop = FALSE]; dimnames(V) <- list(regs, regs)

    cat(sprintf("    IFE %d iters (k_climate = %d)\n", it, nvar))
    list(coefs = tibble(term = regs, estimate = b, std.error = se[1:nvar],
                        statistic = b/se[1:nvar],
                        p.value = 2*pnorm(-abs(b/se[1:nvar])), nobs = obs),
         vcov = V)
}

## ========================================================================== ##
## 3. Run the grid
## ========================================================================== ##
specs <- list(Direct = regs_direct, Interactive = regs_interact)
all_coefs <- list(); all_cum <- list(); all_tests <- list(); all_me <- list()
fits <- list()   # keep coefficients + vcov so tables can be rebuilt without refitting

## representative climate points from the estimation sample
Tq <- quantile(Pdata$tmp, c(.10, .50, .90), na.rm = TRUE)
Pm <- median(Pdata$pre, na.rm = TRUE)

## gradient of d(growth)/dT wrt the climate coefficient vector
grad_T <- function(Tv, Pv, interactive) {
    g <- c(1, 2*Tv, 0, 0)
    if (interactive) g <- c(g, Pv, 2*Tv*Pv, Pv^2, 2*Tv*Pv^2)
    g
}

for (sp in names(specs)) {
    regs <- specs[[sp]]
    interactive <- sp == "Interactive"
    for (L in LAGS) {
        bw <- build_bewley(Pdata, regs, L)
        allregs <- c(bw$level, bw$diffs)
        for (est in c("AFE", "IFE")) {
            cat(sprintf("  %-11s L=%d  %s\n", sp, L, est))
            fit <- if (est == "AFE") run_afe(bw$dat, allregs) else run_ife(bw$dat, allregs)

            tag <- sprintf("%s | L=%d | %s", sp, L, est)
            fits[[tag]] <- list(coefs = fit$coefs, vcov = fit$vcov, spec = sp,
                                L = L, estimator = est, level = bw$level,
                                interactive = interactive)
            all_coefs[[tag]] <- fit$coefs %>% mutate(spec = sp, L = L, estimator = est)

            ## cumulative effect = coefficient on the LEVEL terms
            cum <- fit$coefs %>% filter(term %in% bw$level)
            all_cum[[tag]] <- cum %>% mutate(spec = sp, L = L, estimator = est)

            ## joint Wald test: all cumulative climate coefficients = 0
            b <- cum$estimate
            V <- fit$vcov[bw$level, bw$level, drop = FALSE]
            W <- as.numeric(t(b) %*% solve(V) %*% b)
            all_tests[[tag]] <- tibble(
                spec = sp, L = L, estimator = est,
                wald_cum_zero = W, df = length(b),
                p_cum_zero = pchisq(W, length(b), lower.tail = FALSE),
                nobs = cum$nobs[1])

            ## cumulative marginal effect dg/dT at median P, three T quantiles
            all_me[[tag]] <- map_dfr(names(Tq), function(q) {
                g  <- grad_T(Tq[[q]], Pm, interactive)
                me <- sum(g * b)
                sm <- sqrt(as.numeric(t(g) %*% V %*% g))
                tibble(spec = sp, L = L, estimator = est,
                       T_pct = q, T_val = Tq[[q]], P_val = Pm,
                       dg_dT = me, se = sm, t = me/sm,
                       p.value = 2*pnorm(-abs(me/sm)))
            })
        }
    }
}

coefs <- bind_rows(all_coefs); cum <- bind_rows(all_cum)
tests <- bind_rows(all_tests); me  <- bind_rows(all_me)

## ---- ratio of cumulative to impact marginal effect -------------------------
ratio <- me %>%
    select(spec, estimator, T_pct, L, dg_dT) %>%
    pivot_wider(names_from = L, values_from = dg_dT, names_prefix = "L") %>%
    mutate(`L1/L0` = L1 / L0, `L2/L0` = L2 / L0)

## ========================================================================== ##
## 4. Export
## ========================================================================== ##
saveRDS(fits, file.path(out_dir, "lagged_climate_fits.rds"))
write_csv(coefs, file.path(out_dir, "lagged_climate_coefficients.csv"))
write_csv(cum,   file.path(out_dir, "lagged_climate_cumulative.csv"))
write_csv(tests, file.path(out_dir, "lagged_climate_tests.csv"))
write_csv(me,    file.path(out_dir, "lagged_climate_marginal_effects.csv"))
write_csv(ratio, file.path(out_dir, "lagged_climate_ratio.csv"))

sink(file.path(out_dir, "lagged_climate.txt"))
cat("GROWTH EFFECTS vs LEVEL EFFECTS: distributed lag in climate\n")
cat("===========================================================\n\n")
cat("Bewley form: coefficient on the level term = cumulative effect sum_j beta_j\n")
cat("L = 0 is the contemporaneous-only baseline (impact effect).\n")
cat("sum_j beta_j ~ beta_0 -> growth effect;  sum_j beta_j ~ 0 -> level effect\n\n")

cat("--- Joint Wald test: all cumulative climate coefficients = 0 ---\n")
print(as.data.frame(tests %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

cat("\n\n--- Cumulative marginal effect  d(growth)/dT  at median P ---\n")
cat(sprintf("    (median P = %.3f m/yr; T at 10th/50th/90th pct = %.1f / %.1f / %.1f C)\n\n",
            Pm, Tq[[1]], Tq[[2]], Tq[[3]]))
print(as.data.frame(me %>% mutate(across(where(is.numeric), ~round(.x, 5)))),
      row.names = FALSE)

cat("\n\n--- Cumulative / impact ratio of dg/dT (L1/L0, L2/L0) ---\n")
cat("    ~1 => growth effect persists;  ~0 => level effect (rebound)\n\n")
print(as.data.frame(ratio %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

cat("\n\n--- Cumulative coefficients (level terms) ---\n")
print(as.data.frame(cum %>% mutate(across(where(is.numeric), ~signif(.x, 4)))),
      row.names = FALSE)
sink()

cat("\n--- Wald tests ---\n"); print(as.data.frame(tests), row.names = FALSE)
cat("\n--- dg/dT ---\n");      print(as.data.frame(me), row.names = FALSE)
cat("\n--- ratios ---\n");     print(as.data.frame(ratio), row.names = FALSE)
cat("\nWritten to output/lagged_climate_*.csv and lagged_climate.txt\n")
