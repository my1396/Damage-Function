## =============================================================================
## Four-model comparison: Additive FE vs Interactive FE (Bai 2009)
##   x  Direct climate terms only  vs  + temperature-precipitation interactions
##
## Data : data/GDP_reg_panelData_V2.csv  (122 countries x 59 years, balanced)
## Out  : output/
##
## The Bai (2009) principal-components algorithm is a refactor of
## IFE_model/1_interactive_FE_regression.R -- same estimator, wrapped in a
## function so it can be called with either regressor set. Deterministic
## index alignment (match/rowsum) replaces the merge() calls, which relied on
## merge preserving row order.
## =============================================================================

library(plyr)       # load BEFORE tidyverse so dplyr masks it, not the reverse
suppressMessages(library(tidyverse))
library(plm)
library(lmtest)
library(sandwich)

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- controls ---------------------------------------------------------------
NFAC          <- 4      # number of interactive factors (as in the original script)
INCLUDE_TREND <- TRUE    # country-specific linear + quadratic trends in ALL models
IFE_TOL       <- 5e-4
IFE_MAXIT     <- 1000

## =============================================================================
## 1. Data
## =============================================================================
Pdata <- read_csv("data/GDP_reg_panelData_V2.csv", show_col_types = FALSE) %>%
    arrange(iso, year)

## interaction terms (not present in V2, must be constructed)
Pdata <- Pdata %>%
    mutate(
        tmp2      = tmp^2,
        pre2      = pre^2,
        tmp_pre   = tmp * pre,        # T  * P
        tmp2_pre  = tmp^2 * pre,      # T^2 * P
        pre2_tmp  = pre^2 * tmp,      # T  * P^2
        tmp2_pre2 = tmp^2 * pre^2     # T^2 * P^2
    )

n_iso  <- n_distinct(Pdata$iso)
n_year <- n_distinct(Pdata$year)
stopifnot(nrow(Pdata) == n_iso * n_year)   # balanced & iso-major sorted

cat(sprintf("Panel: N = %d countries, T = %d years, rows = %d, usable obs = %d\n",
            n_iso, n_year, nrow(Pdata), sum(!is.na(Pdata$logD_gdp))))

## country-specific linear & quadratic trends
ttrend  <- diag(n_iso) %x% matrix(1:n_year, ncol = 1)
ttrend2 <- diag(n_iso) %x% matrix((1:n_year)^2, ncol = 1)
colnames(ttrend)  <- paste("T1", 1:n_iso, sep = "_")
colnames(ttrend2) <- paste("T2", 1:n_iso, sep = "_")
trend_names <- c(colnames(ttrend), colnames(ttrend2))

Pdata <- bind_cols(Pdata, as_tibble(ttrend), as_tibble(ttrend2))

regs_direct   <- c("tmp", "tmp2", "pre", "pre2")
regs_interact <- c("tmp", "tmp2", "pre", "pre2",
                   "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")

extra <- if (INCLUDE_TREND) trend_names else character(0)

## =============================================================================
## 2. Additive fixed effects (country + year), within estimator
## =============================================================================
run_afe <- function(dat, regs) {
    f <- as.formula(paste("logD_gdp ~", paste(c(regs, extra), collapse = " + ")))
    m <- plm(f, data = dat, index = c("iso", "year"),
             effect = "twoways", model = "within")
    V  <- vcovHC(m, type = "HC0", cluster = "group")
    ct <- coeftest(m, V)
    keep <- rownames(ct) %in% regs
    list(
        coefs = tibble(
            term      = rownames(ct)[keep],
            estimate  = ct[keep, 1],
            std.error = ct[keep, 2],
            statistic = ct[keep, 3],
            p.value   = ct[keep, 4],
            nobs      = nobs(m)
        ),
        vcov = V[regs, regs, drop = FALSE]
    )
}

## =============================================================================
## 3. Interactive fixed effects, Bai (2009) iterated PC estimator
## =============================================================================
run_ife <- function(dat, regs, nfac = NFAC, tol = IFE_TOL, maxit = IFE_MAXIT) {

    nvar <- length(regs)
    n    <- n_distinct(dat$iso)
    t    <- n_distinct(dat$year)

    ## design: [ gdp | regressors | trends ]; rows iso-major, year-minor
    M <- as.matrix(dat[, c("logD_gdp", regs, extra)])
    ## propagate missingness across the whole row (as in the original script)
    bad <- rowSums(is.na(M)) > 0
    M[bad, ] <- NA

    ok   <- !bad
    obs  <- sum(ok)
    iso_i  <- rep(1:n, each = t)
    year_i <- rep(1:t, times = n)
    tpcs <- as.vector(rowsum(as.numeric(ok), iso_i))   # obs per country

    ## ---- two-way demeaning (na.rm), matching numcolwise(mean, na.rm=TRUE) ----
    M0 <- M; M0[bad, ] <- 0
    cnt_i  <- as.vector(rowsum(as.numeric(ok), iso_i))
    cnt_t  <- as.vector(rowsum(as.numeric(ok), year_i))
    mean_i <- rowsum(M0, iso_i)  / cnt_i
    mean_t <- rowsum(M0, year_i) / cnt_t
    mean_g <- colSums(M0) / obs

    A <- M - mean_i[iso_i, , drop = FALSE] - mean_t[year_i, , drop = FALSE] +
        matrix(mean_g, nrow = nrow(M), ncol = ncol(M), byrow = TRUE)

    XData <- A[, -1, drop = FALSE]; XData[is.na(XData)] <- 0
    YData <- A[,  1];               YData[is.na(YData)] <- 0
    k <- ncol(XData)

    ## ---- initial (within) estimate + first factor extraction ----------------
    Bai1  <- solve(crossprod(XData))
    Bai2  <- Bai1 %*% crossprod(XData, YData)
    resid <- YData - XData %*% Bai2
    resid0 <- t(matrix(resid, nrow = t))               # N x T
    eiv    <- eigen(crossprod(resid0) / obs)           # T x T
    Fmat   <- sqrt(t) * eiv$vectors[, 1:nfac]          # T x r
    Lambda <- t((t(Fmat) %*% t(resid0)) / t)           # N x r
    LFact  <- as.vector(Fmat %*% t(Lambda))            # NT x 1 (iso-major)

    ## ---- iterate to convergence --------------------------------------------
    it <- 0
    repeat {
        bit    <- Bai2
        YData0 <- YData - LFact
        YData0[bad] <- 0
        Bai2   <- Bai1 %*% crossprod(XData, YData0)
        resid  <- YData - XData %*% Bai2
        resid1 <- t(matrix(resid, nrow = t))
        resid1[resid0 == 0] <- LFact[resid0 == 0]      # fill gaps with pcs
        eiv    <- eigen(crossprod(resid1) / obs)
        Fmat   <- sqrt(t) * eiv$vectors[, 1:nfac]
        Lambda <- t((t(Fmat) %*% t(resid1)) / t)
        LFact  <- as.vector(Fmat %*% t(Lambda))
        it     <- it + 1
        if (sqrt(sum((Bai2 - bit)^2)) <= tol || it >= maxit) break
    }
    cat(sprintf("  IFE converged in %d iterations (nvar = %d, r = %d)\n",
                it, nvar, nfac))

    ## ---- standard errors, Bai (2009) pp. 1251-1252 -------------------------
    mfh <- diag(t) - tcrossprod(Fmat) / t              # M_F, T x T

    Aall <- A; Aall[is.na(Aall)] <- 0
    xpcar <- array(Aall[, -1, drop = FALSE], dim = c(t, n, k))
    for (j in seq_len(k)) xpcar[, , j] <- mfh %*% xpcar[, , j]   # defactor

    aik <- Lambda %*% solve(crossprod(Lambda) / n) %*% t(Lambda) # N x N
    for (j in seq_len(k)) xpcar[, , j] <- xpcar[, , j] - (xpcar[, , j] %*% aik) / n
    ZData <- matrix(xpcar, nrow = n * t, ncol = k)
    ZData[bad, ] <- 0
    rm(xpcar, aik)

    insm <- array(0, dim = c(k, k, n))
    for (i in seq_len(n)) {
        idx <- ((i - 1) * t + 1):(i * t)
        insm[, , i] <- crossprod(ZData[idx, , drop = FALSE])
    }
    d0 <- apply(insm, c(1, 2), sum) / obs

    residf <- as.vector(YData - XData %*% Bai2 - LFact)
    residf[bad] <- 0
    eps2 <- vapply(seq_len(n), function(i) {
        idx <- ((i - 1) * t + 1):(i * t); sum(residf[idx]^2)
    }, numeric(1)) / tpcs

    d1 <- matrix(0, k, k)
    for (i in seq_len(n)) d1 <- d1 + insm[, , i] * eps2[i]
    d1 <- d1 / obs

    d0i <- solve(d0)
    Vfull <- d0i %*% d1 %*% d0i / obs
    se  <- sqrt(diag(Vfull))

    b <- as.vector(Bai2)[1:nvar]
    V <- Vfull[1:nvar, 1:nvar, drop = FALSE]
    dimnames(V) <- list(regs, regs)

    list(
        coefs = tibble(
            term      = regs,
            estimate  = b,
            std.error = se[1:nvar],
            statistic = b / se[1:nvar],
            p.value   = 2 * pnorm(-abs(b / se[1:nvar])),
            nobs      = obs
        ),
        vcov = V
    )
}

## =============================================================================
## 4. Run all four
## =============================================================================
cat("\nEstimating...\n")
fits <- list(
    `M1: AFE-Direct`   = run_afe(Pdata, regs_direct),
    `M2: AFE-Interact` = run_afe(Pdata, regs_interact),
    `M3: IFE-Direct`   = run_ife(Pdata, regs_direct),
    `M4: IFE-Interact` = run_ife(Pdata, regs_interact)
)

res <- imap(fits, ~ .x$coefs %>% mutate(model = .y)) %>% bind_rows()

## =============================================================================
## 5. Comparison table
## =============================================================================
star <- function(p) ifelse(is.na(p), "",
                    ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                    ifelse(p < 0.10, "*", ""))))

term_order  <- c("tmp", "tmp2", "pre", "pre2",
                 "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")
term_labels <- c(tmp = "T", tmp2 = "T^2", pre = "P", pre2 = "P^2",
                 tmp_pre = "T x P", tmp2_pre = "T^2 x P",
                 pre2_tmp = "T x P^2", tmp2_pre2 = "T^2 x P^2")
model_order <- c("M1: AFE-Direct", "M2: AFE-Interact",
                 "M3: IFE-Direct", "M4: IFE-Interact")

res <- res %>%
    mutate(term  = factor(term, levels = term_order),
           model = factor(model, levels = model_order)) %>%
    arrange(model, term)

## long, full precision
write_csv(res %>% mutate(term = as.character(term), model = as.character(model)),
          file.path(out_dir, "model_comparison_long.csv"))

## wide display table: coefficient with stars, SE beneath
cell <- res %>%
    transmute(model, term,
              b  = sprintf("%.5f%s", estimate, star(p.value)),
              se = sprintf("(%.5f)", std.error))

wide <- cell %>%
    pivot_longer(c(b, se), names_to = "kind", values_to = "val") %>%
    mutate(kind = factor(kind, levels = c("b", "se"))) %>%
    arrange(term, kind) %>%
    pivot_wider(names_from = model, values_from = val) %>%
    mutate(Variable = ifelse(kind == "b", term_labels[as.character(term)], "")) %>%
    select(Variable, all_of(model_order)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))

nobs_row <- res %>% group_by(model) %>% summarise(n = first(nobs), .groups = "drop") %>%
    pivot_wider(names_from = model, values_from = n) %>%
    mutate(Variable = "Observations") %>% select(Variable, all_of(model_order)) %>%
    mutate(across(-Variable, as.character))

wide <- bind_rows(wide, nobs_row)

write_csv(wide, file.path(out_dir, "model_comparison_table.csv"))

## readable text version
sink(file.path(out_dir, "model_comparison.txt"))
cat("Four-model comparison: climate effects on GDP per-capita growth\n")
cat("Data: GDP_reg_panelData_V2.csv | N =", n_iso, "countries, T =", n_year, "years\n")
cat("AFE = additive country + year fixed effects (within estimator), HC0 SE clustered by country\n")
cat("IFE = interactive fixed effects, Bai (2009) iterated PC,", NFAC, "factors, Bai (2009) SE\n")
cat("Country-specific linear + quadratic trends included in all models:", INCLUDE_TREND, "\n")
cat("Significance: * p<0.10  ** p<0.05  *** p<0.01\n\n")
print(as.data.frame(wide), row.names = FALSE)
cat("\n\n--- point estimates side by side ---\n")
print(as.data.frame(
    res %>% select(model, term, estimate) %>%
        pivot_wider(names_from = model, values_from = estimate)
), row.names = FALSE)
sink()

cat("\n")
print(as.data.frame(wide), row.names = FALSE)
cat("\nWritten to:", out_dir, "\n")
cat("  model_comparison_long.csv   (tidy, full precision)\n")
cat("  model_comparison_table.csv  (4-column display table)\n")
cat("  model_comparison.txt        (formatted)\n")
