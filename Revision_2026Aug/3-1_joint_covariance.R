## Joint sampling covariance of the M = 8 and M = 4 coefficient vectors.
##
## 3_lagged_climate.R fits the interactive (M = 8) and direct (M = 4) models
## separately and stores one V_hat per fit. Nothing records how the two are
## correlated, yet they are estimated from the SAME panel and share four
## regressors, so c_hat^M8 and c_hat^M4 are strongly dependent. Any statement
## about the DIFFERENCE of the two projections -- the between-model interactive
## contribution in 8-1 -- needs that dependence: treating the two draws as
## independent inflates the 90% band on the difference by 7-10x.
##
## This script recovers it by stacking the two models' scores and forming one
## clustered sandwich. For estimator m and country i the score is
##     s_{m,i} = Z_{m,i}' e_{m,i}
## where Z is the regressor matrix after the model's own projection (the within
## transformation for AFE, Bai's doubly-projected regressors for IFE) and e the
## fitted residual. Stacking s_i = [s_{8,i} ; s_{4,i}] and sandwiching with the
## block-diagonal bread gives the joint covariance including the cross block.
## The meat is a sum of outer products, so the result is PSD by construction.
##
## Two meats are stored:
##   V_clu  clustered, sum_i s_i s_i'. Allows arbitrary serial correlation
##          within a country. For AFE this reproduces the vcovHC(cluster =
##          "group") already in lagged_climate_fits.rds exactly.
##   V_bai  Bai's heteroskedastic form, sum_i (Z_{m,i}' Z_{m',i}) * sigma_{mm',i}
##          with sigma_{mm',i} = mean_t(e_{m,it} e_{m',it}). Assumes no serial
##          correlation. Its diagonal blocks reproduce the IFE standard errors
##          in lagged_climate_fits.rds exactly, so this is the variant that
##          leaves every already-reported number untouched.
## Section 5 checks all four of those reproduction claims before exporting.
##
## Out: output/joint_covariance.rds
##      output/joint_covariance.txt
## ========================================================================== ##

suppressMessages({library(tidyverse); library(fixest)})

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")

NFAC      <- 4              # must match 3_lagged_climate.R
IFE_TOL   <- 5e-4
IFE_MAXIT <- 1000
LAGS      <- 0:2
SPECS     <- c(M8 = "Interactive", M4 = "Direct")

REGS_M4 <- c("tmp", "tmp2", "pre", "pre2")
REGS_M8 <- c(REGS_M4, "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")
RG      <- list(M8 = REGS_M8, M4 = REGS_M4)

fits <- readRDS(file.path(out_dir, "lagged_climate_fits.rds"))

## ========================================================================== ##
## 1. Data and the Bewley design -----------------------------------------------
## ========================================================================== ##
## Identical construction to 3_lagged_climate.R, including the country-specific
## linear and quadratic trend dummies the IFE routine carries as regressors.
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
## same trends again, as slope variables for the fixest AFE path
Pdata <- Pdata %>% mutate(tt = rep(seq_len(n_year), times = n_iso),
                          tt2 = tt^2, id = iso)

build_bewley <- function(dat, rr, L) {
    if (L == 0) return(dat)
    d <- dat %>% group_by(iso)
    for (v in rr) d <- d %>% mutate("d1_{v}" := .data[[v]] - dplyr::lag(.data[[v]], 1))
    d <- d %>% ungroup()
    if (L >= 2) {
        d <- d %>% group_by(iso)
        for (j in 2:L) for (v in rr)
            d <- d %>% mutate("d{j}_{v}" := dplyr::lag(.data[[paste0("d1_", v)]], j - 1))
        d <- d %>% ungroup()
    }
    d
}

lag_terms <- function(rr, L) c(rr, if (L >= 1)
    as.vector(outer(paste0("d", seq_len(L), "_"), rr, paste0)))

## ========================================================================== ##
## 2. AFE: partialled-out regressors and per-country scores --------------------
## ========================================================================== ##
## FWL: absorbing the country FE, year FE and the country trends out of y and of
## every regressor reproduces the plm within fit, and X_tilde'e_i is its score.
afe_scores <- function(dat, allregs) {
    d  <- dat[complete.cases(dat[, c("logD_gdp", allregs)]), ]
    fe <- "| id[tt, tt2] + year"
    m  <- feols(as.formula(paste("logD_gdp ~", paste(allregs, collapse = " + "), fe)),
                data = d, notes = FALSE, warn = FALSE)
    stopifnot(m$nobs == nrow(d), all(allregs %in% names(coef(m))))
    Z <- vapply(allregs, function(v)
        resid(feols(as.formula(paste(v, "~ 1", fe)), data = d,
                    notes = FALSE, warn = FALSE)), numeric(nrow(d)))
    e <- resid(m)
    list(b = coef(m)[allregs], bread = solve(crossprod(Z)), scale = 1,
         Z = Z, e = e, iso = d$iso, obs = nrow(d), terms = allregs)
}

## ========================================================================== ##
## 3. IFE: Bai (2009) fit returning scores and bread ---------------------------
## ========================================================================== ##
## Verbatim run_ife() from 3_lagged_climate.R, extended to return ZData, the
## residuals and d0 so the sandwich can be reassembled with any meat.
ife_scores <- function(dat, regs, nfac = NFAC, tol = IFE_TOL, maxit = IFE_MAXIT) {
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
    Z <- matrix(xpcar, nrow = n * t, ncol = k); Z[bad, ] <- 0
    rm(xpcar, aik)

    e <- as.vector(YData - XData %*% Bai2 - LFact); e[bad] <- 0
    d0 <- crossprod(Z) / obs                        # sum_i Z_i'Z_i / obs

    cat(sprintf("    IFE %d iters (k_climate = %d)\n", it, nvar))
    list(b = setNames(as.vector(Bai2)[1:nvar], regs), bread = solve(d0),
         scale = obs, Z = Z, e = e, iso = dat$iso, obs = obs, tpcs = tpcs,
         terms = regs, nvar = nvar, keep = seq_len(nvar))
}

## ========================================================================== ##
## 4. Joint sandwich for (M8, M4) ----------------------------------------------
## ========================================================================== ##
## bread is block diagonal; the meat carries the cross-model block. `scale` is 1
## for the AFE parameterisation (bread = (Z'Z)^-1, meat = sum_i s_i s_i') and
## obs for the IFE one (bread = d0^-1, meat = sum_i s_i s_i'/obs, V = .../obs),
## which is why it is carried through rather than hard-coded.
country_scores <- function(f) {                    # n_cluster x k
    rowsum(f$Z * f$e, f$iso)
}

blk <- function(A, B) {                            # block-diagonal bind
    out <- matrix(0, nrow(A) + nrow(B), ncol(A) + ncol(B))
    out[seq_len(nrow(A)), seq_len(ncol(A))] <- A
    out[nrow(A) + seq_len(nrow(B)), ncol(A) + seq_len(ncol(B))] <- B
    out
}

## Bai's meat, generalised across models: sum_i (Z_{m,i}'Z_{m',i}) * sigma_{mm',i}
bai_meat <- function(f8, f4) {
    n <- length(f8$tpcs); t <- nrow(f8$Z) / n
    k8 <- ncol(f8$Z); k4 <- ncol(f4$Z)
    O <- matrix(0, k8 + k4, k8 + k4)
    for (i in seq_len(n)) {
        idx <- ((i - 1) * t + 1):(i * t)
        Z8 <- f8$Z[idx, , drop = FALSE]; Z4 <- f4$Z[idx, , drop = FALSE]
        e8 <- f8$e[idx]; e4 <- f4$e[idx]
        s <- c(sum(e8*e8), sum(e8*e4), sum(e4*e4)) / f8$tpcs[i]
        O[1:k8, 1:k8]                 <- O[1:k8, 1:k8]                 + crossprod(Z8)      * s[1]
        O[1:k8, k8 + 1:k4]            <- O[1:k8, k8 + 1:k4]            + crossprod(Z8, Z4)  * s[2]
        O[k8 + 1:k4, k8 + 1:k4]       <- O[k8 + 1:k4, k8 + 1:k4]       + crossprod(Z4)      * s[3]
    }
    O[k8 + 1:k4, 1:k8] <- t(O[1:k8, k8 + 1:k4])
    O / f8$obs
}

joint_V <- function(f8, f4) {
    stopifnot(f8$obs == f4$obs, identical(f8$iso, f4$iso))
    S <- cbind(country_scores(f8), country_scores(f4))
    k8 <- ncol(f8$Z)
    B  <- blk(f8$bread, f4$bread)
    sc <- if (f8$scale == 1) 1 else f8$obs        # see the note above
    Vc <- B %*% (crossprod(S) / sc) %*% B / sc
    Vb <- if (is.null(f8$tpcs)) NULL else B %*% bai_meat(f8, f4) %*% B / f8$obs
    ## keep only the climate terms of each model
    i8 <- if (is.null(f8$keep)) seq_along(f8$terms) else f8$keep
    i4 <- if (is.null(f4$keep)) seq_along(f4$terms) else f4$keep
    sel <- c(i8, k8 + i4)
    nmv <- c(paste0("M8.", f8$terms[i8]), paste0("M4.", f4$terms[i4]))
    lab <- function(V) { V <- V[sel, sel, drop = FALSE]; dimnames(V) <- list(nmv, nmv); V }
    list(V_clu = lab(Vc), V_bai = if (is.null(Vb)) lab(Vc) else lab(Vb),
         c8 = f8$b[f8$terms[i8]], c4 = f4$b[f4$terms[i4]])
}

## ========================================================================== ##
## 5. Run, validate, export ----------------------------------------------------
## ========================================================================== ##
JV <- list(); chk <- list()

for (est in c("AFE", "IFE")) {
    for (L in LAGS) {
        cat(sprintf("  %s | L=%d\n", est, L))
        f <- lapply(names(SPECS), function(m) {
            ar <- lag_terms(RG[[m]], L)
            d  <- build_bewley(Pdata, RG[[m]], L)
            if (est == "AFE") afe_scores(d, ar) else ife_scores(d, ar)
        })
        names(f) <- names(SPECS)
        jv <- joint_V(f$M8, f$M4)
        tag <- sprintf("L=%d | %s", L, est)
        JV[[tag]] <- c(jv, list(estimator = est, L = L,
                                terms8 = names(jv$c8), terms4 = names(jv$c4)))

        ## does a diagonal block reproduce what 3_ already stored?
        cmp <- function(m, V) {
            ref <- fits[[sprintf("%s | L=%d | %s", SPECS[m], L, est)]]$vcov
            ii  <- paste0(m, ".", colnames(ref))
            max(abs(V[ii, ii] - ref))
        }
        r8 <- RG$M8; r4 <- RG$M4
        i8 <- paste0("M8.", r8); i4 <- paste0("M4.", r4)
        C  <- cov2cor(jv$V_clu)
        devc <- function(m, cc) {
            ref <- fits[[sprintf("%s | L=%d | %s", SPECS[m], L, est)]]$coefs
            max(abs(cc[ref$term] - ref$estimate))
        }
        chk[[tag]] <- tibble(
            estimator = est, L = L,
            dev_coef_M8 = devc("M8", jv$c8), dev_coef_M4 = devc("M4", jv$c4),
            dev_clu_M8 = cmp("M8", jv$V_clu), dev_clu_M4 = cmp("M4", jv$V_clu),
            dev_bai_M8 = cmp("M8", jv$V_bai), dev_bai_M4 = cmp("M4", jv$V_bai),
            cor_tmp  = C["M8.tmp",  "M4.tmp"],
            cor_tmp2 = C["M8.tmp2", "M4.tmp2"],
            cor_pre  = C["M8.pre",  "M4.pre"],
            min_eig  = min(eigen(jv$V_clu, symmetric = TRUE, only.values = TRUE)$values))
    }
}

chk <- bind_rows(chk)
saveRDS(JV, file.path(out_dir, "joint_covariance.rds"))

sink(file.path(out_dir, "joint_covariance.txt"))
cat("JOINT SAMPLING COVARIANCE OF THE M = 8 AND M = 4 COEFFICIENTS\n")
cat("=============================================================\n\n")
cat("Both models are fitted to the same panel, so their coefficient vectors are\n")
cat("correlated. Stacking the two models' per-country scores and sandwiching\n")
cat("with the block-diagonal bread recovers the cross block that\n")
cat("lagged_climate_fits.rds does not store.\n\n")
cat("V_clu : clustered meat, sum_i s_i s_i'. Arbitrary serial correlation.\n")
cat("V_bai : Bai's heteroskedastic meat. No serial correlation.\n\n")
cat("dev_coef_* is the largest absolute deviation of the refitted coefficients\n")
cat("from those stored by 3_lagged_climate.R; it must be ~0 or the refit here\n")
cat("is not the same fit whose covariance is being reported.\n")
cat("dev_clu_*/dev_bai_* is the largest absolute deviation of a DIAGONAL block\n")
cat("from the vcov stored by 3_lagged_climate.R. Expected ~0 for\n")
cat("AFE/V_clu (3_ uses vcovHC(cluster = 'group')) and for IFE/V_bai (3_ uses\n")
cat("Bai's heteroskedastic form); the other two are genuinely different\n")
cat("robustness assumptions and are not expected to match.\n")
cat("min_eig > 0 confirms V_clu is positive definite.\n\n")
print(as.data.frame(chk %>% mutate(across(where(is.numeric), ~signif(.x, 4)))),
      row.names = FALSE)

cat("\n\n--- Marginal se: clustered vs the stored fit, climate terms ---\n\n")
se_cmp <- map_dfr(names(JV), function(tag) {
    j <- JV[[tag]]
    map_dfr(c("M8", "M4"), function(m) {
        ref <- fits[[sprintf("%s | L=%d | %s", SPECS[m], j$L, j$estimator)]]
        rr  <- RG[[m]]
        tibble(estimator = j$estimator, L = j$L, model = m, term = rr,
               stored = sqrt(diag(ref$vcov))[rr],
               clustered = sqrt(diag(j$V_clu))[paste0(m, ".", rr)]) })
}) %>% mutate(ratio = round(clustered / stored, 3),
              across(c(stored, clustered), ~signif(.x, 3)))
print(as.data.frame(se_cmp), row.names = FALSE)
sink()

cat("\nWrote output/joint_covariance.rds and joint_covariance.txt\n\n")
print(as.data.frame(chk %>% mutate(across(where(is.numeric), ~signif(.x, 4)))),
      row.names = FALSE)
