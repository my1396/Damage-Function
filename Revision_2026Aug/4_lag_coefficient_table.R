## =============================================================================
## Wide table of the ORIGINAL distributed-lag coefficients.
##
## 3_lagged_climate.R estimates the reparameterized form
##     sum_j beta_j x_{t-j} = c_0 x_t + sum_{j=1}^{L} c_j dx_{t-j+1}
## where c_0 = sum_j beta_j and c_j = -(sum_{k>=j} beta_k).
##
## Inverting:
##     beta_L = -c_L
##     beta_j = -c_j + c_{j+1}      for 1 <= j < L
##     beta_0 =  c_0 + c_1
##     sum_j beta_j = c_0
##
## Each is a linear combination of the c's, so standard errors follow from
## g' V g using the covariance block for that variable.
##
## In : output/lagged_climate_fits.rds   (written by 3_lagged_climate.R)
## Out: output/lag_coefficients_wide.csv / .txt
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
out_dir  <- file.path(root_dir, "Revision_2026Aug", "output")

fits <- readRDS(file.path(out_dir, "lagged_climate_fits.rds"))

term_labels <- c(tmp = "T", tmp2 = "T^2", pre = "P", pre2 = "P^2",
                 tmp_pre = "T x P", tmp2_pre = "T^2 x P",
                 pre2_tmp = "T x P^2", tmp2_pre2 = "T^2 x P^2")
term_order  <- names(term_labels)

star <- function(p) ifelse(is.na(p), "",
                    ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                    ifelse(p < 0.10, "*", ""))))

## ---- recover beta_j for one fit --------------------------------------------
recover <- function(f) {
    b <- setNames(f$coefs$estimate, f$coefs$term)
    V <- f$vcov
    L <- f$L
    map_dfr(f$level, function(v) {
        ## names of the c coefficients belonging to this variable
        cn <- c(v, if (L >= 1) paste0("d", seq_len(L), "_", v) else character(0))
        cn <- cn[cn %in% names(b)]
        cc <- b[cn]
        Vc <- V[cn, cn, drop = FALSE]
        nC <- length(cc)   # = L + 1

        ## gradients over (c_0, c_1, ..., c_L)
        grads <- list()
        grads[["sum"]] <- c(1, rep(0, nC - 1))
        if (L == 0) {
            grads[["b0"]] <- c(1)
        } else {
            g0 <- rep(0, nC); g0[1] <- 1; g0[2] <- 1
            grads[["b0"]] <- g0
            if (L >= 2) {
                for (j in 1:(L - 1)) {
                    gj <- rep(0, nC); gj[j + 1] <- -1; gj[j + 2] <- 1
                    grads[[paste0("b", j)]] <- gj
                }
            }
            gL <- rep(0, nC); gL[L + 1] <- -1
            grads[[paste0("b", L)]] <- gL
        }

        map_dfr(names(grads), function(nm) {
            g  <- grads[[nm]]
            est <- sum(g * cc)
            se  <- sqrt(as.numeric(t(g) %*% Vc %*% g))
            tibble(variable = v, coef = nm, estimate = est, std.error = se,
                   p.value = 2 * pnorm(-abs(est / se)))
        })
    }) %>% mutate(spec = f$spec, L = f$L, estimator = f$estimator)
}

res <- map_dfr(fits, recover)

write_csv(res, file.path(out_dir, "lag_coefficients_long.csv"))

## ---- wide table, one per specification -------------------------------------
coef_order <- c(paste0("b", 0:5), "sum")
coef_lab   <- c(b0 = "beta_0 (t)",   b1 = "beta_1 (t-1)", b2 = "beta_2 (t-2)",
                b3 = "beta_3 (t-3)", b4 = "beta_4 (t-4)", b5 = "beta_5 (t-5)",
                sum = "SUM  (cumulative)")

build_wide <- function(sp) {
    d <- res %>%
        filter(spec == sp) %>%
        mutate(model = sprintf("%s L=%d", estimator, L),
               cell  = sprintf("%.5f%s", estimate, star(p.value)),
               secel = sprintf("(%.5f)", std.error))

    model_order <- c(paste0("AFE L=", 0:5), paste0("IFE L=", 0:5))

    d %>%
        select(variable, coef, model, cell, secel) %>%
        pivot_longer(c(cell, secel), names_to = "kind", values_to = "val") %>%
        mutate(kind = factor(kind, levels = c("cell", "secel")),
               variable = factor(variable, levels = term_order),
               coef = factor(coef, levels = coef_order),
               model = factor(model, levels = model_order)) %>%
        arrange(variable, coef, kind) %>%
        pivot_wider(names_from = model, values_from = val) %>%
        mutate(Variable = ifelse(kind == "cell", term_labels[as.character(variable)], ""),
               Coef     = ifelse(kind == "cell", coef_lab[as.character(coef)], "")) %>%
        select(Variable, Coef, any_of(model_order)) %>%
        mutate(across(everything(), ~replace_na(.x, "")))
}

wide_dir <- build_wide("Direct")
wide_int <- build_wide("Interactive")

write_csv(wide_dir, file.path(out_dir, "lag_coefficients_wide_direct.csv"))
write_csv(wide_int, file.path(out_dir, "lag_coefficients_wide_interactive.csv"))
write_csv(bind_rows(wide_dir %>% mutate(Spec = "Direct"),
                    wide_int %>% mutate(Spec = "Interactive")) %>%
              select(Spec, everything()),
          file.path(out_dir, "lag_coefficients_wide.csv"))

sink(file.path(out_dir, "lag_coefficients_wide.txt"))
cat("DISTRIBUTED-LAG COEFFICIENTS IN THE ORIGINAL PARAMETERIZATION\n")
cat("=============================================================\n\n")
cat("Model:  y_t = sum_{j=0}^{L} beta_j' x_{t-j} + country/year FE + country trends\n")
cat("beta_0 = contemporaneous, beta_1 = one-year lag, beta_2 = two-year lag\n")
cat("SUM    = cumulative effect (this is what the projection scales with)\n\n")
cat("AFE = additive country+year FE, HC0 SE clustered by country\n")
cat("IFE = interactive FE, Bai (2009), 4 factors\n")
cat("Significance: * p<0.10  ** p<0.05  *** p<0.01\n")
cat("Standard errors in parentheses (delta method).\n\n")
cat("\n############ DIRECT SPECIFICATION ############\n\n")
print(as.data.frame(wide_dir), row.names = FALSE)
cat("\n\n############ INTERACTIVE SPECIFICATION ############\n\n")
print(as.data.frame(wide_int), row.names = FALSE)
sink()

cat("\n############ DIRECT ############\n")
print(as.data.frame(wide_dir), row.names = FALSE)
cat("\n############ INTERACTIVE ############\n")
print(as.data.frame(wide_int), row.names = FALSE)
cat("\nWritten to output/lag_coefficients_wide.csv / .txt (+ per-spec csv, long csv)\n")
