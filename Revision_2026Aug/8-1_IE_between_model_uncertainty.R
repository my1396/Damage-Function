## Bootstrap uncertainty for the BETWEEN-MODEL interactive contribution.
##
##     IE^between_t = delta^M8_t - delta^M4_t
##
## the difference between the two separately estimated models plotted in
## 8_plot_global_path.R: M = 8 adds the four T x P interactions to the M = 4
## direct terms. Contrast 9-2, which measures the WITHIN-model contribution by
## zeroing the interaction coefficients inside the M = 8 fit -- a counterfactual
## built from coefficients never estimated without the interactions, which
## compounds into +1000% territory for a noticeable share of draws (9-3). Here
## both pathways come from coherent fits, so neither explodes.
##
## THE TWO PATHWAYS MUST BE DRAWN AS A PAIR. Both models are fitted to the same
## panel, so c_hat^M8 and c_hat^M4 are strongly dependent; drawing each from its
## own marginal N(c_hat, V_hat) and differencing inflates the 90% band on the
## difference by 7-10x and turns a clear result into an inconclusive one. Each
## estimator therefore uses whichever paired scheme is feasible for it:
##
##   AFE  country resampling. Both models are refit on the SAME resampled panel,
##        so draw b carries the empirical pairing with no distributional
##        assumption. Feasible because an AFE refit costs ~0.03 s.
##   IFE  joint analytic covariance from 3-1_joint_covariance.R, which stacks
##        the two models' per-country scores into one clustered sandwich and
##        recovers the cross-model block. Bai (2009) cannot be refit 1,000
##        times, so this is the only paired route available.
##
## Section 5 runs the analytic route for AFE as well, where the resampling
## answer is also known. Agreement there is the evidence that the analytic
## route can be trusted for IFE, where no such check is possible.
##
## Out: figures/fig_IE_between_model_fan.png
##      output/IE_between_model_path_quantiles.csv   (M8, M4 and IE, long)
##      output/IE_between_model_draws_2100.csv
##      output/fig_IE_between_model_fan_data.rds  (redraw bundle for the figure)
##      output/fig_IE_between_model_fan_data.csv
##      output/IE_between_model_uncertainty.txt
## ========================================================================== ##

suppressMessages({library(tidyverse); library(fixest)})

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")

set.seed(20260814)
NDRAW <- 1000
SSP   <- "SSP585"
LAGS  <- 0:2
YEARS <- PROJ_YEARS
SPECS <- c(M8 = "Interactive", M4 = "Direct")
RG    <- list(M8 = REGS_INTERACT, M4 = REGS_DIRECT)
MEAT  <- "V_clu"          # "V_clu" clustered (default) or "V_bai" Bai's form

M_RES <- "Country resampling"
M_ANA <- "Joint analytic covariance"
METHOD_OF <- c(AFE = M_RES, IFE = M_ANA)

COL     <- c(AFE = "#2a78d6", IFE = "#eb6834")
SURFACE <- "white"

lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"),
                 show_col_types = FALSE)
inp  <- load_projection_inputs(SSP, root_dir)

jv_file <- file.path(out_dir, "joint_covariance.rds")
if (!file.exists(jv_file))
    stop("run 3-1_joint_covariance.R first: ", jv_file, " not found")
JV <- readRDS(jv_file)

cat(sprintf("Countries: %d | draws: %d | meat: %s\n",
            nrow(inp$cl), NDRAW, MEAT))

## ========================================================================== ##
## 1. Draw coefficients and map Bewley c -> beta_j -----------------------------
## ========================================================================== ##
draw_mvn <- function(mu, V, n) {
    V <- (V + t(V)) / 2
    ev <- eigen(V, symmetric = TRUE)
    ev$values[ev$values < 0] <- 0                  # PSD repair
    A <- ev$vectors %*% diag(sqrt(ev$values), length(ev$values))
    out <- matrix(mu, n, length(mu), byrow = TRUE) +
        matrix(rnorm(n * length(mu)), n) %*% t(A)
    colnames(out) <- names(mu)
    out
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

beta_from_c <- function(cc, L, rr) {
    B <- matrix(0, L + 1, length(rr), dimnames = list(NULL, rr))
    for (v in rr) {
        cn <- c(v, if (L >= 1) paste0("d", seq_len(L), "_", v))
        B[, v] <- c_to_beta(cc[cn], L)
    }
    B
}

## ========================================================================== ##
## 2. The two pathways and their difference ------------------------------------
## ========================================================================== ##
gdelta <- function(B, rr) global_delta(eta_matrix(inp$cl, B, rr),
                                       inp$G, inp$POP, inp$gp0)

ie_between <- function(c8, c4, L) {
    d8 <- gdelta(beta_from_c(c8, L, RG$M8), RG$M8)
    d4 <- gdelta(beta_from_c(c4, L, RG$M4), RG$M4)
    list(M8 = d8, M4 = d4, IE = d8 - d4)
}

point_of <- function(est, L) {
    b8 <- beta_matrix(lagc, SPECS["M8"], est, L)
    b4 <- beta_matrix(lagc, SPECS["M4"], est, L)
    list(M8 = gdelta(b8, RG$M8), M4 = gdelta(b4, RG$M4),
         IE = gdelta(b8, RG$M8) - gdelta(b4, RG$M4))
}

qs <- c(.025, .05, .10, .25, .50, .75, .90, .95, .975)
summarise_draws <- function(M, pt, est, method, L) {
    list(
        q = imap_dfr(M, function(X, nm) {
            Q <- apply(X, 2, quantile, probs = qs)
            tibble(ssp = SSP, estimator = est, method = method, L = L,
                   component = nm, year = YEARS, point = pt[[nm]],
                   q025 = Q["2.5%", ],  q05 = Q["5%", ],  q10 = Q["10%", ],
                   q25  = Q["25%", ],   q50 = Q["50%", ], q75 = Q["75%", ],
                   q90  = Q["90%", ],   q95 = Q["95%", ], q975 = Q["97.5%", ])
        }),
        d = tibble(estimator = est, method = method, L = L,
                   draw = seq_len(nrow(M$IE)),
                   M8_2100 = M$M8[, PROJ_HORIZ], M4_2100 = M$M4[, PROJ_HORIZ],
                   IE_2100 = M$IE[, PROJ_HORIZ]))
}

## ========================================================================== ##
## 3. AFE: country resampling, both models refit on one panel ------------------
## ========================================================================== ##
Pdata <- read_csv("data/GDP_reg_panelData_V2.csv", show_col_types = FALSE) %>%
    arrange(iso, year) %>%
    mutate(tmp2 = tmp^2, pre2 = pre^2,
           tmp_pre = tmp*pre, tmp2_pre = tmp^2*pre,
           pre2_tmp = pre^2*tmp, tmp2_pre2 = tmp^2*pre^2)
n_iso <- n_distinct(Pdata$iso); n_year <- n_distinct(Pdata$year)
stopifnot(nrow(Pdata) == n_iso * n_year)
Pdata <- Pdata %>% mutate(tt = rep(seq_len(n_year), times = n_iso), tt2 = tt^2)

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

afe_fit <- function(dat, allregs) {
    f <- as.formula(paste("logD_gdp ~", paste(allregs, collapse = " + "),
                          "| id[tt, tt2] + year"))
    coef(feols(f, data = dat, notes = FALSE, warn = FALSE))
}

newid <- rep(seq_len(n_iso), each = n_year)
fq <- list(); dr <- list()

for (L in LAGS) {
    dsg <- lapply(names(SPECS), function(m) {
        ar <- lag_terms(RG[[m]], L)
        as.data.frame(build_bewley(Pdata, RG[[m]], L) %>%
            select(all_of(c("iso", "year", "logD_gdp", "tt", "tt2", ar))) %>%
            mutate(id = iso))
    })
    names(dsg) <- names(SPECS)
    blk <- split(seq_len(nrow(dsg$M8)),
                 factor(dsg$M8$iso, levels = unique(dsg$M8$iso)))

    M <- list(M8 = matrix(NA_real_, NDRAW, PROJ_HORIZ),
              M4 = matrix(NA_real_, NDRAW, PROJ_HORIZ),
              IE = matrix(NA_real_, NDRAW, PROJ_HORIZ))
    t0 <- Sys.time()
    for (b in seq_len(NDRAW)) {
        rows <- unlist(blk[sample.int(n_iso, n_iso, replace = TRUE)],
                       use.names = FALSE)
        cf <- lapply(names(SPECS), function(m) {
            Db <- dsg[[m]][rows, ]; Db$id <- newid
            afe_fit(Db, lag_terms(RG[[m]], L))
        })
        names(cf) <- names(SPECS)
        if (anyNA(unlist(cf))) next                # collinear resample
        pb <- ie_between(cf$M8, cf$M4, L)
        for (nm in names(M)) M[[nm]][b, ] <- pb[[nm]]
    }
    ok <- !is.na(M$IE[, 1])
    for (nm in names(M)) M[[nm]] <- M[[nm]][ok, , drop = FALSE]

    pt <- point_of("AFE", L)
    s  <- summarise_draws(M, pt, "AFE", M_RES, L)
    fq[[sprintf("AFE|%d", L)]] <- s$q; dr[[sprintf("AFE|%d", L)]] <- s$d
    cat(sprintf("  AFE  L=%d  %4d draws (%d dropped, %.0f s)  IE %+6.3f  [%+6.3f, %+6.3f]\n",
                L, sum(ok), sum(!ok),
                as.numeric(difftime(Sys.time(), t0, units = "secs")),
                tail(pt$IE, 1), quantile(M$IE[, PROJ_HORIZ], .05),
                quantile(M$IE[, PROJ_HORIZ], .95)))
}

## ========================================================================== ##
## 4. IFE: draws from the joint analytic covariance ----------------------------
## ========================================================================== ##
## One multivariate normal over the STACKED (c^M8, c^M4), so every draw carries
## the estimated cross-model correlation.
draw_joint <- function(est, L, meat = MEAT) {
    j  <- JV[[sprintf("L=%d | %s", L, est)]]
    mu <- c(setNames(j$c8, paste0("M8.", names(j$c8))),
            setNames(j$c4, paste0("M4.", names(j$c4))))
    V  <- j[[meat]]
    stopifnot(identical(names(mu), colnames(V)))
    D  <- draw_mvn(mu, V, NDRAW)
    i8 <- grep("^M8\\.", colnames(D)); i4 <- grep("^M4\\.", colnames(D))
    C8 <- D[, i8, drop = FALSE]; colnames(C8) <- sub("^M8\\.", "", colnames(D)[i8])
    C4 <- D[, i4, drop = FALSE]; colnames(C4) <- sub("^M4\\.", "", colnames(D)[i4])
    list(C8 = C8, C4 = C4)
}

run_analytic <- function(est, L, meat = MEAT) {
    D <- draw_joint(est, L, meat)
    M <- list(M8 = matrix(0, NDRAW, PROJ_HORIZ),
              M4 = matrix(0, NDRAW, PROJ_HORIZ),
              IE = matrix(0, NDRAW, PROJ_HORIZ))
    for (b in seq_len(NDRAW)) {
        pb <- ie_between(D$C8[b, ], D$C4[b, ], L)
        for (nm in names(M)) M[[nm]][b, ] <- pb[[nm]]
    }
    M
}

for (L in LAGS) {
    M  <- run_analytic("IFE", L)
    pt <- point_of("IFE", L)
    s  <- summarise_draws(M, pt, "IFE", M_ANA, L)
    fq[[sprintf("IFE|%d", L)]] <- s$q; dr[[sprintf("IFE|%d", L)]] <- s$d
    cat(sprintf("  IFE  L=%d  %4d draws            IE %+6.3f  [%+6.3f, %+6.3f]\n",
                L, NDRAW, tail(pt$IE, 1),
                quantile(M$IE[, PROJ_HORIZ], .05),
                quantile(M$IE[, PROJ_HORIZ], .95)))
}

fq <- bind_rows(fq); dr <- bind_rows(dr)
write_csv(fq, file.path(out_dir, "IE_between_model_path_quantiles.csv"))
write_csv(dr, file.path(out_dir, "IE_between_model_draws_2100.csv"))

## ========================================================================== ##
## 5. Validation: the two routes agree where both are available ----------------
## ========================================================================== ##
## AFE is the only estimator that can be both refit and drawn analytically. If
## the analytic band matches the resampled one here, the analytic band used for
## IFE is credible. V_bai is reported alongside as a sensitivity on the meat.
## everything below reads from disk, so the figure can be retuned without
## re-running sections 3 and 4
fq <- read_csv(file.path(out_dir, "IE_between_model_path_quantiles.csv"),
               show_col_types = FALSE)
dr <- read_csv(file.path(out_dir, "IE_between_model_draws_2100.csv"),
               show_col_types = FALSE)

w90 <- function(v) as.numeric(diff(quantile(v, c(.05, .95))))
## breaking the pairing while holding both marginals fixed isolates exactly what
## the cross-model covariance is worth; median over 20 permutations
infl <- function(est, L) {
    d <- dr %>% filter(estimator == est, L == !!L)
    median(replicate(20, w90(d$M8_2100 - sample(d$M4_2100)))) /
        w90(d$M8_2100 - d$M4_2100)
}
val <- map_dfr(LAGS, function(L) {
    e_res <- dr %>% filter(estimator == "AFE", L == !!L) %>% pull(IE_2100)
    a_clu <- run_analytic("AFE", L, "V_clu")$IE[, PROJ_HORIZ]
    a_bai <- run_analytic("AFE", L, "V_bai")$IE[, PROJ_HORIZ]
    i_clu <- dr %>% filter(estimator == "IFE", L == !!L) %>% pull(IE_2100)
    i_bai <- run_analytic("IFE", L, "V_bai")$IE[, PROJ_HORIZ]
    tibble(L = L,
           `AFE resampled w90`  = w90(e_res),
           `AFE analytic w90`   = w90(a_clu),
           `AFE ratio`          = w90(a_clu) / w90(e_res),
           `AFE analytic V_bai` = w90(a_bai),
           `IFE analytic w90`   = w90(i_clu),
           `IFE V_bai w90`      = w90(i_bai),
           `AFE indep infl`     = infl("AFE", L),
           `IFE indep infl`     = infl("IFE", L))
})
cat("\nvalidation (90% width of IE_2100):\n")
print(as.data.frame(val %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)

## ========================================================================== ##
## 6. Pack everything the fan chart needs --------------------------------------
## ========================================================================== ##
## Sections 3-5 cost ~6.5 minutes. Everything the figure consumes is assembled
## here and written to disk as one bundle, so the chart can be restyled,
## re-cropped or re-labelled later without touching the bootstraps. Section 7
## then draws ONLY from that bundle, which is what keeps the pack honest: if
## anything were missing the figure would fail to build.
fq <- read_csv(file.path(out_dir, "IE_between_model_path_quantiles.csv"),
               show_col_types = FALSE)
dr <- read_csv(file.path(out_dir, "IE_between_model_draws_2100.csv"),
               show_col_types = FALSE)

## p_mitigate travels with the band rows so the CSV alone carries every number
## the figure shows, labels included
pp <- dr %>% group_by(estimator, L) %>%
    summarise(p_mitigate = mean(IE_2100 > 0), .groups = "drop")

band <- fq %>%
    filter(component == "IE") %>%
    left_join(pp, by = c("estimator", "L")) %>%
    mutate(panel = factor(paste0("L = ", L), levels = paste0("L = ", LAGS)))

## nested bands, darkest in the middle: 50%, 80%, 95%
bands <- tribble(
    ~lo,    ~hi,    ~alpha,
    "q25",  "q75",  0.34,
    "q10",  "q90",  0.20,
    "q025", "q975", 0.11)

label <- band %>% filter(year == max(year)) %>%
    mutate(txt = sprintf("2100: %+.0f pp\n90%% CI [%+.0f, %+.0f]\nP(IE > 0) = %.2f",
                         100 * point, 100 * q05, 100 * q95, p_mitigate))

YLIM   <- c(-0.6, 0.6)
n_clip <- sum(band$q025 < YLIM[1] | band$q975 > YLIM[2])

## Titles and caption are composed HERE, not at draw time, because the caption
## quotes numbers from val (section 5) that are otherwise gone once the bundle
## is reloaded.
fan <- list(
    band = band,
    label = label,
    bands = bands,
    val = val,
    meta = list(
        ssp = SSP, ndraw = NDRAW, lags = LAGS, meat = MEAT,
        col = COL, surface = SURFACE, ylim = YLIM, n_clip = n_clip,
        x_breaks = c(2025, 2050, 2075, 2100),
        width = 8.8, height = 5.8, dpi = 200,
        outfile = file.path(fig_dir, "fig_IE_between_model_fan.png"),
        title = "Between-model interactive contribution with bootstrap uncertainty",
        subtitle = paste0(
            SSP, ", ", format(NDRAW, big.mark = ","),
            " draws. Solid = point estimate, dashed = bootstrap median. ",
            "Shading: 50% / 80% / 95%, darkest to lightest.\n",
            "AFE: both models refit on each resampled country panel. ",
            "IFE: joint analytic covariance."
        ),
        ylab = expression("IE"[t]^between ~ "=" ~ delta[t]^{"M=8"} - delta[t]^{"M=4"})
    )
)

fan_file <- file.path(out_dir, "fig_IE_between_model_fan_data.rds")
saveRDS(fan, fan_file)
write_csv(band, file.path(out_dir, "fig_IE_between_model_fan_data.csv"))
cat(sprintf("packed %d band rows -> output/%s (+ .csv)\n",
            nrow(band), basename(fan_file)))

## ========================================================================== ##
## 7. Fan chart: point estimate + bootstrap density bands ----------------------
## ========================================================================== ##
## Drawn from the packed bundle alone. To redraw without re-running sections
## 3-5, run just this section, or from a clean session:
##     source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
##     fan <- readRDS(file.path(out_dir, "fig_IE_between_model_fan_data.rds"))
##     ggsave(fan$meta$outfile, draw_fan(fan), width = fan$meta$width,
##            height = fan$meta$height, dpi = fan$meta$dpi, bg = fan$meta$surface)
fan <- readRDS(file.path(out_dir, "fig_IE_between_model_fan_data.rds"))

draw_fan <- function(fan) {
    m <- fan$meta
    p <- ggplot(fan$band, aes(year))
    for (i in seq_len(nrow(fan$bands))) {
        p <- p + geom_ribbon(
            aes(ymin = .data[[fan$bands$lo[i]]], ymax = .data[[fan$bands$hi[i]]],
                fill = estimator),
            alpha = fan$bands$alpha[i], colour = NA)
    }
    p +
        geom_hline(yintercept = 0, colour = INK_MUTED, linewidth = 0.3) +
        geom_line(aes(y = q50, colour = estimator), linetype = "22",
                  linewidth = 0.45) +
        geom_line(aes(y = point, colour = estimator), linewidth = 0.85) +
        geom_text(data = fan$label,
                  aes(x = -Inf, y = Inf, label = txt, colour = estimator),
                  hjust = -0.06, vjust = 1.25, size = 2.7, fontface = "bold",
                  lineheight = 0.95, show.legend = FALSE) +
        facet_grid(estimator ~ panel) +
        scale_colour_manual(values = m$col, guide = "none") +
        scale_fill_manual(values = m$col, guide = "none") +
        scale_x_continuous(breaks = m$x_breaks) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        coord_cartesian(ylim = c(-0.3,0.6)) +
        labs(title = m$title, subtitle = m$subtitle, 
            x = NULL, y = m$ylab) +
        my_theme +
        theme(
            plot.background  = element_rect(fill = m$surface, colour = NA),
            panel.background = element_rect(fill = m$surface, colour = NA),
            panel.spacing    = unit(14, "pt"),
            strip.text       = element_text(colour = INK, size = 10,
                                            face = "bold",
                                            margin = margin(b = 4, t = 2)),
            plot.title       = element_text(hjust = 0, size = rel(1.30)),
            plot.subtitle    = element_text(size = rel(0.95), margin = margin(b = 4)),
            plot.caption     = element_text(size = rel(0.75), lineheight = 1.1))
}

p <- draw_fan(fan)
p
ggsave(fan$meta$outfile, p, width = fan$meta$width, height = fan$meta$height,
       dpi = fan$meta$dpi, bg = fan$meta$surface)

## ========================================================================== ##
## 8. Summary ------------------------------------------------------------------
## ========================================================================== ##
tab <- dr %>%
    group_by(estimator, method, L) %>%
    summarise(draws = n(), median = median(IE_2100), sd = sd(IE_2100),
              q05 = quantile(IE_2100, .05), q95 = quantile(IE_2100, .95),
              q025 = quantile(IE_2100, .025), q975 = quantile(IE_2100, .975),
              p_mitigate = mean(IE_2100 > 0), .groups = "drop") %>%
    left_join(fan$band %>% filter(year == max(year)) %>%
                  select(estimator, L, point),
              by = c("estimator", "L")) %>%
    transmute(estimator, L, method, draws,
              `IE 2100 (pp)` = round(100 * point, 1),
              `median (pp)`  = round(100 * median, 1),
              `sd (pp)`      = round(100 * sd, 1),
              `90% CI lo`    = round(100 * q05, 1),
              `90% CI hi`    = round(100 * q95, 1),
              `95% CI lo`    = round(100 * q025, 1),
              `95% CI hi`    = round(100 * q975, 1),
              `P(IE > 0)`    = round(p_mitigate, 3))

sink(file.path(out_dir, "IE_between_model_uncertainty.txt"))
cat("BETWEEN-MODEL INTERACTIVE CONTRIBUTION: BOOTSTRAP UNCERTAINTY\n")
cat("=============================================================\n\n")
cat("Scenario:", SSP, "| draws:", NDRAW, "| countries:", nrow(inp$cl), "\n")
cat("IE^between_t = delta^M8_t - delta^M4_t, the difference between the two\n")
cat("SEPARATELY ESTIMATED models plotted in 8_plot_global_path.R. Contrast 9-2,\n")
cat("which zeroes the interaction coefficients inside the M = 8 fit.\n")
cat("A POSITIVE value means the interactive model projects milder damages.\n\n")
cat("Both models are fitted to the same panel, so every draw keeps the two\n")
cat("PAIRED. AFE refits both on each resampled country panel; IFE draws from\n")
cat("the stacked-score joint covariance of 3-1_joint_covariance.R (meat:",
    MEAT, ").\nDrawing them independently inflates the 90% band by the factor in\n")
cat("the 'indep infl' columns of the second table.\n")
cat("Units: percentage points of the 2100 GDP ratio.\n\n")
print(as.data.frame(tab), row.names = FALSE)

cat("\n\n--- Do the two paired routes agree? (90% width of IE_2100) ---\n\n")
cat("AFE is the only estimator that can be both refit and drawn analytically,\n")
cat("so it is the only place the analytic route can be checked. 'AFE ratio' is\n")
cat("analytic / resampled: near 1 means the joint covariance reproduces what\n")
cat("actually refitting the models gives, which is the evidence that the IFE\n")
cat("bands -- where no refit is possible -- can be trusted.\n")
cat("The ratio comes in below 1: the analytic route imposes normality on c_hat\n")
cat("while resampling picks up its heavier tails, the same effect 9-3 found\n")
cat("when the country-resampled coefficient sd came in 1.1-1.6x the analytic\n")
cat("clustered se. Read the IFE bands as, if anything, slightly narrow.\n")
cat("The V_bai columns re-run the analytic route with Bai's heteroskedastic\n")
cat("meat instead of the clustered one, as a sensitivity on that choice.\n")
cat("'indep infl' is what the 90% width becomes if the M4 draws are shuffled\n")
cat("against the M8 draws, i.e. if the pairing is thrown away.\n\n")
print(as.data.frame(val %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)
sink()

cat("\nWrote figures/fig_IE_between_model_fan.png,\n",
    "output/IE_between_model_{path_quantiles,draws_2100}.csv and",
    "IE_between_model_uncertainty.txt\n\n")
print(as.data.frame(tab), row.names = FALSE)
