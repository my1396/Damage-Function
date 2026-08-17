## =============================================================================
## GDP projections with a DISTRIBUTED LAG in climate, L = 0 ... 5.
##
##     eta_{i,t} = sum_{j=0}^{L} beta_j' (x_{i,t-j} - x_{i,0})
##
## with lags before the base period clamped at x_0. L = 0 reproduces the
## contemporaneous-only specification used in the paper.
##
## GLOBAL AGGREGATION follows BHM: each country's GDP path is compounded first,
## then summed across countries (see _projection_common.R). Results are also
## expressed relative to each row's own L = 0 value, so the L-comparison does not
## depend on the aggregation choice.
##
## Out: output/projection_lagged_country.csv
##      output/projection_lagged_global.csv
##      output/projection_lagged.txt
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

SSPS <- c("SSP585", "SSP370", "SSP245", "SSP126")
LAGS <- 0:5

lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"),
                 show_col_types = FALSE)

country_out <- list(); global_out <- list()

for (ssp in SSPS) {
    if (!dir.exists(file.path("data", ssp))) { cat("skip", ssp, "\n"); next }
    inp <- load_projection_inputs(ssp, root_dir)
    tmed <- median(inp$cl$tmp_start, na.rm = TRUE)

    for (sp in c("Direct", "Interactive")) {
        regs <- regs_for(sp)
        for (L in LAGS) {
            B   <- beta_matrix(lagc, sp, "AFE", L)   # placeholder, overwritten below
            for (est in c("AFE", "IFE")) {
                B   <- beta_matrix(lagc, sp, est, L)
                eta <- eta_matrix(inp$cl, B, regs)

                cdel  <- country_delta(eta, inp$G)          # country paths
                d2100 <- cdel[, PROJ_HORIZ]
                gdel  <- global_delta(eta, inp$G, inp$POP, inp$gp0)

                key <- paste(ssp, sp, est, L)
                country_out[[key]] <- tibble(
                    ssp = ssp, spec = sp, estimator = est, L = L,
                    ISO_C3 = inp$cl$ISO_C3, delta_2100 = d2100)

                cold <- inp$cl$tmp_start <  tmed
                global_out[[key]] <- tibble(
                    ssp = ssp, spec = sp, estimator = est, L = L,
                    n_country   = nrow(inp$cl),
                    delta_bhm   = tail(gdel, 1),
                    delta_unweighted = mean(d2100),
                    delta_cold  = mean(d2100[cold]),
                    delta_hot   = mean(d2100[!cold]),
                    n_gainers   = sum(d2100 > 0))
            }
        }
    }
    cat("done", ssp, "\n")
}

country <- bind_rows(country_out)
global  <- bind_rows(global_out) %>%
    group_by(ssp, spec, estimator) %>%
    mutate(ratio_bhm_vs_L0 = delta_bhm / delta_bhm[L == 0],
           ratio_unw_vs_L0 = delta_unweighted / delta_unweighted[L == 0]) %>%
    ungroup()

write_csv(country, file.path(out_dir, "projection_lagged_country.csv"))
write_csv(global,  file.path(out_dir, "projection_lagged_global.csv"))

sink(file.path(out_dir, "projection_lagged.txt"))
cat("GDP IMPACT IN 2100 WITH A DISTRIBUTED LAG IN CLIMATE\n")
cat("====================================================\n\n")
cat("eta_t = sum_{j=0}^{L} beta_j' (x_{t-j} - x_0);  L=0 reproduces the\n")
cat("contemporaneous-only specification used in the paper.\n\n")
cat("delta_bhm: each country's GDP path compounded first, then summed across\n")
cat("countries weighted by population (BHM aggregation). This is the reported\n")
cat("global impact. ratio_bhm_vs_L0 rescales by each row's own L=0 value.\n\n")
print(as.data.frame(global %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)
sink()

print(as.data.frame(global %>% filter(ssp == "SSP585", spec == "Interactive") %>%
                    select(estimator, L, delta_bhm, ratio_bhm_vs_L0,
                           delta_cold, delta_hot, n_gainers) %>%
                    mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
cat("\nWritten to output/projection_lagged_{country,global}.csv and .txt\n")
