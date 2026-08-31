## ========================================================================== ##
## Persistence ratio -> LaTeX table ------------------------------------------- 
##
## Renders output/lag_ratio_within_model.csv (written by
## 3-2_lag_persistence_ratio.R) as one booktabs table with a panel per
## specification. Columns are the 2 estimators x 5 lag lengths; rows report, at
## each temperature point, the impact marginal effect, the cumulative marginal
## effect, their ratio rho with its delta-method standard error, and the 95%
## confidence band.
##
## rho = 1 is a pure growth effect, rho = 0 a pure level effect, so the band is
## read against BOTH nulls -- that is what the table is for.
##
## In : output/lag_ratio_within_model.csv
##      output/lagged_climate_marginal_effects.csv   (temperature labels)
## Out: <paper>/tables/tab_persistence_ratio.tex
##
## Usage: Rscript 4-2_persistence_tex_table.R [csv_in] [tex_dir]
## ========================================================================== ##

suppressMessages(library(tidyverse))

## ========================================================================== ##
## 1. Configuration ----------------------------------------------------------
## ========================================================================== ##
root_dir  <- "/Users/menghan/Documents/GDP/Shared folder"
out_dir   <- file.path(root_dir, "Revision_2026Aug", "output")
paper_dir <- "/Users/menghan/Documents/GDP/papers/2026-Aug"

args    <- commandArgs(trailingOnly = TRUE)
csv_in  <- if (length(args) >= 1) args[1] else
    file.path(out_dir, "lag_ratio_within_model.csv")
tex_dir <- if (length(args) >= 2) args[2] else file.path(paper_dir, "tables")

D_ME    <- 5                       # decimals on the marginal effects
D_RHO   <- 3                       # decimals on rho, its se and the band
STUB    <- "tab_persistence_ratio"
LABEL   <- "tab:persistence_ratio"
CAPTION <- paste("Persistence of the temperature effect: cumulative versus",
                 "impact marginal effect. The ratio",
                 "$\\rho = (\\partial g/\\partial T \\mid \\sum_j \\beta_j) /",
                 "(\\partial g/\\partial T \\mid \\beta_0)$ equals one under a",
                 "pure growth effect and zero under a pure level effect.")

panel_labs <- c(Direct      = "Panel A: Direct specification",
                Interactive = "Panel B: Interaction specification")

## ========================================================================== ##
## 2. Helpers ----------------------------------------------------------------
## ========================================================================== ##
fmt <- function(x, digits) ifelse(is.na(x), "",
                                  formatC(x, format = "f", digits = digits))

tex_row <- function(...) paste0(paste(c(...), collapse = " & "), " \\\\")

## "50%" -> "50th". The bare % from the T_pct column would otherwise open a
## LaTeX comment and swallow the rest of the row, \\ included.
ordinal <- function(pct) {
    n   <- as.integer(sub("%$", "", pct))
    suf <- if (n %% 100 %in% 11:13) "th" else
        switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
    paste0(n, suf)
}

## ========================================================================== ##
## 3. Table builder ----------------------------------------------------------
## ========================================================================== ##
d <- read_csv(csv_in, show_col_types = FALSE)

need <- c("spec", "estimator", "L", "T_pct", "impact_b0", "cumulative",
          "ratio", "se_ratio", "lo", "hi")
if (!all(need %in% names(d)))
    stop("missing columns in ", csv_in, ": ",
         paste(setdiff(need, names(d)), collapse = ", "))

## temperature values, for the row-block headings
Tval <- read_csv(file.path(out_dir, "lagged_climate_marginal_effects.csv"),
                 show_col_types = FALSE) %>%
    distinct(T_pct, T_val) %>% deframe()

ESTS   <- unique(d$estimator)
LAGS   <- sort(unique(d$L))
T_PCTS <- unique(d$T_pct)
SPECS  <- intersect(names(panel_labs), unique(d$spec))

models <- expand_grid(estimator = ESTS, L = LAGS) %>%
    mutate(key = paste(estimator, L, sep = "_"))
n_col  <- nrow(models) + 1L

d <- d %>% mutate(key = paste(estimator, L, sep = "_"))
missing <- setdiff(models$key, d$key)
if (length(missing))
    stop("missing models: ", paste(missing, collapse = ", "))

## pull one field across the model columns, in order
pick <- function(dd, f) {
    v <- dd[[f]][match(models$key, dd$key)]
    ifelse(is.na(v), "", v)
}

## ---- header ---------------------------------------------------------------
blk <- length(LAGS)
grp <- character(0); rule <- character(0)
for (i in seq_along(ESTS)) {
    lo <- 2L + (i - 1L) * blk
    grp  <- c(grp, sprintf("\\multicolumn{%d}{c}{%s}", blk, ESTS[i]))
    rule <- c(rule, sprintf("\\cmidrule(lr){%d-%d}", lo, lo + blk - 1L))
}

body <- c("\\toprule",
          tex_row("", grp),
          paste(rule, collapse = ""),
          tex_row("", sprintf("$L=%d$", models$L)),
          "\\midrule")

## ---- one panel per specification, one block per temperature point ----------
for (si in seq_along(SPECS)) {
    sp <- SPECS[si]
    if (si > 1) body <- c(body, "\\addlinespace", "\\midrule")
    body <- c(body,
              sprintf("\\multicolumn{%d}{l}{\\emph{%s}} \\\\",
                      n_col, panel_labs[[sp]]))

    for (tp in T_PCTS) {
        dd <- d %>% filter(spec == sp, T_pct == tp)
        head_lab <- sprintf(
            paste0("\\multicolumn{%d}{l}{\\quad Temperature at the %s ",
                   "percentile, $T=%.1f^{\\circ}$C} \\\\"),
            n_col, ordinal(tp), Tval[[tp]])
        body <- c(
            body, "\\addlinespace", head_lab,
            tex_row("\\quad Impact $\\beta_0$",
                    fmt(pick(dd, "impact_b0"), D_ME)),
            tex_row("\\quad Cumulative $\\sum_j \\beta_j$",
                    fmt(pick(dd, "cumulative"), D_ME)),
            tex_row("\\quad Ratio $\\rho$", fmt(pick(dd, "ratio"), D_RHO)),
            tex_row("", paste0("(", fmt(pick(dd, "se_ratio"), D_RHO), ")")),
            tex_row("\\quad 95\\% CI",
                    sprintf("[%s, %s]", fmt(pick(dd, "lo"), D_RHO),
                            fmt(pick(dd, "hi"), D_RHO))))
    }
}
body <- c(body, "\\bottomrule")

## ========================================================================== ##
## 4. Wrap and export --------------------------------------------------------
## ========================================================================== ##
tex <- c(
    "% ---------------------------------------------------------------------",
    "% Auto-generated by 4-2_persistence_tex_table.R -- do not edit by hand.",
    "% ---------------------------------------------------------------------",
    "\\begin{table}[H]",
    "\\centering",
    sprintf("\\caption{%s}", CAPTION),
    sprintf("\\label{%s}", LABEL),
    "\\begin{adjustbox}{max width=\\textwidth}",
    sprintf("\\begin{tabular}{l*{%d}{c}}", n_col - 1L),
    body,
    "\\end{tabular}",
    "\\end{adjustbox}",
    "",
    "% notes live in their own \\textwidth minipage: threeparttable would",
    "% size them to the *unscaled* tabular and run past the right margin.",
    "\\vspace{0.4em}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    paste("\\textit{Notes:} Marginal effects $\\partial g/\\partial T$ are",
          "evaluated at median precipitation and the stated temperature",
          "percentile of the estimation sample. $\\beta_0$ is the impact",
          "(contemporaneous) effect and $\\sum_j \\beta_j$ the cumulative",
          "effect; $L=0$ is omitted because $\\rho \\equiv 1$ there by",
          "construction. Delta-method standard errors in parentheses.",
          "A band excluding $0$ rejects a pure level effect; a band excluding $1$",
          "rejects a pure growth effect. AFE = additive fixed effects,",
          "IFE = interactive fixed effects."),
    "\\end{minipage}",
    "\\end{table}")

dir.create(tex_dir, showWarnings = FALSE, recursive = TRUE)
path <- file.path(tex_dir, paste0(STUB, ".tex"))
writeLines(tex, path)
cat(sprintf("%d panels, %d models -> %s\n", length(SPECS), nrow(models), path))
