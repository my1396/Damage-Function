## ========================================================================== ##
## Cumulative climate coefficients -> LaTeX tables ---------------------------- 
##
## Reads the long-format coefficient CSV written by 3_lagged_climate.R and
## renders one booktabs table per specification ("Direct", "Interactive").
##
## Each table holds 12 models: 2 estimators (AFE, IFE) x 6 lag lengths
## (L = 0,...,5).  Estimates carry significance stars, standard errors sit in
## parentheses on the row below.
##
## In : output/lagged_climate_cumulative.csv
##      columns: term, estimate, std.error, statistic, p.value, nobs,
##               spec, L, estimator
## Out: <paper>/tables/tab_cumulative_direct.tex
##      <paper>/tables/tab_cumulative_interactive.tex
##
## Usage: Rscript 4-1_cumulative_tex_table.R [csv_in] [tex_dir]
## ========================================================================== ##

suppressMessages(library(tidyverse))

## ========================================================================== ##
## 1. Configuration ----------------------------------------------------------
## ========================================================================== ##
root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
out_dir  <- file.path(root_dir, "Revision_2026Aug", "output")
paper_dir <- "/Users/menghan/Documents/GDP/papers/2026-Aug"

args    <- commandArgs(trailingOnly = TRUE)
csv_in  <- if (length(args) >= 1) args[1] else
    file.path(out_dir, "lagged_climate_cumulative.csv")
tex_dir <- if (length(args) >= 2) args[2] else file.path(paper_dir, "tables")

DIGITS     <- 5                       # decimals on estimates and std. errors
ESTIMATORS <- c("AFE", "IFE")         # column blocks, in order
LAGS       <- 0:5                     # columns within each block

## row order and LaTeX labels for the climate terms
term_labels <- c(tmp       = "$T$",
                 tmp2      = "$T^{2}$",
                 pre       = "$P$",
                 pre2      = "$P^{2}$",
                 tmp_pre   = "$T \\times P$",
                 tmp2_pre  = "$T^{2} \\times P$",
                 pre2_tmp  = "$T \\times P^{2}$",
                 tmp2_pre2 = "$T^{2} \\times P^{2}$")

## one entry per specification: file stub, caption, label
spec_meta <- list(
    Direct = list(
        stub    = "tab_cumulative_direct",
        caption = paste("Cumulative climate effects, direct specification.",
                        "Each column reports the cumulative (long-run)",
                        "coefficients $\\sum_{j=0}^{L}\\beta_j$ from the",
                        "distributed-lag regression."),
        label   = "tab:cum_direct"),
    Interactive = list(
        stub    = "tab_cumulative_interactive",
        caption = paste("Cumulative climate effects, interaction",
                        "specification. Each column reports the cumulative",
                        "(long-run) coefficients $\\sum_{j=0}^{L}\\beta_j$",
                        "from the distributed-lag regression."),
        label   = "tab:cum_interactive"))

## ========================================================================== ##
## 2. Helpers ----------------------------------------------------------------
## ========================================================================== ##
star <- function(p) {
    ifelse(is.na(p), "",
    ifelse(p < 0.01, "\\sym{***}",
    ifelse(p < 0.05, "\\sym{**}",
    ifelse(p < 0.10, "\\sym{*}", ""))))
}

fmt_num <- function(x, digits = DIGITS) {
    ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

fmt_int <- function(x) {
    ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ","))
}

## paste a tabular row together
tex_row <- function(...) paste0(paste(c(...), collapse = " & "), " \\\\")

## ========================================================================== ##
## 3. Table builder ----------------------------------------------------------
## ========================================================================== ##
build_table <- function(d, meta) {

    models <- expand_grid(estimator = ESTIMATORS, L = LAGS) %>%
        mutate(key = paste(estimator, L, sep = "_"),
               col = row_number())
    n_col  <- nrow(models) + 1L                   # + the row-label column

    d <- d %>%
        mutate(key   = paste(estimator, L, sep = "_"),
               cell  = paste0(fmt_num(estimate), star(p.value)),
               secel = paste0("(", fmt_num(std.error), ")"))

    missing <- setdiff(models$key, d$key)
    if (length(missing))
        stop("missing models in ", meta$stub, ": ",
             paste(missing, collapse = ", "))

    ## keep only the terms this specification actually estimates
    terms <- names(term_labels)[names(term_labels) %in% unique(d$term)]

    pick <- function(tm, what) {
        v <- d[[what]][match(paste(tm, models$key), paste(d$term, d$key))]
        ifelse(is.na(v), "", v)
    }

    ## ---- header -----------------------------------------------------------
    blk <- length(LAGS)
    grp <- character(0); rule <- character(0)
    for (i in seq_along(ESTIMATORS)) {
        lo <- 2L + (i - 1L) * blk
        hi <- lo + blk - 1L
        grp  <- c(grp, sprintf("\\multicolumn{%d}{c}{%s}", blk, ESTIMATORS[i]))
        rule <- c(rule, sprintf("\\cmidrule(lr){%d-%d}", lo, hi))
    }

    body <- c(
        "\\toprule",
        tex_row("", grp),
        paste(rule, collapse = ""),
        tex_row("", sprintf("(%d)", models$col)),
        tex_row("", sprintf("$L=%d$", models$L)),
        "\\midrule")

    ## ---- coefficient block ------------------------------------------------
    for (tm in terms) {
        body <- c(body,
                  tex_row(term_labels[[tm]], pick(tm, "cell")),
                  tex_row("", pick(tm, "secel")))
    }

    ## ---- footer rows ------------------------------------------------------
    nobs <- d$nobs[match(models$key, d$key)]
    body <- c(body,
              "\\midrule",
              tex_row("Observations", fmt_int(nobs)),
              "\\bottomrule")

    ## ---- wrapper ----------------------------------------------------------
    c("% ---------------------------------------------------------------------",
      "% Auto-generated by 4-1_cumulative_tex_table.R -- do not edit by hand.",
      "% ---------------------------------------------------------------------",
      "\\providecommand{\\sym}[1]{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
      "\\begin{table}[H]",
      "\\centering",
      sprintf("\\caption{%s}", meta$caption),
      sprintf("\\label{%s}", meta$label),
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
      paste("\\textit{Notes:} Standard errors in parentheses. AFE = additive",
            "country and year fixed effects with country-specific quadratic",
            "trends, HC0 standard errors clustered by country; IFE =",
            "interactive fixed effects (Bai, 2009) with four factors. $L$ is",
            "the number of climate lags included. Significance:",
            "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\),",
            "\\sym{***} \\(p<0.01\\)."),
      "\\end{minipage}",
      "\\end{table}")
}

## ========================================================================== ##
## 4. Build and export -------------------------------------------------------
## ========================================================================== ##
cum <- read_csv(csv_in, show_col_types = FALSE)

need <- c("term", "estimate", "std.error", "p.value", "nobs",
          "spec", "L", "estimator")
if (!all(need %in% names(cum)))
    stop("missing columns in ", csv_in, ": ",
         paste(setdiff(need, names(cum)), collapse = ", "))

dir.create(tex_dir, showWarnings = FALSE, recursive = TRUE)

for (sp in names(spec_meta)) {
    meta <- spec_meta[[sp]]
    d    <- cum %>% filter(spec == sp)
    if (!nrow(d)) {
        warning("no rows for spec '", sp, "' -- skipped"); next
    }
    tex  <- build_table(d, meta)
    path <- file.path(tex_dir, paste0(meta$stub, ".tex"))
    writeLines(tex, path)
    cat(sprintf("%-12s %2d models, %2d terms -> %s\n",
                sp, length(ESTIMATORS) * length(LAGS),
                n_distinct(d$term), path))
}
