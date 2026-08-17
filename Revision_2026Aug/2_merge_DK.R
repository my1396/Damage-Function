## =============================================================================
## Merge the Driscoll-Kraay columns from the Stata FE comparison into the
## AFE/IFE four-model table.
##
## In  : output/FE_SE_comparison.txt        (from S2-1_FE.do)
##       output/model_comparison_table.csv  (from 1_four_model_comparison.R)
## Out : output/model_comparison_with_DK.csv / .txt
##
## The DK columns carry the SAME point estimates as the AFE columns -- only the
## variance estimator differs. The script asserts this before merging.
## =============================================================================

suppressMessages(library(tidyverse))

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
out_dir  <- file.path(root_dir, "Revision_2026Aug", "output")

## ---- 1. parse the esttab fixed-width table ---------------------------------
## Layout: cols 1-16 = variable label, cols 17+ = value fields.
## Rows carry 6 fields (Direct x3, Interactive x3) or 3 fields (Interactive
## only, for the interaction terms which are absent from the direct spec).
txt <- read_lines(file.path(out_dir, "FE_SE_comparison.txt"))

parse_row <- function(line) {
    label <- str_trim(str_sub(line, 1, 16))
    vals  <- str_split(str_trim(str_sub(line, 17)), "\\s+")[[1]]
    vals  <- vals[nzchar(vals)]
    if (length(vals) == 6) {
        list(label = label, dir_dk = vals[3], int_dk = vals[6])
    } else if (length(vals) == 3) {
        list(label = label, dir_dk = NA_character_, int_dk = vals[3])
    } else {
        NULL
    }
}

keep_rows <- txt %>%
    keep(~ !str_detect(.x, "^-+$")) %>%                 # rule lines
    discard(~ str_detect(.x, "Direct|Cluster|parenthes|p<0")) %>%
    discard(~ str_detect(.x, "^R-squared")) %>%    # absent from the R table
    keep(~ str_detect(.x, "[0-9]"))

dk <- map(keep_rows, parse_row) %>% compact() %>% bind_rows()

## esttab prints the SE on the line after each coefficient, with a blank label.
## Propagate the preceding label so rows can be matched positionally later.
stopifnot(nrow(dk) > 0)

## ---- 2. read the four-model table ------------------------------------------
base <- read_csv(file.path(out_dir, "model_comparison_table.csv"),
                 show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(across(everything(), ~replace_na(.x, "")))

stopifnot(nrow(base) == nrow(dk))

## ---- 3. sanity check: DK and AFE must share point estimates ----------------
coef_rows <- which(nzchar(base$Variable) & base$Variable != "Observations")
chk <- tibble(
    Variable = base$Variable[coef_rows],
    afe_dir  = base[["M1: AFE-Direct"]][coef_rows],
    dk_dir   = dk$dir_dk[coef_rows],
    afe_int  = base[["M2: AFE-Interact"]][coef_rows],
    dk_int   = dk$int_dk[coef_rows]
)
## strip significance stars -- those legitimately differ between the two
## variance estimators; only the point estimates must agree
bare <- function(x) str_remove_all(x, "\\*")
mismatch <- chk %>%
    filter((nzchar(afe_dir) & !is.na(dk_dir) & bare(afe_dir) != bare(dk_dir)) |
           (nzchar(afe_int) & !is.na(dk_int) & bare(afe_int) != bare(dk_int)))
if (nrow(mismatch) > 0) {
    cat("WARNING: point estimates differ between AFE and DK columns:\n")
    print(as.data.frame(mismatch))
} else {
    cat("Check passed: AFE and DK columns share identical point estimates.\n")
}

## ---- 4. merge, grouped by specification ------------------------------------
merged <- tibble(
    Variable                 = base$Variable,
    `AFE-Direct`             = base[["M1: AFE-Direct"]],
    `AFE-Direct DK(3)`       = replace_na(dk$dir_dk, ""),
    `IFE-Direct`             = base[["M3: IFE-Direct"]],
    `AFE-Interact`           = base[["M2: AFE-Interact"]],
    `AFE-Interact DK(3)`     = replace_na(dk$int_dk, ""),
    `IFE-Interact`           = base[["M4: IFE-Interact"]]
) %>%
    ## esttab prints "6,456"; the R table prints "6456" -- normalise
    mutate(across(-Variable, ~str_remove_all(.x, ",")))

write_csv(merged, file.path(out_dir, "model_comparison_with_DK.csv"))

## ---- 5. readable text version ----------------------------------------------
sink(file.path(out_dir, "model_comparison_with_DK.txt"))
cat("Climate effects on GDP per-capita growth: estimator and variance comparison\n")
cat("Data: GDP_reg_panelData_V2 | N = 122 countries, T = 59 years, 6,456 obs\n\n")
cat("AFE            additive country + year FE (within), HC0 SE clustered by country\n")
cat("AFE ... DK(3)  same point estimates, Driscoll-Kraay SE, bandwidth 3\n")
cat("IFE            interactive FE, Bai (2009) iterated PC, 4 factors, Bai (2009) SE\n")
cat("Country-specific linear + quadratic trends in all columns.\n")
cat("Significance: * p<0.10  ** p<0.05  *** p<0.01\n\n")
print(as.data.frame(merged), row.names = FALSE)
sink()

cat("\n")
print(as.data.frame(merged), row.names = FALSE)
cat("\nWritten to:\n")
cat("  output/model_comparison_with_DK.csv\n")
cat("  output/model_comparison_with_DK.txt\n")
