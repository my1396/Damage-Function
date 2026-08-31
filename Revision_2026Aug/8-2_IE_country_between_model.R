## COUNTRY-LEVEL between-model interactive contribution, the map counterpart of
## 8-1_IE_between_model_uncertainty.R (which does the global path).
##
## For each country i, at 2100:
##     delta^M8_i    interactive fit, T, T^2, P, P^2 and the four T x P terms
##     delta^M4_i    direct fit, T, T^2, P, P^2 only, SEPARATELY ESTIMATED
##     IE_i = delta^M8_i - delta^M4_i
##
## A POSITIVE IE means the interactive model projects a BETTER GDP impact for
## that country than the direct model does.
##
## This is the between-model counterpart of 9-1_IE_country_decomposition.R,
## which measures the WITHIN-model contribution by zeroing the four interaction
## coefficients inside the M = 8 fit. There delta^Dir re-uses coefficients that
## were never estimated without the interactions present; here both pathways
## come from coherent fits, so neither is an artefact of an incoherent
## counterfactual. The two maps are meant to be read side by side.
##
## Outputs
##   output/IE_country_between_model.csv          per-country deltas, IE, signs
##   output/IE_country_between_model_crosstab.txt 2x2x2 sign-combination table
##   figures/fig_IE_country_between_model_map_{afe,ife}.png
##
## Follows 9-1_IE_country_decomposition.R throughout.
## ========================================================================== ##

suppressMessages({
    library(tidyverse)
    library(knitr)
    library(rnaturalearth)
    library(sf)
})
`%ni%` <- Negate(`%in%`)

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

SSP   <- "SSP585"
## L = 0 to sit alongside the within-model map in 9-1, which is pinned there
## because that decomposition is only identified at L = 0. The between-model
## contrast has no such restriction -- both models are separately estimated at
## every L -- so this can be moved to 1 or 2 if the lagged view is wanted.
L_MAP <- 0
SPECS <- c(M8 = "Interactive", M4 = "Direct")
RG    <- list(M8 = REGS_INTERACT, M4 = REGS_DIRECT)

inp  <- load_projection_inputs(SSP, root_dir)
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"), show_col_types = FALSE)
region <- read_csv(file.path(root_dir, "data/region_category.csv"),
                   show_col_types = FALSE) %>%
    select(ISO_C3, cntry.name, region)

## ========================================================================== ##
## 1. Country-level deltas -----------------------------------------------------
## ========================================================================== ##
between_country <- function(est, L = L_MAP) {
    cdelta <- function(m) {
        B <- beta_matrix(lagc, SPECS[[m]], est, L)
        country_delta(eta_matrix(inp$cl, B, RG[[m]]), inp$G)[, PROJ_HORIZ]
    }
    d_m8 <- cdelta("M8")
    d_m4 <- cdelta("M4")

    tibble(
        ISO_C3    = inp$cl$ISO_C3,
        estimator = est,
        L         = L,
        delta_m8  = d_m8,
        delta_m4  = d_m4,
        IE_effect = d_m8 - d_m4
    )
}

cdec <- map_dfr(c("AFE", "IFE"), between_country) %>%
    mutate(
        ## sign labels, mirroring 9-1_IE_country_decomposition.R
        diff_sign = if_else(IE_effect >= 0, "positive", "negative"),  # IE
        m8_sign   = if_else(delta_m8  >= 0, "positive", "negative"),  # M = 8
        m4_sign   = if_else(delta_m4  >= 0, "positive", "negative")   # M = 4
    ) %>%
    left_join(region, by = "ISO_C3")

f_name <- file.path(out_dir, "IE_country_between_model.csv")
write_csv(cdec, f_name)


## ========================================================================== ##
## 2. Cross table of sign combinations (2 x 2 x 2), per estimator --------------
## ========================================================================== ##
format_cross_tab <- function(d, show_pct = FALSE) {
    all_comb <- expand.grid(
        diff_sign = c("negative", "positive"),
        m8_sign   = c("negative", "positive"),
        m4_sign   = c("negative", "positive")
    )
    counts <- d %>%
        group_by(diff_sign, m8_sign, m4_sign) %>%
        summarise(n_countries = n(), .groups = "drop") %>%
        right_join(all_comb, by = c("diff_sign", "m8_sign", "m4_sign")) %>%
        mutate(n_countries = replace_na(n_countries, 0))
    total_n <- nrow(d)

    one <- function(sign_val) {
        tb <- counts %>%
            filter(diff_sign == sign_val) %>%
            select(-diff_sign) %>%
            mutate(
                m8_sign = factor(m8_sign, levels = c("positive", "negative")),
                m4_sign = factor(m4_sign, levels = c("positive", "negative"))
            ) %>%
            pivot_wider(names_from = m4_sign, values_from = n_countries) %>%
            arrange(m8_sign) %>%
            mutate(m8_sign = paste("M8:", m8_sign)) %>%
            column_to_rownames("m8_sign") %>%
            select(positive, negative)
        if (show_pct) tb <- tb %>% mutate(across(everything(),
                                                 ~ sprintf("%.1f%%", . / total_n * 100)))
        colnames(tb) <- paste("M4:", colnames(tb))
        tb
    }

    capture.output({
        cat("IE > 0  (the M = 8 model gives a BETTER GDP impact than M = 4)\n")
        cat(strrep("=", 60), "\n")
        print(kable(one("positive"), format = "simple"))
        cat("\n\nIE < 0  (the M = 8 model gives a WORSE GDP impact than M = 4)\n")
        cat(strrep("=", 60), "\n")
        print(kable(one("negative"), format = "simple"))
    })
}

sink(file.path(out_dir, "IE_country_between_model_crosstab.txt"))
cat("BETWEEN-MODEL INTERACTIVE CONTRIBUTION, BY COUNTRY\n")
cat("==================================================\n\n")
cat("IE_i = delta^M8_i - delta^M4_i at 2100, the difference between the two\n")
cat("SEPARATELY ESTIMATED models. Contrast 9-1, which zeroes the interaction\n")
cat("coefficients inside the M = 8 fit to build its delta^Dir.\n")
for (est in c("AFE", "IFE")) {
    d <- cdec %>% filter(estimator == est)
    cat("\n##########################################################\n")
    cat(sprintf("## %s --- %d countries, %s, L = %d, impacts at 2100\n",
                est, nrow(d), SSP, L_MAP))
    cat("##########################################################\n\n")
    cat(paste(format_cross_tab(d), collapse = "\n"), "\n")
    cat("\n-- as % of countries --\n")
    cat(paste(format_cross_tab(d, show_pct = TRUE), collapse = "\n"), "\n")
    cat("\n-- regional composition of the dominant cell --\n")
    dom <- d %>% count(diff_sign, m8_sign, m4_sign, sort = TRUE) %>% slice(1)
    cat(sprintf("   (IE %s, M8 %s, M4 %s), n = %d\n",
                dom$diff_sign, dom$m8_sign, dom$m4_sign, dom$n))
    print(as.data.frame(
        d %>% filter(diff_sign == dom$diff_sign, m8_sign == dom$m8_sign,
                     m4_sign == dom$m4_sign) %>%
            count(region, name = "n") %>% arrange(desc(n))), row.names = FALSE)
}
sink()

cat("=== cross table (AFE) ===\n")
cat(paste(format_cross_tab(cdec %>% filter(estimator == "AFE")), collapse = "\n"), "\n")

## ========================================================================== ##
## 3. Categorical map of sign combinations -------------------------------------
## ========================================================================== ##
## Label order: (M4, IE, M8) -- i.e. (delta_m4, IE_effect, delta_m8), matching
## the (Dir, IE, All) order used by the within-model map in 9-1.
## Maps keep a transparent background; the white-background rule in
## coding_conventions.md exempts them.
## Category colours (CAT_LEVELS, FLIP_COL) come from _fig_theme.R, shared
## with the within-model map in 9-1 so the two figures cannot drift apart.
f_name <- file.path(out_dir, "IE_country_between_model.csv")
cdec <- read_csv(f_name, show_col_types = FALSE)

map_theme <- my_theme +
    theme(
        text             = element_text(size = 15),
        plot.title       = element_text(margin = margin(b = -5)),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA)
    )

make_map <- function(est) {
    ## A country is a SIGN REVERSAL when adding the interaction terms flips the
    ## direction of the impact, i.e. delta_m4 and delta_m8 have opposite signs.
    ## Those cases are pooled into one group; the remaining countries keep their
    ## (M4, IE, M8) triple.
    plot_data <- cdec %>%
        filter(estimator == est) %>%
        mutate(
            flip = sign(delta_m4) != sign(delta_m8),
            category = if_else(flip, "flip",
                paste(m4_sign, diff_sign, m8_sign, sep = "_")
            )
        ) %>%
        select(ISO_C3, category, flip)

    ## shares are out of ALL sample countries for this estimator, including any
    ## singleton group dropped below, so the percentages describe the sample
    ## rather than just the mapped groups
    ntot <- nrow(plot_data)
    counts <- plot_data %>%
        count(category, name = "freq") %>%
        mutate(pct = round(100 * freq / ntot))

    ## non-reversal groups: fixed colour and number from CAT_LEVELS, so the
    ## legend means the same thing in both maps; singletons dropped
    unknown <- setdiff(
        counts$category[counts$category != "flip"],
        CAT_LEVELS$category
    )
    if (length(unknown)) {
        stop("category outside CAT_LEVELS: ", paste(unknown, collapse = ", "))
    }

    reg <- counts %>%
        filter(category != "flip", freq > 1) %>%
        inner_join(CAT_LEVELS, by = "category") %>%
        arrange(group) %>%
        mutate(category_label = sprintf(
            "Group %d: (%s), n = %d (%d%%)",
            group, triple, freq, pct
        )) %>%
        select(category, freq, colour, category_label)

    ## the pooled sign-reversal group, also dropped if it is a singleton
    flp <- counts %>%
        filter(category == "flip", freq > 1) %>%
        mutate(category_label = sprintf(
            "Sign reversal, n = %d (%d%%)",
            freq, pct
        )) %>%
        select(category, freq, category_label)

    cat_map <- bind_rows(reg, flp)
    dropped <- setdiff(counts$category, cat_map$category)
    if (length(dropped)) {
        cat(sprintf(
            "[%s] dropped singleton group(s): %s\n", est,
            paste(dropped, collapse = ", ")
        ))
    }

    plot_data <- plot_data %>%
        left_join(cat_map %>% select(category, category_label), by = "category")

    world.map <- ne_countries(scale = "medium", returnclass = "sf")
    miss <- plot_data$ISO_C3[plot_data$ISO_C3 %ni% world.map$iso_a3_eh]
    cat(sprintf("[%s] countries in data but not on the map: %d\n", est, length(miss)))

    world.map <- world.map %>%
        left_join(plot_data, by = c("iso_a3_eh" = "ISO_C3"))

    ## colours come from CAT_LEVELS, not from rank; the reversal group is set apart
    pal <- setNames(reg$colour, reg$category_label)
    if (nrow(flp)) pal <- c(pal, setNames(FLIP_COL, flp$category_label))

    p_map <- ggplot(data = world.map %>%
        filter(continent != "Antarctica", name_en != "Greenland")) +
        geom_sf(aes(fill = category_label), colour = "gray50", lwd = 0.2) +
        scale_fill_manual(
            values = pal, na.value = "gray60",
            breaks = cat_map$category_label,
            ## legend title as plotmath: Category (delta^M=4, IE, delta^M=8)
            name = expression("Category " *
                group("(", list(delta^{
                    "M=4"
                }, IE, delta^{
                    "M=8"
                }), ")"))
        ) +
        coord_sf(datum = NA) +
        labs(title = sprintf("%s, %s, L = %d, between-model", est, SSP, L_MAP)) +
        map_theme +
        theme(
            ## inside the map: bottom-left holds only small islands. The anchor
            ## is the box's BOTTOM-left corner, and this legend carries five
            ## entries against the within-model map's four, so it is set lower
            ## than 9-1's 0.15 to keep the taller box off the land.
            legend.position = c(0.02, 0.1),
            legend.justification = c(0, 0),
            legend.background = element_rect(
                fill = alpha("white", 1),
                colour = NA
            ),
            legend.key.size = unit(0.8, "cm")
        )
    p_map
}

## Test the map function
# make_map("AFE")
# ggsave(file.path(fig_dir, "fig_IE_country_between_model_map_afe.png"),
#     make_map("AFE"),
#     width = 14, height = 6.26, dpi = 200, bg = "transparent"
# )

for (est in c("AFE", "IFE")) {
    ggsave(file.path(fig_dir,
                     sprintf("fig_IE_country_between_model_map_%s.png", tolower(est))),
           make_map(est), width = 14, height = 6.26, dpi = 200, bg = "transparent")
}

cat("\nWrote:\n  output/IE_country_between_model.csv\n",
    " output/IE_country_between_model_crosstab.txt\n",
    " figures/fig_IE_country_between_model_map_{afe,ife}.png\n")
