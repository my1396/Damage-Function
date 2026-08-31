## COUNTRY-LEVEL decomposition of the M = 8 projection into direct and
## interactive contributions (9_decompose_IE_effects.R does the global one).
##
## For each country i, at 2100:
##     delta^All_i   full 8-term coefficient vector
##     delta^Dir_i   same coefficients, four interaction terms zeroed
##     IE_i = delta^All_i - delta^Dir_i        (paper eq. 13)
##
## A POSITIVE IE means the interaction MITIGATES damage for that country.
##
## Outputs
##   output/IE_country_decomposition.csv   per-country deltas, IE, signs, region
##   output/IE_country_crosstab.txt        2x2x2 sign-combination cross table
##   figures/fig_IE_country_map.png        categorical map of sign combinations
##
## Follows ERL_2026Feb/5-2_IE-effects_dynmc.R (cross table) and
## ERL_2026Feb/5-2-1_plot_IE-effects.R (map).
## ========================================================================== ##

suppressMessages({
    library(tidyverse)
    library(knitr)
    library(rnaturalearth)
    library(sf)
})

root_dir <- "/Users/menghan/Documents/GDP/Shared folder"
setwd(root_dir)
source(file.path(root_dir, "Revision_2026Aug", "_projection_common.R"))
## CAT_LEVELS / FLIP_COL: category colours shared with the between-model map
## in 8-2, so a triple keeps its colour and number across all four figures.
## (my_theme from this file is deliberately overridden below at BASE_SIZE 15.)
source(file.path(root_dir, "Revision_2026Aug", "_fig_theme.R"))
out_dir <- file.path(root_dir, "Revision_2026Aug", "output")
fig_dir <- file.path(root_dir, "Revision_2026Aug", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

SSP     <- "SSP585"
LAGS    <- 0            # decomposition is only identified at L = 0
regs    <- REGS_INTERACT
idx_dir <- 1:4

`%ni%` <- Negate(`%in%`)

inp  <- load_projection_inputs(SSP, root_dir)
lagc <- read_csv(file.path(out_dir, "lag_coefficients_long.csv"), show_col_types = FALSE)
region <- read_csv(file.path(root_dir, "data/region_category.csv"),
                   show_col_types = FALSE) %>%
    select(ISO_C3, cntry.name, region)

## ========================================================================== ##
## 1. Country-level deltas -----------------------------------------------------
## ========================================================================== ##
decompose_country <- function(est, L = 0) {
    B    <- beta_matrix(lagc, "Interactive", est, L)
    Bdir <- B; Bdir[, -idx_dir] <- 0          # zero the interaction coefficients

    d_all <- country_delta(eta_matrix(inp$cl, B,    regs), inp$G)[, PROJ_HORIZ]
    d_dir <- country_delta(eta_matrix(inp$cl, Bdir, regs), inp$G)[, PROJ_HORIZ]

    tibble(
        ISO_C3    = inp$cl$ISO_C3,
        estimator = est,
        L         = L,
        delta_all = d_all,
        delta_dir = d_dir,
        IE_effect = d_all - d_dir
    )
}

cdec <- map_dfr(c("AFE", "IFE"), decompose_country) %>%
    mutate(
        ## sign labels, mirroring 5-2_IE-effects_dynmc.R
        diff_sign    = if_else(IE_effect >= 0, "positive", "negative"),  # IE
        inter_sign   = if_else(delta_all >= 0, "positive", "negative"),  # All
        nointer_sign = if_else(delta_dir >= 0, "positive", "negative")   # Dir
    ) %>%
    left_join(region, by = "ISO_C3")

f_name <- file.path(out_dir, "IE_country_decomposition.csv")
write_csv(cdec, f_name)

cdec <- read_csv(f_name, show_col_types = FALSE)
## ========================================================================== ##
## 2. Cross table of sign combinations (2 x 2 x 2), per estimator --------------
## ========================================================================== ##
format_cross_tab <- function(d, show_pct = FALSE) {
    all_comb <- expand.grid(
        diff_sign    = c("negative", "positive"),
        inter_sign   = c("negative", "positive"),
        nointer_sign = c("negative", "positive")
    )
    counts <- d %>%
        group_by(diff_sign, inter_sign, nointer_sign) %>%
        summarise(n_countries = n(), .groups = "drop") %>%
        right_join(all_comb, by = c("diff_sign", "inter_sign", "nointer_sign")) %>%
        mutate(n_countries = replace_na(n_countries, 0))
    total_n <- nrow(d)

    one <- function(sign_val) {
        tb <- counts %>%
            filter(diff_sign == sign_val) %>%
            select(-diff_sign) %>%
            mutate(
                inter_sign   = factor(inter_sign,   levels = c("positive", "negative")),
                nointer_sign = factor(nointer_sign, levels = c("positive", "negative"))
            ) %>%
            pivot_wider(names_from = nointer_sign, values_from = n_countries) %>%
            arrange(inter_sign) %>%
            mutate(inter_sign = paste("All:", inter_sign)) %>%
            column_to_rownames("inter_sign") %>%
            select(positive, negative)
        if (show_pct) tb <- tb %>% mutate(across(everything(),
                                                 ~ sprintf("%.1f%%", . / total_n * 100)))
        colnames(tb) <- paste("Dir:", colnames(tb))
        tb
    }

    capture.output({
        cat("IE > 0  (interaction makes the GDP impact BETTER)\n")
        cat(strrep("=", 50), "\n")
        print(kable(one("positive"), format = "simple"))
        cat("\n\nIE < 0  (interaction makes the GDP impact WORSE)\n")
        cat(strrep("=", 50), "\n")
        print(kable(one("negative"), format = "simple"))
    })
}

sink(file.path(out_dir, "IE_country_crosstab.txt"))
for (est in c("AFE", "IFE")) {
    d <- cdec %>% filter(estimator == est)
    cat("\n##########################################################\n")
    cat(sprintf("## %s --- %d countries, SSP585, L = 0, impacts at 2100\n", est, nrow(d)))
    cat("##########################################################\n\n")
    cat(paste(format_cross_tab(d), collapse = "\n"), "\n")
    cat("\n-- as % of countries --\n")
    cat(paste(format_cross_tab(d, show_pct = TRUE), collapse = "\n"), "\n")
    cat("\n-- regional composition of the dominant cell (IE>0, All<0, Dir<0) --\n")
    print(as.data.frame(
        d %>% filter(diff_sign == "positive",
                     inter_sign == "negative",
                     nointer_sign == "negative") %>%
            count(region, name = "n") %>% arrange(desc(n))), row.names = FALSE)
}
sink()

cat("=== cross table (AFE) ===\n")
cat(paste(format_cross_tab(cdec %>% filter(estimator == "AFE")), collapse = "\n"), "\n")

## ========================================================================== ##
## 3. Categorical map of sign combinations -------------------------------------
## ========================================================================== ##
## Label order: (Dir, IE, All) -- i.e. (delta_dir, IE_effect, delta_all)
BASE_SIZE <- 15
COL_AFE <- "#2a78d6"
COL_IFE <- "#eb6834"
INK_SOFT <- "#52514e"

my_theme <- theme_minimal(base_size = BASE_SIZE) +
    theme(
        legend.position  = "top",
        legend.box       = "horizontal",
        legend.title     = element_text(size = rel(1)),
        legend.text      = element_text(size = rel(1)),
        legend.key.width = unit(24, "pt"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e1", linewidth = 0.3),
        panel.spacing    = unit(13, "pt"),
        strip.text       = element_text(face = "bold", size = rel(1.05)),
        plot.title       = element_text(face = "bold", size = rel(1.20), hjust = 0.5, margin = margin(b = -5)),
        plot.subtitle    = element_text(colour = INK_SOFT, size = rel(0.95)),
        axis.title       = element_text(colour = INK_SOFT, size = rel(0.95)),
        axis.text        = element_text(colour = INK_SOFT, size = rel(0.90)),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
    )

make_map <- function(est) {
    ## A country is a SIGN REVERSAL when the interaction flips the direction of the
    ## impact, i.e. delta_dir and delta_all have opposite signs. Those cases are
    ## pooled into one group; the remaining countries keep their (Dir, IE, All) triple.
    plot_data <- cdec %>%
        filter(estimator == est) %>%
        mutate(
            flip     = sign(delta_dir) != sign(delta_all),
            category = if_else(flip, "flip",
                               paste(nointer_sign, diff_sign, inter_sign, sep = "_"))
        ) %>%
        select(ISO_C3, category, flip)

    ## shares are out of ALL sample countries for this estimator, including any
    ## singleton group dropped below, so the percentages describe the sample
    ## rather than just the mapped groups
    ntot   <- nrow(plot_data)
    counts <- plot_data %>% count(category, name = "freq") %>%
        mutate(pct = round(100 * freq / ntot))

    ## non-reversal groups: fixed colour and number from CAT_LEVELS, so the
    ## legend means the same thing in every map; singletons dropped
    unknown <- setdiff(counts$category[counts$category != "flip"],
                       CAT_LEVELS$category)
    if (length(unknown))
        stop("category outside CAT_LEVELS: ", paste(unknown, collapse = ", "))

    reg <- counts %>%
        filter(category != "flip", freq > 1) %>%
        inner_join(CAT_LEVELS, by = "category") %>%
        arrange(group) %>%
        mutate(category_label = sprintf("Group %d: (%s), n = %d (%d%%)",
                                        group, triple, freq, pct)) %>%
        select(category, freq, colour, category_label)

    ## the pooled sign-reversal group, also dropped if it is a singleton
    flp <- counts %>%
        filter(category == "flip", freq > 1) %>%
        mutate(category_label = sprintf("Sign reversal, n = %d (%d%%)",
                                        freq, pct)) %>%
        select(category, freq, category_label)

    cat_map <- bind_rows(reg, flp)
    dropped  <- setdiff(counts$category, cat_map$category)
    if (length(dropped))
        cat(sprintf("[%s] dropped singleton group(s): %s\n", est,
                    paste(dropped, collapse = ", ")))

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

    ggplot(data = world.map %>%
               filter(continent != "Antarctica", name_en != "Greenland")) +
        geom_sf(aes(fill = category_label), colour = "gray50", lwd = 0.2) +
        scale_fill_manual(
            values = pal, na.value = "gray60",
            breaks = cat_map$category_label,
            ## legend title as a plotmath expression: Category (delta^Dir, IE, delta^All)
            name = expression("Category " *
                              group("(", list(delta^Dir, IE, delta^All), ")"))
        ) +
        coord_sf(datum = NA) +
        labs(title = sprintf("%s, %s, L = 0", est, SSP)) +
        my_theme +
        theme(
            ## inside the map: bottom-left holds only small islands
            legend.position      = c(0, 0.15),
            legend.justification = c(0, 0),
            legend.background    = element_rect(fill = alpha("white", 1),
                                                colour = NA),
            legend.key.size      = unit(0.8, "cm")
        )
}

for (est in c("AFE", "IFE")) {
    ggsave(file.path(fig_dir, sprintf("fig_IE_country_map_%s.png", tolower(est))),
           make_map(est), width = 14, height = 6.26, dpi = 200, bg = "transparent")
}

cat("\nWrote:\n  output/IE_country_decomposition.csv\n",
    " output/IE_country_crosstab.txt\n",
    " figures/fig_IE_country_map_{afe,ife}.png\n")
