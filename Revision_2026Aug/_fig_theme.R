library(tidyverse)
BASE_SIZE <- 11
INK <- "#0b0b0b"
INK_SOFT <- "#52514e"
INK_MUTED <- "#8a8985"

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
    # facet lables if you have multiple panels
    strip.text       = element_text(face = "bold", colour = INK, size = rel(1.05)),
    plot.title       = element_text(face = "bold", size = rel(1.20), hjust = 0.5),
    plot.subtitle    = element_text(colour = INK_SOFT, size = rel(0.95)),
    axis.title       = element_text(colour = INK_SOFT, size = rel(0.95)),
    axis.text        = element_text(colour = INK_SOFT, size = rel(0.90)),
    plot.caption = element_text(
      colour = INK_MUTED, hjust = 0,
      margin = margin(t = 10)
    )
  )


## Category colours for the IE sign-combination maps (9-1 within-model, 8-2
## between-model). Keyed on the category triple so a triple keeps its colour
## and its group number across every map; ranking by frequency within each map
## gives the same triple different colours in different figures.
## Only four non-reversal triples are possible: a non-reversal country has
## sign(delta_counterfactual) == sign(delta_full), leaving the IE sign free.
CAT_LEVELS <- tibble::tribble(
    ~category,                    ~triple,   ~colour,
    "negative_positive_negative", "–, +, –", "#3B4992FF",   # blue
    "negative_negative_negative", "–, –, –", "#EE0000FF",   # red
    "positive_negative_positive", "+, –, +", "#008B45FF",   # green
    "positive_positive_positive", "+, +, +", "#FF8C00"      # orange
)
CAT_LEVELS$group <- seq_len(nrow(CAT_LEVELS))
FLIP_COL <- "#8E44AD"                                       # purple
