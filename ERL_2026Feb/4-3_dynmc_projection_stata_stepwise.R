## Project economic impacts based on Stata dynamic models results
## Step-by-step prediction [NOT implemented yet]
## Evaluate necessity, see whether it makes sense and if the results differ from those of reversing lag polynomial

f_name <- "data/stata/coef_vector.txt"
beta_hat <- read_delim("data/stata/coef_vector.txt",
    delim = "\t"
)
test <- read.tsv(f_name)
beta_hat <- t(beta_hat)
beta_hat <- beta_hat %>% tail(-1)
beta_hat <- beta_hat %>% head(-1)
beta_hat %>% tail()
beta_hat %>% str()
beta_hat <- beta_hat %>%
    as_tibble(rownames = "variable")
colnames(beta_hat)[2] <- "estimate"
beta_hat <- beta_hat %>%
    mutate(estimate = as.numeric(estimate))
# coef estimates preview
beta_hat %>% data.table()

beta_hat$estimate[1:9]

# ============================================================================ #
# Projection -------------------------------------------------------------------

# choose ssp scenario: one of "SSP126", "SSP245", "SSP370", "SSP585"
# ssp <- "SSP126"
# ssp <- "SSP245"
# ssp <- "SSP370"
ssp <- "SSP585"


## Prepare regressor matrix ----------------------------------------------------
## load climate projections
tmp_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_tas.csv", ssp))
pre_df <- read_csv(sprintf("data/%s/climate_trend/climate_trend_pr.csv", ssp))
# convert to annual total precipitation
pre_df <- pre_df %>%
    mutate_at(c("start", "end", "avg", "trend_annual"), ~ . * 12 / 1000)
# calculate country-specific pathways
temp_path_df <- tmp_df %>%
    crossing(year = 1:80) %>% # Repeat each country for 80 years
    mutate(tmp = start + year * trend_annual) # Compute temperature pathway
pre_path_df <- pre_df %>%
    crossing(year = 1:80) %>%
    mutate(pre = start + year * trend_annual)
regressor_df <- temp_path_df[, c("ISO_C3", "year", "tmp")] %>%
    left_join(pre_path_df[, c("ISO_C3", "year", "pre")],
        by = c("ISO_C3", "year")
    )

# contemporaneous regressors
regressor_df <- regressor_df %>%
    mutate(
        tmp2 = tmp^2,
        pre2 = pre^2,
        tmp_pre = tmp * pre,
        tmp2_pre = tmp2 * pre,
        pre2_tmp = pre2 * tmp,
        tmp2_pre2 = tmp2 * pre2
    )

# calculate the increment since BOP
regressor_df <- regressor_df %>%
    group_by(ISO_C3) %>%
    mutate(
        across(
            .cols = -year,
            .fns = ~ .x - first(.x)
        )
    ) %>%
    ungroup()

regressor_df

# Initial dep variable values
f_name <- "data/GDP_reg_panelData.csv"
f_name

Pdata <- read_csv(f_name)
y0 <- Pdata %>% 
    group_by(iso) %>% 
    filter(!is.na(logD_gdp)) %>%
    slice_max(year, n = 1)
y0$year %>% unique()
y0 %>% arrange(year)


