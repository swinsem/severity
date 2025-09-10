source("R/utils.R")

## Get the best model for each ecoregion

library(ggplot2)
library(dplyr)

data_dir <- "data/"
fig_dir <- "figs/"


#### read ard with spatial folds from github severity/data folder
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

# replace the domains with > 300 rows (except western-us) for ecoregional models
domains_to_replace <- ard %>%
  count(domain, name = "n") %>%
  filter(n > 300, domain != "western-us") %>%
  pull(domain)


# Read data
cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))
cpi_results_large = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.1.csv"))

# splice in the new domain data
cpi_results_full <- bind_rows(
  cpi_results_full  %>% filter(!(domain %in% domains_to_replace)),
  cpi_results_large %>% filter(  domain %in% domains_to_replace)
)

## filter to ecoregion models
cpi_results_er = cpi_results_full[cpi_results_full$domain != "western-us", ]
names(cpi_results_er)

head(cpi_results_er)
cpi_results = cpi_results_er |>
  group_by(domain) %>%
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Get the model with the best R2 (overall = plot-based calc rather than avg of fold R2s)
bestr2 <- cpi_results %>%
  group_by(domain) %>%
  slice_max(r2_important_variables_overall, n = 1, with_ties = FALSE) %>%
  ungroup()
names(bestr2)
bestr2table <- bestr2[, c(1:5, 18,20:22,26:27)]

bestr2table$r2_important_variables_overall <- signif(bestr2table$r2_important_variables_overall, digits=3)
bestr2table$r2_mean_important_variables <- signif(bestr2table$r2_mean_important_variables, digits=3)
head(bestr2table)

write.csv(bestr2table, paste0(data_dir, "saved/ecoregion_models_bestr2.csv"))


