library(ggplot2)
library(dplyr)

data_dir <- "data/"
fig_dir <- "figs/"

source("R/utils.R")


### Get a set of hyperparameters and important variables to test
cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))

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

#####
# testing Arizona Mountains 
best_fit_ar <- bestr2table[1, c(3:6)]

filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

# Latest ARD data with latest spatial folds
ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

### Break up Fold 4 in the Arizona Mountains forests by appending 
### specific fire names to the fold label
ard_for_task_az <- ard |>
  dplyr::filter(domain == "Arizona Mountains forests") |> 
  dplyr::mutate(spatial_fold = as.character(spatial_fold)) |> 
  dplyr::mutate(spatial_fold = ifelse(spatial_fold == "Fold4", yes = paste0(spatial_fold, "_", Fire), no = spatial_fold)) |> 
  dplyr::mutate(spatial_fold = factor(spatial_fold))

table(ard_for_task_az$spatial_fold)

cv_results_az <- cross_validate(
  data = ard_for_task_az, 
  hyperparameters = best_fit_ar
) 

coef_of_determin(obs=cv_results_az$obs, pred=cv_results_az$pred)

### Cluster by fire instead of spatially
###
ard_for_task_az <- ard |>
  dplyr::filter(domain == "Arizona Mountains forests") |>
  dplyr::select(-spatial_fold) |>
  rsample::group_vfold_cv(group = "Fire")

ard_with_spatial_folds_az = ard_for_task_az |>
  purrr::pmap(.f = unpack_rsample_splits) |>
  data.table::rbindlist() |>
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
  tibble::as_tibble()

cv_results_az <- cross_validate(
  data = ard_with_spatial_folds_az, 
  hyperparameters = best_fit_ar
) 

coef_of_determin(obs=cv_results_az$obs, pred=cv_results_az$pred)

### Trying different spatial arrangements
# Set the seed first
set.seed(20250121)
filename <- "20250804"

# Read the ARD data
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard_no_spatial_folds_yet <- sf::st_read(
  paste0(data_dir, "saved/ARD_", filename, ".gpkg")
  ) |>
  dplyr::filter(!is.na(pcnt_ba_mo)) 

# How many observations per ecoregion?
ard_no_spatial_folds_yet |> 
  dplyr::group_by(ecoregion) |> 
  dplyr::tally() |> 
  dplyr::arrange(n)

# Retain the spatial coordinates so we can easily map after doing the spatial
# folding, then do v-fold spatial cross folding

# 10-fold spatial cross validation
ard_for_task_az = ard_no_spatial_folds_yet |> 
  dplyr::mutate(x = sf::st_coordinates(ard_no_spatial_folds_yet)[, "X"]) |> 
  dplyr::mutate(y = sf::st_coordinates(ard_no_spatial_folds_yet)[, "Y"]) |> 
  dplyr::filter(ecoregion == "Arizona Mountains forests") |>
  spatialsample::spatial_clustering_cv(v = 10)

ard_with_spatial_folds_az = ard_for_task_az |>
  purrr::pmap(.f = unpack_rsample_splits) |>
  data.table::rbindlist() |>
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
  tibble::as_tibble()

ard_with_spatial_folds_az_sf <- ard_with_spatial_folds_az |> 
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(ard))

# View the folds
mapview::mapview(ard_with_spatial_folds_az_sf, zcol = "spatial_fold")

cv_results_az <- cross_validate(
  data = ard_with_spatial_folds_az, 
  hyperparameters = best_fit_ar
) 

coef_of_determin(obs=cv_results_az$obs, pred=cv_results_az$pred)
