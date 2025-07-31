source("scripts/deploy/02a_conditional-predictive-impact-functions.R")

cpi_results_version <- "v6.0"

# install from patched version which allows custom resamplers
# See https://github.com/bips-hb/cpi/pull/22
# unloadNamespace("cpi")
# remotes::install_github(repo = "mikoontz/cpi@resampling-fix") 

set.seed(20250121)

# read data and set up spatial folds
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard <- sf::st_read("data/ARD_01212025.gpkg") |>
  dplyr::filter(!is.na(pcnt_ba_mo)) |> 
  dplyr::select(-lat, -aspectRad)

ard$x_4326 <- sf::st_coordinates(ard)[, "X"]
ard$y_4326 <- sf::st_coordinates(ard)[, "Y"]

# Set up the pieces of the random forest model input
# The features (i.e., the predictors)
features <- ard |> 
  sf::st_drop_geometry() |> 
  dplyr::select(
    -c(
      PlotID, YrFireName, Dataset, pcnt_ba_mo, 
      UniqueID, ecoregion, x_4326, y_4326
    )
  ) |> 
  colnames()

# The target (i.e., response variable)
target <- "pcnt_ba_mo"

# The full model formula
full_rf_formula <- glue::glue("{target} ~ {paste(features, collapse = ' + ')}")

# Set up the input data
# spatial cross validation for the global model
ard_for_task = ard |> 
  spatialsample::spatial_clustering_cv(v = 10)

ard_with_spatial_folds_global = ard_for_task |>
  purrr::pmap(.f = unpack_rsample_splits) |>
  data.table::rbindlist() |>
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
  tibble::as_tibble()

ard_with_spatial_folds_global <- list(ard_with_spatial_folds_global) |> 
  setNames("western-us")

# Next set up ecoregion specific models
ecoregions_to_model <- ard |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(ecoregion) |> 
  dplyr::summarize(n = dplyr::n()) |> 
  dplyr::filter(n >= 10) |> 
  dplyr::pull(ecoregion)

# ecoregions_to_model <- c(
#   'Eastern Cascades forests', 
#   'Central-Southern Cascades Forests', 
#   'Sierra Nevada forests', 
#   'Klamath-Siskiyou forests', 
#   'South Central Rockies forests', 
#   'North Cascades conifer forests', 
#   'Northern Rockies conifer forests', 
#   'Blue Mountains forests', 
#   'Arizona Mountains forests', 
#   'Montana Valley and Foothill grasslands', 
#   'Wasatch and Uinta montane forests', 
#   'Colorado Plateau shrublands', 
#   'Colorado Rockies forests'
# )

ard_for_task_by_ecoregion_grouped <- ard |>
  dplyr::filter(ecoregion %in% ecoregions_to_model) |> 
  dplyr::group_by(ecoregion)

# Just 5-fold spatial cross validation if we've already subset to individual
# ecoregions otherwise the folds will be too sparse. They are arguably too
# sparse even at 5 folds given that some of those folds within ecoregions
# have no variation in observed basal area loss
ard_for_task_by_ecoregion <- ard_for_task_by_ecoregion_grouped |> 
  dplyr::group_split() |> 
  purrr::map(
    .f = spatialsample::spatial_clustering_cv, 
    v = 5, 
    .progress = TRUE
  ) |> 
  setNames(dplyr::group_keys(ard_for_task_by_ecoregion_grouped)$ecoregion)

ard_with_spatial_folds_by_ecoregion <- ard_for_task_by_ecoregion |>
  purrr::map(
    .f = \(x) {
      out <- purrr::pmap(.l = x, .f = unpack_rsample_splits) |>
        data.table::rbindlist() |>
        dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
        tibble::as_tibble()
      
      out
    }
  )

# Combine the global analysis-ready data with the ecoregion-specific (both
# of which already have their spatial folds set up)
ard_with_spatial_folds <- tibble::tibble(
  ard = c(
    ard_with_spatial_folds_global, 
    ard_with_spatial_folds_by_ecoregion
  ),
  domain = names(ard)
)

# All possible hyperparameters and input data
hyperparameters <-
  tidyr::expand_grid(
    variablesPerSplit = 3:12, 
    bagFraction = c(0.4, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
    minLeafPopulation = c(1, 5, 10, 25, 50, 60, 70, 80, 90, 100, 125, 150),
    ard_with_spatial_folds
  ) |> 
  dplyr::arrange(domain, minLeafPopulation, bagFraction, variablesPerSplit)

# Test one hyperparameter combination for a given input domain
# idx <- 1
# 
# test_out = tune_validate_varselect_assess(
#   variablesPerSplit = hyperparameters$variablesPerSplit[[idx]],
#   bagFraction = hyperparameters$bagFraction[[idx]],
#   minLeafPopulation = hyperparameters$minLeafPopulation[[idx]],
#   resampling_approach = "resampler",
#   ard = hyperparameters$ard[[idx]],
#   domain = hyperparameters$domain[[idx]]
# )

# Define the learner to be a {ranger} regression and give it the 
# tuned hyperparameters
tictoc::tic()
future::plan(future::multisession, workers = 10)
results_list = furrr::future_pmap(
  .l = hyperparameters, 
  .progress = TRUE,
  .options = furrr::furrr_options(
    seed = TRUE, 
    packages = c("mlr3verse", "cpi", "ranger")
  ),
  .f = tune_validate_varselect_assess,
  resampling_approach = "resampler"
)
tictoc::toc()

results = data.table::rbindlist(results_list)

out_fname <- glue::glue(
  "data/interim/",
  "conditional-predictive-impact-results_{cpi_results_version}.csv"
)

dir.create(dirname(out_fname), recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(
  x = results, 
  file = out_fname
)
