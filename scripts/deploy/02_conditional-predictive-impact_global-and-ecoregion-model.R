source("./R/utils.R")

cpi_results_version <- "v7.0"

# install from patched version which allows custom resamplers
# See https://github.com/bips-hb/cpi/pull/22
# unloadNamespace("cpi")
# remotes::install_github(repo = "mikoontz/cpi@resampling-fix") 
library(mlr3verse)

filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

# Read in the analysis-ready data but make sure that the spatial_fold attribute
# is a factor
ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

# Set up the pieces of the random forest model input
# The features (i.e., the predictors)
features <- ard |> 
  sf::st_drop_geometry() |> 
  dplyr::select(
    -c(
      Fire, FireYear, pcnt_ba_mo, 
      UniqueID, ecoregion, spatial_fold, domain
    )
  ) |> 
  colnames()

# The target (i.e., response variable)
target <- "pcnt_ba_mo"

# The full model formula
full_rf_formula <- glue::glue("{target} ~ {paste(features, collapse = ' + ')}")

ard_nested <- ard |> 
  tidyr::nest(.by = "domain", .key = "ard")

# All possible hyperparameters and input data
hyperparameters <-
  tidyr::expand_grid(
    variablesPerSplit = 3:12, 
    bagFraction = c(0.4, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
    minLeafPopulation = c(1, 5, 10, 25, 50, 60, 70, 80, 90, 100, 125, 150),
    ard_nested
  ) |> 
  dplyr::arrange(domain, minLeafPopulation, bagFraction, variablesPerSplit)

# Test one hyperparameter combination for a given input domain
idx <- 1

test_out = tune_validate_varselect_assess(
  variablesPerSplit = hyperparameters$variablesPerSplit[[idx]],
  bagFraction = hyperparameters$bagFraction[[idx]],
  minLeafPopulation = hyperparameters$minLeafPopulation[[idx]],
  resampling_approach = "resampler",
  ard = hyperparameters$ard[[idx]],
  domain = hyperparameters$domain[[idx]]
)

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
  "data/processed/",
  "conditional-predictive-impact-results_{cpi_results_version}.csv"
)

dir.create(dirname(out_fname), recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(
  x = results, 
  file = out_fname
)
