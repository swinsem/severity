library(ggplot2)
library(cpi)
library(caret)
library(rPref)

# set.seed(20240724)
# 
# # read data and set up spatial folds
# # https://spatialsample.tidymodels.org/articles/spatialsample.html
# ard = sf::st_read("data/ARD_08262024.gpkg") |>
#   dplyr::filter(!is.na(pcnt_ba_mo)) |> 
#   dplyr::select(-lat, -aspectRad)

set.seed(20250121)

# read data and set up spatial folds
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard = sf::st_read("data/ARD_01212025.gpkg") |>
  dplyr::filter(!is.na(pcnt_ba_mo)) |> 
  dplyr::select(-lat, -aspectRad)

ard$x_4326 <- sf::st_coordinates(ard)[, "X"]
ard$y_4326 <- sf::st_coordinates(ard)[, "Y"]

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

ard_for_task_by_ecoregion_grouped = ard |>
  dplyr::filter(ecoregion %in% ecoregions_to_model) |> 
  dplyr::group_by(ecoregion)

ard_for_task_by_ecoregion <- ard_for_task_by_ecoregion_grouped |> 
  dplyr::group_split() |> 
  purrr::map(
    .f = spatialsample::spatial_clustering_cv, 
    v = 5, 
    .progress = TRUE
  ) |> 
  setNames(dplyr::group_keys(ard_for_task_by_ecoregion_grouped)$ecoregion)

ard_with_spatial_folds_by_ecoregion = ard_for_task_by_ecoregion |>
  purrr::map(
    .f = \(x) {
      out <- x |> 
        purrr::pmap(
          .f = \(id, splits) {
            
            spatial_fold <- id
            assessment_data =
              splits |>
              rsample::assessment() |>
              sf::st_drop_geometry()
            
            return(cbind(assessment_data, spatial_fold))
          }
        ) |>
        data.table::rbindlist() |>
        dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
        tibble::as_tibble()
      
      out
    }
  )

features = ard |> 
  sf::st_drop_geometry() |> 
  dplyr::select(-c(PlotID, YrFireName, Dataset, pcnt_ba_mo, UniqueID, ecoregion, x_4326, y_4326)) |> 
  colnames()

target = "pcnt_ba_mo"

full_rf_formula = glue::glue("{target} ~ {paste(features, collapse = ' + ')}")

hyperparameters_full =
  tidyr::expand_grid(
    variablesPerSplit = 3:12, 
    bagFraction = c(0.4, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
    minLeafPopulation = c(1, 5, 10, 25, 50, 60, 70, 80, 90, 100, 125, 150),
    ecoregion = ecoregions_to_model
  ) |> 
  dplyr::arrange(ecoregion, minLeafPopulation, bagFraction, variablesPerSplit)

# hyperparameters = hyperparameters_full[sample(x = 1:nrow(hyperparameters_full), size = 10), ]
hyperparameters = hyperparameters_full

tune_validate_varselect_assess = function(variablesPerSplit, 
                                          bagFraction, 
                                          minLeafPopulation, 
                                          resampling_approach,
                                          ecoregion) {
  
  library(mlr3verse)
  
  ard_with_spatial_folds <- ard_with_spatial_folds_by_ecoregion[[ecoregion]]
  
  # Set up the leaner with the (currently) 3 hyperparameters
  learner_sev_biomass <- mlr3::lrn(
    .key = "regr.ranger",
    mtry = variablesPerSplit,
    num.trees = 300,
    sample.fraction = bagFraction,
    replace = FALSE,
    min.node.size = minLeafPopulation,
    num.threads = 1,
    keep.inbag = TRUE
  )
  
  # Set up the task using the formula notation with the full set of predictors
  task_sev_biomass =
    mlr3::as_task_regr(
      x = as.formula(full_rf_formula),
      data = ard_with_spatial_folds[, c(target, features)],
      id = target
    )
  
  # Set up and instantiate the resampler using the known spatial folds as the
  # groups
  resampler_sev_biomass = rsmp("custom_cv")
  resampler_sev_biomass$instantiate(
    task_sev_biomass, 
    f = ard_with_spatial_folds$spatial_fold
  )
  
  if (resampling_approach == "resampler") {
    # Calculate conditional predictive impact
    cpi_results = cpi::cpi(
      task = task_sev_biomass,
      learner = learner_sev_biomass,
      measure = "regr.mse",
      resampling = resampler_sev_biomass,
      test = "t"
    )
  } else if (resampling_approach == "oob") {
    cpi_results = cpi::cpi(
      task = task_sev_biomass,
      learner = learner_sev_biomass,
      measure = "regr.mse",
      resampling = "oob",
      test = "t"
    )
  }
  
  # Spatially cross validated model assessment the {mlr3} way
  assessment_full = resample(
    task = task_sev_biomass, 
    learner = learner_sev_biomass, 
    resampling = resampler_sev_biomass
  )
  
  # Pull out a specific model skill metric aggregated across the spatial folds
  obs_preds_full = assessment_full$predictions(predict_sets = "test") |> 
    purrr::imap(.f = \(x, idx) tibble::tibble(
      obs = getElement(x, "truth"), 
      pred = getElement(x, "response"),
      spatial_fold = idx,
    )
    ) |> 
    data.table::rbindlist()
  
  pred_full = obs_preds_full$pred
  obs_full = obs_preds_full$obs
  
  r2_derivatives_full <- obs_preds_full |> 
    dplyr::group_by(spatial_fold) |> 
    dplyr::summarize(r2 = caret::R2(pred, obs)) |> 
    dplyr::summarize(
      r2_mean_across_spatial_folds = mean(r2, na.rm = TRUE),
      r2_stdev_across_spatial_folds = sd(r2, na.rm = TRUE),
      r2_na_count = sum(is.na(r2))
    ) |> 
    suppressWarnings()
  
  r2_full <- r2_derivatives_full$r2_mean_across_spatial_folds
  r2_stdev_full <- r2_derivatives_full$r2_stdev_across_spatial_folds
  r2_na_count_full <- r2_derivatives_full$r2_na_count
  
  rmse_full = assessment_full$aggregate(measures = msr("regr.rmse"))
  mae_full = assessment_full$aggregate(measures = msr("regr.mae"))
  mse_full = assessment_full$aggregate(measures = msr("regr.mse"))
  
  rmse_full_overall = caret::RMSE(pred = pred_full, obs = obs_full)
  r2_full_overall = caret::R2(pred = pred_full, obs = obs_full)
  mae_full_overall = caret::MAE(pred = pred_full, obs = obs_full)
  mse_full_overall = rmse_full_overall^2
  
  # Initial pass at finding important variables
  important_variables = cpi_results |> 
    dplyr::filter(ci.lo > 0) |> 
    dplyr::pull(Variable)
  
  # Create the formula that could be used for the next iteration of the 
  # spatial cross validation (using the reduced set of only important predictors)
  important_variable_rf_formula = glue::glue(
    "{target} ~ {paste(important_variables, collapse = ' + ')}"
  )
  
  if (variablesPerSplit <= length(important_variables)) {
    # Set up the task using the formula notation with the full set of predictors
    task_sev_biomass_important_variables =
      mlr3::as_task_regr(
        x = as.formula(important_variable_rf_formula),
        data = ard_with_spatial_folds[, c(target, important_variables)],
        id = target
      )
    
    resampler_sev_biomass_important_variables = rsmp("custom_cv")
    resampler_sev_biomass_important_variables$instantiate(
      task_sev_biomass_important_variables, 
      f = ard_with_spatial_folds$spatial_fold
    )
    
    # Spatially cross validated model assessment the {mlr3} way
    assessment_important_variables = resample(
      task = task_sev_biomass_important_variables, 
      learner = learner_sev_biomass, 
      resampling = resampler_sev_biomass_important_variables
    )
    
    obs_preds_important_variables = assessment_important_variables$predictions(predict_sets = "test") |> 
      purrr::imap(.f = \(x, idx) tibble::tibble(
        obs = getElement(x, "truth"), 
        pred = getElement(x, "response"),
        spatial_fold = idx
      )
      ) |> 
      data.table::rbindlist()
    
    pred_important_variables = obs_preds_important_variables$pred
    obs_important_variables = obs_preds_important_variables$obs
    
    # Pull out a specific model skill metric aggregated across the spatial folds
    # Not relying on mlr3 approach gives us some more control, like the ability
    # to calculate a measure of spread of the model skill metric across spatial
    # folds in addition to its central tendency, or an ability to drop NAs in
    # doing either calculation
    r2_derivatives_important_variables <- obs_preds_important_variables |> 
      dplyr::group_by(spatial_fold) |> 
      dplyr::summarize(r2 = caret::R2(pred, obs)) |> 
      dplyr::summarize(
        r2_mean_across_spatial_folds = mean(r2, na.rm = TRUE),
        r2_stdev_across_spatial_folds = sd(r2, na.rm = TRUE),
        r2_na_count = sum(is.na(r2))
      ) |> 
      suppressWarnings()
    
    r2_important_variables <- r2_derivatives_important_variables$r2_mean_across_spatial_folds
    r2_stdev_important_variables <- r2_derivatives_important_variables$r2_stdev_across_spatial_folds
    r2_na_count_important_variables <- r2_derivatives_important_variables$r2_na_count
    
    rmse_important_variables = assessment_important_variables$aggregate(
      measures = msr("regr.rmse")
    )
    
    mae_important_variables = assessment_important_variables$aggregate(
      measures = msr("regr.mae")
    )
    
    mse_important_variables = assessment_important_variables$aggregate(
      measures = msr("regr.mse")
    )
    
    rmse_important_variables_overall = caret::RMSE(pred = pred_important_variables, obs = obs_important_variables)
    r2_important_variables_overall = caret::R2(pred = pred_important_variables, obs = obs_important_variables)
    mae_important_variables_overall = caret::MAE(pred = pred_important_variables, obs = obs_important_variables)
    mse_important_variables_overall = rmse_important_variables_overall^2
    
    
  } else {
    
    rmse_important_variables = NA
    r2_important_variables = NA
    r2_stdev_important_variables = NA
    r2_na_count_important_variables = NA
    mae_important_variables = NA
    mse_important_variables = NA
    rmse_important_variables_overall = NA
    r2_important_variables_overall = NA
    mae_important_variables_overall = NA
    mse_important_variables_overall = NA
    
  }
  
  # Build the final output table that includes the unique set of hyperparameters
  # the CPI resutls, the new formula to use only important variables, the 
  # number of important variables (so we don't bother trying to re-run the 
  # reduced set of variables when the mtry/variablesPerSplit hyperparameter is greater than the
  # number of variables in the model)
  out = tibble::tibble(
    ecoregion = ecoregion,
    n_obs = length(obs_full),
    mtry = variablesPerSplit,
    sample.fraction = bagFraction,
    min.node.size = minLeafPopulation,
    rmse_full = rmse_full,
    r2_full = r2_full,
    r2_stdev_full = r2_stdev_full,
    r2_na_count_full = r2_na_count_full,
    mae_full = mae_full,
    mse_full = mse_full,
    rmse_full_overall = rmse_full_overall,
    r2_full_overall = r2_full_overall,
    mae_full_overall = mae_full_overall,
    mse_full_overall = mse_full_overall,
    n_important_variables = length(important_variables),
    important_variable_rf_formula = important_variable_rf_formula,
    rmse_important_variables = rmse_important_variables,
    r2_important_variables = r2_important_variables,
    r2_stdev_important_variables = r2_stdev_important_variables,
    r2_na_count_important_variables = r2_na_count_important_variables,
    mae_important_variables = mae_important_variables,
    mse_important_variables = mse_important_variables,
    rmse_important_variables_overall = rmse_important_variables_overall,
    r2_important_variables_overall = r2_important_variables_overall,
    mae_important_variables_overall = mae_important_variables_overall,
    mse_important_variables_overall = mse_important_variables_overall,
    cpi_results = list(cpi_results),
  ) |> 
    tidyr::unnest(cols = cpi_results)
  
  out
}

idx <- 1

variablesPerSplit = hyperparameters$variablesPerSplit[idx]
bagFraction = hyperparameters$bagFraction[idx]
minLeafPopulation = hyperparameters$minLeafPopulation[idx]
ecoregion = hyperparameters$ecoregion[idx]
resampling_approach = "resampler"

test_out = tune_validate_varselect_assess(
  variablesPerSplit = variablesPerSplit,
  bagFraction = bagFraction,
  minLeafPopulation = minLeafPopulation,
  resampling_approach = resampling_approach,
  ecoregion = ecoregion
)

# # Define the learner to be a {ranger} regression and give it the tuned hyperparameters
tictoc::tic()
future::plan(future::multisession, workers = 10)
# future::plan("sequential")
results_list = furrr::future_pmap(
  .l = hyperparameters, 
  .progress = TRUE,
  .options = furrr::furrr_options(seed = TRUE),
  .f = tune_validate_varselect_assess,
  resampling_approach = "resampler"
)
tictoc::toc()

results = data.table::rbindlist(results_list)

dir.create("data/processed")
data.table::fwrite(x = results, file = "data/processed/conditional-predictive-impact-results_v5.1.csv")
