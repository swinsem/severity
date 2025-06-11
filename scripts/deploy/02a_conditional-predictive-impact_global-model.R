set.seed(20240724)

# read data and set up spatial folds
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard = sf::st_read("data/ARD_08262024.gpkg") |>
  dplyr::filter(!is.na(pcnt_ba_mo)) |> 
  dplyr::select(-lat, -aspectRad)

ard_for_task = ard |> 
  spatialsample::spatial_clustering_cv(v = 10)

ard_with_spatial_folds = ard_for_task |>
  purrr::pmap(.f = function(id, splits) {
    
    spatial_fold <- id
    assessment_data =
      splits |>
      rsample::assessment() |>
      sf::st_drop_geometry()
    
    return(cbind(assessment_data, spatial_fold))
  }) |>
  data.table::rbindlist() |>
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
  tibble::as_tibble()

features = ard |> 
  sf::st_drop_geometry() |> 
  dplyr::select(-c(PlotID, YrFireName, Dataset, pcnt_ba_mo, UniqueID)) |> 
  colnames()

target = "pcnt_ba_mo"

full_rf_formula = glue::glue("{target} ~ {paste(features, collapse = ' + ')}")

hyperparameters_full =
  expand.grid(
    variablesPerSplit = 3:12, 
    bagFraction = c(0.4, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
    minLeafPopulation = c(1, 5, 10, 25, 50, 60, 70, 80, 90, 100, 125, 150)
  )

# hyperparameters = hyperparameters_full[sample(x = 1:nrow(hyperparameters_full), size = 10), ]
hyperparameters = hyperparameters_full

tune_validate_varselect_assess = function(variablesPerSplit, 
                                          bagFraction, 
                                          minLeafPopulation, 
                                          resampling_approach) {
  
  library(mlr3verse)
  
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
    purrr::map(.f = \(x) tibble::tibble(
      obs = getElement(x, "truth"), 
      preds = getElement(x, "response")
    )
    ) |> 
    data.table::rbindlist()
  
  pred_full = obs_preds_full$preds
  obs_full = obs_preds_full$obs
  
  rmse_full = assessment_full$aggregate(measures = msr("regr.rmse"))
  r2_full = assessment_full$aggregate(measures = msr("regr.rsq"))
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
      purrr::map(.f = \(x) tibble::tibble(
        obs = getElement(x, "truth"), 
        preds = getElement(x, "response")
      )
      ) |> 
      data.table::rbindlist()
    
    pred_important_variables = obs_preds_important_variables$preds
    obs_important_variables = obs_preds_important_variables$obs
    
    # Pull out a specific model skill metric aggregated across the spatial folds
    rmse_important_variables = assessment_important_variables$aggregate(
      measures = msr("regr.rmse")
    )
    
    r2_important_variables = assessment_important_variables$aggregate(
      measures = msr("regr.rsq")
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
    mtry = variablesPerSplit,
    sample.fraction = bagFraction,
    min.node.size = minLeafPopulation,
    rmse_full = rmse_full,
    r2_full = r2_full,
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
    mae_important_variables = mae_important_variables,
    mse_important_variables = mse_important_variables,
    rmse_important_variables_overall = rmse_important_variables_overall,
    r2_important_variables_overall = r2_important_variables_overall,
    mae_important_variables_overall = mae_important_variables_overall,
    mse_important_variables_overall = mse_important_variables_overall,
    cpi_results = list(cpi_results),
  ) |> 
    tidyr::unnest(cols = cpi_results)
  
  return(out)
}


variablesPerSplit = hyperparameters$variablesPerSplit[1]
bagFraction = hyperparameters$bagFraction[1]
minLeafPopulation = hyperparameters$minLeafPopulation[1]
resampling_approach = "resampler"

test_out = tune_validate_varselect_assess(
  variablesPerSplit = variablesPerSplit,
  bagFraction = bagFraction,
  minLeafPopulation = minLeafPopulation,
  resampling_approach = resampling_approach
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
data.table::fwrite(x = results, file = "data/processed/conditional-predictive-impact-results_v4.0.csv")
