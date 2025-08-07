#' @description
#' Computes sum of squares for residuals
#' @param obs observed values, y
#' @param pred predicted values, yhat
#' @returns The sum of squares of the residuals
#' 
sum_squares_residuals <- function(obs, pred) {
  sum((obs - pred)^2)
}

#' @description
#' Computes sum of squares total
#' @param obs observed values, y
#' @returns The sum of squares total
#' 
sum_squares_total <- function(obs) {
  sum((obs - mean(obs))^2)
}

#' @description
#' Computes the coefficient of determination
#' @param obs observed values, y
#' @param pred predicted values, yhat
#' @returns The coefficient of determination (R^2)
#' 
coef_of_determin <- function(obs, pred) {
  ss_res <- sum_squares_residuals(obs, pred)
  ss_tot <- sum_squares_total(obs)
  1 - (ss_res / ss_tot)
}

#' @description
#' Takes rsample-like data and returns in a more interoperable format
#' @param id A unique id for the split
#' @param splits the rsample split itself
#' @returns A tibble
unpack_rsample_splits <- function(id, splits) {
  
  spatial_fold <- id
  assessment_data <- splits |>
    rsample::assessment() |>
    sf::st_drop_geometry()
  
  cbind(assessment_data, spatial_fold)
}

#' @description
#' Tunes the ranger random forest model, selects variables, and measures model skill
#' using spatial cross validation 
#' @param variablesPerSplit ranger random forest hyperparameter
#' @param bagFraction ranger random forest hyperparameter
#' @param minLeafPopulation ranger random forest hyperparameter
#' @param resampling_approach One of "resampling" or "oob" to determine how
#' the conditional predictive impact works
#' @param ard A tibble representing the data to be modeled; must have already
#' been divided up into proper spatial folds with a factor attribute that
#' represents factor level/spatial fold membership
#' @param domain The name of the region that the `ard` data represent (either
#' 'western-us' or one of the ecoregions)
#' @returns A tibble with the result of every variables conditional predictive
#' impact for the given hyperparameter combination and input data
#' 
tune_validate_varselect_assess <- function(variablesPerSplit, 
                                           bagFraction, 
                                           minLeafPopulation, 
                                           resampling_approach,
                                           ard,
                                           domain) {
  
  # Set up the leaner with the (currently) 3 hyperparameters
  learner_sev_biomass <- mlr3::lrn(
    .key = "regr.ranger",
    mtry = variablesPerSplit,
    num.trees = 500,
    sample.fraction = bagFraction,
    replace = FALSE,
    min.node.size = minLeafPopulation,
    num.threads = 1,
    keep.inbag = TRUE
  )
  
  # Set up the task using the formula notation with the full set of predictors
  task_sev_biomass <-
    mlr3::as_task_regr(
      x = as.formula(full_rf_formula),
      data = ard[, c(target, features)],
      id = target
    )
  
  # Set up and instantiate the resampler using the known spatial folds as the
  # groups
  resampler_sev_biomass <- rsmp("custom_cv")
  resampler_sev_biomass$instantiate(
    task_sev_biomass, 
    f = ard$spatial_fold
  )
  
  if (resampling_approach == "resampler") {
    # Calculate conditional predictive impact
    cpi_results <- cpi::cpi(
      task = task_sev_biomass,
      learner = learner_sev_biomass,
      measure = "regr.mse",
      resampling = resampler_sev_biomass,
      test = "t"
    )
  } else if (resampling_approach == "oob") {
    cpi_results <- cpi::cpi(
      task = task_sev_biomass,
      learner = learner_sev_biomass,
      measure = "regr.mse",
      resampling = "oob",
      test = "t"
    )
  }
  
  # Spatially cross validated model assessment the {mlr3} way
  assessment_full <- resample(
    task = task_sev_biomass, 
    learner = learner_sev_biomass, 
    resampling = resampler_sev_biomass
  )
  
  # Pull out a specific model skill metric aggregated across the spatial folds
  obs_preds_full <- assessment_full$predictions(predict_sets = "test") |> 
    purrr::imap(.f = \(x, idx) tibble::tibble(
      obs = getElement(x, "truth"), 
      pred = getElement(x, "response"),
      spatial_fold = idx,
    )
    ) |> 
    data.table::rbindlist()
  
  pred_full <- obs_preds_full$pred
  obs_full <- obs_preds_full$obs
  
  r2_derivatives_full <- obs_preds_full |> 
    dplyr::group_by(spatial_fold) |> 
    dplyr::summarize(
      ss_res = sum_squares_residuals(obs, pred),
      ss_tot = sum_squares_total(obs),
      r2 = coef_of_determin(obs, pred)
    ) |> 
    # Don't include spatial folds where sum of squares total is 0
    dplyr::summarize(
      r2_mean_across_spatial_folds = mean(r2[ss_tot != 0], na.rm = TRUE),
      r2_median_across_spatial_folds = median(r2[ss_tot != 0], na.rm = TRUE),
      r2_stdev_across_spatial_folds = sd(r2[ss_tot != 0], na.rm = TRUE),
      r2_na_count = sum(ss_tot == 0)
    ) |> 
    suppressWarnings()
  
  r2_mean_full <- r2_derivatives_full$r2_mean_across_spatial_folds
  r2_median_full <- r2_derivatives_full$r2_median_across_spatial_folds
  r2_stdev_full <- r2_derivatives_full$r2_stdev_across_spatial_folds
  r2_na_count_full <- r2_derivatives_full$r2_na_count
  
  rmse_full <- assessment_full$aggregate(measures = msr("regr.rmse"))
  mae_full <- assessment_full$aggregate(measures = msr("regr.mae"))
  mse_full <- assessment_full$aggregate(measures = msr("regr.mse"))
  
  rmse_full_overall <- caret::RMSE(pred = pred_full, obs = obs_full)
  r2_full_overall <- coef_of_determin(pred = pred_full, obs = obs_full)
  mae_full_overall <- caret::MAE(pred = pred_full, obs = obs_full)
  mse_full_overall <- rmse_full_overall^2
  
  # Initial pass at finding important variables
  important_variables <- cpi_results |> 
    dplyr::filter(ci.lo > 0) |> 
    dplyr::pull(Variable)
  
  # Create the formula that could be used for the next iteration of the 
  # spatial cross validation (using the reduced set of only important predictors)
  important_variable_rf_formula = glue::glue(
    "{target} ~ {paste(important_variables, collapse = ' + ')}"
  )
  
  if (variablesPerSplit <= length(important_variables)) {
    # Set up the task using the formula notation with the full set of predictors
    task_sev_biomass_important_variables <- mlr3::as_task_regr(
      x = as.formula(important_variable_rf_formula),
      data = ard[, c(target, important_variables)],
      id = target
    )
    
    resampler_sev_biomass_important_variables <- rsmp("custom_cv")
    resampler_sev_biomass_important_variables$instantiate(
      task_sev_biomass_important_variables, 
      f = ard$spatial_fold
    )
    
    # Spatially cross validated model assessment the {mlr3} way
    assessment_important_variables <- resample(
      task = task_sev_biomass_important_variables, 
      learner = learner_sev_biomass, 
      resampling = resampler_sev_biomass_important_variables
    )
    
    obs_preds_important_variables <- assessment_important_variables$predictions(predict_sets = "test") |> 
      purrr::imap(
        .f = \(x, idx) {
          tibble::tibble(
            obs = getElement(x, "truth"), 
            pred = getElement(x, "response"),
            spatial_fold = idx
          )
        }
      ) |> 
      data.table::rbindlist()
    
    pred_important_variables <- obs_preds_important_variables$pred
    obs_important_variables <- obs_preds_important_variables$obs
    
    # Pull out a specific model skill metric aggregated across the spatial folds
    # Not relying on mlr3 approach gives us some more control, like the ability
    # to calculate a measure of spread of the model skill metric across spatial
    # folds in addition to its central tendency, or an ability to drop NAs in
    # doing either calculation
    r2_derivatives_important_variables <- obs_preds_important_variables |> 
      dplyr::group_by(spatial_fold) |> 
      dplyr::summarize(
        ss_res = sum_squares_residuals(obs, pred),
        ss_tot = sum_squares_total(obs),
        r2 = coef_of_determin(obs, pred)
      ) |> 
      # Don't include spatial folds where sum of squares total is 0
      dplyr::summarize(
        r2_mean_across_spatial_folds = mean(r2[ss_tot != 0], na.rm = TRUE),
        r2_median_across_spatial_folds = median(r2[ss_tot != 0], na.rm = TRUE),
        r2_stdev_across_spatial_folds = sd(r2[ss_tot != 0], na.rm = TRUE),
        r2_na_count = sum(ss_tot == 0)
      ) |> 
      suppressWarnings()
    
    r2_mean_important_variables <- r2_derivatives_important_variables$r2_mean_across_spatial_folds
    r2_median_important_variables <- r2_derivatives_important_variables$r2_median_across_spatial_folds
    r2_stdev_important_variables <- r2_derivatives_important_variables$r2_stdev_across_spatial_folds
    r2_na_count_important_variables <- r2_derivatives_important_variables$r2_na_count
    
    rmse_important_variables <- assessment_important_variables$aggregate(
      measures = msr("regr.rmse")
    )
    
    mae_important_variables <- assessment_important_variables$aggregate(
      measures = msr("regr.mae")
    )
    
    mse_important_variables <- assessment_important_variables$aggregate(
      measures = msr("regr.mse")
    )
    
    rmse_important_variables_overall <- caret::RMSE(pred = pred_important_variables, obs = obs_important_variables)
    r2_important_variables_overall <- coef_of_determin(pred = pred_important_variables, obs = obs_important_variables)
    mae_important_variables_overall <- caret::MAE(pred = pred_important_variables, obs = obs_important_variables)
    mse_important_variables_overall <- rmse_important_variables_overall^2
    
    
  } else {
    
    rmse_important_variables = NA
    r2_mean_important_variables = NA
    r2_median_important_variables = NA
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
    domain = domain,
    n_obs = length(obs_full),
    mtry = variablesPerSplit,
    sample.fraction = bagFraction,
    min.node.size = minLeafPopulation,
    rmse_full = rmse_full,
    r2_mean_full = r2_mean_full,
    r2_median_full = r2_median_full,
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
    r2_mean_important_variables = r2_mean_important_variables,
    r2_median_important_variables = r2_median_important_variables,
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


#' @description
#' Performs a cross-validation of a ranger random forest model
#' @param data 
#' @param hyperparameters A tibble representing the hyperparameter values to use
#' for the random forest model
#' @returns A tibble with each row representing the observed percent basal area
#' loss for one observation and the model prediction using a model trained on
#' all the data not belonging to the observation's spatial fold

cross_validate <- function(data, hyperparameters) {
  results <- list() 
  
  spatial_folds <- unique(data$spatial_fold)
  
  for(i in seq_along(spatial_folds)) {
    
    train_data = data |> 
      dplyr::filter(spatial_fold != spatial_folds[i])
    
    test_data = data |> 
      dplyr::filter(spatial_fold == spatial_folds[i])
    
    fm = ranger::ranger(
      formula = as.formula(hyperparameters$important_variable_rf_formula), 
      data = train_data, 
      num.trees = 1000, 
      mtry = hyperparameters$mtry, 
      min.node.size = hyperparameters$min.node.size, 
      sample.fraction = hyperparameters$sample.fraction
    )
    # Store results with UniqueID and BA loss
    fold_results <- data.frame(
      unique_id = test_data$UniqueID,  
      obs = test_data$pcnt_ba_mo,  
      pred = predict(object = fm, data = test_data)$predictions,
      fold = i  
    )
    
    # Append to the list of results
    results[[i]] <- fold_results
  }
  
  # Combine all fold results into a single data frame
  combined_results <- do.call(rbind, results)
  
  # Check for potential mismatches
  if (any(is.na(combined_results$obs) | is.na(combined_results$pred))) {
    warning("NA values found in observations or predictions.")
  }
  
  combined_results
  
}