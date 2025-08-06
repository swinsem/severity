source("./R/utils.R")

# Random forest cross validation

library(ranger)

data_dir <- "research/severity/data/"
fig_dir <- "research/severity/figs/"



# Read data
cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))

cpi_results = cpi_results_full |>
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Plot Pareto frontier of cross-fold mean R2 and overall R2 of important variable reduced model
r2_pareto_front = rPref::psel(
  df = cpi_results,
  pref = rPref::high(r2_important_variables_overall) * rPref::high(r2_important_variables)
)

top_results = r2_pareto_front |> na.omit()

best_fit = top_results[2,]


####
here::here()
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)
ard = read.csv(paste0(data_dir, "saved/ARD_20250804_with-spatial-folds.gpkg")) 
spatfolds = read.csv()

features = ard |> 
  sf::st_drop_geometry() |> 
  dplyr::select(-c(PlotID, YrFireName, Dataset, pcnt_ba_mo, UniqueID)) |> 
  colnames()

target = "pcnt_ba_mo"


## using top_results from other script
best_fit = top_results[2,]


spatial_folds = unique(ard_with_spatial_folds$spatial_fold)

##### cross-validation #####

# cross validation function
cross_validate <- function(data) {  
  results <- list() 
  
  for(i in seq_along(spatial_folds)) {
   
    train_data = ard_with_spatial_folds |> 
      sf::st_drop_geometry() |> 
      dplyr::filter(spatial_fold != spatial_folds[i])
    
    test_data = ard_with_spatial_folds |> 
      sf::st_drop_geometry() |> 
      dplyr::filter(spatial_fold == spatial_folds[i])
    
    fm = ranger::ranger(
      formula = as.formula(best_fit$important_variable_rf_formula), 
      data = train_data, 
      num.trees = 1000, 
      mtry = best_fit$mtry, 
      min.node.size = best_fit$min.node.size, 
      sample.fraction = best_fit$sample.fraction
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
  
  return(combined_results)

}


# Run CV function
cv_results <- cross_validate(data = ard_with_spatial_folds) 


# str(full_model_results)
str(cv_results)
names(cv_results) <- c("UniqueID", "obs", "pred", "fold")

# Check results
head(cv_results)
caret::R2(obs=cv_results$obs, pred=cv_results$pred)
caret::RMSE(obs=cv_results$obs, pred=cv_results$pred)
caret::MAE(obs=cv_results$obs, pred=cv_results$pred)


### Categorical binning

## three classes (Miller et al 2009)
cv_results$pred_bin <- ifelse(cv_results$pred < .25, 1, 
                                  ifelse(cv_results$pred >=.25 & cv_results$pred < .75, 2, 3))
cv_results$obs_bin <- ifelse(cv_results$obs < .25, 1,  
                                 ifelse(cv_results$obs >=.25 & cv_results$obs < .75, 2, 3))

## five classes - overwrite above
cv_results$pred_bin <- ifelse(cv_results$pred < .25, 1, 
                                      ifelse(cv_results$pred >=.25 & cv_results$pred < .5, 2, 
                                             ifelse(cv_results$pred >=.5 & cv_results$pred < .75, 3, 
                                                    ifelse(cv_results$pred >=.75 & cv_results$pred < .9, 4, 5))))
cv_results$obs_bin <- ifelse(cv_results$obs < .25, 1,  
                                     ifelse(cv_results$obs >=.25 & cv_results$obs < .5, 2, 
                                            ifelse(cv_results$obs >=.5 & cv_results$obs < .75, 3, 
                                                   ifelse(cv_results$obs >=.75 & cv_results$obs < .9, 4, 5))))
# Save results
write.csv(cv_results, paste0(data_dir, "saved/ranger_cv_results.csv"), row.names=FALSE)

# Merge with coords and write
rfcoords <- merge(ardcoords[, c("UniqueID", "ecoregion")], cv_results, by = "UniqueID")
names(rfcoords)
writeVector(rfcoords, paste0(data_dir, "saved/ranger_cv_results.gpkg"))

##### 
##### SAVE THE MODEL #####
ard_df = ard |> 
  sf::st_drop_geometry()

# Fit on all data
final_mod <- ranger::ranger(
  formula = as.formula(best_fit$important_variable_rf_formula),
  data = ard_df,
  num.trees = 1000,
  mtry = best_fit$mtry,
  min.node.size = best_fit$min.node.size,
  sample.fraction = best_fit$sample.fraction,
  seed = 20250709
)

# 2) Save it once
saveRDS(final_mod, file = file.path(data_dir, "rf_final_model.rds"))


