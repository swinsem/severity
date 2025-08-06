source("./R/utils.R")

# Random forest cross validation

library(ranger)

data_dir <- "research/severity/data/"
fig_dir <- "research/severity/figs/"



# Read data
cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))

## filter for western-us domain results
cpi_results_full <- cpi_results_full[cpi_results_full$domain == "western-us", ]

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


#### read ard with spatial folds from github severity/data folder
here::here()
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

ard_west <- ard[ard$domain=="western-us",]


##### cross-validation #####

# Run CV function
cv_results <- cross_validate(data = ard_west, hyperparameters = best_fit) 


# str(full_model_results)
str(cv_results)
names(cv_results) <- c("UniqueID", "obs", "pred", "fold")

# Check results
head(cv_results)
coef_of_determin(obs=cv_results$obs, pred=cv_results$pred)
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

## does this have ecoregion already? if so, delete vector write with coords?

write.csv(cv_results, paste0(data_dir, "saved/ranger_cv_results.csv"), row.names=FALSE)

# Merge with coords and write
# rfcoords <- merge(ardcoords[, c("UniqueID", "ecoregion")], cv_results, by = "UniqueID")
# names(rfcoords)
# writeVector(rfcoords, paste0(data_dir, "saved/ranger_cv_results.gpkg"))

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


