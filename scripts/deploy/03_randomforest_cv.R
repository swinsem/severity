source("./R/utils.R")

# Random forest cross validation

library(ranger)

data_dir <- "data/"
fig_dir <- "figs/"



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
  pref = rPref::high(r2_important_variables_overall) * rPref::high(r2_mean_important_variables)
)

top_results = r2_pareto_front |> na.omit()

best_fit = top_results[2,]


#### read ard with spatial folds from github severity/data folder
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


str(cv_results)
names(cv_results) <- c("UniqueID", "obs", "pred", "fold")

# Check results
head(cv_results)
coef_of_determin(obs=cv_results$obs, pred=cv_results$pred)
caret::RMSE(obs=cv_results$obs, pred=cv_results$pred)
caret::MAE(obs=cv_results$obs, pred=cv_results$pred)

### Categorical binning

## three classes (Miller et al 2009)
cv_results$pred_bin <- ifelse(cv_results$pred < 0.25, 1, 
                              ifelse(cv_results$pred >= 0.25 & cv_results$pred < 0.75, 2, 3))

# Could also rewrite the nested ifelse() using dplyr::case_when()
# The assignments are made in order, so just using TRUE for the last case 
# is just saying "everything else not covered by previous cases is a 3"
# cv_results <- cv_results |> 
#   dplyr::mutate(
#     pred_bin = dplyr::case_when(
#       pred < 0.25 ~ 1, 
#       pred >= 0.25 & pred < 0.75 ~ 2, 
#       TRUE ~ 3
#     )
#   )

cv_results$obs_bin <- ifelse(cv_results$obs < 0.25, 1,  
                             ifelse(cv_results$obs >= 0.25 & cv_results$obs < 0.75, 2, 3))

## five classes - overwrites above
cv_results$pred_bin <- ifelse(cv_results$pred < 0.25, 1, 
                              ifelse(cv_results$pred >= 0.25 & cv_results$pred < 0.5, 2, 
                                     ifelse(cv_results$pred >= 0.5 & cv_results$pred < 0.75, 3, 
                                            ifelse(cv_results$pred >= 0.75 & cv_results$pred < 0.9, 4, 5))))
cv_results$obs_bin <- ifelse(cv_results$obs < 0.25, 1,  
                             ifelse(cv_results$obs >= 0.25 & cv_results$obs < 0.5, 2, 
                                    ifelse(cv_results$obs >= 0.5 & cv_results$obs < 0.75, 3, 
                                           ifelse(cv_results$obs >= 0.75 & cv_results$obs < 0.9, 4, 5))))

##### 
##### SAVE THE MODEL #####

# Merge with ecoregion
cv_results <- merge(cv_results, ard_west[, c("UniqueID", "ecoregion")], by = "UniqueID")
names(cv_results)

write.csv(cv_results, paste0(data_dir, "saved/ranger_cv_results.csv"), row.names=FALSE)


# Fit on all data
final_mod <- ranger::ranger(
  formula = as.formula(best_fit$important_variable_rf_formula),
  data = ard_west,
  num.trees = 1000,
  mtry = best_fit$mtry,
  min.node.size = best_fit$min.node.size,
  sample.fraction = best_fit$sample.fraction,
  seed = 20250709
)

# 2) Save it once
saveRDS(
  object = final_mod, 
  file = here::here("data/rf_final_model.rds")
)
