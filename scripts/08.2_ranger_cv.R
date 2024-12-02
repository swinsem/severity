
# Random forest cross validation
library(ranger)


set.seed(20240724)

# read data and set up spatial folds
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard = sf::st_read("VP/severity_tmp/data/saved/ARD_08262024.gpkg") |>
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


## using top_results from other script
best_fit = top_results[2,]


fm1 = ranger::ranger(
  formula = as.formula(best_fit$important_variable_rf_formula), 
  data = sf::st_drop_geometry(ard[, c(target, features)]), 
  num.trees = 1000, 
  mtry = best_fit$mtry, 
  min.node.size = best_fit$min.node.size, 
  sample.fraction = best_fit$sample.fraction
)

spatial_folds = unique(ard_with_spatial_folds$spatial_fold)

##### cross-validation #####

# cross validation function
cross_validate <- function(data, vars, fold_id) { # 
  results <- list() # Store results
  
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
    
    # Store results ensuring correct assignment
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



ranger_results <- cross_validate(data = ard_with_spatial_folds) 

head(ranger_results)
caret::R2(obs=ranger_results$obs, pred=ranger_results$pred)
caret::RMSE(obs=ranger_results$obs, pred=ranger_results$pred)
caret::MAE(obs=ranger_results$obs, pred=ranger_results$pred)




str(full_model_results)
names(ranger_results) <- c("UniqueID", "obs", "pred", "fold")

ggplot(ranger_results) +
  geom_smooth(aes(x=obs, y=pred)) +
  geom_point(aes(x=obs, y=pred)) +
  theme_light() +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss")


## three classes (Miller et al 2009)
ranger_results$pred_bin <- ifelse(ranger_results$pred < .25, 1, 
                                  ifelse(ranger_results$pred >=.25 & ranger_results$pred < .75, 2, 3))
ranger_results$obs_bin <- ifelse(ranger_results$obs < .25, 1,  
                                 ifelse(ranger_results$obs >=.25 & ranger_results$obs < .75, 2, 3))

## five classes
ranger_results$pred_bin <- ifelse(ranger_results$pred < .25, 1, 
                                      ifelse(ranger_results$pred >=.25 & ranger_results$pred < .5, 2, 
                                             ifelse(ranger_results$pred >=.5 & ranger_results$pred < .75, 3, 
                                                    ifelse(ranger_results$pred >=.75 & ranger_results$pred < .9, 4, 5))))
ranger_results$obs_bin <- ifelse(ranger_results$obs < .25, 1,  
                                     ifelse(ranger_results$obs >=.25 & ranger_results$obs < .5, 2, 
                                            ifelse(ranger_results$obs >=.5 & ranger_results$obs < .75, 3, 
                                                   ifelse(ranger_results$obs >=.75 & ranger_results$obs < .9, 4, 5))))

write.csv(ranger_results, "VP/severity_tmp/data/saved/outputs/ranger_cv_results.csv", row.names=FALSE)

ggplot(ranger_results) +
  geom_boxplot(aes(x=as.factor(obs_bin), y=pred)) +
  theme_light() +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss")
ggplot(full_model_results) +
  geom_boxplot(aes(x=obs, y=as.factor(pred_bin))) +
  theme_light() +
  ylab("Predicted class") +
  xlab("Observed BA loss")
ggsave("VP/severity_tmp/plots/rf_pred_bins.png", width = 8, height = 8, units = "in")

table(ranger_results$obs_bin, ranger_results$pred_bin)
class_metrics <- caret::confusionMatrix(as.factor(ranger_results$pred_bin), as.factor(ranger_results$obs_bin))$byClass
class_metrics
write.csv(class_metrics, "VP/severity_tmp/data/saved/outputs/ranger_cv_confusionmatrix.csv")

mean(ranger_results[ranger_results$pred_bin==5,]$pred)


## ChatGPT o1-preview suggestions for categorical data comparison

# Generate the confusion matrix
conf_mat <- table(Observed = ranger_results$obs_bin, Predicted = ranger_results$pred_bin)
print(conf_mat)

library(ggplot2)
library(reshape2)

# Melt the confusion matrix for plotting
conf_mat_melt <- melt(conf_mat)

# Plot the heatmap - ideally it would be proportion based rather than just number
ggplot(conf_mat_melt, aes(x = Predicted, y = Observed, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(legend.title=element_blank())
ggsave("VP/severity_tmp/plots/confusion_matrix.png", width = 6, height = 6, units = "in")

library(caret)

# Calculate classification metrics
confusion <- confusionMatrix(as.factor(ranger_results$pred_bin), as.factor(ranger_results$obs_bin))
print(confusion)

# Extract Kappa statistic
kappa_value <- confusion$overall['Kappa']
print(kappa_value)

# Calculate Weighted Kappa
weighted_kappa <- confusionMatrix(as.factor(ranger_results$pred_bin), as.factor(ranger_results$obs_bin), mode = "everything", dnn = c("Predicted", "Observed"))
print(weighted_kappa$overall['Kappa'])

# Spearman's Rank Correlation
spearman_corr <- cor(ranger_results$obs_bin, ranger_results$pred_bin, method = "spearman")
print(spearman_corr)

# Calculate per-category accuracy
per_category_accuracy <- diag(conf_mat) / rowSums(conf_mat)
print(per_category_accuracy)

ggplot(ranger_results, aes(x = obs, y = as.factor(pred_bin))) +
  geom_boxplot() +
  theme_light() +
  ylab("Predicted Class") +
  xlab("Observed BA Loss (%)") +
  labs(title = "Observed BA Loss vs. Predicted Categories")





# Merge with original data
rfcoords <- merge(ardcoords[, "UniqueID"], full_model_results, by = "UniqueID")
names(rfcoords)
write.csv(rfcoords, "VP/severity_tmp/data/saved/outputs/rf_cv5_results.shp")

