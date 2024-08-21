
# Random forest cross validation

# using ard from 06_gee_exploratory

dd_cv <- ard[,!names(ard) %in% c("PlotID", "Dataset")]

# Create folds
set.seed(157) # for reproducibility
folds <- createFolds(unique(dd_cv$YrFireName), k = 5, list = TRUE, returnTrain = FALSE)

# Create a vector that will hold the fold number for each fire
fire_to_fold <- integer(length(unique(dd_cv$YrFireName)))

# Assign fold numbers
for (i in seq_along(folds)) {
  fire_to_fold[folds[[i]]] <- i
}

# Map the fold numbers back to the original dataset 
# Create a named vector where names are fires and values are folds
names(fire_to_fold) <- unique(dd_cv$YrFireName)

# Match fold numbers to each row in the original data
dd_cv$fold <- fire_to_fold[dd_cv$YrFireName]

table(dd_cv$fold) # might be very uneven


###### method to balance fold size #####
# # Calculate the number of plots for each fire
# plot_counts <- table(allmets$YrFireName)
# 
# # Convert it to a data frame
# fire_sizes <- data.frame(FireName = names(plot_counts), PlotCount = as.integer(plot_counts))
# 
# # Sort fires by size (largest to smallest)
# fire_sizes <- fire_sizes[order(-fire_sizes$PlotCount),]
# 
# # Initialize folds
# k <- 5
# folds <- vector("list", k)
# fold_sizes <- integer(k)
# 
# # Greedy assignment of fires to folds
# for (i in seq_len(nrow(fire_sizes))) {
#   # Find the fold with the minimum number of plots
#   min_fold <- which.min(fold_sizes)
#   # Assign this fire to the fold
#   folds[[min_fold]] <- c(folds[[min_fold]], fire_sizes$FireName[i])
#   # Update the size of the fold
#   fold_sizes[min_fold] <- fold_sizes[min_fold] + fire_sizes$PlotCount[i]
# }
# 
# # Now map the fold assignments back to the original dataset
# fold_assignments <- rep(NA, nrow(allmets))
# for (i in seq_along(folds)) {
#   fold_assignments[allmets$YrFireName %in% folds[[i]]] <- i
# }
# allmets$fold <- fold_assignments
# 
# # Check the balance
# table(allmets$fold)

##### cross-validation #####


dd_cv <- dd_cv[, -which(names(dd_cv) == "YrFireName")]

#all_vars_full <- names(allmets_cv)
#all_vars_full <- setdiff(all_vars_full, c("fold", "pcnt_ba_mo", "lat", "SWIR1.NIR_post", "SWIR2.NIR_post", "SWIR2.SWIR1_post")) 

## rerun with fold removed!!! and maybe lat??
#all_vars_full <- c("rbr", "dSWIR1", "dndvi", "slope", "meanAET", "dBlue", "aspectRad") # short list for testing

# cross validation function
cross_validate <- function(vars, data, fold_id) {
  results <- list() # Store results
  
  for(i in unique(fold_id)) {
    training_data <- data[data$fold != i, ]
    testing_data <- data[data$fold == i, ]
    
    # Calculate minLeaf equivalent
    minLeaf <- round(nrow(training_data) / 75 / length(vars))
    
    # Fit model
    rf_model <- randomForest(pcnt_ba_mo ~ ., data = training_data[, vars],
                             ntree = 500,
                             mtry = floor(length(vars)/3),
                             nodesize = minLeaf)
    # # Fit model
    # rf_model <- randomForest(pcnt_ba_mo ~ ., data = training_data[, c(vars, "pcnt_ba_mo")], ## pcnt_ba_mo is already there
    #                          ntree = 500,
    #                          mtry = floor(length(vars)/3),
    #                          nodesize = minLeaf)
    
    # Predict on testing data
    predictions <- predict(rf_model, testing_data)
    
    # Store results
    results[[i]] <- data.frame(
      unique_id <- testing_data$UniqueID,
      obs = testing_data$pcnt_ba_mo, 
      pred = predictions, 
      fold = i)
  }
  
  return(do.call(rbind, results))
}


r_squared <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  r_squared <- 1 - ss_res / ss_tot
  return(r_squared)
}

########################################################
###### RF with VSURF chosen variables ######

# variables selected in 06_gee_exploratory from VSURF
column_names_selected 

set.seed(444)

# Full model
full_model_results <- cross_validate(vars = column_names_selected, data = dd_cv, fold_id = dd_cv$fold)

full_r2 <- r_squared(full_model_results$obs, full_model_results$pred)
full_r2
rmse <- RMSE(pred=full_model_results$pred, obs = full_model_results$obs)
mae <- MAE(pred=full_model_results$pred, obs = full_model_results$obs)

str(full_model_results)
names(full_model_results) <- c("UniqueID", "obs", "pred", "fold")

ggplot(full_model_results) +
  geom_smooth(aes(x=obs, y=pred)) +
  geom_point(aes(x=obs, y=pred)) +
  theme_light() +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss")

full_model_results$pred_bin <- ifelse(full_model_results$pred < .25, 1, 
                                      ifelse(full_model_results$pred >=.25 & full_model_results$pred < .5, 2, 
                                             ifelse(full_model_results$pred >=.5 & full_model_results$pred < .75, 3, 
                                                    ifelse(full_model_results$pred >=.75 & full_model_results$pred < .9, 4, 5))))
full_model_results$obs_bin <- ifelse(full_model_results$obs < .25, 1, 
                                      ifelse(full_model_results$obs >=.25 & full_model_results$obs < .5, 2, 
                                             ifelse(full_model_results$obs >=.5 & full_model_results$obs < .75, 3, 
                                                    ifelse(full_model_results$obs >=.75 & full_model_results$obs < .9, 4, 5))))

write.csv(full_model_results, "VP/severity_tmp/data/saved/outputs/rf_cv5_results.csv", row.names=FALSE)

ggplot(full_model_results) +
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

table(full_model_results$obs_bin, full_model_results$pred_bin)
class_metrics <- confusionMatrix(as.factor(full_model_results$pred_bin), as.factor(full_model_results$obs_bin))$byClass
class_metrics
write.csv(class_metrics, "VP/severity_tmp/data/saved/outputs/rf_cv5_confusionmatrix.csv")

# Merge with original data
rfcoords <- merge(ardcoords[, "UniqueID"], full_model_results, by = "UniqueID")
names(rfcoords)
write.csv(rfcoords, "VP/severity_tmp/data/saved/outputs/rf_cv5_results.shp")



########################################################
###### RFs to iteratively exclude variables ######


# Set seed before running all the RFs
set.seed(444)

# Full model
full_model_results <- cross_validate(vars = all_vars_full, data = allmets_cv, fold_id = allmets_cv$fold)

full_r2 <- r_squared(full_model_results$obs, full_model_results$pred)
full_r2

# Prepare to store results
results_summary <- data.frame(Variable = character(), R2 = numeric(), R2_Difference = numeric(), stringsAsFactors = FALSE)

#var = "rbr"
# Dropping each variable and calculating R2
for (var in all_vars_full) {
  vars_minus_one <- setdiff(all_vars_full, var)
  drop_model_results <- cross_validate(vars_minus_one, data = allmets_cv, fold_id = allmets_cv$fold)
  drop_r2 <- r_squared(drop_model_results$obs, drop_model_results$pred)
  
  # Calculate the difference in R2 and store results
  r2_difference <- full_r2 - drop_r2
  results_summary <- rbind(results_summary, data.frame(Variable = var, R2 = drop_r2, R2_Difference = r2_difference))
}

results_summary

# Identifying variables to drop where the R2 drop is less than 0.005
variables_to_drop <- results_summary$Variable[results_summary$R2_Difference < 0.005]
variables_to_drop

min(results_summary$R2_Difference)

### Repeat ###
# Remove the variables to drop
remaining_vars <- setdiff(all_vars_full, variables_to_drop)
remaining_vars

# Prepare to store results
results_summary2 <- data.frame(Variable = character(), R2_Difference = numeric(), stringsAsFactors = FALSE)

# Dropping each variable and calculating R2
for (var in remaining_vars) {
  vars_minus_one <- setdiff(remaining_vars, var)
  drop_model_results <- cross_validate(vars_minus_one, data = allmets_cv, fold_id = allmets_cv$fold)
  drop_r2 <- r_squared(drop_model_results$obs, drop_model_results$pred)
  
  # Calculate the difference in R2 and store results
  r2_difference <- full_r2 - drop_r2
  results_summary2 <- rbind(results_summary2, data.frame(Variable = var, R2_Difference = r2_difference))
}

results_summary2

# Identifying variables to drop where the R2 drop is less than 0.005
variables_to_drop2 <- results_summary2$Variable[results_summary2$R2_Difference < 0.005]
variables_to_drop2


### Repeat x2 ###
# Remove the variables to drop
remaining_vars2 <- setdiff(remaining_vars, variables_to_drop2)

# Prepare to store results
results_summary3 <- data.frame(Variable = character(), R2_Difference = numeric(), stringsAsFactors = FALSE)

# Dropping each variable and calculating R2
for (var in remaining_vars2) {
  vars_minus_one <- setdiff(remaining_vars2, var)
  drop_model_results <- cross_validate(vars_minus_one, data = allmets_cv, fold_id = allmets_cv$fold)
  drop_r2 <- r_squared(drop_model_results$obs, drop_model_results$pred)
  
  # Calculate the difference in R2 and store results
  r2_difference <- full_r2 - drop_r2
  results_summary3 <- rbind(results_summary3, data.frame(Variable = var, R2_Difference = r2_difference))
}

results_summary3

# Identifying variables to drop where the R2 drop is less than 0.005
variables_to_drop3 <- results_summary3$Variable[results_summary3$R2_Difference < 0.005]
variables_to_drop3

