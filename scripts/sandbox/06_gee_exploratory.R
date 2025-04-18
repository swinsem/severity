library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(terra)

ard <- read.csv("VP/severity_tmp/data/saved/ARD_nocoords.csv")

names(ard)

##### remove columns I don't want to test (keep YrFireName?)
allmets <- ard[,!names(ard) %in% c("PlotID", "UniqueID", "Dataset", "lat")] # , "lat"

names(allmets)
corr <- round(cor(allmets[,2:41]), 2)
p.mat <- cor_pmat(allmets[,2:41])
str(allmets)
ggcorrplot(corr) 

summary(allmets$pcnt_ba_mo)

unique_fires <- unique(allmets$YrFireName)

library(randomForest)
library(caret)
library(VSURF)

##################################################
##### what variables come out with all the data?
rfdata <- allmets[, -which(names(allmets) == "YrFireName")]

##### VSURF on all data
set.seed(245)
vsurf_res <- VSURF(pcnt_ba_mo ~ ., data = rfdata, ntree = 500)  
selected_vars <- vsurf_res$varselect.pred
rfdata[1, selected_vars]
selected_vars <- append(selected_vars, 1)
rf_vsurf_all <- randomForest(pcnt_ba_mo ~ ., data = rfdata[, selected_vars], proximity = TRUE, ntree = 1000)
rf_vsurf_all

column_names <- names(rfdata)
column_names_df <- data.frame(Column_Names = column_names)
column_names_selected <- names(rfdata[, selected_vars])
column_names_selected
hist(rfdata$pcnt_ba_mo)

rf_formula = glue::glue("{target} ~ {paste(column_names_selected, collapse = ' + ')}")
#pcnt_ba_mo ~ dswir2 + dswir1nir + rbr + post_swir2nir + post_nbr + zScoreCWD0 + zScoreAET1 + dswir2swir1 + zScorePrecip1 + zScorePrecip0 + meanVPD + meanPrecip + pcnt_ba_mo

varImpPlot(rf_vsurf_all,
           sort = T,
           main = "VSURF Variable Importance")






##### simple RF
rf_all <- randomForest(pcnt_ba_mo ~ ., data = rfdata, proximity = TRUE, ntree = 1000)
varImpPlot(rf_all,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

#c("rdnbr", "post_nbr", "rbr", "post_mirbi", "dnbr", "dndvi", "dndmi", "elevation", "slope", "eastness", "northness", "meanVPD", "meanAET")



#################################################################
##### RF CV with VSURF (takes a very long time to run!!)
results_summary <- data.frame(YrFireName = character(), RMSE = numeric(), Rsquared = numeric(), MAE = numeric(), stringsAsFactors = FALSE)
results_plot <- data.frame(YrFireName = character(), pcnt_ba_mo = numeric(), pred = numeric(), stringsAsFactors = FALSE)

set.seed(222)

for(fire in unique_fires) {
  tryCatch({
    # Split the data
    train <- allmets[allmets$YrFireName != fire, ]
    test <- allmets[allmets$YrFireName == fire, ]
    
    # Remove the YrFireName column
    train <- train[, -which(names(train) == "YrFireName")]
    test <- test[, -which(names(test) == "YrFireName")]
    
    # Check if train or test set is empty
    if (nrow(train) == 0 || nrow(test) == 0) {
      next # Skip to the next iteration
    }
    
    # Perform variable selection
    vsurf_res <- VSURF(pcnt_ba_mo ~ ., data = train, ntree = 500)
    selected_vars <- vsurf_res$varselect.pred
    
    # Check if selected_vars is empty
    if (length(selected_vars) == 0) {
      next # Skip to the next iteration
    }
    
    if (!1 %in% selected_vars) {
      selected_vars <- append(selected_vars, 1) # Append the index of 'pcnt_ba_mo'
    }
    
    # Train the model
    rf_vsurf <- randomForest(pcnt_ba_mo ~ ., data = train[, selected_vars], proximity = TRUE)
    
    # Make predictions
    predictions <- predict(rf_vsurf, test[, selected_vars])
    
    # Evaluate the model
    eval_res <- postResample(pred = predictions, obs = test$pcnt_ba_mo)
    results_summary <- rbind(results_summary, data.frame(YrFireName = fire, RMSE = eval_res[1], Rsquared = eval_res[2], MAE = eval_res[3]))
    results_plot <- rbind(results_plot, data.frame(YrFireName = fire, pcnt_ba_mo = test$pcnt_ba_mo, pred = predictions))
    
  }, error = function(e) {
    cat("Error in processing YrFireName:", fire, "\nError Message:", e$message, "\n")
  })
}

results_summary
head(results_plot)
mean(results_summary$RMSE)
mean(results_summary$Rsquared, na.rm=TRUE)
mean(results_summary$MAE)
# for comparison - with fewer 0 values
# results_summary_0
# tail(results_plot_0)
# mean(results_summary_0$RMSE)
# mean(results_summary_0$Rsquared, na.rm=TRUE)
# mean(results_summary_0$MAE)

ggplot(results_plot) +
  geom_point(aes(x=pcnt_ba_mo, y=pred, color=YrFireName, alpha = .6), show.legend = FALSE) +
  geom_smooth(aes(x=pcnt_ba_mo, y=pred)) +
  theme_light() +
  ylab("RF prediction") +
  xlab("Field measurement") +
  ggtitle("Percent BA mortality")
ggsave("VP/severity_tmp/plots/BAmortality_v2.png", width = 6.4, height = 6.5, units = "in")

names(rfdata)

# post_nbr, rbr, post_mirbi, dndvi, dndmi, zScoreAET1, zScorePrecip0, meanAET, zScoreCWD0, slope, eastness
## the indices have somewhat logical curves, but the climate variables are really all over the place and nonsensical. 
## climate variables may be leading to some overfitting?
ggplot(ard) +
  geom_point(aes(x=zScorePrecip0, y=pcnt_ba_mo, color=Dataset)) +
  geom_smooth(aes(x=zScorePrecip0, y=pcnt_ba_mo)) +
  theme_light()
ggplot(ard, aes(x=zScorePrecip0, y=pcnt_ba_mo)) +
  geom_point(aes(color=YrFireName)) +
  geom_smooth() +
  facet_wrap(~Dataset)+
  theme_light() +
  theme(legend.position="none")
ggsave("VP/severity_tmp/plots/BAmortality_Precip.png", width = 6.4, height = 6.5, units = "in")
# if this doesn't work, combine fires into a 5-fold cv so it runs faster and to avoid any issues of sample size



######################################################################
##### RF leave-one-fire-out without the vsurf step for each iteration
## use selected_vars from full model
results_summary_1 <- data.frame(YrFireName = character(), RMSE = numeric(), Rsquared = numeric(), MAE = numeric(), stringsAsFactors = FALSE)
results_plot_1 <- data.frame(YrFireName = character(), pcnt_ba_mo = numeric(), pred = numeric(), stringsAsFactors = FALSE)

set.seed(222)

for(fire in unique_fires) {
  tryCatch({
    # Split the data
    train <- allmets[allmets$YrFireName != fire, ]
    test <- allmets[allmets$YrFireName == fire, ]
    
    # Remove the YrFireName column
    train <- train[, -which(names(train) == "YrFireName")]
    test <- test[, -which(names(test) == "YrFireName")]
    
    # Check if train or test set is empty
    if (nrow(train) == 0 || nrow(test) == 0) {
      next # Skip to the next iteration
    }
    
    # Train the model
    rf_singlevsurf <- randomForest(pcnt_ba_mo ~ ., data = train[, c("rdnbr", "post_nbr", "rbr", "post_mirbi", "dnbr", "dndvi", "dndmi", "elevation", "slope", "eastness", "northness", "meanVPD", "meanAET", "pcnt_ba_mo")], proximity = TRUE)
    
    # Make predictions
    predictions <- predict(rf_singlevsurf, test[, c("rdnbr", "post_nbr", "rbr", "post_mirbi", "dnbr", "dndvi", "dndmi", "elevation", "slope", "eastness", "northness", "meanVPD", "meanAET", "pcnt_ba_mo")])
    
    # Evaluate the model
    eval_res_1 <- postResample(pred = predictions, obs = test$pcnt_ba_mo)
    results_summary_1 <- rbind(results_summary_1, data.frame(YrFireName = fire, RMSE = eval_res_1[1], Rsquared = eval_res_1[2], MAE = eval_res_1[3]))
    results_plot_1 <- rbind(results_plot_1, data.frame(YrFireName = fire, pcnt_ba_mo = test$pcnt_ba_mo, pred = predictions))
    
  }, error = function(e) {
    cat("Error in processing YrFireName:", fire, "\nError Message:", e$message, "\n")
  })
}

results_summary_1
head(results_plot_1)
mean(results_summary_1$RMSE)
mean(results_summary_1$Rsquared, na.rm=TRUE)
mean(results_summary_1$MAE)

ggplot(results_plot_1) +
  geom_point(aes(x=pcnt_ba_mo, y=pred, color=YrFireName, alpha = .6), show.legend = FALSE) +
  geom_smooth(aes(x=pcnt_ba_mo, y=pred)) +
  theme_light() +
  ylab("RF prediction") +
  xlab("Field measurement") +
  ggtitle("Percent BA mortality")


######################################################################
##### Plot level training and testing - don't use this! Go to 08_randomforest_cv instead

## static sampling of 70% training, 30% testing

ind <- sample(2, nrow(allmets), replace = TRUE, prob = c(0.7, 0.3))
train <- allmets[ind==1,]
test <- allmets[ind==2,]

# variable selection
vsurf_res <- VSURF(pcnt_ba_mo ~ ., data = train, ntree = 500)

plot(vsurf_res, step = "thresh")
plot(vsurf_res, step = "interp")
plot(vsurf_res, step = "pred")

selected_vars <- vsurf_res$varselect.pred
selected_vars <- append(selected_vars, 1) # add pcnt_ba_mo
rf_vsurf <- randomForest(pcnt_ba_mo ~ ., data = train[, selected_vars], proximity = TRUE)
predictions <- predict(rf_vsurf, test[, selected_vars])

postResample(pred = predictions, obs = test$pcnt_ba_mo)


rf <- randomForest(pcnt_ba_mo~., data=train, proximity=TRUE) 
print(rf)
p1 <- predict(rf, train)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(rf,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")
importance(rf)
