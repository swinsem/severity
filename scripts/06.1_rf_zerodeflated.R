library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)

ind <- read.csv("VP/severity_tmp/data/saved/GEE/zs_VIs_102723.csv")
#sdind <- read.csv("VP/severity_tmp/data/saved/GEE/zs_SD_NDVI_110123.csv") # not working!! 
topo <- read.csv("VP/severity_tmp/data/saved/GEE/zs_topo_102723.csv")

#sdind$system.index <- NULL
#sdind$.geo <- NULL
#sdind_unique <- unique(sdind)


#gridmet <- read.csv("VP/severity_tmp/data/saved/GEE/gridmet_110123.csv")
terraclim <- read.csv("VP/severity_tmp/data/saved/GEE/terraclimate_112823.csv")
names(ind)
ind$system.index <- NULL
ind$.geo <- NULL
terraclim$system.index <- NULL
terraclim$.geo <- NULL

#alldata <- merge(ind, gridmet, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "YrFireName", "pcnt_ba_mo"))
alldata <- merge(ind, terraclim, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))
names(alldata)

library(dplyr)
library(stringr)
library(ggcorrplot)

# Rename columns for gridmet and terraclimate
#alldata <- alldata %>% 
#  rename_with(~str_replace(., "\\.x$", "_gm"), ends_with(".x")) %>%
#  rename_with(~str_replace(., "\\.y$", "_tc"), ends_with(".y"))

# NPS data has pcnt ba mortality in 0-100 range, others have 0-1 range; also some NPS plots had negative ba mortality values so I set these to 0
alldata$pcnt_ba_mo <- ifelse(alldata$Dataset=="NPS", alldata$pcnt_ba_mo/100, alldata$pcnt_ba_mo)
alldata$pcnt_ba_mo <- ifelse(alldata$pcnt_ba_mo <= 0, 0, alldata$pcnt_ba_mo)

# Merge in topo data
topo$system.index <- NULL
topo$.geo <- NULL
alldata <- merge(alldata, topo, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))



ggplot(alldata, aes(x=precip0, y=meanPrecip)) +
  geom_point(alpha=.1) +
  ylab("Precipitation - mean") +
  xlab("Precipitation - year 0") +
  theme_light()

ggplot(alldata, aes(x=meanVPD, y=vpd0, color=pcnt_ba_mo)) +
  geom_point() +
  scale_color_viridis_c(option = "turbo") +
  ylab("VPD - summer of fire") +
  xlab("VPD - 30 year mean summer") +
  labs(color = "BA loss") +
  theme_light()

## Z-scores

alldata$zScorePrecip0 <- (alldata$precip0 - alldata$meanPrecip)/alldata$stdDevPrecipValue
alldata$zScoreVPD0 <- (alldata$vpd0 - alldata$meanVPD)/alldata$stdDevVPDValue
alldata$zScoreAET0 <- (alldata$aet0 - alldata$meanAET)/alldata$stdDevAETValue
alldata$zScoreCWD0 <- (alldata$cwd0 - alldata$meanCWD)/alldata$stdDevCWDValue  
alldata$zScorePrecip1 <- (alldata$precip1 - alldata$meanPrecip)/alldata$stdDevPrecipValue  
alldata$zScoreAET1 <- (alldata$aet1 - alldata$meanAET)/alldata$stdDevAETValue
alldata$zScoreCWD1 <- (alldata$cwd1 - alldata$meanCWD)/alldata$stdDevCWDValue  


names(alldata)

allmets <- alldata[,!names(alldata) %in% c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "precip0", "precip1", "vpd0", "aet0", "aet1", "cwd0", "cwd1", "stdDevAETValue", "stdDevCWDValue", "stdDevPrecipValue", "stdDevVPDValue")] #,"devi", "rbr", "dnbr"

names(allmets)
allmets <- allmets[!is.na(allmets$pcnt_ba_mo),]
corr <- round(cor(allmets[,2:30]), 1)
p.mat <- cor_pmat(allmets[,2:30])
str(allmets)
ggcorrplot(corr) 

summary(allmets$pcnt_ba_mo)

library(randomForest)
library(caret)
library(VSURF)

# Assuming your dataframe is allmets
set.seed(222)  # For reproducibility

# Subset where pcnt_ba_mo is 0
zeros_subset <- allmets[allmets$pcnt_ba_mo == 0, ]

# Randomly sample 50 rows from zeros_subset
sampled_zeros <- zeros_subset[sample(nrow(zeros_subset), 30), ]

# Subset where pcnt_ba_mo is not 0
non_zeros_subset <- allmets[allmets$pcnt_ba_mo != 0, ]

# Combine sampled_zeros with non_zeros_subset
allmets0 <- rbind(sampled_zeros, non_zeros_subset)
hist(allmets0$pcnt_ba_mo)




results_summary_0 <- data.frame(YrFireName = character(), RMSE = numeric(), Rsquared = numeric(), MAE = numeric(), stringsAsFactors = FALSE)
results_plot_0 <- data.frame(YrFireName = character(), pcnt_ba_mo = numeric(), pred = numeric(), stringsAsFactors = FALSE)

unique_fires0 <- unique(allmets0$YrFireName)
#fire = "2001 - Green Knoll"

set.seed(222)

for(fire in unique_fires0) {
  tryCatch({
    # Split the data
    train <- allmets0[allmets0$YrFireName != fire, ]
    test <- allmets0[allmets0$YrFireName == fire, ]
    
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
    predictions0 <- predict(rf_vsurf, test[, selected_vars])
    
    # Evaluate the model
    eval_res <- postResample(pred = predictions0, obs = test$pcnt_ba_mo)
    results_summary_0 <- rbind(results_summary_0, data.frame(YrFireName = fire, RMSE = eval_res[1], Rsquared = eval_res[2], MAE = eval_res[3]))
    results_plot_0 <- rbind(results_plot_0, data.frame(YrFireName = fire, pcnt_ba_mo = test$pcnt_ba_mo, pred = predictions0))
    
  }, error = function(e) {
    cat("Error in processing YrFireName:", fire, "\nError Message:", e$message, "\n")
  })
}

results_summary_0
tail(results_plot_0)
mean(results_summary$RMSE) # for comparison - with all plots and with variable selection
mean(results_summary_0$RMSE)
mean(results_summary_0$Rsquared, na.rm=TRUE)
mean(results_summary_0$MAE)


ggplot(results_plot) +
  geom_point(aes(x=pcnt_ba_mo, y=pred, color=YrFireName, alpha = .6), show.legend = FALSE) +
  theme_light() +
  ylab("RF prediction") +
  xlab("Field measurement") +
  ggtitle("Percent BA mortality")

# if this doesn't work, combine fires into a 5-fold cv so it runs faster and to avoid any issues of sample size

# without trycatch
for(fire in unique_fires) {
  # Split the data
  train <- allmets[allmets$YrFireName != fire, ]
  test <- allmets[allmets$YrFireName == fire, ]
  
  # Remove the YrFireName column
  train <- train[, -which(names(train) == "YrFireName")]
  test <- test[, -which(names(test) == "YrFireName")]
  
  # Perform variable selection
  vsurf_res <- VSURF(pcnt_ba_mo ~ ., data = train, ntree = 500)
  selected_vars <- vsurf_res$varselect.pred
  if (!1 %in% selected_vars) {
    selected_vars <- append(selected_vars, 1) # Append the index of 'pcnt_ba_mo'
  }
  #selected_vars <- append(selected_vars, 1) # add pcnt_ba_mo
  print(selected_vars)
  # Train the model
  rf_vsurf <- randomForest(pcnt_ba_mo ~ ., data = train[, selected_vars], proximity = TRUE)
  print(rf_vsurf)
  # Make predictions
  predictions <- predict(rf_vsurf, test[, selected_vars])
  print(predictions)
  # Evaluate the model
  eval_res <- postResample(pred = predictions, obs = test$pcnt_ba_mo)
  results_summary <- rbind(results, data.frame(YrFireName = fire, RMSE = eval_res[1], Rsquared = eval_res[2], MAE = eval_res[3]))
  results_plot <- rbind(results_plot, data.frame(YrFireName = fire, pcnt_ba_mo = test$pcnt_ba_mo, pred = predictions))
}

results_summary
head(results_plot)


## static sampling of 70% training, 30% testing

ind <- sample(2, nrow(allmets0), replace = TRUE, prob = c(0.7, 0.3))
train <- allmets0[ind==1,]
test <- allmets0[ind==2,]

# variable selection
vsurf_res <- VSURF(pcnt_ba_mo ~ ., data = train, ntree = 500)

plot(vsurf_res, step = "thresh")
plot(vsurf_res, step = "interp")
plot(vsurf_res, step = "pred")

selected_vars <- vsurf_res$varselect.pred
selected_vars <- append(selected_vars, 2) # add pcnt_ba_mo
rf_vsurf0 <- randomForest(pcnt_ba_mo ~ ., data = train[, selected_vars], proximity = TRUE)
predictions <- predict(rf_vsurf0, test[, selected_vars])

postResample(pred = predictions, obs = test$pcnt_ba_mo)


rf <- randomForest(pcnt_ba_mo~., data=train, proximity=TRUE) 
print(rf)
p1 <- predict(rf, train)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(rf_vsurf0,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")
importance(rf)
