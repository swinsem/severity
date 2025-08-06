source("./R/utils.R")

# Get n plots and R2 for general model (Table 1)
# Plot Figure 2
# Confusion matrix and class stats for categorical


library(terra)
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)

data_dir <- "research/severity/data/"

# Read cross-validation results with locations
cv_results <- read.csv(paste0(data_dir, "saved/ranger_cv_results.csv"))

# Number of plots per ecoregion (before combining)
table(cv_results$ecoregion)

# Recategorize some
cv_results$ecoregion <- ifelse(cv_results$ecoregion=="California interior chaparral and woodlands", "Klamath-Siskiyou forests", cv_results$ecoregion)
cv_results$ecoregion <- ifelse(cv_results$ecoregion=="Great Basin shrub steppe", "Sierra Nevada forests", cv_results$ecoregion)


# Calculate R2 for each ecoregion - Table 1
r2_table <- as.data.frame(cv_results) %>%
  group_by(ecoregion) %>%
  summarise(R2 = coef_of_determin(obs, pred), n_rows = n())
r2_table


cv_results$ecoregion <- ifelse(cv_results$ecoregion=="Colorado Plateau shrublands", "Wasatch and Uinta montane forests", cv_results$ecoregion)

# already a df?
rfeco <- as.data.frame(cv_results)

#### Plot Figure 2 ####
ggplot(rfeco) +
  geom_point(aes(x=obs, y=pred), alpha=.6) +
  geom_abline() +
  geom_smooth(aes(x=obs, y=pred), method = "loess") +
  theme_bw() +
  ylim(0, 1) +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss") +
  facet_wrap(~ ecoregion, scales = "free", 
             labeller = label_wrap_gen(width = 20))
ggsave("VP/severity_tmp/plots/rf_pred_by_ecoregion3.png", width = 8, height = 9, units = "in")


## Correlation between R2 and number of plots
cor(r2_table$R2, r2_table$n_rows)

# Interactive plotly plot - R2 vs n_rows
library(plotly)

p <- ggplot(r2_table) +
  geom_point(aes(x = n_rows, y = R2, text = ecoregion)) +  # Add 'text' aesthetic for interactivity?
  labs(y = "R-squared (R2)", x = "Number of Rows (n_rows)")  
interactive_plot <- ggplotly(p, tooltip = "text")  # Set 'tooltip' to display the 'text' aesthetic
interactive_plot


#########################################
####### obs vs pred, class stats #######
#########################################


### Plotting obs vs pred
# Both continuous
ggplot(cv_results) +
  geom_smooth(aes(x=obs, y=pred)) +
  geom_point(aes(x=obs, y=pred)) +
  theme_light() +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss")

# Categorical visualizations
ggplot(cv_results) +
  geom_boxplot(aes(x=as.factor(obs_bin), y=pred)) +
  theme_light() +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss class")
ggsave(paste0(fig_dir, "rf_pred_bins.png"), width = 8, height = 8, units = "in")

ggplot(cv_results, aes(x = obs, y = as.factor(pred_bin))) +
  geom_boxplot() +
  theme_light() +
  ylab("Predicted BA loss class") +
  xlab("Observed BA loss")


#### Confusion matrix ####
conf_mat <- table(Observed = cv_results$obs_bin, Predicted = cv_results$pred_bin)
conf_mat

conf_mat_melt <- melt(conf_mat)

# Plot CM as heatmap - ideally it would be proportion based rather than just number?? -- EDIT THIS???
ggplot(conf_mat_melt, aes(x = Predicted, y = Observed, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(legend.title=element_blank())
ggsave(paste0(fig_dir, "confusion_matrix.png"), width = 6, height = 6, units = "in")


#### Class metrics ####
class_metrics <- caret::confusionMatrix(as.factor(cv_results$pred_bin), as.factor(cv_results$obs_bin))$byClass
class_metrics
write.csv(class_metrics, paste0("saved/ranger_cv_confusionmatrix.csv"))

mean(cv_results[cv_results$pred_bin==5,]$pred)

# Calculate classification metrics - 
confusion <- confusionMatrix(as.factor(cv_results$pred_bin), as.factor(cv_results$obs_bin))
confusion

# Kappa statistic
kappa_value <- confusion$overall['Kappa']
kappa_value # weighted kappa

# Spearman's Rank Correlation
spearman_corr <- cor(cv_results$obs_bin, cv_results$pred_bin, method = "spearman")
print(spearman_corr)

# Calculate per-category accuracy
per_category_accuracy <- diag(conf_mat) / rowSums(conf_mat)
print(per_category_accuracy)


