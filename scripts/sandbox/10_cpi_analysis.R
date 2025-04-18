
library(ggplot2)
library(dplyr)

cpiresults <- read.csv("VP/severity_tmp/data/saved/outputs/conditional-predictive-impact-results.csv")

head(cpiresults)

# lowest_rmse <- cpiresults %>%
#   group_by(mtry, sample.fraction, min.node.size) %>%  # Group by hyperparameters
#   filter(rmse_important_variables == min(rmse_important_variables)) %>%  # Filter for the lowest RMSE
#   ungroup()

lowest_rmse <- cpiresults %>%
  group_by(mtry, sample.fraction, min.node.size, important_variable_rf_formula, rmse_important_variables, r2_important_variables) %>%
  summarize()

head(lowest_rmse)

ggplot(cpiresults) +
  geom_point(aes(mtry, sample.fraction, color=rmse_important_variables))
ggplot(lowest_rmse, aes(x = mtry, y = sample.fraction, color = rmse_important_variables)) +
  geom_point(size = 3) +  # Adjust the point size as needed
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for RMSE
  labs(title = "RMSE for Different Combinations of Hyperparameters",
       x = "mtry",
       y = "Sample Fraction",
       color = "RMSE") +
  theme_minimal()


gridExtra::grid.arrange(ggplot(lowest_rmse) +
                          geom_point(aes(x=mtry, y=rmse_important_variables)) +
                          ylim(0.225, .3),
                        ggplot(lowest_rmse) +
                          geom_point(aes(x=mtry, y=r2_important_variables)) +
                          ylim(0, .5), 
                        ncol=2)
gridExtra::grid.arrange(ggplot(lowest_rmse) +
                          geom_point(aes(x=sample.fraction, y=rmse_important_variables)) +
                          ylim(0.225, .3),
                        ggplot(lowest_rmse) +
                          geom_point(aes(x=sample.fraction, y=r2_important_variables)) +
                          ylim(0, .5),
                        ncol=2)
gridExtra::grid.arrange(ggplot(lowest_rmse) +
                          geom_point(aes(x=min.node.size, y=rmse_important_variables)) +
                          ylim(0.2, .3),
                        ggplot(lowest_rmse) +
                          geom_point(aes(x=min.node.size, y=r2_important_variables)) +
                          ylim(0, .5),
                        ncol=2)

ggplot(lowest_rmse) +
  geom_point(aes(rmse_important_variables, r2_important_variables)) +
  ylim(0, .5)


bestmodels <- lowest_rmse[lowest_rmse$rmse_important_variables <.26 & lowest_rmse$r2_important_variables > .355 & !is.na(lowest_rmse$rmse_important_variables),]



